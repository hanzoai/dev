//! Role-Based Access Control (RBAC)
//!
//! Provides role and binding management with optional sled persistence.

use super::permission::{Action, Permission, Resource};
use crate::storage::KeyValueStore;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;

/// Storage trees for RBAC
const ROLES_TREE: &str = "iam:roles";
const BINDINGS_TREE: &str = "iam:bindings";

/// A role defines a set of permissions
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Role {
    /// Role name (e.g., "admin", "operator", "viewer")
    pub name: String,
    /// Human-readable description
    pub description: String,
    /// Permissions granted by this role
    pub permissions: Vec<Permission>,
    /// Roles this role inherits from
    pub inherits: Vec<String>,
}

impl Role {
    /// Create a new role
    pub fn new(name: &str, description: &str) -> Self {
        Self {
            name: name.to_string(),
            description: description.to_string(),
            permissions: vec![],
            inherits: vec![],
        }
    }

    /// Add a permission to this role
    pub fn with_permission(mut self, permission: Permission) -> Self {
        self.permissions.push(permission);
        self
    }

    /// Add multiple permissions
    pub fn with_permissions(mut self, permissions: Vec<Permission>) -> Self {
        self.permissions.extend(permissions);
        self
    }

    /// Inherit from another role
    pub fn inherits_from(mut self, role_name: &str) -> Self {
        self.inherits.push(role_name.to_string());
        self
    }

    /// Check if this role has a specific permission
    pub fn has_permission(&self, resource: &Resource, action: &Action) -> bool {
        self.permissions
            .iter()
            .any(|p| p.allows(resource, action))
    }

    /// Create the anonymous role (no permissions except health check)
    pub fn anonymous() -> Self {
        Self::new("anonymous", "Unauthenticated access")
            .with_permission(Permission::health_read())
    }

    /// Create the viewer role (read-only access)
    pub fn viewer() -> Self {
        Self::new("viewer", "Read-only access to node resources")
            .with_permissions(vec![
                Permission::health_read(),
                Permission::metrics_read(),
                Permission::deployment_read(),
                Permission::container_read(),
                Permission::logs_read(),
                Permission::events_read(),
            ])
    }

    /// Create the operator role (deploy and manage workloads)
    pub fn operator() -> Self {
        Self::new("operator", "Deploy and manage workloads")
            .inherits_from("viewer")
            .with_permissions(vec![
                Permission::deployment_manage(),
                Permission::new(Resource::Container, Action::All),
                Permission::container_exec(),
            ])
    }

    /// Create the admin role (full access)
    pub fn admin() -> Self {
        Self::new("admin", "Full administrative access")
            .with_permission(Permission::admin())
    }

    /// Create the node role (for node-to-node communication)
    pub fn node() -> Self {
        Self::new("node", "Node-to-node communication")
            .with_permissions(vec![
                Permission::health_read(),
                Permission::metrics_read(),
                Permission::new(Resource::Node, Action::All),
                Permission::events_read(),
            ])
    }
}

/// Role binding - associates an identity with a role
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RoleBinding {
    /// Unique identifier
    pub id: String,
    /// Role name to bind
    pub role: String,
    /// Subject (user/service account ID)
    pub subject: String,
    /// Subject type (user, service_account, group)
    pub subject_type: SubjectType,
    /// Namespace/scope restriction (None = cluster-wide)
    pub namespace: Option<String>,
    /// When the binding was created
    pub created_at: chrono::DateTime<chrono::Utc>,
}

/// Type of subject for role binding
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum SubjectType {
    User,
    ServiceAccount,
    Group,
    ApiKey,
}

impl RoleBinding {
    /// Create a new role binding
    pub fn new(role: &str, subject: &str, subject_type: SubjectType) -> Self {
        Self {
            id: uuid::Uuid::new_v4().to_string(),
            role: role.to_string(),
            subject: subject.to_string(),
            subject_type,
            namespace: None,
            created_at: chrono::Utc::now(),
        }
    }

    /// Restrict to a namespace
    pub fn in_namespace(mut self, namespace: &str) -> Self {
        self.namespace = Some(namespace.to_string());
        self
    }
}

/// Role manager - handles role definitions and lookups
pub struct RoleManager {
    /// Defined roles
    roles: parking_lot::RwLock<HashMap<String, Role>>,
    /// Role bindings
    bindings: parking_lot::RwLock<Vec<RoleBinding>>,
    /// Optional persistent storage
    storage: Option<Arc<dyn KeyValueStore>>,
}

impl RoleManager {
    /// Create a new role manager without storage
    pub fn new() -> Self {
        Self {
            roles: parking_lot::RwLock::new(HashMap::new()),
            bindings: parking_lot::RwLock::new(vec![]),
            storage: None,
        }
    }

    /// Create a role manager with persistent storage
    pub fn with_storage(storage: Arc<dyn KeyValueStore>) -> Self {
        let mut roles = HashMap::new();
        let mut bindings = Vec::new();

        // Load roles from storage
        if let Ok(items) = storage.kv_list(ROLES_TREE) {
            for (_, value) in items {
                if let Ok(role) = serde_json::from_slice::<Role>(&value) {
                    roles.insert(role.name.clone(), role);
                }
            }
        }

        // Load bindings from storage
        if let Ok(items) = storage.kv_list(BINDINGS_TREE) {
            for (_, value) in items {
                if let Ok(binding) = serde_json::from_slice::<RoleBinding>(&value) {
                    bindings.push(binding);
                }
            }
        }

        Self {
            roles: parking_lot::RwLock::new(roles),
            bindings: parking_lot::RwLock::new(bindings),
            storage: Some(storage),
        }
    }

    /// Create a role manager with default roles
    pub fn new_with_defaults() -> Self {
        let mut manager = Self::new();

        // Register default roles
        manager.register_role(Role::anonymous());
        manager.register_role(Role::viewer());
        manager.register_role(Role::operator());
        manager.register_role(Role::admin());
        manager.register_role(Role::node());

        manager
    }

    /// Create a role manager with default roles and persistent storage
    pub fn with_defaults_and_storage(storage: Arc<dyn KeyValueStore>) -> Self {
        let mut manager = Self::with_storage(storage);

        // Register default roles if not already present
        let has_defaults = {
            let roles = manager.roles.read();
            roles.contains_key("anonymous")
        };

        if !has_defaults {
            manager.register_role(Role::anonymous());
            manager.register_role(Role::viewer());
            manager.register_role(Role::operator());
            manager.register_role(Role::admin());
            manager.register_role(Role::node());
        }

        manager
    }

    /// Register a new role
    pub fn register_role(&mut self, role: Role) {
        // Persist to storage if available
        if let Some(ref store) = self.storage {
            if let Ok(data) = serde_json::to_vec(&role) {
                let _ = store.kv_put(ROLES_TREE, role.name.as_bytes(), &data);
            }
        }

        let mut roles = self.roles.write();
        roles.insert(role.name.clone(), role);
    }

    /// Get a role by name
    pub fn get_role(&self, name: &str) -> Option<Role> {
        let roles = self.roles.read();
        roles.get(name).cloned()
    }

    /// Get the default role for authenticated users
    pub fn default_role(&self) -> Role {
        self.get_role("viewer").unwrap_or_else(Role::viewer)
    }

    /// Get all roles
    pub fn list_roles(&self) -> Vec<Role> {
        let roles = self.roles.read();
        roles.values().cloned().collect()
    }

    /// Add a role binding
    pub fn add_binding(&mut self, binding: RoleBinding) {
        // Persist to storage if available
        if let Some(ref store) = self.storage {
            if let Ok(data) = serde_json::to_vec(&binding) {
                let _ = store.kv_put(BINDINGS_TREE, binding.id.as_bytes(), &data);
            }
        }

        let mut bindings = self.bindings.write();
        bindings.push(binding);
    }

    /// Remove a role binding by ID
    pub fn remove_binding(&mut self, binding_id: &str) -> bool {
        let mut bindings = self.bindings.write();
        let len_before = bindings.len();
        bindings.retain(|b| b.id != binding_id);
        let removed = bindings.len() < len_before;

        if removed {
            if let Some(ref store) = self.storage {
                let _ = store.kv_delete(BINDINGS_TREE, binding_id.as_bytes());
            }
        }

        removed
    }

    /// Get bindings for a subject
    pub fn get_bindings(&self, subject: &str) -> Vec<RoleBinding> {
        let bindings = self.bindings.read();
        bindings
            .iter()
            .filter(|b| b.subject == subject)
            .cloned()
            .collect()
    }

    /// Get the effective role for a subject (considering inheritance)
    pub fn effective_role(&self, subject: &str) -> Option<Role> {
        let bindings = self.get_bindings(subject);
        if bindings.is_empty() {
            return None;
        }

        // Collect all permissions from all bound roles
        let mut permissions = vec![];
        for binding in bindings {
            if let Some(role) = self.get_role(&binding.role) {
                permissions.extend(self.collect_permissions(&role));
            }
        }

        // Create a combined role
        Some(Role {
            name: format!("effective_{subject}"),
            description: format!("Effective permissions for {subject}"),
            permissions,
            inherits: vec![],
        })
    }

    /// Collect all permissions including inherited ones
    fn collect_permissions(&self, role: &Role) -> Vec<Permission> {
        let mut permissions = role.permissions.clone();

        for inherited_name in &role.inherits {
            if let Some(inherited_role) = self.get_role(inherited_name) {
                permissions.extend(self.collect_permissions(&inherited_role));
            }
        }

        permissions
    }

    /// Check if a subject has a specific permission
    pub fn has_permission(
        &self,
        subject: &str,
        resource: &Resource,
        action: &Action,
    ) -> bool {
        if let Some(role) = self.effective_role(subject) {
            role.has_permission(resource, action)
        } else {
            false
        }
    }
}

impl Default for RoleManager {
    fn default() -> Self {
        Self::new_with_defaults()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::storage::Storage;
    use tempfile::TempDir;

    #[test]
    fn test_role_permissions() {
        let viewer = Role::viewer();
        assert!(viewer.has_permission(&Resource::Health, &Action::Read));
        assert!(viewer.has_permission(&Resource::Deployment, &Action::Read));
        assert!(!viewer.has_permission(&Resource::Deployment, &Action::Create));

        let admin = Role::admin();
        assert!(admin.has_permission(&Resource::Deployment, &Action::Create));
        assert!(admin.has_permission(&Resource::Admin, &Action::Manage));
    }

    #[test]
    fn test_role_manager() {
        let mut manager = RoleManager::new_with_defaults();

        // Add a binding
        let binding = RoleBinding::new("operator", "user-123", SubjectType::User);
        manager.add_binding(binding);

        // Check effective permissions
        assert!(manager.has_permission("user-123", &Resource::Deployment, &Action::Create));
        assert!(manager.has_permission("user-123", &Resource::Health, &Action::Read));

        // Unknown user has no permissions
        assert!(!manager.has_permission("unknown", &Resource::Health, &Action::Read));
    }

    #[test]
    fn test_role_inheritance() {
        let manager = RoleManager::new_with_defaults();

        // Operator inherits from viewer
        let operator = manager.get_role("operator").expect("operator exists");
        assert!(operator.inherits.contains(&"viewer".to_string()));

        // Collect permissions should include inherited ones
        let permissions = manager.collect_permissions(&operator);

        // Should have both operator permissions (deployment:all) and viewer permissions (health:read)
        let has_deploy = permissions
            .iter()
            .any(|p| p.resource == Resource::Deployment && p.action == Action::All);
        assert!(has_deploy);

        let has_health = permissions
            .iter()
            .any(|p| p.resource == Resource::Health && p.action == Action::Read);
        assert!(has_health);
    }

    #[test]
    fn test_role_manager_with_storage() {
        let tmp = TempDir::new().unwrap();
        let storage = Arc::new(Storage::new(tmp.path()).unwrap());

        // Create manager with storage and add a custom role
        let mut manager = RoleManager::with_defaults_and_storage(storage.clone());
        let custom_role = Role::new("custom", "Custom role")
            .with_permission(Permission::health_read());
        manager.register_role(custom_role);

        // Add a binding
        let binding = RoleBinding::new("custom", "user-456", SubjectType::User);
        manager.add_binding(binding.clone());

        // Create new manager from same storage and verify data persisted
        let manager2 = RoleManager::with_storage(storage);

        let loaded_role = manager2.get_role("custom");
        assert!(loaded_role.is_some());
        assert_eq!(loaded_role.unwrap().description, "Custom role");

        let bindings = manager2.get_bindings("user-456");
        assert_eq!(bindings.len(), 1);
        assert_eq!(bindings[0].role, "custom");
    }

    #[test]
    fn test_remove_binding() {
        let tmp = TempDir::new().unwrap();
        let storage = Arc::new(Storage::new(tmp.path()).unwrap());

        let mut manager = RoleManager::with_defaults_and_storage(storage.clone());

        let binding = RoleBinding::new("viewer", "user-789", SubjectType::User);
        let binding_id = binding.id.clone();
        manager.add_binding(binding);

        // Verify binding exists
        assert_eq!(manager.get_bindings("user-789").len(), 1);

        // Remove binding
        let removed = manager.remove_binding(&binding_id);
        assert!(removed);

        // Verify binding is gone
        assert_eq!(manager.get_bindings("user-789").len(), 0);

        // Verify removed from storage too
        let stored = storage.kv_get(BINDINGS_TREE, binding_id.as_bytes()).unwrap();
        assert!(stored.is_none());
    }
}
