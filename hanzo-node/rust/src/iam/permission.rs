//! Permission types for RBAC

use serde::{Deserialize, Serialize};
use std::fmt;

/// Resource types that can be protected
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Resource {
    /// Node health and status
    Health,
    /// Node metrics
    Metrics,
    /// Node registration and lifecycle
    Node,
    /// Deployments
    Deployment,
    /// Containers
    Container,
    /// Logs
    Logs,
    /// Events
    Events,
    /// API keys (administrative)
    ApiKey,
    /// Users and roles (administrative)
    User,
    /// System administration
    Admin,
    /// All resources (wildcard)
    All,
}

impl Resource {
    /// Parse resource from string
    pub fn from_str(s: &str) -> Option<Self> {
        match s.to_lowercase().as_str() {
            "health" => Some(Self::Health),
            "metrics" => Some(Self::Metrics),
            "node" | "nodes" => Some(Self::Node),
            "deployment" | "deployments" => Some(Self::Deployment),
            "container" | "containers" => Some(Self::Container),
            "log" | "logs" => Some(Self::Logs),
            "event" | "events" => Some(Self::Events),
            "apikey" | "api_key" | "api-key" => Some(Self::ApiKey),
            "user" | "users" => Some(Self::User),
            "admin" => Some(Self::Admin),
            "*" | "all" => Some(Self::All),
            _ => None,
        }
    }

    /// Check if this resource matches another (considering wildcards)
    pub fn matches(&self, other: &Self) -> bool {
        *self == Self::All || *other == Self::All || *self == *other
    }
}

impl fmt::Display for Resource {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Self::Health => "health",
            Self::Metrics => "metrics",
            Self::Node => "node",
            Self::Deployment => "deployment",
            Self::Container => "container",
            Self::Logs => "logs",
            Self::Events => "events",
            Self::ApiKey => "api_key",
            Self::User => "user",
            Self::Admin => "admin",
            Self::All => "*",
        };
        write!(f, "{s}")
    }
}

/// Actions that can be performed on resources
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Action {
    /// Read/view resource
    Read,
    /// Create new resource
    Create,
    /// Update existing resource
    Update,
    /// Delete resource
    Delete,
    /// Execute operations (e.g., exec in container)
    Execute,
    /// Manage resource (administrative actions)
    Manage,
    /// All actions (wildcard)
    All,
}

impl Action {
    /// Parse action from string
    pub fn from_str(s: &str) -> Option<Self> {
        match s.to_lowercase().as_str() {
            "read" | "get" | "list" | "view" => Some(Self::Read),
            "create" | "post" | "add" => Some(Self::Create),
            "update" | "put" | "patch" | "edit" => Some(Self::Update),
            "delete" | "remove" | "destroy" => Some(Self::Delete),
            "execute" | "exec" | "run" => Some(Self::Execute),
            "manage" | "admin" => Some(Self::Manage),
            "*" | "all" => Some(Self::All),
            _ => None,
        }
    }

    /// Check if this action matches another (considering wildcards)
    pub fn matches(&self, other: &Self) -> bool {
        *self == Self::All || *other == Self::All || *self == *other
    }
}

impl fmt::Display for Action {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Self::Read => "read",
            Self::Create => "create",
            Self::Update => "update",
            Self::Delete => "delete",
            Self::Execute => "execute",
            Self::Manage => "manage",
            Self::All => "*",
        };
        write!(f, "{s}")
    }
}

/// A permission grants an action on a resource
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Permission {
    /// The resource this permission applies to
    pub resource: Resource,
    /// The action allowed
    pub action: Action,
    /// Optional condition (e.g., "owner" = only own resources)
    pub condition: Option<String>,
}

impl Permission {
    /// Create a new permission
    pub fn new(resource: Resource, action: Action) -> Self {
        Self {
            resource,
            action,
            condition: None,
        }
    }

    /// Create permission with condition
    pub fn with_condition(resource: Resource, action: Action, condition: &str) -> Self {
        Self {
            resource,
            action,
            condition: Some(condition.to_string()),
        }
    }

    /// Parse permission from string (e.g., "deployment:read", "*:*")
    pub fn from_str(s: &str) -> Option<Self> {
        let parts: Vec<&str> = s.split(':').collect();
        match parts.len() {
            2 => {
                let resource = Resource::from_str(parts[0])?;
                let action = Action::from_str(parts[1])?;
                Some(Self::new(resource, action))
            }
            3 => {
                let resource = Resource::from_str(parts[0])?;
                let action = Action::from_str(parts[1])?;
                Some(Self::with_condition(resource, action, parts[2]))
            }
            _ => None,
        }
    }

    /// Check if this permission grants access for a resource/action pair
    pub fn allows(&self, resource: &Resource, action: &Action) -> bool {
        self.resource.matches(resource) && self.action.matches(action)
    }
}

impl fmt::Display for Permission {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.condition {
            Some(cond) => write!(f, "{}:{}:{}", self.resource, self.action, cond),
            None => write!(f, "{}:{}", self.resource, self.action),
        }
    }
}

/// Predefined permissions for common operations
impl Permission {
    /// Read health status
    pub fn health_read() -> Self {
        Self::new(Resource::Health, Action::Read)
    }

    /// Read metrics
    pub fn metrics_read() -> Self {
        Self::new(Resource::Metrics, Action::Read)
    }

    /// Read deployments
    pub fn deployment_read() -> Self {
        Self::new(Resource::Deployment, Action::Read)
    }

    /// Manage deployments (CRUD)
    pub fn deployment_manage() -> Self {
        Self::new(Resource::Deployment, Action::All)
    }

    /// Read containers
    pub fn container_read() -> Self {
        Self::new(Resource::Container, Action::Read)
    }

    /// Execute in containers
    pub fn container_exec() -> Self {
        Self::new(Resource::Container, Action::Execute)
    }

    /// Read logs
    pub fn logs_read() -> Self {
        Self::new(Resource::Logs, Action::Read)
    }

    /// Read events
    pub fn events_read() -> Self {
        Self::new(Resource::Events, Action::Read)
    }

    /// Full admin access
    pub fn admin() -> Self {
        Self::new(Resource::All, Action::All)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_permission_parsing() {
        let perm = Permission::from_str("deployment:read").expect("parse failed");
        assert_eq!(perm.resource, Resource::Deployment);
        assert_eq!(perm.action, Action::Read);

        let perm = Permission::from_str("*:*").expect("parse failed");
        assert_eq!(perm.resource, Resource::All);
        assert_eq!(perm.action, Action::All);
    }

    #[test]
    fn test_permission_allows() {
        let admin = Permission::admin();
        assert!(admin.allows(&Resource::Deployment, &Action::Create));
        assert!(admin.allows(&Resource::Container, &Action::Delete));

        let read_only = Permission::new(Resource::Deployment, Action::Read);
        assert!(read_only.allows(&Resource::Deployment, &Action::Read));
        assert!(!read_only.allows(&Resource::Deployment, &Action::Create));
    }

    #[test]
    fn test_resource_matching() {
        assert!(Resource::All.matches(&Resource::Deployment));
        assert!(Resource::Deployment.matches(&Resource::All));
        assert!(Resource::Deployment.matches(&Resource::Deployment));
        assert!(!Resource::Deployment.matches(&Resource::Container));
    }
}
