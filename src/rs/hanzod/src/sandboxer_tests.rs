//! Unit tests for sandboxer module

#[cfg(test)]
mod tests {
    use crate::sandboxer::*;

    #[test]
    fn test_sandboxer_creation() {
        let sandboxer = Sandboxer::new();
        assert!(sandboxer.is_enabled());
    }

    #[test]
    fn test_sandbox_policy() {
        let policy = SandboxPolicy {
            allow_network: false,
            allow_filesystem: FilesystemPolicy::ReadOnly,
            allow_process_creation: false,
            memory_limit: 512 * 1024 * 1024,
            cpu_limit: 1.0,
            time_limit: Some(300), // 5 minutes
        };

        assert!(!policy.allow_network);
        assert_eq!(policy.allow_filesystem, FilesystemPolicy::ReadOnly);
        assert!(!policy.allow_process_creation);
    }

    #[test]
    fn test_security_context() {
        let context = SecurityContext {
            user_id: 1000,
            group_id: 1000,
            supplementary_groups: vec![100, 101],
            no_new_privileges: true,
            read_only_root_filesystem: true,
            capabilities: Capabilities {
                add: vec![],
                drop: vec!["ALL".to_string()],
            },
        };

        assert_eq!(context.user_id, 1000);
        assert!(context.no_new_privileges);
        assert!(context.read_only_root_filesystem);
        assert_eq!(context.capabilities.drop[0], "ALL");
    }

    #[test]
    fn test_filesystem_policy() {
        let fs_policy = FilesystemPolicy::Custom {
            read_paths: vec!["/data".to_string(), "/models".to_string()],
            write_paths: vec!["/tmp".to_string(), "/output".to_string()],
            exec_paths: vec![],
        };

        match fs_policy {
            FilesystemPolicy::Custom { read_paths, write_paths, exec_paths } => {
                assert_eq!(read_paths.len(), 2);
                assert_eq!(write_paths.len(), 2);
                assert_eq!(exec_paths.len(), 0);
            }
            _ => panic!("Expected Custom filesystem policy"),
        }
    }

    #[test]
    fn test_network_policy() {
        let network = NetworkPolicy {
            enabled: true,
            allowed_hosts: vec!["api.hanzo.ai".to_string(), "localhost".to_string()],
            allowed_ports: vec![80, 443, 8080],
            deny_all: false,
        };

        assert!(network.enabled);
        assert_eq!(network.allowed_hosts.len(), 2);
        assert_eq!(network.allowed_ports.len(), 3);
        assert!(!network.deny_all);
    }

    #[test]
    fn test_syscall_filter() {
        let filter = SyscallFilter {
            mode: FilterMode::Whitelist,
            syscalls: vec![
                "read".to_string(),
                "write".to_string(),
                "open".to_string(),
                "close".to_string(),
                "mmap".to_string(),
            ],
        };

        assert_eq!(filter.mode, FilterMode::Whitelist);
        assert_eq!(filter.syscalls.len(), 5);
        assert!(filter.syscalls.contains(&"read".to_string()));
    }

    #[test]
    fn test_resource_limits_enforcement() {
        let limits = ResourceLimits {
            memory_bytes: 1024 * 1024 * 1024, // 1GB
            cpu_shares: 512,
            pids_max: 100,
            open_files_max: 256,
            core_dump_disabled: true,
        };

        assert_eq!(limits.memory_bytes, 1024 * 1024 * 1024);
        assert_eq!(limits.cpu_shares, 512);
        assert_eq!(limits.pids_max, 100);
        assert!(limits.core_dump_disabled);
    }

    #[test]
    fn test_sandbox_isolation_levels() {
        let minimal = IsolationLevel::Minimal;
        let standard = IsolationLevel::Standard;
        let strict = IsolationLevel::Strict;
        let paranoid = IsolationLevel::Paranoid;

        // Test ordering (paranoid > strict > standard > minimal)
        assert!(paranoid > strict);
        assert!(strict > standard);
        assert!(standard > minimal);
    }

    #[test]
    fn test_audit_logging() {
        let audit = AuditConfig {
            enabled: true,
            log_syscalls: true,
            log_network: true,
            log_filesystem: true,
            log_process: true,
            output_path: "/var/log/sandboxer/audit.log".to_string(),
        };

        assert!(audit.enabled);
        assert!(audit.log_syscalls);
        assert!(audit.log_network);
        assert!(audit.log_filesystem);
        assert!(audit.log_process);
    }

    #[test]
    fn test_sandbox_validation() {
        let mut sandboxer = Sandboxer::new();

        let policy = SandboxPolicy {
            allow_network: false,
            allow_filesystem: FilesystemPolicy::None,
            allow_process_creation: false,
            memory_limit: 256 * 1024 * 1024,
            cpu_limit: 0.5,
            time_limit: Some(60),
        };

        let result = sandboxer.validate_policy(&policy);
        assert!(result.is_ok());

        // Test invalid policy
        let invalid_policy = SandboxPolicy {
            allow_network: true,
            allow_filesystem: FilesystemPolicy::Full, // Too permissive with network
            allow_process_creation: true,
            memory_limit: 0, // Invalid
            cpu_limit: -1.0, // Invalid
            time_limit: None,
        };

        let result = sandboxer.validate_policy(&invalid_policy);
        assert!(result.is_err());
    }

    #[test]
    fn test_namespace_configuration() {
        let namespaces = NamespaceConfig {
            mount: true,
            uts: true,
            ipc: true,
            pid: true,
            network: true,
            user: true,
            cgroup: true,
        };

        assert!(namespaces.mount);
        assert!(namespaces.uts);
        assert!(namespaces.ipc);
        assert!(namespaces.pid);
        assert!(namespaces.network);
        assert!(namespaces.user);
        assert!(namespaces.cgroup);
    }

    #[test]
    fn test_apparmor_profile() {
        let apparmor = AppArmorProfile {
            enabled: true,
            profile_name: "hanzo-sandboxer".to_string(),
            profile_path: "/etc/apparmor.d/hanzo-sandboxer".to_string(),
            mode: AppArmorMode::Enforce,
        };

        assert!(apparmor.enabled);
        assert_eq!(apparmor.profile_name, "hanzo-sandboxer");
        assert_eq!(apparmor.mode, AppArmorMode::Enforce);
    }

    #[test]
    fn test_seccomp_profile() {
        let seccomp = SeccompProfile {
            enabled: true,
            default_action: SeccompAction::Kill,
            architectures: vec!["x86_64".to_string(), "aarch64".to_string()],
            allowed_syscalls: vec![
                "read".to_string(),
                "write".to_string(),
                "exit".to_string(),
                "exit_group".to_string(),
            ],
        };

        assert!(seccomp.enabled);
        assert_eq!(seccomp.default_action, SeccompAction::Kill);
        assert_eq!(seccomp.architectures.len(), 2);
        assert_eq!(seccomp.allowed_syscalls.len(), 4);
    }
}