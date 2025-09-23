//! Unit tests for CLI module

#[cfg(test)]
mod tests {
    use crate::cli::*;
    use clap::Parser;

    #[test]
    fn test_cli_parsing_default() {
        let args = vec!["hanzod"];
        let cli = Cli::try_parse_from(args);
        assert!(cli.is_ok());

        let cli = cli.unwrap();
        assert_eq!(cli.port, 8080); // Default port
        assert_eq!(cli.host, "0.0.0.0"); // Default host
    }

    #[test]
    fn test_cli_parsing_with_port() {
        let args = vec!["hanzod", "--port", "3000"];
        let cli = Cli::try_parse_from(args);
        assert!(cli.is_ok());

        let cli = cli.unwrap();
        assert_eq!(cli.port, 3000);
    }

    #[test]
    fn test_cli_parsing_with_host() {
        let args = vec!["hanzod", "--host", "127.0.0.1"];
        let cli = Cli::try_parse_from(args);
        assert!(cli.is_ok());

        let cli = cli.unwrap();
        assert_eq!(cli.host, "127.0.0.1");
    }

    #[test]
    fn test_cli_parsing_verbose() {
        let args = vec!["hanzod", "-v"];
        let cli = Cli::try_parse_from(args);
        assert!(cli.is_ok());

        let cli = cli.unwrap();
        assert_eq!(cli.verbose, 1);

        let args = vec!["hanzod", "-vvv"];
        let cli = Cli::try_parse_from(args);
        assert!(cli.is_ok());

        let cli = cli.unwrap();
        assert_eq!(cli.verbose, 3);
    }

    #[test]
    fn test_cli_parsing_config_file() {
        let args = vec!["hanzod", "--config", "/etc/hanzod/config.toml"];
        let cli = Cli::try_parse_from(args);
        assert!(cli.is_ok());

        let cli = cli.unwrap();
        assert_eq!(cli.config, Some("/etc/hanzod/config.toml".into()));
    }

    #[test]
    fn test_cli_parsing_features() {
        let args = vec!["hanzod", "--enable-blockchain", "--enable-marketplace"];
        let cli = Cli::try_parse_from(args);
        assert!(cli.is_ok());

        let cli = cli.unwrap();
        assert!(cli.enable_blockchain);
        assert!(cli.enable_marketplace);
        assert!(!cli.enable_quantum_staking); // Not enabled
    }

    #[test]
    fn test_cli_parsing_all_features() {
        let args = vec![
            "hanzod",
            "--enable-blockchain",
            "--enable-marketplace",
            "--enable-quantum-staking",
        ];
        let cli = Cli::try_parse_from(args);
        assert!(cli.is_ok());

        let cli = cli.unwrap();
        assert!(cli.enable_blockchain);
        assert!(cli.enable_marketplace);
        assert!(cli.enable_quantum_staking);
    }

    #[test]
    fn test_cli_parsing_data_dir() {
        let args = vec!["hanzod", "--data-dir", "/var/lib/hanzod"];
        let cli = Cli::try_parse_from(args);
        assert!(cli.is_ok());

        let cli = cli.unwrap();
        assert_eq!(cli.data_dir, Some("/var/lib/hanzod".into()));
    }

    #[test]
    fn test_cli_parsing_workers() {
        let args = vec!["hanzod", "--workers", "8"];
        let cli = Cli::try_parse_from(args);
        assert!(cli.is_ok());

        let cli = cli.unwrap();
        assert_eq!(cli.workers, Some(8));
    }

    #[test]
    fn test_cli_parsing_combined() {
        let args = vec![
            "hanzod",
            "--host", "192.168.1.100",
            "--port", "9000",
            "--config", "custom.toml",
            "-vv",
            "--enable-blockchain",
            "--workers", "4",
        ];
        let cli = Cli::try_parse_from(args);
        assert!(cli.is_ok());

        let cli = cli.unwrap();
        assert_eq!(cli.host, "192.168.1.100");
        assert_eq!(cli.port, 9000);
        assert_eq!(cli.config, Some("custom.toml".into()));
        assert_eq!(cli.verbose, 2);
        assert!(cli.enable_blockchain);
        assert_eq!(cli.workers, Some(4));
    }

    #[test]
    fn test_cli_parsing_invalid_port() {
        let args = vec!["hanzod", "--port", "not_a_number"];
        let cli = Cli::try_parse_from(args);
        assert!(cli.is_err());
    }

    #[test]
    fn test_cli_parsing_help() {
        let args = vec!["hanzod", "--help"];
        let cli = Cli::try_parse_from(args);
        // Help causes early exit, which is an error for try_parse_from
        assert!(cli.is_err());
    }

    #[test]
    fn test_cli_parsing_version() {
        let args = vec!["hanzod", "--version"];
        let cli = Cli::try_parse_from(args);
        // Version causes early exit, which is an error for try_parse_from
        assert!(cli.is_err());
    }
}