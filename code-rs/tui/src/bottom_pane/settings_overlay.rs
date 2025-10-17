#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum SettingsSection {
    Model,
    Theme,
    Updates,
    Agents,
    AutoDrive,
    Validation,
    Github,
    Limits,
    Chrome,
    Mcp,
    Notifications,
}

impl SettingsSection {
    pub(crate) const ALL: [SettingsSection; 11] = [
        SettingsSection::Model,
        SettingsSection::Theme,
        SettingsSection::Updates,
        SettingsSection::Agents,
        SettingsSection::AutoDrive,
        SettingsSection::Validation,
        SettingsSection::Github,
        SettingsSection::Limits,
        SettingsSection::Chrome,
        SettingsSection::Mcp,
        SettingsSection::Notifications,
    ];

    pub(crate) const fn label(self) -> &'static str {
        match self {
            SettingsSection::Model => "Model",
            SettingsSection::Theme => "Theme",
            SettingsSection::Updates => "Updates",
            SettingsSection::Agents => "Agents",
            SettingsSection::AutoDrive => "Auto Drive",
            SettingsSection::Validation => "Validation",
            SettingsSection::Github => "GitHub",
            SettingsSection::Limits => "Limits",
            SettingsSection::Chrome => "Chrome",
            SettingsSection::Mcp => "MCP",
            SettingsSection::Notifications => "Notifications",
        }
    }

    pub(crate) const fn placeholder(self) -> &'static str {
        match self {
            SettingsSection::Model => "Model settings coming soon.",
            SettingsSection::Theme => "Theme settings coming soon.",
            SettingsSection::Updates => "Upgrade Codex and manage automatic updates.",
            SettingsSection::Agents => "Agents configuration coming soon.",
            SettingsSection::AutoDrive => "Auto Drive controls coming soon.",
            SettingsSection::Validation => "Validation harness controls coming soon.",
            SettingsSection::Github => "GitHub workflow watcher controls coming soon.",
            SettingsSection::Limits => "Limits usage visualization coming soon.",
            SettingsSection::Chrome => "Chrome integration settings coming soon.",
            SettingsSection::Mcp => "MCP server management coming soon.",
            SettingsSection::Notifications => "Notification preferences coming soon.",
        }
    }

    pub(crate) fn shortcut(self) -> Option<char> {
        match self {
            SettingsSection::Model => Some('m'),
            SettingsSection::Theme => Some('t'),
            SettingsSection::Updates => Some('u'),
            SettingsSection::Agents => Some('a'),
            SettingsSection::AutoDrive => Some('d'),
            SettingsSection::Validation => Some('v'),
            SettingsSection::Github => Some('g'),
            SettingsSection::Limits => Some('l'),
            SettingsSection::Chrome => Some('c'),
            SettingsSection::Mcp => Some('p'),
            SettingsSection::Notifications => Some('n'),
        }
    }

    pub(crate) fn from_hint(value: &str) -> Option<Self> {
        match value.trim().to_ascii_lowercase().as_str() {
            "model" | "models" => Some(SettingsSection::Model),
            "theme" | "themes" => Some(SettingsSection::Theme),
            "update" | "updates" => Some(SettingsSection::Updates),
            "agent" | "agents" => Some(SettingsSection::Agents),
            "auto" | "autodrive" | "drive" => Some(SettingsSection::AutoDrive),
            "validation" | "validate" => Some(SettingsSection::Validation),
            "github" | "gh" => Some(SettingsSection::Github),
            "limit" | "limits" | "usage" => Some(SettingsSection::Limits),
            "chrome" | "browser" => Some(SettingsSection::Chrome),
            "mcp" => Some(SettingsSection::Mcp),
            "notification" | "notifications" | "notify" | "notif" => Some(SettingsSection::Notifications),
            _ => None,
        }
    }
}
