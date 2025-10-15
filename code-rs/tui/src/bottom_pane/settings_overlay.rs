#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum SettingsSection {
    Model,
    Theme,
    Agents,
    Limits,
    Chrome,
    Mcp,
    Notifications,
}

impl SettingsSection {
    pub(crate) const ALL: [SettingsSection; 7] = [
        SettingsSection::Model,
        SettingsSection::Theme,
        SettingsSection::Agents,
        SettingsSection::Limits,
        SettingsSection::Chrome,
        SettingsSection::Mcp,
        SettingsSection::Notifications,
    ];

    pub(crate) const fn label(self) -> &'static str {
        match self {
            SettingsSection::Model => "Model",
            SettingsSection::Theme => "Theme",
            SettingsSection::Agents => "Agents",
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
            SettingsSection::Agents => "Agents configuration coming soon.",
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
            SettingsSection::Agents => Some('a'),
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
            "agent" | "agents" => Some(SettingsSection::Agents),
            "limit" | "limits" | "usage" => Some(SettingsSection::Limits),
            "chrome" | "browser" => Some(SettingsSection::Chrome),
            "mcp" => Some(SettingsSection::Mcp),
            "notification" | "notifications" | "notify" | "notif" => Some(SettingsSection::Notifications),
            _ => None,
        }
    }
}
