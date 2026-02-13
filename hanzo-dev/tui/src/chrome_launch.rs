#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ChromeLaunchOption {
    CloseAndUseProfile,
    UseTempProfile,
    UseInternalBrowser,
    Cancel,
}

pub const CHROME_LAUNCH_CHOICES: &[(ChromeLaunchOption, &str, &str)] = &[
    (
        ChromeLaunchOption::CloseAndUseProfile,
        "Close existing browser & use your profile",
        "Closes running browser and launches with your profile",
    ),
    (
        ChromeLaunchOption::UseTempProfile,
        "Use temporary profile",
        "Launches browser with a clean profile (no saved logins)",
    ),
    (
        ChromeLaunchOption::UseInternalBrowser,
        "Use built-in browser (/browser)",
        "Uses the built-in headless browser",
    ),
    (
        ChromeLaunchOption::Cancel,
        "Cancel",
        "Don't launch any browser",
    ),
];
