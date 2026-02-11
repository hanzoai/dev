// Compile-time embedded version string.
// Prefer the CODE_VERSION provided by CI; fall back to the package
// version for local builds.
pub const CODE_VERSION: &str = {
    match option_env!("CODE_VERSION") {
        Some(v) => v,
        None => env!("CARGO_PKG_VERSION"),
    }
};

// Human-readable build metadata for diagnostics.
// This may include tag/hash/date and is intentionally separate from
// CODE_VERSION so semantic version checks remain stable.
pub const CODE_VERSION_LONG: &str = {
    match option_env!("CODE_VERSION_LONG") {
        Some(v) => v,
        None => CODE_VERSION,
    }
};

pub const CODE_GIT_DESCRIBE: &str = {
    match option_env!("CODE_GIT_DESCRIBE") {
        Some(v) => v,
        None => "",
    }
};

pub const CODE_GIT_SHA: &str = {
    match option_env!("CODE_GIT_SHA") {
        Some(v) => v,
        None => "",
    }
};

pub const CODE_GIT_DATE: &str = {
    match option_env!("CODE_GIT_DATE") {
        Some(v) => v,
        None => "",
    }
};

#[inline]
pub fn version() -> &'static str {
    CODE_VERSION
}

#[inline]
pub fn long_version() -> &'static str {
    CODE_VERSION_LONG
}

#[inline]
pub fn git_describe() -> Option<&'static str> {
    if CODE_GIT_DESCRIBE.is_empty() {
        None
    } else {
        Some(CODE_GIT_DESCRIBE)
    }
}

#[inline]
pub fn git_sha() -> Option<&'static str> {
    if CODE_GIT_SHA.is_empty() {
        None
    } else {
        Some(CODE_GIT_SHA)
    }
}

#[inline]
pub fn git_date() -> Option<&'static str> {
    if CODE_GIT_DATE.is_empty() {
        None
    } else {
        Some(CODE_GIT_DATE)
    }
}
