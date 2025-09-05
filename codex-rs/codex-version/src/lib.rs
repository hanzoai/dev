// Generated at build time via build.rs; contains:
//   pub const CODE_VERSION: &str = "x.y.z";
include!(concat!(env!("OUT_DIR"), "/code_version.rs"));

#[inline]
pub fn version() -> &'static str {
    CODE_VERSION
}

