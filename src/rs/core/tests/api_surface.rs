//! Compile-only API surface checks for codex-core.
//! These tests intentionally reference public re-exports that must remain
//! stable for workspace consumers and external tools. They are lightweight
//! and do not execute any runtime logic.

#[allow(dead_code)]
fn assert_type<T>() {}

#[test]
fn hanzo_dev_public_api_reexports_exist() {
    // Core client and stream types must remain publicly re-exported from
    // hanzo_dev so downstream crates (tests, tools) can compile unchanged.
    assert_type::<hanzo_dev::ModelClient>();
    assert_type::<hanzo_dev::Prompt>();
    assert_type::<hanzo_dev::ResponseEvent>();
    assert_type::<hanzo_dev::ResponseStream>();
}

#[test]
fn hanzo_dev_protocol_models_are_exposed() {
    // The models namespace should remain accessible via hanzo_dev::models
    // to keep imports stable in TUI/tests.
    assert_type::<hanzo_dev::models::ResponseItem>();
}

