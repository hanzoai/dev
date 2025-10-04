//! Compile-only API surface checks for codex-core.
//! These tests intentionally reference public re-exports that must remain
//! stable for workspace consumers and external tools. They are lightweight
//! and do not execute any runtime logic.

#[allow(dead_code)]
fn assert_type<T>() {}

#[test]
fn code_core_public_api_reexports_exist() {
    // Core client and stream types must remain publicly re-exported from
    // code_core so downstream crates (tests, tools) can compile unchanged.
    assert_type::<code_core::ModelClient>();
    assert_type::<code_core::Prompt>();
    assert_type::<code_core::ResponseEvent>();
    assert_type::<code_core::ResponseStream>();
}

#[test]
fn code_core_protocol_models_are_exposed() {
    // The models namespace should remain accessible via code_core::models
    // to keep imports stable in TUI/tests.
    assert_type::<code_core::models::ResponseItem>();
}

