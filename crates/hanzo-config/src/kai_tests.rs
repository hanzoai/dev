use super::*;

#[test]
fn no_file_means_no_kai_in_the_loop() {
    let home = tempfile::tempdir().unwrap();
    assert_eq!(Kai::load(home.path()).unwrap(), None);
}

#[test]
fn every_operation_defaults_to_shadow_on_its_shipped_program() {
    let home = tempfile::tempdir().unwrap();
    let kai = Kai::parse("", home.path()).unwrap();
    for op in Operation::ALL {
        assert_eq!(kai.op(op).mode, Mode::Shadow, "{}", op.name());
        assert_eq!(kai.op(op).program, op.program());
    }
    assert_eq!(kai.op(Operation::Tools).k, Some(8));
    assert_eq!(kai.op(Operation::Context).k, Some(4));
    assert_eq!(kai.op(Operation::Risk).k, None);
    assert_eq!(kai.url, "https://api.hanzo.ai/v1");
    assert_eq!(kai.model, "laya-agent");
    assert_eq!(kai.trace, home.path().join("kai/decisions.jsonl"));
}

#[test]
fn an_operation_takes_its_program_mode_thresholds_and_k() {
    let home = tempfile::tempdir().unwrap();
    std::fs::write(
        home.path().join(FILE),
        r#"
url = "http://127.0.0.1:8080/v1/"
model = "kai"
trace = "/var/log/kai.jsonl"

[ops.risk]
program = "action.risk@1"
mode = "enforced"
thresholds = { verdict = 0.7 }

[ops.tools]
mode = "advisory"
k = 3
"#,
    )
    .unwrap();
    let kai = Kai::load(home.path()).unwrap().unwrap();
    let risk = kai.op(Operation::Risk);
    assert_eq!(risk.program, "action.risk@1");
    assert_eq!(risk.mode, Mode::Enforced);
    assert_eq!(risk.thresholds.get("verdict"), Some(&0.7));
    assert_eq!(kai.op(Operation::Tools).mode, Mode::Advisory);
    assert_eq!(kai.op(Operation::Tools).k, Some(3));
    assert_eq!(kai.op(Operation::Model).mode, Mode::Shadow);
    assert_eq!(kai.url, "http://127.0.0.1:8080/v1");
    assert_eq!(kai.model, "kai");
    assert_eq!(kai.trace, PathBuf::from("/var/log/kai.jsonl"));
}

#[test]
fn an_unknown_operation_or_mode_is_refused() {
    let home = tempfile::tempdir().unwrap();
    let error = Kai::parse("[ops.approve]\nmode = \"enforced\"", home.path()).unwrap_err();
    assert!(
        error.to_string().contains("no operation \"approve\""),
        "{error}"
    );
    assert!(Kai::parse("[ops.risk]\nmode = \"strict\"", home.path()).is_err());
    assert!(Kai::parse("[ops.risk]\nmodel = \"x\"", home.path()).is_err());
}
