extern crate dev_execpolicy;

use std::vec;

use dev_execpolicy::Error;
use dev_execpolicy::ExecCall;
use dev_execpolicy::MatchedExec;
use dev_execpolicy::MatchedFlag;
use dev_execpolicy::Policy;
use dev_execpolicy::PositionalArg;
use dev_execpolicy::ValidExec;
use dev_execpolicy::get_default_policy;

#[expect(clippy::expect_used)]
fn setup() -> Policy {
    get_default_policy().expect("failed to load default policy")
}

#[test]
fn test_pwd_no_args() {
    let policy = setup();
    let pwd = ExecCall::new("pwd", &[]);
    assert_eq!(
        Ok(MatchedExec::Match {
            exec: ValidExec {
                program: "pwd".into(),
                ..Default::default()
            }
        }),
        policy.check(&pwd)
    );
}

#[test]
fn test_pwd_capital_l() {
    let policy = setup();
    let pwd = ExecCall::new("pwd", &["-L"]);
    assert_eq!(
        Ok(MatchedExec::Match {
            exec: ValidExec {
                program: "pwd".into(),
                flags: vec![MatchedFlag::new("-L")],
                ..Default::default()
            }
        }),
        policy.check(&pwd)
    );
}

#[test]
fn test_pwd_capital_p() {
    let policy = setup();
    let pwd = ExecCall::new("pwd", &["-P"]);
    assert_eq!(
        Ok(MatchedExec::Match {
            exec: ValidExec {
                program: "pwd".into(),
                flags: vec![MatchedFlag::new("-P")],
                ..Default::default()
            }
        }),
        policy.check(&pwd)
    );
}

#[test]
fn test_pwd_extra_args() {
    let policy = setup();
    let pwd = ExecCall::new("pwd", &["foo", "bar"]);
    assert_eq!(
        Err(Error::UnexpectedArguments {
            program: "pwd".to_string(),
            args: vec![
                PositionalArg {
                    index: 0,
                    value: "foo".to_string()
                },
                PositionalArg {
                    index: 1,
                    value: "bar".to_string()
                },
            ],
        }),
        policy.check(&pwd)
    );
}
