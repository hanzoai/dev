use dev_execpolicy::ArgType;
use dev_execpolicy::Error;
use dev_execpolicy::ExecCall;
use dev_execpolicy::MatchedArg;
use dev_execpolicy::MatchedExec;
use dev_execpolicy::PolicyParser;
use dev_execpolicy::Result;
use dev_execpolicy::ValidExec;

extern crate dev_execpolicy;

#[test]
fn test_invalid_subcommand() -> Result<()> {
    let unparsed_policy = r#"
define_program(
    program="fake_executable",
    args=["subcommand", "sub-subcommand"],
)
"#;
    let parser = PolicyParser::new("test_invalid_subcommand", unparsed_policy);
    let policy = parser.parse().expect("failed to parse policy");
    let valid_call = ExecCall::new("fake_executable", &["subcommand", "sub-subcommand"]);
    assert_eq!(
        Ok(MatchedExec::Match {
            exec: ValidExec::new(
                "fake_executable",
                vec![
                    MatchedArg::new(0, ArgType::Literal("subcommand".to_string()), "subcommand")?,
                    MatchedArg::new(
                        1,
                        ArgType::Literal("sub-subcommand".to_string()),
                        "sub-subcommand"
                    )?,
                ],
                &[]
            )
        }),
        policy.check(&valid_call)
    );

    let invalid_call = ExecCall::new("fake_executable", &["subcommand", "not-a-real-subcommand"]);
    assert_eq!(
        Err(Error::LiteralValueDidNotMatch {
            expected: "sub-subcommand".to_string(),
            actual: "not-a-real-subcommand".to_string()
        }),
        policy.check(&invalid_call)
    );
    Ok(())
}
