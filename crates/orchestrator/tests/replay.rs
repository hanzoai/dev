//! Byte-identical replay of every recorded corpus against the Rust decide functions.

use serde::Deserialize;
use serde_json::value::RawValue;

#[derive(Deserialize)]
struct Case {
    source: String,
    #[serde(rename = "fn")]
    name: String,
    input: Box<RawValue>,
    output: Box<RawValue>,
}

fn replay(corpus: &str, minimum: usize) -> Result<(), Box<dyn std::error::Error>> {
    let mut real = 0;
    let mut total = 0;
    for line in corpus.lines() {
        let case: Case = serde_json::from_str(line)?;
        let got = dev_orchestrator::apply(&case.name, &case.input)
            .ok_or_else(|| format!("unknown fn {}", case.name))??;
        assert_eq!(got, case.output.get(), "{} {}", case.name, case.input.get());
        real += usize::from(case.source == "real");
        total += 1;
    }
    assert!(total >= minimum && real > 0, "{total} cases, {real} real");
    Ok(())
}

#[test]
fn dispatch_mode_limit() -> Result<(), Box<dyn std::error::Error>> {
    replay(include_str!("../corpus/dispatch_mode_limit.jsonl"), 3000)
}

#[test]
fn model_switch() -> Result<(), Box<dyn std::error::Error>> {
    replay(include_str!("../corpus/model_switch.jsonl"), 1000)
}
