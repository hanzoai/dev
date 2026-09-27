//! Programs: the typed questions Dev puts to Kai about one state, and the gate that reads the
//! answers.
//!
//! A program is the Decisions API's agent form: questions in the wire shape (`noul`, `choice`,
//! `score`), a threshold per question, an optional verdict rule, and the calibration its
//! thresholds were set against. The gate turns each answer into a signal: a noul holds at
//! P(true) >= its threshold (default 0.5); a choice or score is accepted at the probability of
//! its likeliest option >= its threshold (default 0). A verdict rule takes Kai's verdict from
//! an accepted choice labelled allow/ask/deny and raises it by every escalation whose noul
//! holds. The programs Dev asks ship in `programs/`; a path names any other.

use hanzo_loop::Verdict;
use indexmap::IndexMap;
use serde::Deserialize;
use serde::Serialize;
use serde_json::Value;
use sha2::Digest;
use sha2::Sha256;

/// The programs Dev ships, by id.
pub const SHIPPED: &[&str] = &[
    include_str!("../programs/tools.select@1.json"),
    include_str!("../programs/agent.command-risk@1.json"),
    include_str!("../programs/router.model@1.json"),
    include_str!("../programs/reasoning.budget@1.json"),
    include_str!("../programs/context.select@1.json"),
    include_str!("../programs/agent.progress@1.json"),
    include_str!("../programs/agent.complete@1.json"),
    include_str!("../programs/fix.pick@1.json"),
    include_str!("../programs/fix.next@1.json"),
];

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Kind {
    Noul,
    Choice,
    Score,
}

/// A question in the Decisions wire shape.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Question {
    #[serde(rename = "type")]
    pub kind: Kind,
    pub instructions: String,
    /// Choice: label -> description. Score: level descriptions, index 0 first. Noul: optional
    /// descriptions keyed `true`/`false`.
    #[serde(default, skip_serializing_if = "Value::is_null")]
    pub criteria: Value,
}

impl Question {
    /// Option keys in option order: a choice's labels, a score's levels `"0"`, `"1"`, …, a
    /// noul's `"false"`, `"true"`.
    pub fn keys(&self) -> Vec<String> {
        match self.kind {
            Kind::Choice => match &self.criteria {
                Value::Object(m) => m.keys().cloned().collect(),
                Value::Array(a) => a
                    .iter()
                    .filter_map(|v| v.as_str().map(str::to_string))
                    .collect(),
                _ => Vec::new(),
            },
            Kind::Score => (0..self.criteria.as_array().map_or(0, Vec::len))
                .map(|i| i.to_string())
                .collect(),
            Kind::Noul => vec!["false".into(), "true".into()],
        }
    }
}

/// Kai's verdict: from an allow/ask/deny choice, raised by escalations.
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Rule {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub from: Option<String>,
    #[serde(default, skip_serializing_if = "IndexMap::is_empty")]
    pub escalate: IndexMap<String, String>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Program {
    /// `name@version`.
    pub id: String,
    pub description: String,
    /// The calibration the thresholds were set against: an answer under another runs in
    /// shadow.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub calibration: Option<String>,
    pub questions: IndexMap<String, Question>,
    #[serde(default, skip_serializing_if = "IndexMap::is_empty")]
    pub thresholds: IndexMap<String, f64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub verdict: Option<Rule>,
}

/// One question's reading at its threshold.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Signal {
    /// The most probable option.
    pub answer: String,
    /// Its probability; a noul's is `max(p, 1 - p)`.
    pub certainty: f64,
    /// Choice and score: certainty reaches the threshold.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub accepted: Option<bool>,
    /// Noul: P(true) reaches the threshold.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub holds: Option<bool>,
}

pub fn parse_verdict(s: &str) -> Option<Verdict> {
    match s {
        "allow" => Some(Verdict::Allow),
        "ask" => Some(Verdict::Ask),
        "deny" => Some(Verdict::Deny),
        _ => None,
    }
}

/// The first index of the largest value, and the value.
pub fn argmax(p: &[f64]) -> (usize, f64) {
    let mut best = 0;
    for (i, v) in p.iter().enumerate() {
        if *v > p[best] {
            best = i;
        }
    }
    (best, p.get(best).copied().unwrap_or(0.0))
}

impl Program {
    /// A shipped program by id, or a program file.
    pub fn load(name: &str) -> Result<Program, String> {
        let shipped = SHIPPED
            .iter()
            .find(|text| serde_json::from_str::<Value>(text).is_ok_and(|v| v["id"] == name));
        let text = match shipped {
            Some(text) => (*text).to_string(),
            None => std::fs::read_to_string(name)
                .map_err(|e| format!("{name}: no shipped program, and not a file: {e}"))?,
        };
        Program::parse(&text).map_err(|e| format!("{name}: {e}"))
    }

    /// A program from its JSON, checked.
    pub fn parse(text: &str) -> Result<Program, String> {
        let program: Program = serde_json::from_str(text).map_err(|e| e.to_string())?;
        program.check()?;
        Ok(program)
    }

    fn check(&self) -> Result<(), String> {
        if self.questions.is_empty() {
            return Err("no questions".into());
        }
        for (id, q) in &self.questions {
            if q.instructions.trim().is_empty() {
                return Err(format!("questions.{id}: instructions are empty"));
            }
            if q.keys().len() < 2 {
                return Err(format!("questions.{id}: fewer than two options"));
            }
        }
        for (q, t) in &self.thresholds {
            if !self.questions.contains_key(q) {
                return Err(format!("thresholds: no question {q:?}"));
            }
            if !(0.0..=1.0).contains(t) {
                return Err(format!("thresholds.{q}: {t} is outside [0, 1]"));
            }
        }
        if let Some(rule) = &self.verdict {
            if let Some(from) = &rule.from {
                let verdicts = self.questions.get(from).is_some_and(|q| {
                    q.kind == Kind::Choice && q.keys().iter().all(|k| parse_verdict(k).is_some())
                });
                if !verdicts {
                    return Err(format!(
                        "verdict.from: {from:?} is not a choice labelled allow/ask/deny"
                    ));
                }
            }
            for (q, v) in &rule.escalate {
                if self.questions.get(q).map(|q| q.kind) != Some(Kind::Noul) {
                    return Err(format!("verdict.escalate: {q:?} is not a noul"));
                }
                if parse_verdict(v).is_none() {
                    return Err(format!(
                        "verdict.escalate.{q}: {v:?} is not allow, ask or deny"
                    ));
                }
            }
        }
        Ok(())
    }

    /// Merges `thresholds` over the program's, by question.
    pub fn configure(
        &mut self,
        thresholds: impl IntoIterator<Item = (String, f64)>,
    ) -> Result<(), String> {
        for (q, t) in thresholds {
            self.thresholds.insert(q, t);
        }
        self.check()
    }

    /// `sha256:` of the program as it runs.
    pub fn hash(&self) -> String {
        digest(&serde_json::to_value(self).unwrap_or_default())
    }

    fn threshold(&self, q: &str, kind: Kind) -> f64 {
        let default = if kind == Kind::Noul { 0.5 } else { 0.0 };
        self.thresholds.get(q).copied().unwrap_or(default)
    }

    /// Each question's signal from its distribution, in option order.
    pub fn signals(&self, answers: &IndexMap<String, Vec<f64>>) -> IndexMap<String, Signal> {
        self.questions
            .iter()
            .filter_map(|(id, q)| {
                let p = answers.get(id)?;
                let keys = q.keys();
                let (best, pmax) = argmax(p);
                let t = self.threshold(id, q.kind);
                let answer = keys.get(best).cloned().unwrap_or_default();
                let signal = if q.kind == Kind::Noul {
                    let yes = p.get(1).copied().unwrap_or(0.0);
                    Signal {
                        answer,
                        certainty: yes.max(1.0 - yes),
                        accepted: None,
                        holds: Some(yes >= t),
                    }
                } else {
                    Signal {
                        answer,
                        certainty: pmax,
                        accepted: Some(pmax >= t),
                        holds: None,
                    }
                };
                Some((id.clone(), signal))
            })
            .collect()
    }

    /// Kai's verdict under the rule; `None` when nothing fired or there is no rule.
    pub fn verdict(&self, signals: &IndexMap<String, Signal>) -> Option<Verdict> {
        let rule = self.verdict.as_ref()?;
        let mut verdict = rule.from.as_ref().and_then(|f| {
            let s = signals.get(f)?;
            (s.accepted == Some(true))
                .then(|| parse_verdict(&s.answer))
                .flatten()
        });
        for (q, v) in &rule.escalate {
            if signals.get(q).and_then(|s| s.holds) == Some(true)
                && let Some(v) = parse_verdict(v)
            {
                verdict = Some(verdict.map_or(v, |x| x.join(v)));
            }
        }
        verdict
    }

    /// The first noul, which a selection ranks by.
    pub fn first_noul(&self) -> Option<&str> {
        self.questions
            .iter()
            .find(|(_, q)| q.kind == Kind::Noul)
            .map(|(id, _)| id.as_str())
    }
}

/// `sha256:` of `value` as JSON.
pub fn digest(value: &Value) -> String {
    let text = serde_json::to_vec(value).unwrap_or_default();
    let hash = Sha256::digest(&text);
    let hex: String = hash.iter().map(|b| format!("{b:02x}")).collect();
    format!("sha256:{hex}")
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn every_shipped_program_parses_under_its_id() {
        for text in SHIPPED {
            let v: Value = serde_json::from_str(text).unwrap();
            let id = v["id"].as_str().unwrap();
            let p = Program::load(id).unwrap();
            assert_eq!(p.id, id);
        }
    }

    #[test]
    fn the_gate_reads_nouls_and_choices_at_their_thresholds() {
        let p = Program::load("agent.command-risk@1").unwrap();
        let answers: IndexMap<String, Vec<f64>> = [
            ("verdict".to_string(), vec![0.2, 0.7, 0.1]),
            ("secret_exposure".to_string(), vec![0.6, 0.4]),
            ("production_impact".to_string(), vec![0.9, 0.1]),
            ("network".to_string(), vec![0.9, 0.1]),
        ]
        .into_iter()
        .collect();
        let s = p.signals(&answers);
        assert_eq!(s["verdict"].answer, "ask");
        assert_eq!(s["verdict"].accepted, Some(true));
        // 0.4 reaches the 0.3 threshold: the escalation to deny fires.
        assert_eq!(s["secret_exposure"].holds, Some(true));
        assert_eq!(s["secret_exposure"].answer, "false");
        assert_eq!(p.verdict(&s), Some(Verdict::Deny));
    }

    #[test]
    fn an_unaccepted_verdict_choice_leaves_only_escalations() {
        let mut p = Program::load("agent.command-risk@1").unwrap();
        p.configure([("verdict".to_string(), 0.9)]).unwrap();
        let answers: IndexMap<String, Vec<f64>> = [
            ("verdict".to_string(), vec![0.1, 0.1, 0.8]),
            ("secret_exposure".to_string(), vec![0.9, 0.1]),
            ("production_impact".to_string(), vec![0.9, 0.1]),
            ("network".to_string(), vec![0.2, 0.8]),
        ]
        .into_iter()
        .collect();
        assert_eq!(p.verdict(&p.signals(&answers)), Some(Verdict::Ask));
    }

    #[test]
    fn a_threshold_must_name_a_question() {
        let mut p = Program::load("tools.select@1").unwrap();
        let e = p.configure([("verdict".to_string(), 0.5)]).unwrap_err();
        assert!(e.contains("no question \"verdict\""), "{e}");
        assert!(Program::load("no.such@1").is_err());
    }
}
