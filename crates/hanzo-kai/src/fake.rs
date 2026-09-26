//! Fakes for tests: Kai answering by a rule, Zen answering by a rule, a thread that records
//! what Kai did to it.

use crate::agent::Act;
use control::Answers;
use control::Job;
use hanzo_loop::Answer;
use program::Question;
use program::Revision;
use serde_json::Value;
use std::sync::Mutex;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;

/// The calibration the shipped agent programs pin: an answer under another runs in shadow.
pub const CALIBRATION: &str = "cal_6dbb072f8109a222";

type Rule = Box<dyn Fn(&str, &Question, &Value) -> Vec<f64> + Send + Sync>;

/// Kai answering each question by `rule(question id, question, state)`.
pub struct Kai {
    rule: Rule,
    pub calls: AtomicUsize,
}

impl Kai {
    pub fn new(rule: impl Fn(&str, &Question, &Value) -> Vec<f64> + Send + Sync + 'static) -> Kai {
        Kai {
            rule: Box::new(rule),
            calls: AtomicUsize::new(0),
        }
    }
}

impl control::Ask for Kai {
    fn ask(&self, _model: &str, jobs: &[Job]) -> anyhow::Result<Vec<anyhow::Result<Answers>>> {
        self.calls.fetch_add(1, Ordering::SeqCst);
        Ok(jobs
            .iter()
            .map(|job| {
                Ok(job
                    .questions
                    .iter()
                    .map(|(id, q)| (id.clone(), (self.rule)(id, q, &job.state)))
                    .collect())
            })
            .collect())
    }

    fn revision(&self, model: &str) -> anyhow::Result<Revision> {
        Ok(Revision {
            model: format!("{model}@fake"),
            calibration: Some(CALIBRATION.into()),
        })
    }
}

/// Probabilities putting `p` on `label` of `q`'s options and the rest evenly.
pub fn on(q: &Question, label: &str, p: f64) -> Vec<f64> {
    let keys = q.keys();
    let rest = (1.0 - p) / (keys.len() as f64 - 1.0).max(1.0);
    keys.iter()
        .map(|k| if k == label { p } else { rest })
        .collect()
}

/// Zen answering by `rule(prompt, context)`.
pub struct Zen(pub Box<dyn Fn(&str, &Value) -> String + Send + Sync>);

impl program::Zen for Zen {
    fn generate(&self, prompt: &str, context: &Value) -> program::Result<String> {
        Ok((self.0)(prompt, context))
    }

    fn revision(&self) -> Revision {
        Revision {
            model: "zen@fake".into(),
            calibration: None,
        }
    }
}

/// What Kai did to a thread.
#[derive(Default)]
pub struct Thread {
    pub warned: Mutex<Vec<String>>,
    pub steered: Mutex<Vec<String>>,
    pub resumed: Mutex<Vec<String>>,
}

impl Act for Thread {
    fn warn(&self, _turn: &str, message: String) {
        self.warned.lock().unwrap().push(message);
    }

    fn steer(&self, text: String) -> Answer<'_, ()> {
        self.steered.lock().unwrap().push(text);
        Box::pin(async {})
    }

    fn resume(&self, text: String) -> Answer<'_, bool> {
        self.resumed.lock().unwrap().push(text);
        Box::pin(async { true })
    }
}

/// The trace's lines, once there are `n`, waiting at most five seconds for them.
pub fn lines(path: &std::path::Path, n: usize) -> Vec<Value> {
    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(5);
    loop {
        let lines: Vec<Value> = std::fs::read_to_string(path)
            .unwrap_or_default()
            .lines()
            .map(|l| serde_json::from_str(l).expect("a trace line is JSON"))
            .collect();
        if lines.len() >= n || std::time::Instant::now() > deadline {
            return lines;
        }
        std::thread::sleep(std::time::Duration::from_millis(20));
    }
}
