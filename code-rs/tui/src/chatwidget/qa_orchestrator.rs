use std::num::NonZeroUsize;
use std::sync::mpsc::{self, Receiver, Sender};
use std::thread::{self, JoinHandle};
use std::time::Duration;

use crate::app_event::AppEvent;
use crate::app_event_sender::AppEventSender;

/// Internal commands understood by the QA orchestrator loop.
enum QaMsg {
    Shutdown,
    #[allow(dead_code)]
    Tick,
    TurnFinished { has_diff: bool },
    Finalize { has_diff: bool },
}

/// Handle giving the caller control over the QA orchestrator thread.
pub struct QaOrchestratorHandle {
    tx: Sender<QaMsg>,
    join: Option<JoinHandle<()>>,
}

impl QaOrchestratorHandle {
    /// Request shutdown and wait for the orchestrator thread to exit.
    pub fn stop(mut self) {
        let _ = self.tx.send(QaMsg::Shutdown);
        if let Some(join) = self.join.take() {
            let _ = join.join();
        }
    }

    /// Notify the orchestrator that an Auto Drive turn finished.
    pub fn notify_turn_finished(&self, has_diff: bool) {
        let _ = self.tx.send(QaMsg::TurnFinished { has_diff });
    }

    /// Notify the orchestrator that Auto Drive is about to stop so it can run
    /// a final QA cadence check before shutting down.
    pub fn notify_finalize(&self, has_diff: bool) {
        let _ = self.tx.send(QaMsg::Finalize { has_diff });
    }
}

/// Spawn a placeholder QA orchestrator thread. For now it only idles until
/// told to shut down; future iterations will drive cross-check + review logic.
pub fn start_qa_orchestrator(app_tx: AppEventSender) -> QaOrchestratorHandle {
    let (tx, rx) = mpsc::channel();
    let join = thread::Builder::new()
        .name("qa-orchestrator".into())
        .spawn(move || orchestrator_loop(app_tx, rx))
        .expect("failed to spawn qa orchestrator thread");

    QaOrchestratorHandle {
        tx,
        join: Some(join),
    }
}

const ENV_QA_CADENCE: &str = "CODE_QA_CADENCE";
const ENV_QA_REVIEW_COOLDOWN_TURNS: &str = "CODE_QA_REVIEW_COOLDOWN_TURNS";

#[derive(Debug)]
struct QaCadenceState {
    cadence: NonZeroUsize,
    completed: usize,
    review_cooldown: NonZeroUsize,
    since_last_review: usize,
}

impl QaCadenceState {
    fn from_env() -> Self {
        let cadence = std::env::var(ENV_QA_CADENCE)
            .ok()
            .and_then(|raw| raw.parse::<usize>().ok())
            .and_then(NonZeroUsize::new)
            .unwrap_or_else(|| NonZeroUsize::new(3).expect("3 is non-zero"));
        let review_cooldown = std::env::var(ENV_QA_REVIEW_COOLDOWN_TURNS)
            .ok()
            .and_then(|raw| raw.parse::<usize>().ok())
            .and_then(NonZeroUsize::new)
            .unwrap_or_else(|| NonZeroUsize::new(1).expect("1 is non-zero"));

        Self {
            cadence,
            completed: 0,
            review_cooldown,
            since_last_review: review_cooldown.get(),
        }
    }

    fn record_turn(&mut self) -> bool {
        self.completed += 1;
        if self.completed >= self.cadence.get() {
            self.completed = 0;
            true
        } else {
            false
        }
    }

    fn reset(&mut self) {
        self.completed = 0;
        self.since_last_review = self.review_cooldown.get();
    }

    fn record_review_turn(&mut self, has_diff: bool) -> Option<usize> {
        self.since_last_review = (self.since_last_review + 1)
            .min(self.review_cooldown.get());

        if has_diff && self.since_last_review >= self.review_cooldown.get() {
            let waited = self.since_last_review;
            self.since_last_review = 0;
            Some(waited)
        } else {
            None
        }
    }
}

fn orchestrator_loop(app_tx: AppEventSender, rx: Receiver<QaMsg>) {
    let mut cadence = QaCadenceState::from_env();

    // Wait for commands; once we get Shutdown we return and end the thread.
    while let Ok(msg) = rx.recv_timeout(Duration::from_secs(60)) {
        match msg {
            QaMsg::Shutdown => {
                cadence.reset();
                break;
            }
            QaMsg::Tick => {
                // Placeholder: emit AppEvent::AutoQaUpdate or AutoReviewRequest.
            }
            QaMsg::TurnFinished { has_diff } => {
                if cadence.record_turn() {
                    let diff_note = if has_diff { "changes detected" } else { "no changes" };
                    let note = format!(
                        "Auto QA cadence checkpoint ({} turns, {diff_note}).",
                        cadence.cadence.get()
                    );
                    let _ = app_tx.send(AppEvent::AutoQaUpdate { note });
                }

                if let Some(turns_waited) = cadence.record_review_turn(has_diff) {
                    let summary = Some(format!(
                        "Automated QA review requested after {turns_waited} turn(s) with workspace changes."
                    ));
                    let _ = app_tx.send(AppEvent::AutoReviewRequest { summary });
                }
            }
            QaMsg::Finalize { has_diff } => {
                let note = format!(
                    "Final QA check ({}-turn cooldown)",
                    cadence.review_cooldown.get()
                );
                let _ = app_tx.send(AppEvent::AutoQaUpdate { note });

                if has_diff && cadence.since_last_review > 0 {
                    let summary = Some("Final safety review before stop.".to_string());
                    let _ = app_tx.send(AppEvent::AutoReviewRequest { summary });
                }

                cadence.reset();
            }
        }
    }
}

impl Drop for QaOrchestratorHandle {
    fn drop(&mut self) {
        if let Some(join) = self.join.take() {
            let _ = self.tx.send(QaMsg::Shutdown);
            let _ = join.join();
        }
    }
}
