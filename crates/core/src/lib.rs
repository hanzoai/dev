//! The loop, as a state machine — no clock, no disk, no socket.
//!
//! A [`Session`] holds the conversation and what the turn is waiting for. Feed
//! it an [`Event`] and it answers with the [`Action`]s the host should perform.
//! It never performs one itself: there is no `std::fs`, no `std::process`, no
//! `std::net` and no async runtime anywhere below this line, which is what lets
//! the same core run in cloud, in a terminal, and inside wasm.
//!
//! Two rules make the loop replayable:
//!
//! - Every action gets the next id from a counter that is part of the
//!   snapshot, so a restored session continues the sequence rather than
//!   repeating it.
//! - An observation is accepted only while its id is outstanding. A duplicate
//!   delivery of the same result changes nothing and produces no action, so
//!   at-least-once delivery of an event is not at-least-once execution of an
//!   effect.
//!
//! # What upstream this does not reuse, and why
//!
//! `codex-core` is the closest existing loop, and it is not separable as it
//! stands: it depends on `tokio`, `reqwest` through `codex-http-client`, a
//! rollout store on disk, a pty, and a sandbox policy of its own — roughly
//! eighty crates, several of which are effects. Even `codex-protocol`, the
//! message vocabulary alone, pulls in `tokio` and the HTTP client. Taking
//! either into this crate would put a filesystem and a socket inside the
//! reasoning half and would not build for wasm. So the model-request and
//! tool-result loop is driven here.
//!
//! What stays coupled upstream, precisely: prompt assembly and the system
//! prompt (`codex-prompts`), context compaction (`codex-core::compact*`),
//! apply-patch parsing (`codex-apply-patch`), exec policy
//! (`codex-execpolicy`), and the provider-to-tool-call mapping that turns a
//! model's function call into a [`Call`]. Each of those is a pure function of
//! its inputs and can move into this crate one at a time; none of them is
//! reimplemented here in the meantime. Until then a host supplies the prompt
//! prelude, maps provider calls onto [`Call`], and keeps the context budget.

use std::collections::BTreeMap;
use std::collections::VecDeque;

use dev_protocol::Action;
use dev_protocol::Answer;
use dev_protocol::Ask;
use dev_protocol::Call;
use dev_protocol::Config;
use dev_protocol::Event;
use dev_protocol::Malformed;
use dev_protocol::Message;
use dev_protocol::Op;
use dev_protocol::Outcome;
use dev_protocol::Output;
use dev_protocol::Role;
use serde::Deserialize;
use serde::Serialize;

/// Which family of observation answers an outstanding effect.
#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
enum Wants {
    Model,
    Exec,
    File,
    Git,
    Browse,
}

/// Conversation and turn state. Effects belong to the host.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Session {
    config: Config,
    messages: Vec<Message>,
    /// The id the next action will carry.
    next: u64,
    /// Actions dispatched and not yet answered.
    outstanding: BTreeMap<u64, Wants>,
    /// Prompts that arrived while a turn was in flight.
    queue: VecDeque<String>,
    /// A turn is in flight.
    busy: bool,
}

impl Session {
    pub fn new(config: Config) -> Self {
        let mut messages = Vec::new();
        if let Some(prelude) = config.prelude.clone() {
            messages.push(Message {
                role: Role::Agent,
                text: prelude,
            });
        }
        Self {
            config,
            messages,
            next: 1,
            outstanding: BTreeMap::new(),
            queue: VecDeque::new(),
            busy: false,
        }
    }

    /// The conversation so far.
    pub fn messages(&self) -> &[Message] {
        &self.messages
    }

    /// The id the next action will carry.
    pub fn next_id(&self) -> u64 {
        self.next
    }

    /// Advance the machine.
    pub fn step(&mut self, event: Event) -> Vec<Action> {
        match event {
            Event::Turn(turn) => self.turn(turn.prompt),
            Event::Model(answer) => self.answered(answer),
            Event::Exec(output) => self.observed(Wants::Exec, output),
            Event::File(output) => self.observed(Wants::File, output),
            Event::Git(output) => self.observed(Wants::Git, output),
            Event::Browse(output) => self.observed(Wants::Browse, output),
            Event::Timer => self.tick(),
            Event::Cancel => self.cancel(),
        }
    }

    /// The core's own snapshot — one of the three facts a session is.
    pub fn snapshot(&self) -> Vec<u8> {
        dev_protocol::encode(self)
    }

    /// Rebuild a session from its snapshot.
    pub fn restore(state: &[u8]) -> Result<Self, Malformed> {
        dev_protocol::decode(state)
    }

    fn turn(&mut self, prompt: String) -> Vec<Action> {
        if self.busy {
            self.queue.push_back(prompt);
            return Vec::new();
        }
        self.messages.push(Message {
            role: Role::User,
            text: prompt,
        });
        self.busy = true;
        vec![self.ask()]
    }

    fn answered(&mut self, answer: Answer) -> Vec<Action> {
        if self.outstanding.remove(&answer.id) != Some(Wants::Model) {
            return Vec::new();
        }
        if let Some(text) = answer.reply.text.clone() {
            self.messages.push(Message {
                role: Role::Agent,
                text,
            });
        }
        if answer.reply.calls.is_empty() {
            return self.finish(answer.reply.text);
        }
        answer
            .reply
            .calls
            .into_iter()
            .map(|call| self.dispatch(call))
            .collect()
    }

    fn observed(&mut self, wants: Wants, output: Output) -> Vec<Action> {
        if self.outstanding.remove(&output.id) != Some(wants) {
            return Vec::new();
        }
        let mark = if output.failed { "failed" } else { "ok" };
        self.messages.push(Message {
            role: Role::Tool,
            text: format!("{} {mark}: {}", output.id, output.text),
        });
        if self.outstanding.is_empty() {
            vec![self.ask()]
        } else {
            Vec::new()
        }
    }

    fn tick(&mut self) -> Vec<Action> {
        if self.busy {
            vec![self.action(Op::Save)]
        } else {
            Vec::new()
        }
    }

    fn cancel(&mut self) -> Vec<Action> {
        if !self.busy {
            return Vec::new();
        }
        self.outstanding.clear();
        self.queue.clear();
        self.busy = false;
        vec![
            self.action(Op::Save),
            self.action(Op::Done(Outcome::Cancelled)),
        ]
    }

    /// End the turn, or start the next queued prompt instead.
    fn finish(&mut self, text: Option<String>) -> Vec<Action> {
        let mut actions = Vec::new();
        if let Some(text) = text {
            actions.push(self.action(Op::Emit(text)));
        }
        actions.push(self.action(Op::Save));
        match self.queue.pop_front() {
            Some(prompt) => {
                self.messages.push(Message {
                    role: Role::User,
                    text: prompt,
                });
                actions.push(self.ask());
            }
            None => {
                self.busy = false;
                actions.push(self.action(Op::Done(Outcome::Complete)));
            }
        }
        actions
    }

    fn ask(&mut self) -> Action {
        let op = Op::Model(Ask {
            model: self.config.model.clone(),
            messages: self.messages.clone(),
        });
        let action = self.action(op);
        self.outstanding.insert(action.id, Wants::Model);
        action
    }

    fn dispatch(&mut self, call: Call) -> Action {
        let (op, wants) = match call {
            Call::Read(read) => (Op::Read(read), Wants::File),
            Call::Write(write) => (Op::Write(write), Wants::File),
            Call::Patch(patch) => (Op::Patch(patch), Wants::File),
            Call::Exec(exec) => (Op::Exec(exec), Wants::Exec),
            Call::Git(git) => (Op::Git(git), Wants::Git),
            Call::Browse(browse) => (Op::Browse(browse), Wants::Browse),
        };
        let action = self.action(op);
        self.outstanding.insert(action.id, wants);
        action
    }

    fn action(&mut self, op: Op) -> Action {
        let id = self.next;
        self.next += 1;
        Action { id, op }
    }
}
