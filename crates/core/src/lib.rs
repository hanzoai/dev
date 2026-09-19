//! The loop, as a state machine — no clock, no disk, no socket.
//!
//! A [`Session`] holds the conversation and what the turn is waiting for. Feed
//! it an [`Event`] and it answers with the [`Action`]s the host should perform.
//! It never performs one itself: there is no `std::fs`, no `std::process`, no
//! `std::net` and no async runtime anywhere below this line, which is what lets
//! the same core run in cloud, in a terminal, and inside wasm.
//!
//! # What is keyed, and what that buys
//!
//! Every action gets the next id from a counter that is part of the snapshot,
//! so a restored session continues the sequence rather than repeating it. On
//! top of that, each event class is keyed differently, and the difference is
//! the whole replay story:
//!
//! - A **result** ([`Event::Exec`], [`Event::File`], [`Event::Git`],
//!   [`Event::Browse`], [`Event::Model`]) is accepted only while its id is
//!   outstanding *and* was dispatched to that family. A second delivery, or
//!   one from the wrong family, changes nothing and produces no action — and
//!   in particular does not consume the entry the genuine result still needs.
//! - A **[`Event::Turn`]** carries the host's id. The core keeps the highest
//!   it has accepted and refuses anything at or below it, so a redelivered
//!   prompt is not run twice.
//! - A **[`Event::Timer`]** carries no id and needs none: it asks only for
//!   [`Op::Save`], and only when the sequence has moved since the last one, so
//!   a redelivered timer asks for nothing.
//! - A **[`Event::Cancel`]** is idempotent: there is one turn to stop.
//!
//! So at-least-once delivery of an event is at-most-once execution of an
//! effect, for every event this protocol has.
//!
//! # Every accepted prompt ends in a [`Done`] that names it
//!
//! A prompt that arrives mid-turn is queued, and queueing answers with an
//! [`Op::Save`] because the queue is state the host must not lose. A prompt
//! the core refuses answers with nothing at all, so the two are distinct at
//! the call. Whichever way the turn ends — its own answer, a chained queue, a
//! cancel — exactly one [`Op::Done`] names the turn id that asked for it.
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
use dev_protocol::Dispatch;
use dev_protocol::Done;
use dev_protocol::Event;
use dev_protocol::Malformed;
use dev_protocol::Message;
use dev_protocol::Op;
use dev_protocol::Outcome;
use dev_protocol::Output;
use dev_protocol::Turn;
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
    /// The turn in flight, by the id the host gave it.
    turn: Option<u64>,
    /// The highest turn id accepted. One at or below it is a redelivery.
    high: u64,
    /// Actions dispatched and not yet answered.
    outstanding: BTreeMap<u64, Wants>,
    /// Prompts that arrived while a turn was in flight.
    queue: VecDeque<Turn>,
    /// `next` as it stood when the last [`Op::Save`] was asked for.
    saved: u64,
}

impl Session {
    pub fn new(config: Config) -> Self {
        Self {
            config,
            messages: Vec::new(),
            next: 1,
            turn: None,
            high: 0,
            outstanding: BTreeMap::new(),
            queue: VecDeque::new(),
            saved: 1,
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
            Event::Turn(turn) => self.turn(turn),
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
    pub fn snapshot(&self) -> Result<Vec<u8>, Malformed> {
        dev_protocol::pack(self)
    }

    /// Rebuild a session from its snapshot.
    pub fn restore(state: &[u8]) -> Result<Self, Malformed> {
        dev_protocol::unpack(state)
    }

    fn turn(&mut self, turn: Turn) -> Vec<Action> {
        if turn.id <= self.high {
            return Vec::new();
        }
        self.high = turn.id;
        if self.turn.is_some() {
            self.queue.push_back(turn);
            return vec![self.save()];
        }
        self.turn = Some(turn.id);
        self.messages.push(Message::User(turn.prompt));
        vec![self.ask()]
    }

    fn answered(&mut self, answer: Answer) -> Vec<Action> {
        let Some(turn) = self.turn else {
            return Vec::new();
        };
        if self.outstanding.get(&answer.id) != Some(&Wants::Model) {
            return Vec::new();
        }
        self.outstanding.remove(&answer.id);
        if answer.reply.calls.is_empty() {
            if let Some(text) = answer.reply.text.clone() {
                self.messages.push(Message::Agent {
                    text: Some(text),
                    calls: Vec::new(),
                });
            }
            return self.finish(turn, answer.reply.text);
        }

        // Mint an id for every call, whether it goes out or not: the transcript
        // records the call the model made and the answer it gets, and the two
        // agree on the id even when the answer is a refusal.
        let mut actions = Vec::new();
        let mut calls = Vec::new();
        let mut refused = Vec::new();
        for call in answer.reply.calls {
            let id = self.mint();
            match call.escape() {
                None => actions.push(self.dispatch(id, call.clone())),
                Some(path) => refused.push(Message::Tool {
                    id,
                    text: format!("refused: {path} is not inside the workspace"),
                    failed: true,
                }),
            }
            calls.push(Dispatch { id, call });
        }
        self.messages.push(Message::Agent {
            text: answer.reply.text,
            calls,
        });
        self.messages.extend(refused);
        if self.outstanding.is_empty() {
            // Every call was refused, so nothing will arrive to resume the
            // turn: tell the model now, with the refusals in the transcript.
            actions.push(self.ask());
        }
        actions
    }

    fn observed(&mut self, wants: Wants, output: Output) -> Vec<Action> {
        if self.outstanding.get(&output.id) != Some(&wants) {
            return Vec::new();
        }
        self.outstanding.remove(&output.id);
        self.messages.push(Message::Tool {
            id: output.id,
            text: output.text,
            failed: output.failed,
        });
        if self.outstanding.is_empty() {
            vec![self.ask()]
        } else {
            Vec::new()
        }
    }

    fn tick(&mut self) -> Vec<Action> {
        if self.turn.is_some() && self.next != self.saved {
            vec![self.save()]
        } else {
            Vec::new()
        }
    }

    fn cancel(&mut self) -> Vec<Action> {
        let Some(turn) = self.turn.take() else {
            return Vec::new();
        };
        self.outstanding.clear();
        let dropped: Vec<u64> = self.queue.drain(..).map(|turn| turn.id).collect();
        let mut actions = vec![self.save(), self.done(turn, Outcome::Cancelled)];
        for queued in dropped {
            actions.push(self.done(queued, Outcome::Cancelled));
        }
        actions
    }

    /// End the turn, then start the next queued prompt if there is one.
    fn finish(&mut self, turn: u64, text: Option<String>) -> Vec<Action> {
        let mut actions = Vec::new();
        if let Some(text) = text {
            actions.push(self.action(Op::Emit(text)));
        }
        actions.push(self.save());
        actions.push(self.done(turn, Outcome::Complete));
        match self.queue.pop_front() {
            Some(next) => {
                self.turn = Some(next.id);
                self.messages.push(Message::User(next.prompt));
                actions.push(self.ask());
            }
            None => self.turn = None,
        }
        actions
    }

    fn ask(&mut self) -> Action {
        let op = Op::Model(Ask {
            model: self.config.model.clone(),
            prelude: self.config.prelude.clone(),
            messages: self.messages.clone(),
        });
        let action = self.action(op);
        self.outstanding.insert(action.id, Wants::Model);
        action
    }

    fn dispatch(&mut self, id: u64, call: Call) -> Action {
        let (op, wants) = match call {
            Call::Read(read) => (Op::Read(read), Wants::File),
            Call::Write(write) => (Op::Write(write), Wants::File),
            Call::Patch(patch) => (Op::Patch(patch), Wants::File),
            Call::Exec(exec) => (Op::Exec(exec), Wants::Exec),
            Call::Git(git) => (Op::Git(git), Wants::Git),
            Call::Browse(browse) => (Op::Browse(browse), Wants::Browse),
        };
        self.outstanding.insert(id, wants);
        Action { id, op }
    }

    fn save(&mut self) -> Action {
        let action = self.action(Op::Save);
        self.saved = self.next;
        action
    }

    fn done(&mut self, turn: u64, outcome: Outcome) -> Action {
        self.action(Op::Done(Done { turn, outcome }))
    }

    fn action(&mut self, op: Op) -> Action {
        Action {
            id: self.mint(),
            op,
        }
    }

    fn mint(&mut self) -> u64 {
        let id = self.next;
        self.next += 1;
        id
    }
}
