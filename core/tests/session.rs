#![allow(clippy::expect_used, clippy::unwrap_used)]

use dev_core::Session;
use dev_protocol::Action;
use dev_protocol::Answer;
use dev_protocol::Ask;
use dev_protocol::Call;
use dev_protocol::Cancel;
use dev_protocol::Config;
use dev_protocol::Dispatch;
use dev_protocol::Done;
use dev_protocol::Event;
use dev_protocol::Exec;
use dev_protocol::Message;
use dev_protocol::Op;
use dev_protocol::Outcome;
use dev_protocol::Output;
use dev_protocol::Read;
use dev_protocol::Reply;
use dev_protocol::Turn;

fn turn(id: u64, prompt: &str) -> Event {
    Event::Turn(Turn {
        id,
        prompt: prompt.to_string(),
    })
}

fn cancel(turn: u64) -> Event {
    Event::Cancel(Cancel { turn })
}

fn tools(calls: Vec<Call>, id: u64) -> Event {
    Event::Model(Answer {
        id,
        reply: Reply { text: None, calls },
    })
}

fn says(text: &str, id: u64) -> Event {
    Event::Model(Answer {
        id,
        reply: Reply {
            text: Some(text.to_string()),
            calls: Vec::new(),
        },
    })
}

fn test() -> Call {
    Call::Exec(Exec {
        argv: vec!["cargo".to_string(), "test".to_string()],
        cwd: None,
    })
}

fn read(path: &str) -> Call {
    Call::Read(Read {
        path: path.to_string(),
    })
}

fn result(id: u64, text: &str) -> Output {
    Output {
        id,
        text: text.to_string(),
        failed: false,
    }
}

fn done(turn: u64, outcome: Outcome) -> Op {
    Op::Done(Done { turn, outcome })
}

fn ops(actions: &[Action]) -> Vec<&Op> {
    actions.iter().map(|a| &a.op).collect()
}

/// The request an [`Op::Model`] action carries, or a failure naming what it
/// carried instead.
fn asked(action: &Action) -> &Ask {
    match &action.op {
        Op::Model(ask) => ask,
        op => panic!("asked for {op:?}"),
    }
}

#[test]
fn a_turn_asks_a_model_then_runs_the_tool_then_asks_again() {
    let mut session = Session::new(Config::default());

    let opened = session.step(turn(1, "fix the build"));
    assert_eq!(opened.len(), 1);
    assert_eq!(opened[0].id, 1);
    assert!(matches!(opened[0].op, Op::Model(_)));

    let dispatched = session.step(tools(vec![test()], 1));
    assert_eq!(dispatched.len(), 1);
    assert_eq!(dispatched[0].id, 2);
    assert!(matches!(dispatched[0].op, Op::Exec(_)));

    let again = session.step(Event::Exec(result(2, "ok")));
    assert_eq!(again.len(), 1);
    assert_eq!(again[0].id, 3);
    assert!(matches!(again[0].op, Op::Model(_)));

    let ended = session.step(says("built", 3));
    assert_eq!(
        ops(&ended),
        vec![
            &Op::Emit("built".to_string()),
            &Op::Save,
            &done(1, Outcome::Complete),
        ]
    );
    assert_eq!(ended[0].id, 4);
    assert_eq!(ended[2].id, 6);
}

#[test]
fn every_call_of_one_reply_gets_its_own_id() {
    let mut session = Session::new(Config::default());
    session.step(turn(1, "run the suite twice"));

    let dispatched = session.step(tools(vec![test(), test()], 1));
    let ids: Vec<u64> = dispatched.iter().map(|a| a.id).collect();
    assert_eq!(ids, vec![2, 3]);

    // One answer is not enough: the model is asked again only when the last
    // outstanding effect has been observed.
    assert!(session.step(Event::Exec(result(2, "ok"))).is_empty());
    let again = session.step(Event::Exec(result(3, "ok")));
    assert_eq!(again.len(), 1);
    assert!(matches!(again[0].op, Op::Model(_)));
}

#[test]
fn the_prelude_rides_beside_the_conversation_rather_than_inside_it() {
    let mut session = Session::new(Config {
        model: Some("zen5.8".to_string()),
        prelude: Some("you are a coding agent".to_string()),
    });
    let opened = session.step(turn(1, "fix the build"));
    let ask = asked(&opened[0]);
    assert_eq!(ask.model.as_deref(), Some("zen5.8"));
    assert_eq!(ask.prelude.as_deref(), Some("you are a coding agent"));
    assert_eq!(
        ask.messages,
        vec![Message::User("fix the build".to_string())]
    );
}

#[test]
fn snapshot_and_restore_continue_at_the_same_id() {
    let mut session = Session::new(Config::default());
    session.step(turn(1, "fix the build"));

    let state = session.snapshot().expect("snapshot");
    let mut restored = Session::restore(&state).expect("restore");
    assert_eq!(restored.next_id(), session.next_id());
    assert_eq!(restored.messages(), session.messages());

    let here = session.step(tools(vec![test()], 1));
    let there = restored.step(tools(vec![test()], 1));
    assert_eq!(here, there);
    assert_eq!(there[0].id, 2);
}

#[test]
fn a_timer_checkpoints_only_inside_a_turn() {
    let mut session = Session::new(Config::default());
    assert!(session.step(Event::Timer).is_empty());

    session.step(turn(1, "fix the build"));
    let saved = session.step(Event::Timer);
    assert_eq!(ops(&saved), vec![&Op::Save]);
}

#[test]
fn cancel_ends_the_turn_and_drops_what_was_outstanding() {
    let mut session = Session::new(Config::default());
    session.step(turn(1, "fix the build"));
    session.step(tools(vec![test()], 1));

    let stopped = session.step(cancel(1));
    assert_eq!(ops(&stopped), vec![&Op::Save, &done(1, Outcome::Cancelled)]);
    // A result for the cancelled effect is no longer outstanding.
    assert!(session.step(Event::Exec(result(2, "ok"))).is_empty());
    assert!(session.step(cancel(1)).is_empty());
}

#[test]
fn an_answer_to_an_id_that_was_never_asked_is_refused() {
    let mut session = Session::new(Config::default());
    session.step(turn(1, "fix the build"));
    assert!(session.step(says("hello", 99)).is_empty());
}

#[test]
fn a_failed_result_reaches_the_model_as_a_failure() {
    let mut session = Session::new(Config::default());
    session.step(turn(1, "fix the build"));
    session.step(tools(vec![test()], 1));

    let again = session.step(Event::Exec(Output {
        id: 2,
        text: "2 tests failed".to_string(),
        failed: true,
    }));
    assert_eq!(
        asked(&again[0]).messages.last(),
        Some(&Message::Tool {
            id: 2,
            text: "2 tests failed".to_string(),
            failed: true,
        })
    );
}

// Everything below is a delivery the core must refuse. A refusal answers with
// no action, and — the part that took a wedged turn to learn — changes nothing.

#[test]
fn a_wrong_family_result_leaves_the_entry_for_the_right_one() {
    let mut session = Session::new(Config::default());
    session.step(turn(1, "fix the build"));
    session.step(tools(vec![test()], 1));

    // Id 2 was dispatched as an Exec. A Git result naming it is refused...
    assert!(session.step(Event::Git(result(2, "ok"))).is_empty());
    // ...and the Exec result it was standing in front of still lands.
    let again = session.step(Event::Exec(result(2, "ok")));
    assert_eq!(again.len(), 1);
    assert!(matches!(again[0].op, Op::Model(_)));

    let ended = session.step(says("built", again[0].id));
    assert_eq!(
        ended.last().map(|a| &a.op),
        Some(&done(1, Outcome::Complete))
    );
}

#[test]
fn a_wrong_family_result_does_not_resume_the_turn_early() {
    let mut session = Session::new(Config::default());
    session.step(turn(1, "run the suite twice"));
    session.step(tools(vec![test(), test()], 1));

    assert!(session.step(Event::Git(result(2, "ok"))).is_empty());
    // Two effects are still in flight, so the second result is not the last.
    assert!(session.step(Event::Exec(result(3, "second"))).is_empty());
    let again = session.step(Event::Exec(result(2, "first")));
    assert_eq!(again.len(), 1);

    // The model is asked once both results are in the transcript, never with
    // one of them missing.
    let results: Vec<&Message> = asked(&again[0])
        .messages
        .iter()
        .filter(|message| matches!(message, Message::Tool { .. }))
        .collect();
    assert_eq!(
        results,
        vec![
            &Message::Tool {
                id: 3,
                text: "second".to_string(),
                failed: false,
            },
            &Message::Tool {
                id: 2,
                text: "first".to_string(),
                failed: false,
            },
        ]
    );
}

#[test]
fn a_model_event_naming_a_tool_id_leaves_the_entry_alone() {
    let mut session = Session::new(Config::default());
    session.step(turn(1, "fix the build"));
    session.step(tools(vec![test()], 1));

    // Id 2 is an Exec, not the model request.
    assert!(session.step(says("here you go", 2)).is_empty());
    assert_eq!(session.step(Event::Exec(result(2, "ok"))).len(), 1);
}

#[test]
fn a_replayed_result_runs_nothing_twice() {
    let mut session = Session::new(Config::default());
    session.step(turn(1, "fix the build"));
    session.step(tools(vec![test()], 1));

    assert_eq!(session.step(Event::Exec(result(2, "ok"))).len(), 1);
    assert!(session.step(Event::Exec(result(2, "ok"))).is_empty());
}

#[test]
fn a_redelivered_turn_runs_the_prompt_once() {
    let mut session = Session::new(Config::default());
    let opened = session.step(turn(1, "ship it"));
    assert_eq!(opened.len(), 1);
    assert!(session.step(turn(1, "ship it")).is_empty());

    // The model is asked once, with one prompt in front of it.
    let ended = session.step(says("shipped", 1));
    assert_eq!(
        ended.last().map(|a| &a.op),
        Some(&done(1, Outcome::Complete))
    );
    let again = session.step(turn(2, "and again"));
    assert_eq!(
        asked(&again[0]).messages,
        vec![
            Message::User("ship it".to_string()),
            Message::Agent {
                text: Some("shipped".to_string()),
                calls: Vec::new(),
            },
            Message::User("and again".to_string()),
        ]
    );
}

#[test]
fn a_redelivered_timer_asks_for_nothing() {
    let mut session = Session::new(Config::default());
    session.step(turn(1, "fix the build"));

    assert_eq!(ops(&session.step(Event::Timer)), vec![&Op::Save]);
    assert!(session.step(Event::Timer).is_empty());
    assert!(session.step(Event::Timer).is_empty());

    // The next one checkpoints what the turn has moved on to since.
    session.step(tools(vec![test()], 1));
    assert_eq!(ops(&session.step(Event::Timer)), vec![&Op::Save]);
}

#[test]
fn the_transcript_keeps_the_call_each_result_answers() {
    let mut session = Session::new(Config::default());
    session.step(turn(1, "fix the build"));
    session.step(tools(vec![test()], 1));
    let again = session.step(Event::Exec(result(2, "ok")));

    assert_eq!(
        asked(&again[0]).messages,
        vec![
            Message::User("fix the build".to_string()),
            Message::Agent {
                text: None,
                calls: vec![Dispatch {
                    id: 2,
                    call: test(),
                }],
            },
            Message::Tool {
                id: 2,
                text: "ok".to_string(),
                failed: false,
            },
        ]
    );
}

#[test]
fn a_prompt_that_arrives_mid_turn_is_acknowledged_and_ends_in_its_own_done() {
    let mut session = Session::new(Config::default());
    session.step(turn(1, "first"));
    // Accepted, so the queue is state worth persisting — and the host can tell
    // this apart from the empty answer a refusal gives.
    assert_eq!(ops(&session.step(turn(2, "second"))), vec![&Op::Save]);

    let chained = session.step(says("first done", 1));
    assert_eq!(chained.len(), 4);
    assert_eq!(chained[0].op, Op::Emit("first done".to_string()));
    assert_eq!(chained[1].op, Op::Save);
    assert_eq!(chained[2].op, done(1, Outcome::Complete));
    assert!(matches!(chained[3].op, Op::Model(_)));

    let closed = session.step(says("second done", chained[3].id));
    assert_eq!(
        closed.last().map(|a| &a.op),
        Some(&done(2, Outcome::Complete))
    );
}

#[test]
fn a_cancel_says_what_became_of_every_prompt_it_discards() {
    let mut session = Session::new(Config::default());
    session.step(turn(1, "first"));
    session.step(turn(2, "second"));
    session.step(turn(3, "third"));

    let stopped = session.step(cancel(1));
    assert_eq!(
        ops(&stopped),
        vec![
            &Op::Save,
            &done(1, Outcome::Cancelled),
            &done(2, Outcome::Cancelled),
            &done(3, Outcome::Cancelled),
        ]
    );
}

#[test]
fn a_redelivered_cancel_does_not_stop_the_turn_that_came_after() {
    let mut session = Session::new(Config::default());
    session.step(turn(1, "first"));
    let stopped = session.step(cancel(1));
    assert_eq!(ops(&stopped), vec![&Op::Save, &done(1, Outcome::Cancelled)]);

    // A new turn nobody cancelled, and then the transport delivers the first
    // cancel again. It names turn 1, which is over.
    let opened = session.step(turn(2, "second"));
    assert!(session.step(cancel(1)).is_empty());

    // Turn 2 is still in flight: its answer lands and it ends the ordinary way.
    let ended = session.step(says("second done", opened[0].id));
    assert_eq!(
        ended.last().map(|a| &a.op),
        Some(&done(2, Outcome::Complete))
    );
}

#[test]
fn a_cancel_of_a_queued_prompt_takes_that_prompt_and_no_other() {
    let mut session = Session::new(Config::default());
    let opened = session.step(turn(1, "first"));
    session.step(turn(2, "second"));
    session.step(turn(3, "third"));

    // The queue changed, so it is saved, and the prompt gets its own Done.
    let removed = session.step(cancel(2));
    assert_eq!(ops(&removed), vec![&Op::Save, &done(2, Outcome::Cancelled)]);
    assert!(session.step(cancel(2)).is_empty());

    // Turn 1 never noticed, and turn 3 is what it chains into.
    let chained = session.step(says("first done", opened[0].id));
    assert_eq!(chained[2].op, done(1, Outcome::Complete));
    assert_eq!(
        asked(&chained[3]).messages.last(),
        Some(&Message::User("third".to_string()))
    );
}

#[test]
fn a_cancel_that_names_no_live_turn_changes_nothing() {
    let mut session = Session::new(Config::default());
    // Nothing is in flight, so there is nothing to name.
    assert!(session.step(cancel(1)).is_empty());

    let opened = session.step(turn(1, "fix the build"));
    session.step(tools(vec![test()], opened[0].id));
    // Neither a turn the core has not been given nor id zero reaches turn 1...
    assert!(session.step(cancel(2)).is_empty());
    assert!(session.step(cancel(0)).is_empty());
    // ...whose effect is still outstanding and still resumes it.
    let again = session.step(Event::Exec(result(2, "ok")));
    assert_eq!(again.len(), 1);
    assert!(matches!(again[0].op, Op::Model(_)));

    // And a cancel that arrived ahead of its turn did not spend that id.
    let ended = session.step(says("built", again[0].id));
    assert_eq!(
        ended.last().map(|a| &a.op),
        Some(&done(1, Outcome::Complete))
    );
    assert_eq!(session.step(turn(2, "and again")).len(), 1);
}

#[test]
fn a_path_that_leaves_the_workspace_is_refused_rather_than_dispatched() {
    let mut session = Session::new(Config::default());
    session.step(turn(1, "read my keys"));

    let dispatched = session.step(tools(
        vec![read("../../../../etc/passwd"), read("src/lib.rs")],
        1,
    ));
    // Only the confined read goes out; the other never becomes an action.
    assert_eq!(dispatched.len(), 1);
    assert_eq!(
        ops(&dispatched),
        vec![&Op::Read(Read {
            path: "src/lib.rs".to_string()
        })]
    );
    assert_eq!(dispatched[0].id, 3);

    // The model is told, under the id its own call was recorded with.
    let again = session.step(Event::File(result(3, "the file")));
    assert_eq!(
        asked(&again[0]).messages[1..],
        [
            Message::Agent {
                text: None,
                calls: vec![
                    Dispatch {
                        id: 2,
                        call: read("../../../../etc/passwd"),
                    },
                    Dispatch {
                        id: 3,
                        call: read("src/lib.rs"),
                    },
                ],
            },
            Message::Tool {
                id: 2,
                text: "refused: ../../../../etc/passwd is not inside the workspace".to_string(),
                failed: true,
            },
            Message::Tool {
                id: 3,
                text: "the file".to_string(),
                failed: false,
            },
        ]
    );
}

#[test]
fn a_turn_whose_every_call_is_refused_asks_again_instead_of_waiting() {
    let mut session = Session::new(Config::default());
    session.step(turn(1, "read my keys"));

    let answered = session.step(tools(vec![read("/Users/z/.ssh/authorized_keys")], 1));
    assert_eq!(answered.len(), 1, "{:?}", ops(&answered));
    let ask = asked(&answered[0]);
    assert_eq!(
        ask.messages.last(),
        Some(&Message::Tool {
            id: 2,
            text: "refused: /Users/z/.ssh/authorized_keys is not inside the workspace".to_string(),
            failed: true,
        })
    );

    // And the turn is still live: it ends the ordinary way.
    let ended = session.step(says("I cannot read that", answered[0].id));
    assert_eq!(
        ended.last().map(|a| &a.op),
        Some(&done(1, Outcome::Complete))
    );
}

#[test]
fn a_snapshot_the_store_handed_back_changed_is_refused() {
    let mut session = Session::new(Config::default());
    session.step(turn(1, "fix the build"));
    let state = session.snapshot().expect("snapshot");

    // Re-mint an id the host has already dispatched under.
    let stamp = 12;
    let body = String::from_utf8(state[stamp..].to_vec()).expect("a json body");
    let forged = body.replace("\"next\":2", "\"next\":1");
    assert_ne!(forged, body, "the snapshot no longer spells the next id");
    let mut bytes = state[..stamp].to_vec();
    bytes.extend_from_slice(forged.as_bytes());

    let err = Session::restore(&bytes).expect_err("a forged snapshot was restored");
    assert!(err.to_string().contains("checksum"), "{err}");
}
