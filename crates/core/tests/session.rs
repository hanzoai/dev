#![allow(clippy::expect_used, clippy::unwrap_used)]

use dev_core::Session;
use dev_protocol::Action;
use dev_protocol::Answer;
use dev_protocol::Call;
use dev_protocol::Config;
use dev_protocol::Event;
use dev_protocol::Exec;
use dev_protocol::Op;
use dev_protocol::Outcome;
use dev_protocol::Output;
use dev_protocol::Reply;
use dev_protocol::Turn;

fn turn(prompt: &str) -> Event {
    Event::Turn(Turn {
        prompt: prompt.to_string(),
    })
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

fn ops(actions: &[Action]) -> Vec<&Op> {
    actions.iter().map(|a| &a.op).collect()
}

#[test]
fn a_turn_asks_a_model_then_runs_the_tool_then_asks_again() {
    let mut session = Session::new(Config::default());

    let asked = session.step(turn("fix the build"));
    assert_eq!(asked.len(), 1);
    assert_eq!(asked[0].id, 1);
    assert!(matches!(asked[0].op, Op::Model(_)));

    let dispatched = session.step(tools(vec![test()], 1));
    assert_eq!(dispatched.len(), 1);
    assert_eq!(dispatched[0].id, 2);
    assert!(matches!(dispatched[0].op, Op::Exec(_)));

    let again = session.step(Event::Exec(Output {
        id: 2,
        text: "ok".to_string(),
        failed: false,
    }));
    assert_eq!(again.len(), 1);
    assert_eq!(again[0].id, 3);
    assert!(matches!(again[0].op, Op::Model(_)));

    let done = session.step(says("built", 3));
    assert_eq!(
        ops(&done),
        vec![
            &Op::Emit("built".to_string()),
            &Op::Save,
            &Op::Done(Outcome::Complete),
        ]
    );
    assert_eq!(done[0].id, 4);
    assert_eq!(done[2].id, 6);
}

#[test]
fn every_call_of_one_reply_gets_its_own_id() {
    let mut session = Session::new(Config::default());
    session.step(turn("run the suite twice"));

    let dispatched = session.step(tools(vec![test(), test()], 1));
    let ids: Vec<u64> = dispatched.iter().map(|a| a.id).collect();
    assert_eq!(ids, vec![2, 3]);

    // One answer is not enough: the model is asked again only when the last
    // outstanding effect has been observed.
    let quiet = session.step(Event::Exec(Output {
        id: 2,
        text: "ok".to_string(),
        failed: false,
    }));
    assert!(quiet.is_empty());
    let asked = session.step(Event::Exec(Output {
        id: 3,
        text: "ok".to_string(),
        failed: false,
    }));
    assert_eq!(asked.len(), 1);
    assert!(matches!(asked[0].op, Op::Model(_)));
}

#[test]
fn a_replayed_result_runs_nothing_twice() {
    let mut session = Session::new(Config::default());
    session.step(turn("fix the build"));
    session.step(tools(vec![test()], 1));

    let answered = Event::Exec(Output {
        id: 2,
        text: "ok".to_string(),
        failed: false,
    });
    assert_eq!(session.step(answered.clone()).len(), 1);
    assert!(session.step(answered).is_empty());
    // The same id delivered by the wrong family is refused too.
    assert!(
        session
            .step(Event::Git(Output {
                id: 2,
                text: "ok".to_string(),
                failed: false,
            }))
            .is_empty()
    );
}

#[test]
fn snapshot_and_restore_continue_at_the_same_id() {
    let mut session = Session::new(Config::default());
    session.step(turn("fix the build"));

    let state = session.snapshot();
    let mut restored = Session::restore(&state).expect("restore");
    assert_eq!(restored.next_id(), session.next_id());
    assert_eq!(restored.messages(), session.messages());

    let here = session.step(tools(vec![test()], 1));
    let there = restored.step(tools(vec![test()], 1));
    assert_eq!(here, there);
    assert_eq!(there[0].id, 2);
}

#[test]
fn a_prompt_mid_turn_waits_for_the_turn() {
    let mut session = Session::new(Config::default());
    session.step(turn("first"));
    assert!(session.step(turn("second")).is_empty());

    let actions = session.step(says("first done", 1));
    assert_eq!(actions.len(), 3);
    assert_eq!(actions[0].op, Op::Emit("first done".to_string()));
    assert_eq!(actions[1].op, Op::Save);
    assert!(matches!(actions[2].op, Op::Model(_)));

    let closed = session.step(says("second done", actions[2].id));
    assert!(matches!(closed.last().map(|a| &a.op), Some(Op::Done(_))));
}

#[test]
fn a_timer_checkpoints_only_inside_a_turn() {
    let mut session = Session::new(Config::default());
    assert!(session.step(Event::Timer).is_empty());

    session.step(turn("fix the build"));
    let saved = session.step(Event::Timer);
    assert_eq!(ops(&saved), vec![&Op::Save]);
}

#[test]
fn cancel_ends_the_turn_and_drops_what_was_outstanding() {
    let mut session = Session::new(Config::default());
    session.step(turn("fix the build"));
    session.step(tools(vec![test()], 1));

    let stopped = session.step(Event::Cancel);
    assert_eq!(
        ops(&stopped),
        vec![&Op::Save, &Op::Done(Outcome::Cancelled)]
    );
    // A result for the cancelled effect is no longer outstanding.
    assert!(
        session
            .step(Event::Exec(Output {
                id: 2,
                text: "ok".to_string(),
                failed: false,
            }))
            .is_empty()
    );
    assert!(session.step(Event::Cancel).is_empty());
}

#[test]
fn an_answer_to_an_id_that_was_never_asked_is_refused() {
    let mut session = Session::new(Config::default());
    session.step(turn("fix the build"));
    assert!(session.step(says("hello", 99)).is_empty());
}
