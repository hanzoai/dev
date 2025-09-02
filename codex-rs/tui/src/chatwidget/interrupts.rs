// (none)

use codex_core::protocol::ApplyPatchApprovalRequestEvent;
use codex_core::protocol::ExecApprovalRequestEvent;
use codex_core::protocol::ExecCommandBeginEvent;
use codex_core::protocol::ExecCommandEndEvent;
use codex_core::protocol::McpToolCallBeginEvent;
use codex_core::protocol::McpToolCallEndEvent;
use codex_core::protocol::PatchApplyEndEvent;
use codex_protocol::plan_tool::UpdatePlanArgs;

use super::ChatWidget;
use super::tools;

#[derive(Debug)]
pub(crate) enum QueuedInterrupt {
    ExecApproval { seq: u64, id: String, ev: ExecApprovalRequestEvent },
    ApplyPatchApproval { seq: u64, id: String, ev: ApplyPatchApprovalRequestEvent },
    ExecBegin { seq: u64, ev: ExecCommandBeginEvent },
    ExecEnd { seq: u64, ev: ExecCommandEndEvent },
    McpBegin { seq: u64, ev: McpToolCallBeginEvent },
    McpEnd { seq: u64, ev: McpToolCallEndEvent },
    PatchEnd { seq: u64, ev: PatchApplyEndEvent },
    PlanUpdate { seq: u64, ev: UpdatePlanArgs },
}

#[derive(Default)]
pub(crate) struct InterruptManager {
    queue: Vec<QueuedInterrupt>,
}

impl InterruptManager {
    pub(crate) fn new() -> Self {
        Self {
            queue: Vec::new(),
        }
    }


    pub(crate) fn push_exec_approval(&mut self, seq: u64, id: String, ev: ExecApprovalRequestEvent) {
        self.queue.push(QueuedInterrupt::ExecApproval { seq, id, ev });
    }

    pub(crate) fn push_apply_patch_approval(
        &mut self,
        seq: u64,
        id: String,
        ev: ApplyPatchApprovalRequestEvent,
    ) {
        self.queue.push(QueuedInterrupt::ApplyPatchApproval { seq, id, ev });
    }

    pub(crate) fn push_exec_begin(&mut self, seq: u64, ev: ExecCommandBeginEvent) {
        self.queue.push(QueuedInterrupt::ExecBegin { seq, ev });
    }

    pub(crate) fn push_exec_end(&mut self, seq: u64, ev: ExecCommandEndEvent) {
        self.queue.push(QueuedInterrupt::ExecEnd { seq, ev });
    }

    pub(crate) fn push_mcp_begin(&mut self, seq: u64, ev: McpToolCallBeginEvent) {
        self.queue.push(QueuedInterrupt::McpBegin { seq, ev });
    }

    pub(crate) fn push_mcp_end(&mut self, seq: u64, ev: McpToolCallEndEvent) {
        self.queue.push(QueuedInterrupt::McpEnd { seq, ev });
    }

    pub(crate) fn push_patch_end(&mut self, seq: u64, ev: PatchApplyEndEvent) {
        self.queue.push(QueuedInterrupt::PatchEnd { seq, ev });
    }

    pub(crate) fn push_plan_update(&mut self, seq: u64, ev: UpdatePlanArgs) {
        self.queue.push(QueuedInterrupt::PlanUpdate { seq, ev });
    }

    pub(crate) fn flush_all(&mut self, chat: &mut ChatWidget<'_>) {
        // Ensure stable order
        self.queue.sort_by(|a, b| seq_of(a).cmp(&seq_of(b)));
        for q in self.queue.drain(..) {
            match q {
                QueuedInterrupt::ExecApproval { id, ev, .. } => chat.handle_exec_approval_now(id, ev),
                QueuedInterrupt::ApplyPatchApproval { seq, id, ev } => {
                    chat.handle_apply_patch_approval_now(id, ev);
                    chat.maybe_move_last_before_final_assistant(seq);
                }
                QueuedInterrupt::ExecBegin { ev, .. } => {
                    let call_id = ev.call_id.clone();
                    chat.handle_exec_begin_now(ev);
                    chat.maybe_move_last_before_final_assistant_exec(&call_id);
                }
                QueuedInterrupt::ExecEnd { ev, .. } => chat.handle_exec_end_now(ev),
                QueuedInterrupt::McpBegin { ev, .. } => {
                    let call_id = ev.call_id.clone();
                    tools::mcp_begin(chat, ev);
                    chat.maybe_move_last_before_final_assistant_tool(&call_id);
                }
                QueuedInterrupt::McpEnd { ev, .. } => tools::mcp_end(chat, ev),
                QueuedInterrupt::PatchEnd { seq, ev, .. } => {
                    chat.handle_patch_apply_end_now(ev);
                    chat.maybe_move_last_before_final_assistant(seq);
                }
                QueuedInterrupt::PlanUpdate { seq, ev, .. } => {
                    chat.history_push(crate::history_cell::new_plan_update(ev));
                    chat.maybe_move_last_before_final_assistant(seq);
                }
            }
        }
    }
}

fn seq_of(q: &QueuedInterrupt) -> u64 {
    match q {
        QueuedInterrupt::ExecApproval { seq, .. }
        | QueuedInterrupt::ApplyPatchApproval { seq, .. }
        | QueuedInterrupt::ExecBegin { seq, .. }
        | QueuedInterrupt::ExecEnd { seq, .. }
        | QueuedInterrupt::McpBegin { seq, .. }
        | QueuedInterrupt::McpEnd { seq, .. }
        | QueuedInterrupt::PatchEnd { seq, .. }
        | QueuedInterrupt::PlanUpdate { seq, .. } => *seq,
    }
}
