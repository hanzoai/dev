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
    ApplyPatchApproval { seq: u64, id: String, ev: ApplyPatchApprovalRequestEvent, order: Option<codex_core::protocol::OrderMeta>, turn_win: Option<usize> },
    ExecBegin { seq: u64, ev: ExecCommandBeginEvent, order: Option<codex_core::protocol::OrderMeta>, turn_win: Option<usize>, turn_id: Option<String> },
    ExecEnd { seq: u64, ev: ExecCommandEndEvent, turn_win: Option<usize> },
    McpBegin { seq: u64, ev: McpToolCallBeginEvent, order: Option<codex_core::protocol::OrderMeta>, turn_win: Option<usize>, turn_id: Option<String> },
    McpEnd { seq: u64, ev: McpToolCallEndEvent, turn_win: Option<usize> },
    PatchEnd { seq: u64, ev: PatchApplyEndEvent, order: Option<codex_core::protocol::OrderMeta>, turn_win: Option<usize> },
    PlanUpdate { seq: u64, ev: UpdatePlanArgs, order: Option<codex_core::protocol::OrderMeta>, turn_win: Option<usize> },
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
        order: Option<codex_core::protocol::OrderMeta>,
        turn_win: Option<usize>,
    ) {
        self.queue.push(QueuedInterrupt::ApplyPatchApproval { seq, id, ev, order, turn_win });
    }

    pub(crate) fn push_exec_begin(&mut self, seq: u64, ev: ExecCommandBeginEvent, order: Option<codex_core::protocol::OrderMeta>, turn_win: Option<usize>, turn_id: Option<String>) {
        self.queue.push(QueuedInterrupt::ExecBegin { seq, ev, order, turn_win, turn_id });
    }

    pub(crate) fn push_exec_end(&mut self, seq: u64, ev: ExecCommandEndEvent, turn_win: Option<usize>) {
        self.queue.push(QueuedInterrupt::ExecEnd { seq, ev, turn_win });
    }

    pub(crate) fn push_mcp_begin(&mut self, seq: u64, ev: McpToolCallBeginEvent, order: Option<codex_core::protocol::OrderMeta>, turn_win: Option<usize>, turn_id: Option<String>) {
        self.queue.push(QueuedInterrupt::McpBegin { seq, ev, order, turn_win, turn_id });
    }

    pub(crate) fn push_mcp_end(&mut self, seq: u64, ev: McpToolCallEndEvent, turn_win: Option<usize>) {
        self.queue.push(QueuedInterrupt::McpEnd { seq, ev, turn_win });
    }

    pub(crate) fn push_patch_end(&mut self, seq: u64, ev: PatchApplyEndEvent, order: Option<codex_core::protocol::OrderMeta>, turn_win: Option<usize>) {
        self.queue.push(QueuedInterrupt::PatchEnd { seq, ev, order, turn_win });
    }

    pub(crate) fn push_plan_update(&mut self, seq: u64, ev: UpdatePlanArgs, order: Option<codex_core::protocol::OrderMeta>, turn_win: Option<usize>) {
        self.queue.push(QueuedInterrupt::PlanUpdate { seq, ev, order, turn_win });
    }

    pub(crate) fn flush_all(&mut self, chat: &mut ChatWidget<'_>) {
        // Ensure stable order
        self.queue.sort_by(|a, b| seq_of(a).cmp(&seq_of(b)));
        for q in self.queue.drain(..) {
            match q {
                QueuedInterrupt::ExecApproval { id, ev, .. } => chat.handle_exec_approval_now(id, ev),
                QueuedInterrupt::ApplyPatchApproval { seq, id, ev, order, turn_win } => {
                    let ok = super::ChatWidget::order_key_from_order_meta(order.as_ref())
                        .unwrap_or_else(|| chat.next_unordered_seq());
                    tracing::info!("[order/flush] ApplyPatchApproval: queue_seq={} chosen_key={}", seq, ok.0);
                    chat.set_pending_insert_seq(ok.0);
                    if let Some(wi) = turn_win { chat.pending_insert_turn = Some(wi); }
                    chat.handle_apply_patch_approval_now(id, ev);
                }
                QueuedInterrupt::ExecBegin { seq, ev, order, turn_win, turn_id } => {
                    let ok = super::ChatWidget::order_key_from_order_meta(order.as_ref())
                        .unwrap_or_else(|| chat.next_unordered_seq());
                    tracing::info!("[order/flush] ExecBegin: queue_seq={} chosen_key={}", seq, ok.0);
                    chat.set_pending_insert_seq(ok.0);
                    if let Some(wi) = turn_win { chat.pending_insert_turn = Some(wi); }
                    if let Some(tid) = turn_id { chat.call_id_to_turn.insert(ev.call_id.clone(), tid); }
                    chat.handle_exec_begin_now(ev);
                }
                QueuedInterrupt::ExecEnd { ev, turn_win, .. } => { if let Some(wi) = turn_win { chat.pending_insert_turn = Some(wi); } chat.handle_exec_end_now(ev) },
                QueuedInterrupt::McpBegin { seq, ev, order, turn_win, turn_id } => {
                    let ok = super::ChatWidget::order_key_from_order_meta(order.as_ref())
                        .unwrap_or_else(|| chat.next_unordered_seq());
                    tracing::info!("[order/flush] McpBegin: queue_seq={} chosen_key={}", seq, ok.0);
                    chat.set_pending_insert_seq(ok.0);
                    if let Some(wi) = turn_win { chat.pending_insert_turn = Some(wi); }
                    if let Some(tid) = turn_id { chat.call_id_to_turn.insert(ev.call_id.clone(), tid); }
                    tools::mcp_begin(chat, ev);
                }
                QueuedInterrupt::McpEnd { ev, turn_win, .. } => { if let Some(wi) = turn_win { chat.pending_insert_turn = Some(wi); } tools::mcp_end(chat, ev) },
                QueuedInterrupt::PatchEnd { seq, ev, order, turn_win } => {
                    let ok = super::ChatWidget::order_key_from_order_meta(order.as_ref())
                        .unwrap_or_else(|| chat.next_unordered_seq());
                    tracing::info!("[order/flush] PatchEnd: queue_seq={} chosen_key={}", seq, ok.0);
                    chat.set_pending_insert_seq(ok.0);
                    if let Some(wi) = turn_win { chat.pending_insert_turn = Some(wi); }
                    chat.handle_patch_apply_end_now(ev);
                }
                QueuedInterrupt::PlanUpdate { seq, ev, order, turn_win } => {
                    let ok = super::ChatWidget::order_key_from_order_meta(order.as_ref())
                        .unwrap_or_else(|| chat.next_unordered_seq());
                    tracing::info!("[order/flush] PlanUpdate: queue_seq={} chosen_key={}", seq, ok.0);
                    if let Some(wi) = turn_win { chat.pending_insert_turn = Some(wi); }
                    chat.history_insert_with_seq(crate::history_cell::new_plan_update(ev), ok.0);
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
