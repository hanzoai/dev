//! Exec and tool call lifecycle helpers for `ChatWidget`.

use super::ChatWidget;
use crate::app_event::AppEvent;
use crate::height_manager::HeightEvent;
use crate::history_cell::{self, HistoryCell};
use crate::history_cell::CommandOutput;
use codex_core::protocol::{ExecCommandBeginEvent, ExecCommandEndEvent, OrderMeta};

pub(super) fn finalize_exec_cell_at(
    chat: &mut ChatWidget<'_>,
    idx: usize,
    exit_code: i32,
    stdout: String,
    stderr: String,
) {
    if idx >= chat.history_cells.len() { return; }
    if let Some(exec) = chat.history_cells[idx]
        .as_any()
        .downcast_ref::<history_cell::ExecCell>()
    {
        if exec.output.is_none() {
            let completed = history_cell::new_completed_exec_command(
                exec.command.clone(),
                exec.parsed.clone(),
                CommandOutput { exit_code, stdout, stderr },
            );
            chat.history_replace_at(idx, Box::new(completed));
        }
    }
}

pub(super) fn finalize_all_running_as_interrupted(chat: &mut ChatWidget<'_>) {
    let interrupted_msg = "Cancelled by user.".to_string();
    let stdout_empty = String::new();
    let running: Vec<(super::ExecCallId, Option<usize>)> = chat
        .exec
        .running_commands
        .iter()
        .map(|(k, v)| (k.clone(), v.history_index))
        .collect();
    for (_call_id, maybe_idx) in running {
        if let Some(idx) = maybe_idx {
            finalize_exec_cell_at(chat, idx, 130, stdout_empty.clone(), interrupted_msg.clone());
        }
    }
    // Track cancelled exec call_ids so late ExecEnd events are dropped.
    for (call_id, _) in chat.exec.running_commands.iter() {
        chat.canceled_exec_call_ids.insert(call_id.clone());
    }
    chat.exec.running_commands.clear();

    if !chat.tools_state.running_custom_tools.is_empty() {
        let entries: Vec<(super::ToolCallId, usize)> = chat
            .tools_state
            .running_custom_tools
            .iter()
            .map(|(k, i)| (k.clone(), *i))
            .collect();
        for (_k, idx) in entries {
            if idx < chat.history_cells.len() {
                let completed = history_cell::new_completed_custom_tool_call(
                    "custom".to_string(),
                    None,
                    std::time::Duration::from_millis(0),
                    false,
                    "Cancelled by user.".to_string(),
                );
                chat.history_replace_at(idx, Box::new(completed));
            }
        }
        chat.tools_state.running_custom_tools.clear();
        chat.invalidate_height_cache();
        chat.request_redraw();
    }

    if !chat.tools_state.running_web_search.is_empty() {
        let entries: Vec<(super::ToolCallId, (usize, Option<String>))> = chat
            .tools_state
            .running_web_search
            .iter()
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect();
        for (call_id, (idx, query_opt)) in entries {
            let mut target_idx = None;
            if idx < chat.history_cells.len() {
                let is_ws = chat.history_cells[idx]
                    .as_any()
                    .downcast_ref::<history_cell::RunningToolCallCell>()
                    .is_some_and(|rt| rt.has_title("Web Search..."));
                if is_ws { target_idx = Some(idx); }
            }
            if target_idx.is_none() {
                for i in (0..chat.history_cells.len()).rev() {
                    if let Some(rt) = chat.history_cells[i]
                        .as_any()
                        .downcast_ref::<history_cell::RunningToolCallCell>()
                    {
                        if rt.has_title("Web Search...") { target_idx = Some(i); break; }
                    }
                }
            }
            if let Some(i) = target_idx {
                if let Some(rt) = chat.history_cells[i]
                    .as_any()
                    .downcast_ref::<history_cell::RunningToolCallCell>()
                {
                    let completed = rt.finalize_web_search(false, query_opt);
                    chat.history_replace_at(i, Box::new(completed));
                }
            }
            chat.tools_state.running_web_search.remove(&call_id);
        }
    }

    chat.bottom_pane.update_status_text("cancelled".to_string());
    let any_tasks_active = !chat.active_task_ids.is_empty();
    if !any_tasks_active {
        chat.bottom_pane.set_task_running(false);
    }
    if let Some(idx) = chat.exec.running_read_agg_index.take() {
        if idx < chat.history_cells.len() {
            if let Some(agg) = chat.history_cells[idx]
                .as_any_mut()
                .downcast_mut::<history_cell::ReadAggregationCell>()
            {
                agg.finalize();
                chat.invalidate_height_cache();
                chat.request_redraw();
            }
        }
    }
    chat.maybe_hide_spinner();
}

pub(super) fn finalize_all_running_due_to_answer(chat: &mut ChatWidget<'_>) {
    let note = "Completed (final answer received)".to_string();
    let stdout_empty = String::new();
    let running: Vec<(super::ExecCallId, Option<usize>)> = chat
        .exec
        .running_commands
        .iter()
        .map(|(k, v)| (k.clone(), v.history_index))
        .collect();
    for (_call_id, maybe_idx) in running {
        if let Some(idx) = maybe_idx {
            finalize_exec_cell_at(chat, idx, 0, stdout_empty.clone(), note.clone());
        }
    }
    chat.exec.running_commands.clear();

    if !chat.tools_state.running_custom_tools.is_empty() {
        let entries: Vec<(super::ToolCallId, usize)> = chat
            .tools_state
            .running_custom_tools
            .iter()
            .map(|(k, i)| (k.clone(), *i))
            .collect();
        for (_k, idx) in entries {
            if idx < chat.history_cells.len() {
                let completed = history_cell::new_completed_custom_tool_call(
                    "custom".to_string(),
                    None,
                    std::time::Duration::from_millis(0),
                    true,
                    "Final answer received".to_string(),
                );
                chat.history_replace_at(idx, Box::new(completed));
            }
        }
        chat.tools_state.running_custom_tools.clear();
        chat.invalidate_height_cache();
        chat.request_redraw();
    }

    if !chat.tools_state.running_web_search.is_empty() {
        let entries: Vec<(super::ToolCallId, (usize, Option<String>))> = chat
            .tools_state
            .running_web_search
            .iter()
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect();
        for (call_id, (idx, query_opt)) in entries {
            let mut target_idx = None;
            if idx < chat.history_cells.len() {
                let is_ws = chat.history_cells[idx]
                    .as_any()
                    .downcast_ref::<history_cell::RunningToolCallCell>()
                    .is_some_and(|rt| rt.has_title("Web Search..."));
                if is_ws { target_idx = Some(idx); }
            }
            if target_idx.is_none() {
                for i in (0..chat.history_cells.len()).rev() {
                    if let Some(rt) = chat.history_cells[i]
                        .as_any()
                        .downcast_ref::<history_cell::RunningToolCallCell>()
                    {
                        if rt.has_title("Web Search...") { target_idx = Some(i); break; }
                    }
                }
            }
            if let Some(i) = target_idx {
                if let Some(rt) = chat.history_cells[i]
                    .as_any()
                    .downcast_ref::<history_cell::RunningToolCallCell>()
                {
                    let completed = rt.finalize_web_search(true, query_opt);
                    chat.history_replace_at(i, Box::new(completed));
                }
            }
            chat.tools_state.running_web_search.remove(&call_id);
        }
    }
    if let Some(idx) = chat.exec.running_read_agg_index.take() {
        if idx < chat.history_cells.len() {
            if let Some(agg) = chat.history_cells[idx]
                .as_any_mut()
                .downcast_mut::<history_cell::ReadAggregationCell>()
            {
                agg.finalize();
                chat.invalidate_height_cache();
                chat.request_redraw();
            }
        }
    }
}

pub(super) fn try_merge_completed_exec_at(chat: &mut ChatWidget<'_>, idx: usize) {
    if idx == 0 || idx >= chat.history_cells.len() {
        return;
    }
    let to_kind = |e: &history_cell::ExecCell| -> history_cell::ExecKind {
        match history_cell::action_enum_from_parsed(&e.parsed) {
            history_cell::ExecAction::Read => history_cell::ExecKind::Read,
            history_cell::ExecAction::Search => history_cell::ExecKind::Search,
            history_cell::ExecAction::List => history_cell::ExecKind::List,
            history_cell::ExecAction::Run => history_cell::ExecKind::Run,
        }
    };

    let new_exec = match chat.history_cells[idx]
        .as_any()
        .downcast_ref::<history_cell::ExecCell>()
    {
        Some(e) if e.output.is_some() => e,
        _ => return,
    };
    let new_kind = to_kind(new_exec);
    if matches!(new_kind, history_cell::ExecKind::Run) { return; }

    if let Some(prev_exec) = chat.history_cells[idx - 1]
        .as_any()
        .downcast_ref::<history_cell::ExecCell>()
    {
        if prev_exec.output.is_some() {
            if to_kind(prev_exec) == new_kind {
                let mut merged = history_cell::MergedExecCell::from_exec(prev_exec);
                if let Some(current_exec) = chat.history_cells[idx]
                    .as_any()
                    .downcast_ref::<history_cell::ExecCell>()
                {
                    merged.push_exec(current_exec);
                }
                chat.history_replace_at(idx - 1, Box::new(merged));
                chat.history_remove_at(idx);
                chat.invalidate_height_cache();
                chat.autoscroll_if_near_bottom();
                chat.bottom_pane.set_has_chat_history(true);
                chat.process_animation_cleanup();
                chat.app_event_tx.send(AppEvent::RequestRedraw);
                return;
            }
        }
    }

    let mut did_merge_into_prev = false;
    if idx < chat.history_cells.len() {
        let (left, right) = chat.history_cells.split_at_mut(idx);
        if let Some(prev_merged) = left[idx - 1]
            .as_any_mut()
            .downcast_mut::<history_cell::MergedExecCell>()
        {
            if prev_merged.exec_kind() == new_kind {
                if let Some(current_exec) = right[0]
                    .as_any()
                    .downcast_ref::<history_cell::ExecCell>()
                {
                    prev_merged.push_exec(current_exec);
                    did_merge_into_prev = true;
                }
            }
        }
    }
    if did_merge_into_prev {
        chat.history_remove_at(idx);
        chat.invalidate_height_cache();
        chat.autoscroll_if_near_bottom();
        chat.bottom_pane.set_has_chat_history(true);
        chat.process_animation_cleanup();
        chat.app_event_tx.send(AppEvent::RequestRedraw);
    }
}

pub(super) fn handle_exec_begin_now(chat: &mut ChatWidget<'_>, ev: ExecCommandBeginEvent, order: &OrderMeta) {
    if chat.ended_call_ids.contains(&super::ExecCallId(ev.call_id.clone())) { return; }
    for cell in &chat.history_cells { cell.trigger_fade(); }
    let parsed_command = ev.parsed_cmd.clone();
    let action = history_cell::action_enum_from_parsed(&parsed_command);
    chat.height_manager.borrow_mut().record_event(HeightEvent::RunBegin);

    if matches!(action, history_cell::ExecAction::Read) {
        chat.exec.running_commands.insert(
            super::ExecCallId(ev.call_id.clone()),
            super::RunningCommand { command: ev.command.clone(), parsed: parsed_command.clone(), history_index: None },
        );
            let agg_index = match chat.exec.running_read_agg_index {
                Some(idx) if idx < chat.history_cells.len()
                    && chat.history_cells[idx]
                        .as_any()
                        .downcast_ref::<history_cell::ReadAggregationCell>()
                        .is_some() => Some(idx),
                _ => None,
            };
        let idx = if let Some(i) = agg_index { i } else {
            // Reserve an ordered slot for the read aggregation header if the provider
            // supplied OrderMeta; otherwise it will fall back to unordered.
            let key = ChatWidget::order_key_from_order_meta(order);
            let i = chat.history_insert_with_key_global(Box::new(history_cell::ReadAggregationCell::new()), key);
            // If the immediately-previous cell is also a ReadAggregationCell (from an
            // earlier attempt), merge into it so consecutive Read blocks collapse into
            // a single "Read" section as per UX rules.
            if i > 0 {
                let prev_is_read_agg = chat.history_cells[i - 1]
                    .as_any()
                    .downcast_ref::<history_cell::ReadAggregationCell>()
                    .is_some();
                if prev_is_read_agg {
                    // Move this new cell's lines into the previous aggregator, then remove self.
                    if let Some(new_cell) = chat.history_cells[i]
                        .as_any_mut()
                        .downcast_mut::<history_cell::ReadAggregationCell>()
                    {
                        let lines = new_cell.display_lines();
                        // drop header and push body lines only
                        let mut body: Vec<ratatui::text::Line<'static>> = lines.into_iter().skip(1).collect();
                        if let Some(prev_cell) = chat.history_cells[i - 1]
                            .as_any_mut()
                            .downcast_mut::<history_cell::ReadAggregationCell>()
                        {
                            prev_cell.push_lines(body.drain(..).collect());
                        }
                    }
                    chat.history_remove_at(i);
                    chat.invalidate_height_cache();
                    chat.autoscroll_if_near_bottom();
                    chat.bottom_pane.set_has_chat_history(true);
                    chat.process_animation_cleanup();
                    chat.app_event_tx.send(AppEvent::RequestRedraw);
                    // Use the previous cell as the active aggregator
                    chat.exec.running_read_agg_index = Some(i - 1);
                    i - 1
                } else {
                    chat.exec.running_read_agg_index = Some(i);
                    i
                }
            } else {
                chat.exec.running_read_agg_index = Some(i);
                i
            }
        };
        let tmp = history_cell::new_active_exec_command(ev.command.clone(), parsed_command.clone());
        let mut lines = tmp.display_lines();
        if !lines.is_empty() { lines.remove(0); }
        if let Some(agg) = chat.history_cells[idx]
            .as_any_mut()
            .downcast_mut::<history_cell::ReadAggregationCell>()
        {
            agg.push_lines(lines);
            chat.invalidate_height_cache();
            chat.request_redraw();
        }
        chat.bottom_pane.update_status_text("reading files…".to_string());
        return;
    }

    let cell = history_cell::new_active_exec_command(ev.command.clone(), parsed_command.clone());
    let key = ChatWidget::order_key_from_order_meta(order);
    let idx = chat.history_insert_with_key_global(Box::new(cell), key);
    chat.exec.running_commands.insert(
        super::ExecCallId(ev.call_id.clone()),
        super::RunningCommand { command: ev.command.clone(), parsed: parsed_command, history_index: Some(idx) },
    );
    if !chat.tools_state.running_web_search.is_empty() {
        chat.bottom_pane.update_status_text("Searched".to_string());
    } else {
        let preview = chat
            .exec
            .running_commands
            .get(&super::ExecCallId(ev.call_id.clone()))
            .map(|rc| rc.command.join(" "))
            .unwrap_or_else(|| "command".to_string());
        let preview_short = if preview.chars().count() > 40 {
            let mut truncated: String = preview.chars().take(40).collect();
            truncated.push('…');
            truncated
        } else {
            preview
        };
        chat.bottom_pane
            .update_status_text(format!("running command: {}", preview_short));
    }
}

pub(super) fn handle_exec_end_now(chat: &mut ChatWidget<'_>, ev: ExecCommandEndEvent, order: &OrderMeta) {
    chat.ended_call_ids.insert(super::ExecCallId(ev.call_id.clone()));
    // If this call was already marked as cancelled, drop the End to avoid
    // inserting a duplicate completed cell after the user interrupt.
    if chat
        .canceled_exec_call_ids
        .remove(&super::ExecCallId(ev.call_id.clone()))
    {
        chat.maybe_hide_spinner();
        return;
    }
    let ExecCommandEndEvent { call_id, exit_code, duration: _, stdout, stderr } = ev;
    let cmd = chat.exec.running_commands.remove(&super::ExecCallId(call_id.clone()));
    chat.height_manager.borrow_mut().record_event(HeightEvent::RunEnd);
    let (command, parsed, history_index) = cmd
        .map(|cmd| (cmd.command, cmd.parsed, cmd.history_index))
        .unwrap_or_else(|| (vec![call_id.clone()], vec![], None));

    let action = history_cell::action_enum_from_parsed(&parsed);
    if matches!(action, history_cell::ExecAction::Read) {
        let any_read_running = chat
            .exec
            .running_commands
            .values()
            .any(|rc| matches!(history_cell::action_enum_from_parsed(&rc.parsed), history_cell::ExecAction::Read));
        if !any_read_running {
            if let Some(idx) = chat.exec.running_read_agg_index.take() {
                if idx < chat.history_cells.len() {
                    if let Some(agg) = chat.history_cells[idx]
                        .as_any_mut()
                        .downcast_mut::<history_cell::ReadAggregationCell>()
                    {
                        agg.finalize();
                        chat.invalidate_height_cache();
                        chat.request_redraw();
                    }
                }
            }
        }
        if exit_code == 0 {
            chat.bottom_pane.update_status_text("files read".to_string());
        } else {
            chat.bottom_pane.update_status_text(format!("read failed (exit {})", exit_code));
        }
        chat.maybe_hide_spinner();
        return;
    }

    let command_for_watch = command.clone();
    let mut completed_opt = Some(history_cell::new_completed_exec_command(
        command,
        parsed,
        CommandOutput { exit_code, stdout, stderr },
    ));

    let mut replaced = false;
    if let Some(idx) = history_index {
        if idx < chat.history_cells.len() {
            let is_match = chat.history_cells[idx]
                .as_any()
                .downcast_ref::<history_cell::ExecCell>()
                .map(|e| {
                    if let Some(ref c) = completed_opt {
                        e.output.is_none() && e.command == c.command
                    } else { false }
                })
                .unwrap_or(false);
            if is_match {
                if let Some(c) = completed_opt.take() { chat.history_replace_and_maybe_merge(idx, Box::new(c)); }
                replaced = true;
            }
        }
        if !replaced {
            let mut found: Option<usize> = None;
            for i in (0..chat.history_cells.len()).rev() {
                if let Some(exec) = chat.history_cells[i].as_any().downcast_ref::<history_cell::ExecCell>() {
                    let is_same = if let Some(ref c) = completed_opt { exec.command == c.command } else { false };
                    if exec.output.is_none() && is_same { found = Some(i); break; }
                }
            }
            if let Some(i) = found {
                if let Some(c) = completed_opt.take() { chat.history_replace_and_maybe_merge(i, Box::new(c)); }
                replaced = true;
            }
        }
    }

    if !replaced {
        if let Some(c) = completed_opt.take() {
            let key = ChatWidget::order_key_from_order_meta(order);
            let idx = chat.history_insert_with_key_global(Box::new(c), key);
            // Attempt standard merge with previous Exec if applicable.
            crate::chatwidget::exec_tools::try_merge_completed_exec_at(chat, idx);
        }
    }

    if exit_code == 0 {
        chat.bottom_pane.update_status_text("command completed".to_string());
        // If this was a successful `git push`, start background GH Actions watch if enabled.
        crate::chatwidget::gh_actions::maybe_watch_after_push(
            chat.app_event_tx.clone(),
            chat.config.clone(),
            &command_for_watch,
        );
    } else {
        chat.bottom_pane.update_status_text(format!("command failed (exit {})", exit_code));
    }
    chat.maybe_hide_spinner();
}

// Stable ordering now inserts at the correct position; these helpers are removed.

// `handle_exec_approval_now` remains on ChatWidget in chatwidget.rs because
// it is referenced directly from interrupt handling and is trivial.
