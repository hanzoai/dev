use super::{ChatWidget, OrderKey};
use crate::history::state::HistoryId;
use crate::history_cell::HistoryCell;

pub(super) struct ToolCardSlot {
    pub order_key: OrderKey,
    pub history_id: Option<HistoryId>,
    pub cell_index: Option<usize>,
    pub cell_key: Option<String>,
    signature: Option<String>,
}

impl ToolCardSlot {
    pub fn new(order_key: OrderKey) -> Self {
        Self {
            order_key,
            history_id: None,
            cell_index: None,
            cell_key: None,
            signature: None,
        }
    }

    pub fn set_order_key(&mut self, order_key: OrderKey) {
        self.order_key = order_key;
    }

    pub fn set_key(&mut self, key: Option<String>) {
        self.cell_key = key;
    }

    pub fn key(&self) -> Option<&str> {
        self.cell_key.as_deref()
    }

    pub fn set_signature(&mut self, signature: Option<String>) {
        self.signature = signature;
    }

    pub fn signature(&self) -> Option<&str> {
        self.signature.as_deref()
    }
}

pub(crate) trait ToolCardCell: HistoryCell + Clone + 'static {
    fn tool_card_key(&self) -> Option<&str>;
    fn set_tool_card_key(&mut self, key: Option<String>);
    fn dedupe_signature(&self) -> Option<String> {
        None
    }
}

pub(super) fn assign_tool_card_key<C: ToolCardCell>(
    slot: &mut ToolCardSlot,
    cell: &mut C,
    key: Option<String>,
) {
    let signature = cell.dedupe_signature();
    slot.set_key(key.clone());
    slot.set_signature(signature);
    cell.set_tool_card_key(key);
}

pub(super) fn ensure_tool_card<C: ToolCardCell>(
    chat: &mut ChatWidget<'_>,
    slot: &mut ToolCardSlot,
    cell: &C,
) -> usize {
    let signature = cell.dedupe_signature();
    if slot.signature() != signature.as_deref() {
        slot.set_signature(signature.clone());
    }
    if let Some(id) = slot.history_id {
        if let Some(idx) = chat.cell_index_for_history_id(id) {
            if chat
                .history_cells
                .get(idx)
                .and_then(|cell| cell.as_any().downcast_ref::<C>())
                .is_some()
            {
                slot.cell_index = Some(idx);
                return idx;
            }
        }
    }

    if let Some(idx) = slot.cell_index {
        if idx < chat.history_cells.len()
            && chat
                .history_cells
                .get(idx)
                .and_then(|cell| cell.as_any().downcast_ref::<C>())
                .is_some()
        {
            return idx;
        }
    }

    if let Some(idx) = find_card_index::<C>(chat, slot.key(), signature.as_deref()) {
        slot.cell_index = Some(idx);
        slot.history_id = chat.history_cell_ids.get(idx).and_then(|slot| *slot);
        return idx;
    }

    let idx = chat.history_insert_with_key_global(Box::new(cell.clone()), slot.order_key);
    slot.cell_index = Some(idx);
    slot.history_id = chat.history_cell_ids.get(idx).and_then(|slot| *slot);
    idx
}

pub(super) fn replace_tool_card<C: ToolCardCell>(
    chat: &mut ChatWidget<'_>,
    slot: &mut ToolCardSlot,
    cell: &C,
) -> usize {
    let idx = ensure_tool_card(chat, slot, cell);
    chat.history_replace_at(idx, Box::new(cell.clone()));
    slot.cell_index = Some(idx);
    slot.history_id = chat.history_cell_ids.get(idx).and_then(|slot| *slot);
    let signature = slot.signature().map(|s| s.to_string());
    prune_tool_card_duplicates::<C>(chat, slot, idx, signature.as_deref());
    idx
}

fn prune_tool_card_duplicates<C: ToolCardCell>(
    chat: &mut ChatWidget<'_>,
    slot: &mut ToolCardSlot,
    keep_idx: usize,
    signature: Option<&str>,
) {
    let key = slot.key();
    let mut removals: Vec<usize> = Vec::new();
    for (idx, cell) in chat.history_cells.iter().enumerate() {
        if idx == keep_idx {
            continue;
        }
        let Some(typed) = cell.as_any().downcast_ref::<C>() else {
            continue;
        };

        let key_match = match (key, typed.tool_card_key()) {
            (Some(expected), Some(actual)) => actual == expected,
            (Some(_), None) => false,
            (None, None) => true,
            (None, Some(_)) => false,
        };

        let order_match = chat
            .cell_order_seq
            .get(idx)
            .copied()
            .map(|order| order == slot.order_key)
            .unwrap_or(false);

        let signature_match = match (signature, typed.dedupe_signature().as_deref()) {
            (Some(lhs), Some(rhs)) => lhs == rhs,
            _ => false,
        };

        if key_match || order_match || signature_match {
            removals.push(idx);
        }
    }

    if removals.is_empty() {
        return;
    }

    removals.sort_unstable();
    for idx in removals.into_iter().rev() {
        chat.history_remove_at(idx);
    }

    if let Some(new_idx) = find_card_index::<C>(chat, key, signature) {
        slot.cell_index = Some(new_idx);
        slot.history_id = chat.history_cell_ids.get(new_idx).and_then(|slot| *slot);
    } else {
        slot.cell_index = None;
        slot.history_id = None;
    }
}

fn find_card_index<C: ToolCardCell>(
    chat: &ChatWidget<'_>,
    key: Option<&str>,
    signature: Option<&str>,
) -> Option<usize> {
    chat.history_cells.iter().enumerate().find_map(|(idx, cell)| {
        let typed = cell.as_any().downcast_ref::<C>()?;
        if identity_matches(typed, key, signature) {
            Some(idx)
        } else {
            None
        }
    })
}

fn identity_matches<C: ToolCardCell>(typed: &C, key: Option<&str>, signature: Option<&str>) -> bool {
    let key_match = match (key, typed.tool_card_key()) {
       (Some(expected), Some(actual)) => actual == expected,
       (Some(_), None) => false,
       (None, _) => true,
    };
    let signature_match = match (signature, typed.dedupe_signature().as_deref()) {
        (Some(lhs), Some(rhs)) => lhs == rhs,
        _ => false,
    };
    key_match || signature_match
}
