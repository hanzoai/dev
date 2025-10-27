use std::collections::VecDeque;

use code_core::protocol::TokenUsage;

const DEFAULT_PROMPT_ESTIMATE: u64 = 4_000;

#[derive(Debug, Clone)]
pub struct SessionMetrics {
    running_total: TokenUsage,
    last_turn: TokenUsage,
    turn_count: u32,
    recent_prompt_tokens: VecDeque<u64>,
    window: usize,
}

impl Default for SessionMetrics {
    fn default() -> Self {
        Self::new(3)
    }
}

impl SessionMetrics {
    pub fn new(window: usize) -> Self {
        Self {
            running_total: TokenUsage::default(),
            last_turn: TokenUsage::default(),
            turn_count: 0,
            recent_prompt_tokens: VecDeque::with_capacity(window),
            window: window.max(1),
        }
    }

    pub fn record_turn(&mut self, usage: &TokenUsage) {
        self.running_total.add_assign(usage);
        self.last_turn = usage.clone();
        self.turn_count = self.turn_count.saturating_add(1);
        self.push_prompt_observation(usage.non_cached_input());
    }

    pub fn sync_absolute(&mut self, total: TokenUsage, last: TokenUsage, turn_count: u32) {
        self.running_total = total;
        self.last_turn = last.clone();
        self.turn_count = turn_count;
        self.recent_prompt_tokens.clear();
        self.push_prompt_observation(last.non_cached_input());
    }

    pub fn running_total(&self) -> &TokenUsage {
        &self.running_total
    }

    pub fn last_turn(&self) -> &TokenUsage {
        &self.last_turn
    }

    pub fn turn_count(&self) -> u32 {
        self.turn_count
    }

    pub fn blended_total(&self) -> u64 {
        self.running_total.blended_total()
    }

    pub fn estimated_next_prompt_tokens(&self) -> u64 {
        if !self.recent_prompt_tokens.is_empty() {
            let sum: u64 = self.recent_prompt_tokens.iter().copied().sum();
            return sum / self.recent_prompt_tokens.len() as u64;
        }
        let fallback = self.last_turn.non_cached_input();
        if fallback > 0 {
            fallback
        } else {
            DEFAULT_PROMPT_ESTIMATE
        }
    }

    pub fn reset(&mut self) {
        *self = Self::new(self.window);
    }

    fn push_prompt_observation(&mut self, tokens: u64) {
        if tokens == 0 {
            return;
        }
        if self.recent_prompt_tokens.len() == self.window {
            self.recent_prompt_tokens.pop_front();
        }
        self.recent_prompt_tokens.push_back(tokens);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn usage(input: u64, output: u64) -> TokenUsage {
        TokenUsage {
            input_tokens: input,
            cached_input_tokens: 0,
            output_tokens: output,
            reasoning_output_tokens: 0,
            total_tokens: input + output,
        }
    }

    #[test]
    fn record_turn_tracks_totals_and_estimate() {
        let mut metrics = SessionMetrics::default();
        metrics.record_turn(&usage(1_000, 500));
        metrics.record_turn(&usage(4_000, 2_000));

        assert_eq!(metrics.turn_count(), 2);
        assert_eq!(metrics.running_total().input_tokens, 5_000);
        assert_eq!(metrics.running_total().output_tokens, 2_500);

        // Average of observed prompt tokens (non-cached input)
        assert_eq!(metrics.estimated_next_prompt_tokens(), 2_500);
    }

    #[test]
    fn sync_absolute_resets_window() {
        let mut metrics = SessionMetrics::default();
        metrics.record_turn(&usage(1_000, 500));
        metrics.sync_absolute(usage(10_000, 4_000), usage(3_000, 1_000), 3);

        assert_eq!(metrics.turn_count(), 3);
        assert_eq!(metrics.running_total().input_tokens, 10_000);
        assert_eq!(metrics.last_turn().input_tokens, 3_000);
        assert_eq!(metrics.estimated_next_prompt_tokens(), 3_000);
    }
}
