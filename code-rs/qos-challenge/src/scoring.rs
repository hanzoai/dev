//! QoS scoring system.
//!
//! This module provides the scoring engine that calculates composite QoS scores
//! from challenge results and tracks provider performance over time.

use std::collections::{HashMap, VecDeque};

use crate::types::{
    ChallengeResult, ChallengeType, PeerId, ProviderState, QoSComponents,
    QoSScore, ScoreWeights,
};

/// Configuration for the scoring engine.
#[derive(Debug, Clone)]
pub struct ScoringConfig {
    /// Maximum results to keep in history per provider
    pub max_history_size: usize,
    /// Minimum results needed for confident scoring
    pub min_results_for_confidence: usize,
    /// Weight decay factor for older results (0.0-1.0)
    pub time_decay_factor: f64,
    /// Score weights
    pub weights: ScoreWeights,
}

impl Default for ScoringConfig {
    fn default() -> Self {
        Self {
            max_history_size: 100,
            min_results_for_confidence: 10,
            time_decay_factor: 0.95,
            weights: ScoreWeights::default(),
        }
    }
}

/// History entry for a challenge result.
#[derive(Debug, Clone)]
struct ResultHistory {
    result: ChallengeResult,
    challenge_type: ChallengeType,
    #[allow(dead_code)]
    timestamp: u64,
}

/// Scoring engine for QoS evaluation.
pub struct ScoringEngine {
    /// Configuration
    config: ScoringConfig,
    /// Per-provider result history
    history: HashMap<PeerId, VecDeque<ResultHistory>>,
    /// Cached scores
    scores: HashMap<PeerId, QoSScore>,
}

impl ScoringEngine {
    /// Create a new scoring engine.
    pub fn new() -> Self {
        Self {
            config: ScoringConfig::default(),
            history: HashMap::new(),
            scores: HashMap::new(),
        }
    }

    /// Create with custom configuration.
    pub fn with_config(config: ScoringConfig) -> Self {
        Self {
            config,
            history: HashMap::new(),
            scores: HashMap::new(),
        }
    }

    /// Record a challenge result and update scores.
    pub fn record_result(
        &mut self,
        result: ChallengeResult,
        challenge_type: ChallengeType,
    ) -> QoSScore {
        let provider_id = result.provider_id.clone();

        // Add to history
        let entry = ResultHistory {
            result: result.clone(),
            challenge_type,
            timestamp: current_timestamp_ms(),
        };

        let history = self.history.entry(provider_id.clone()).or_default();
        history.push_back(entry);

        // Trim history if too large
        while history.len() > self.config.max_history_size {
            history.pop_front();
        }

        // Recalculate score
        let score = self.calculate_score(&provider_id);
        self.scores.insert(provider_id, score.clone());

        score
    }

    /// Calculate QoS score for a provider from their history.
    pub fn calculate_score(&self, provider_id: &PeerId) -> QoSScore {
        let history = match self.history.get(provider_id) {
            Some(h) => h,
            None => return QoSScore::default(),
        };

        if history.is_empty() {
            return QoSScore::default();
        }

        // Group results by challenge type
        let mut compute_scores = Vec::new();
        let mut latency_scores = Vec::new();
        let mut bandwidth_scores = Vec::new();
        let mut availability_scores = Vec::new();

        for entry in history.iter() {
            let score = if entry.result.passed {
                (entry.result.performance_score * 1000.0) as u32
            } else {
                0
            };

            match &entry.challenge_type {
                ChallengeType::Compute(_) => compute_scores.push(score),
                ChallengeType::Latency => latency_scores.push(score),
                ChallengeType::Bandwidth(_) => bandwidth_scores.push(score),
                ChallengeType::Availability => availability_scores.push(score),
                ChallengeType::Model(_) => compute_scores.push(score), // Count model with compute
            }
        }

        // Calculate component scores with time decay
        let components = QoSComponents {
            compute_score: self.weighted_average(&compute_scores).unwrap_or(500),
            latency_score: self.weighted_average(&latency_scores).unwrap_or(500),
            bandwidth_score: self.weighted_average(&bandwidth_scores).unwrap_or(500),
            availability_score: self.weighted_average(&availability_scores).unwrap_or(500),
            consistency_score: self.calculate_consistency(history),
        };

        // Calculate composite score
        let mut score = QoSScore::calculate(&components, &self.config.weights);

        // Set confidence based on sample size
        let total_samples = history.len();
        score.confidence = (total_samples as f64 / self.config.min_results_for_confidence as f64)
            .min(1.0);

        // Calculate trend from recent vs older results
        score.trend = self.calculate_trend(history);

        score
    }

    /// Calculate weighted average with time decay.
    fn weighted_average(&self, scores: &[u32]) -> Option<u32> {
        if scores.is_empty() {
            return None;
        }

        let mut weighted_sum = 0.0;
        let mut weight_sum = 0.0;

        for (i, score) in scores.iter().enumerate() {
            // More recent results have higher weight
            let weight = self.config.time_decay_factor.powi((scores.len() - 1 - i) as i32);
            weighted_sum += *score as f64 * weight;
            weight_sum += weight;
        }

        if weight_sum > 0.0 {
            Some((weighted_sum / weight_sum) as u32)
        } else {
            None
        }
    }

    /// Calculate consistency score from result variance.
    fn calculate_consistency(&self, history: &VecDeque<ResultHistory>) -> u32 {
        if history.len() < 5 {
            return 500; // Neutral score for insufficient data
        }

        // Group by challenge type and calculate coefficient of variation
        let mut type_cvs = Vec::new();

        let by_type = self.group_by_type(history);

        for (_, results) in by_type {
            if results.len() < 3 {
                continue;
            }

            let scores: Vec<f64> = results
                .iter()
                .filter(|r| r.passed)
                .map(|r| r.performance_score)
                .collect();

            if scores.len() < 3 {
                continue;
            }

            // Calculate coefficient of variation
            let mean: f64 = scores.iter().sum::<f64>() / scores.len() as f64;
            if mean > 0.0 {
                let variance: f64 = scores.iter().map(|s| (s - mean).powi(2)).sum::<f64>()
                    / scores.len() as f64;
                let std_dev = variance.sqrt();
                let cv = std_dev / mean;
                type_cvs.push(cv);
            }
        }

        if type_cvs.is_empty() {
            return 500;
        }

        // Average CV across types
        let avg_cv: f64 = type_cvs.iter().sum::<f64>() / type_cvs.len() as f64;

        // Lower CV = higher consistency
        // CV of 0.05 = perfect (1000), CV of 0.5 = poor (0)
        let consistency = ((1.0 - (avg_cv / 0.5).min(1.0)) * 1000.0) as u32;

        consistency
    }

    /// Calculate trend from recent vs older results.
    fn calculate_trend(&self, history: &VecDeque<ResultHistory>) -> f64 {
        if history.len() < 10 {
            return 0.0;
        }

        let mid = history.len() / 2;

        let older: Vec<f64> = history
            .iter()
            .take(mid)
            .filter(|e| e.result.passed)
            .map(|e| e.result.performance_score)
            .collect();

        let newer: Vec<f64> = history
            .iter()
            .skip(mid)
            .filter(|e| e.result.passed)
            .map(|e| e.result.performance_score)
            .collect();

        if older.is_empty() || newer.is_empty() {
            return 0.0;
        }

        let older_avg: f64 = older.iter().sum::<f64>() / older.len() as f64;
        let newer_avg: f64 = newer.iter().sum::<f64>() / newer.len() as f64;

        // Trend is percentage change
        if older_avg > 0.0 {
            (newer_avg - older_avg) / older_avg
        } else {
            0.0
        }
    }

    /// Group results by challenge type.
    fn group_by_type<'a>(
        &self,
        history: &'a VecDeque<ResultHistory>,
    ) -> HashMap<String, Vec<&'a ChallengeResult>> {
        let mut grouped: HashMap<String, Vec<&ChallengeResult>> = HashMap::new();

        for entry in history.iter() {
            let key = match &entry.challenge_type {
                ChallengeType::Compute(_) => "compute",
                ChallengeType::Latency => "latency",
                ChallengeType::Bandwidth(_) => "bandwidth",
                ChallengeType::Availability => "availability",
                ChallengeType::Model(_) => "model",
            };
            grouped.entry(key.to_string()).or_default().push(&entry.result);
        }

        grouped
    }

    /// Get cached score for a provider.
    pub fn get_score(&self, provider_id: &PeerId) -> Option<&QoSScore> {
        self.scores.get(provider_id)
    }

    /// Get provider's result history.
    pub fn get_history(&self, provider_id: &PeerId) -> Option<Vec<ChallengeResult>> {
        self.history.get(provider_id).map(|h| {
            h.iter().map(|e| e.result.clone()).collect()
        })
    }

    /// Get number of tracked providers.
    pub fn provider_count(&self) -> usize {
        self.history.len()
    }

    /// Clear history for a provider.
    pub fn clear_provider(&mut self, provider_id: &PeerId) {
        self.history.remove(provider_id);
        self.scores.remove(provider_id);
    }

    /// Update provider state with new score.
    pub fn update_provider_state(&self, state: &mut ProviderState) {
        if let Some(score) = self.scores.get(&state.id) {
            state.qos_score = score.clone();
        }
    }
}

impl Default for ScoringEngine {
    fn default() -> Self {
        Self::new()
    }
}

/// Calculator for consistency metrics.
pub struct ConsistencyCalculator {
    /// Minimum samples needed
    min_samples: usize,
}

impl ConsistencyCalculator {
    /// Create a new consistency calculator.
    pub fn new(min_samples: usize) -> Self {
        Self { min_samples }
    }

    /// Calculate consistency score from challenge results.
    pub fn calculate(&self, results: &[ChallengeResult]) -> u32 {
        if results.len() < self.min_samples {
            return 500; // Neutral for insufficient data
        }

        let scores: Vec<f64> = results
            .iter()
            .filter(|r| r.passed)
            .map(|r| r.performance_score)
            .collect();

        if scores.len() < self.min_samples {
            return 500;
        }

        let mean: f64 = scores.iter().sum::<f64>() / scores.len() as f64;
        if mean <= 0.0 {
            return 0;
        }

        let variance: f64 = scores.iter().map(|s| (s - mean).powi(2)).sum::<f64>()
            / scores.len() as f64;
        let std_dev = variance.sqrt();
        let cv = std_dev / mean;

        // Lower CV = higher consistency
        ((1.0 - (cv / 0.5).min(1.0)) * 1000.0) as u32
    }
}

impl Default for ConsistencyCalculator {
    fn default() -> Self {
        Self::new(5)
    }
}

/// Get current timestamp in milliseconds.
fn current_timestamp_ms() -> u64 {
    use std::time::{SystemTime, UNIX_EPOCH};
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_millis() as u64)
        .unwrap_or(0)
}

#[cfg(test)]
mod tests {
    use super::*;
    use uuid::Uuid;

    fn make_result(provider: &str, passed: bool, score: f64) -> ChallengeResult {
        ChallengeResult {
            challenge_id: Uuid::new_v4(),
            provider_id: provider.to_string(),
            passed,
            performance_score: score,
            tee_verified: false,
            verified_at: current_timestamp_ms(),
            failure_reason: if passed { None } else { Some("failed".to_string()) },
        }
    }

    #[test]
    fn test_scoring_engine_creation() {
        let engine = ScoringEngine::new();
        assert_eq!(engine.provider_count(), 0);
    }

    #[test]
    fn test_record_result() {
        let mut engine = ScoringEngine::new();

        let result = make_result("provider-1", true, 1.0);
        let score = engine.record_result(result, ChallengeType::Availability);

        assert!(score.composite > 0);
        assert_eq!(engine.provider_count(), 1);
    }

    #[test]
    fn test_multiple_results() {
        let mut engine = ScoringEngine::new();

        // Add multiple results
        for i in 0..20 {
            let result = make_result("provider-1", true, 1.0 + (i as f64 * 0.01));
            engine.record_result(result, ChallengeType::Compute(crate::types::ComputeMetric::GpuFp32));
        }

        let score = engine.get_score(&"provider-1".to_string());
        assert!(score.is_some());
        assert!(score.as_ref().map(|s| s.confidence > 0.5).unwrap_or(false));
    }

    #[test]
    fn test_consistency_calculator() {
        let calc = ConsistencyCalculator::new(5);

        // Consistent results
        let consistent: Vec<ChallengeResult> = (0..10)
            .map(|_| make_result("p1", true, 1.0))
            .collect();
        let consistent_score = calc.calculate(&consistent);

        // Inconsistent results
        let inconsistent: Vec<ChallengeResult> = (0..10)
            .map(|i| make_result("p1", true, if i % 2 == 0 { 0.5 } else { 1.5 }))
            .collect();
        let inconsistent_score = calc.calculate(&inconsistent);

        // Consistent should score higher
        assert!(consistent_score > inconsistent_score);
    }

    #[test]
    fn test_score_calculation() {
        let components = QoSComponents {
            compute_score: 800,
            latency_score: 900,
            bandwidth_score: 700,
            availability_score: 950,
            consistency_score: 850,
        };

        let weights = ScoreWeights::default();
        let score = QoSScore::calculate(&components, &weights);

        // Composite should be within component range
        assert!(score.composite >= 700);
        assert!(score.composite <= 950);
    }

    #[test]
    fn test_get_history() {
        let mut engine = ScoringEngine::new();

        let result = make_result("provider-1", true, 1.0);
        engine.record_result(result, ChallengeType::Latency);

        let history = engine.get_history(&"provider-1".to_string());
        assert!(history.is_some());
        assert_eq!(history.as_ref().map(|h| h.len()), Some(1));
    }

    #[test]
    fn test_clear_provider() {
        let mut engine = ScoringEngine::new();

        let result = make_result("provider-1", true, 1.0);
        engine.record_result(result, ChallengeType::Availability);

        assert_eq!(engine.provider_count(), 1);

        engine.clear_provider(&"provider-1".to_string());

        assert_eq!(engine.provider_count(), 0);
    }
}
