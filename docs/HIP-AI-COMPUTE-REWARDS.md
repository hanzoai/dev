# HIP-XXX: AI Compute Contribution Rewards

**Status**: Draft
**Type**: Standards Track
**Category**: Economic Protocol
**Created**: 2026-01-24
**Authors**: Hanzo AI
**Requires**: HIP-004 (HMM), HIP-006 (AI Mining Protocol), ZIP-002 (PoAI)

---

## Abstract

This HIP specifies the economic design for AI coin rewards distributed to contributors of compute resources in the Hanzo decentralized compute marketplace. The system builds upon existing Hanzo infrastructure (HMM, PoAI, NVTrust attestation) to create a fair, sustainable, and Sybil-resistant reward mechanism that incentivizes high-quality compute contributions.

---

## Table of Contents

1. [Motivation](#motivation)
2. [Design Principles](#design-principles)
3. [Compute Contribution Tracking](#compute-contribution-tracking)
4. [Reward Calculation Algorithm](#reward-calculation-algorithm)
5. [Token Distribution Mechanism](#token-distribution-mechanism)
6. [Anti-Gaming Mechanisms](#anti-gaming-mechanisms)
7. [Integration with Existing Systems](#integration-with-existing-systems)
8. [Economic Analysis](#economic-analysis)
9. [Implementation Roadmap](#implementation-roadmap)
10. [Security Considerations](#security-considerations)

---

## 1. Motivation

The Hanzo compute marketplace (hanzo.network) requires a robust economic incentive system that:

1. **Attracts compute providers** by offering competitive rewards for GPU/CPU contributions
2. **Ensures quality** by tying rewards to verified, useful AI work
3. **Prevents gaming** through hardware attestation and reputation systems
4. **Scales sustainably** without hyperinflation or death spirals
5. **Integrates seamlessly** with existing Hanzo protocols (HMM, PoAI, Teleport)

### Current State

Hanzo already has foundational components:
- **HIP-004 (HMM)**: Hamiltonian Market Maker for compute pricing
- **HIP-006 (AI Mining)**: NVTrust-based work attestation and chain-binding
- **PAT-DEAI-004**: Compute reputation system design from Zoo Labs
- **Platform Schema**: Compute pools, offers, leases, and usage tracking

This HIP connects these pieces into a unified reward economy.

---

## 2. Design Principles

### 2.1 First Principles

1. **Useful Work**: Rewards must be tied to verifiable, useful AI compute (not busy work)
2. **Quality Over Quantity**: High-quality providers earn more than raw capacity
3. **Sustainable Emissions**: Reward rate must not exceed value creation
4. **Fair Distribution**: Proportional to actual contribution, not capital concentration
5. **Sybil Resistant**: Hardware attestation prevents fake capacity claims

### 2.2 Economic Invariants

```
Total Rewards <= Total Value Created
Quality Score >= Minimum Threshold for Rewards
Stake >= Minimum Bond (anti-Sybil)
```

---

## 3. Compute Contribution Tracking

### 3.1 Contribution Units

All compute is normalized to **Hanzo Compute Units (HCU)**:

```
HCU = weighted_sum(
    gpu_flops_normalized * w_gpu,
    cpu_cycles_normalized * w_cpu,
    memory_bandwidth_normalized * w_mem,
    storage_iops_normalized * w_storage,
    network_bandwidth_normalized * w_net
)
```

**Normalization Reference** (H100 = 1.0):

| Resource | Reference | HCU Weight |
|----------|-----------|------------|
| GPU FP16 | H100 (1.98 PFLOPS) | 0.50 |
| GPU VRAM | 80GB | 0.15 |
| CPU | 64 cores @ 3GHz | 0.10 |
| Memory | 512GB @ 400GB/s | 0.10 |
| Storage | 10TB NVMe @ 7GB/s | 0.10 |
| Network | 100Gbps | 0.05 |

### 3.2 Contribution Metrics

Each provider's contribution is tracked across multiple dimensions:

```rust
pub struct ContributionMetrics {
    // Capacity metrics (what you can provide)
    pub total_capacity_hcu: f64,
    pub available_capacity_hcu: f64,

    // Utilization metrics (what was actually used)
    pub utilized_hcu_hours: f64,
    pub jobs_completed: u64,
    pub jobs_failed: u64,

    // Quality metrics (how well you performed)
    pub average_latency_ms: f64,
    pub throughput_tokens_per_sec: f64,
    pub sla_compliance_rate: f64,  // 0.0 - 1.0

    // Uptime metrics (reliability)
    pub uptime_seconds: u64,
    pub total_registered_seconds: u64,
    pub consecutive_heartbeats: u64,

    // Attestation (trust)
    pub nvtrust_attested: bool,
    pub hardware_verified: bool,
    pub last_attestation_timestamp: u64,
}
```

### 3.3 Hardware Attestation (NVTrust Integration)

Building on HIP-006, all contributions require NVTrust attestation:

```rust
pub struct AttestationProof {
    // From HIP-006 SPDMEvidence
    pub spdm_evidence: SPDMEvidence,

    // Hardware identity
    pub device_id: [u8; 32],
    pub gpu_model: String,
    pub compute_capability: u32,
    pub vram_gb: u32,

    // Attested capacity
    pub attested_capacity_hcu: f64,

    // Timestamp and signature
    pub timestamp: u64,
    pub nvtrust_signature: Vec<u8>,
}
```

**Trust Scores by GPU Type** (from HIP-006):

| GPU | NVTrust Support | Trust Score | Reward Multiplier |
|-----|-----------------|-------------|-------------------|
| B200/GB200 | Full + TEE-I/O | 100 | 1.00x |
| H100/H200 | Full NVTrust | 95 | 0.95x |
| RTX PRO 6000 | NVTrust | 85 | 0.85x |
| RTX 5090/4090 | Software only | 60 | 0.60x |

### 3.4 On-Chain Contribution Records

```solidity
contract ContributionRegistry {
    struct ProviderContribution {
        address provider;
        bytes32 hardwareId;           // NVTrust device ID
        uint256 totalHcuHours;        // Lifetime HCU-hours
        uint256 currentEpochHcuHours; // This epoch's contribution
        uint256 qualityScore;         // 0-10000 (100.00%)
        uint256 trustScore;           // Hardware trust (0-100)
        uint256 uptimeRatio;          // 0-10000 (100.00%)
        uint256 lastUpdateBlock;
        bool nvtrustVerified;
    }

    mapping(address => ProviderContribution) public contributions;
    mapping(bytes32 => bool) public attestedHardware;

    // Submit attested contribution
    function recordContribution(
        bytes calldata attestationProof,
        uint256 hcuHours,
        uint256 jobsCompleted,
        uint256 jobsFailed
    ) external;

    // Update quality metrics (called by PoAI validators)
    function updateQualityScore(
        address provider,
        uint256 newScore,
        bytes calldata poaiProof
    ) external onlyValidator;
}
```

---

## 4. Reward Calculation Algorithm

### 4.1 Epoch-Based Distribution

Rewards are distributed in **epochs** (24 hours) to:
- Smooth out variance in job availability
- Allow time for quality verification
- Prevent gaming through rapid state changes

### 4.2 Reward Pool Composition

Each epoch's reward pool consists of:

```
R_epoch = R_base + R_fees + R_inflation

Where:
  R_base     = Fixed base emission (decreasing over time)
  R_fees     = 20% of HMM trading fees (recycled to providers)
  R_inflation = Controlled inflation (capped at 5% annually)
```

**Emission Schedule**:

| Year | Daily Base Emission | Annual Inflation Cap |
|------|---------------------|---------------------|
| 1 | 1,000,000 AI | 10% |
| 2 | 750,000 AI | 7.5% |
| 3 | 500,000 AI | 5% |
| 4+ | 250,000 AI | 3% |

### 4.3 Provider Reward Formula

Each provider's reward is calculated as:

```
R_provider = R_epoch * (W_provider / Sum(W_all_providers))

Where W_provider = weighted contribution score:

W_provider = HCU_utilized
           * Quality_multiplier
           * Trust_multiplier
           * Uptime_multiplier
           * Stake_multiplier
```

#### 4.3.1 Quality Multiplier (0.5x - 2.0x)

```
Quality_multiplier = 0.5 + 1.5 * (QualityScore / 10000)

QualityScore = weighted_average(
    SLA_compliance * 0.4,      // Meeting latency/throughput SLAs
    Job_success_rate * 0.3,    // completed / (completed + failed)
    PoAI_attestation_score * 0.2,  // Quality from ZIP-002
    Customer_feedback * 0.1    // Optional dispute resolution
)
```

#### 4.3.2 Trust Multiplier (0.6x - 1.0x)

```
Trust_multiplier = TrustScore / 100

Where TrustScore from hardware attestation (see table above)
```

#### 4.3.3 Uptime Multiplier (0.7x - 1.2x)

```
Uptime_multiplier = 0.7 + 0.5 * (UptimeRatio / 10000)

UptimeRatio = uptime_seconds / total_registered_seconds
```

**Bonus for consistent uptime**:
- 7+ consecutive days at 99%+ uptime: +5% bonus
- 30+ consecutive days at 99%+ uptime: +10% bonus

#### 4.3.4 Stake Multiplier (1.0x - 1.5x)

```
Stake_multiplier = 1.0 + 0.5 * min(1.0, log(Stake) / log(MAX_STAKE))

MAX_STAKE = 1,000,000 AI (diminishing returns after this)
```

### 4.4 Worked Example

**Provider Profile**:
- Contributed: 100 HCU-hours this epoch
- Quality Score: 8500 (85%)
- Trust Score: 95 (H100 with NVTrust)
- Uptime Ratio: 9900 (99%)
- Stake: 100,000 AI

**Calculation**:
```
Quality_multiplier = 0.5 + 1.5 * (8500/10000) = 1.775
Trust_multiplier = 95/100 = 0.95
Uptime_multiplier = 0.7 + 0.5 * (9900/10000) = 1.195
Stake_multiplier = 1.0 + 0.5 * min(1.0, log(100000)/log(1000000)) = 1.417

W_provider = 100 * 1.775 * 0.95 * 1.195 * 1.417 = 285.5

If total network weight is 10,000 and R_epoch = 1,000,000 AI:
R_provider = 1,000,000 * (285.5 / 10,000) = 28,550 AI
```

### 4.5 Minimum Thresholds

To receive rewards, providers must meet:

| Metric | Minimum Threshold |
|--------|-------------------|
| Quality Score | 5000 (50%) |
| Uptime Ratio | 9000 (90%) |
| Stake | 1000 AI |
| HCU-hours/epoch | 1.0 |
| Hardware Attestation | Required |

---

## 5. Token Distribution Mechanism

### 5.1 Distribution Flow

```
                                  ┌─────────────────┐
                                  │   Epoch Timer   │
                                  │   (24 hours)    │
                                  └────────┬────────┘
                                           │
                                           ▼
┌─────────────────┐              ┌─────────────────┐
│  Contribution   │──────────────│   Reward Pool   │
│    Registry     │              │    Calculator   │
└─────────────────┘              └────────┬────────┘
                                           │
         ┌─────────────────────────────────┼─────────────────────────────────┐
         │                                 │                                 │
         ▼                                 ▼                                 ▼
┌─────────────────┐              ┌─────────────────┐              ┌─────────────────┐
│    Provider     │              │    Treasury     │              │      Burn       │
│    Rewards      │              │     (20%)       │              │     (10%)       │
│     (70%)       │              │                 │              │                 │
└────────┬────────┘              └─────────────────┘              └─────────────────┘
         │
         ▼
┌─────────────────────────────────────────────────────────────────────────────────┐
│                              Teleport Bridge                                     │
│                                                                                  │
│   ┌─────────────┐        ┌─────────────┐        ┌─────────────┐                │
│   │ Hanzo EVM   │        │  Zoo EVM    │        │ Lux C-Chain │                │
│   │   36963     │        │   200200    │        │    96369    │                │
│   └─────────────┘        └─────────────┘        └─────────────┘                │
└─────────────────────────────────────────────────────────────────────────────────┘
```

### 5.2 Reward Allocation

| Allocation | Percentage | Purpose |
|------------|------------|---------|
| Provider Rewards | 70% | Direct rewards to compute providers |
| Treasury | 20% | Protocol development, grants, insurance |
| Burn | 10% | Deflationary pressure |

### 5.3 Vesting and Claiming

**Vesting Schedule**:
- 50% available immediately
- 25% vests over 7 days
- 25% vests over 30 days

**Rationale**: Prevents immediate dumping and encourages long-term alignment.

```solidity
contract RewardVesting {
    struct VestingSchedule {
        uint256 totalAmount;
        uint256 immediateAmount;    // 50%
        uint256 weeklyAmount;       // 25%
        uint256 monthlyAmount;      // 25%
        uint256 epochTimestamp;
        uint256 claimedAmount;
    }

    mapping(address => VestingSchedule[]) public vestingSchedules;

    function claimAvailable() external returns (uint256 claimed);
    function getClaimableAmount(address provider) external view returns (uint256);
}
```

### 5.4 Cross-Chain Distribution

Providers can choose their reward destination:

```solidity
enum RewardDestination {
    HanzoEVM,      // Chain 36963 (default)
    ZooEVM,        // Chain 200200
    LuxCChain,     // Chain 96369
    Staking        // Auto-compound to stake
}

function setRewardDestination(RewardDestination dest) external;
```

---

## 6. Anti-Gaming Mechanisms

### 6.1 Sybil Resistance

**Problem**: Creating multiple identities to claim more rewards.

**Solutions**:

1. **Hardware Attestation (Primary)**
   - Each GPU has unique NVTrust device ID
   - Same device cannot attest to multiple providers
   - Hardware IDs are chain-bound (HIP-006)

2. **Stake Requirements**
   - Minimum 1000 AI stake to participate
   - Stake increases linearly with claimed capacity
   - Economic cost to create fake identities

3. **Geographic Distribution**
   - IP analysis for suspicious clustering
   - Latency verification from multiple vantage points

### 6.2 Quality Gaming Prevention

**Problem**: Providers gaming quality metrics.

**Solutions**:

1. **PoAI Verification (ZIP-002)**
   - Independent validators verify AI work
   - Redundant execution for high-value jobs
   - Statistical anomaly detection

2. **Randomized Verification**
   - 10% of jobs receive additional verification
   - Providers don't know which jobs are audited

3. **Customer Feedback Loop**
   - Disputes affect quality score
   - Arbitration for contested results

### 6.3 Capacity Inflation Prevention

**Problem**: Claiming more capacity than available.

**Solutions**:

1. **NVTrust Attestation**
   - Hardware capacity cryptographically attested
   - Cannot claim more than physical limits

2. **Utilization Verification**
   - Actual utilization tracked and verified
   - Rewards based on utilized, not claimed capacity

3. **Periodic Re-attestation**
   - Hardware must re-attest every 24 hours
   - Prevents "ghost" capacity claims

### 6.4 Collusion Prevention

**Problem**: Providers creating fake jobs for each other.

**Solutions**:

1. **Job Value Weighting**
   - Rewards weighted by actual job payment
   - Self-dealing is economically irrational

2. **Network-Wide Analysis**
   - Graph analysis for suspicious job patterns
   - Statistical detection of circular flows

3. **Slashing**
   - Provable collusion = stake slashing
   - 50% of slashed stake to reporter

---

## 7. Integration with Existing Systems

### 7.1 HMM Integration (HIP-004)

Compute rewards integrate with Hamiltonian Market Maker:

```
Reward Flow:
  Job Payment (via HMM) → 80% to Provider (immediate)
                        → 20% to Reward Pool (epoch distribution)

HMM provides:
  - Real-time compute pricing (psi/theta dynamics)
  - SLA-aware routing (quality-weighted allocation)
  - Fee collection for reward pool
```

### 7.2 PoAI Integration (ZIP-002)

Quality verification from Zoo's Proof of AI:

```
PoAI provides:
  - TEE attestations for compute correctness
  - Quality scoring (Delta-I, Delta-U metrics)
  - Slashing for fraudulent attestations

PoAI Bonus:
  R_poai = rho * V_job    where rho <= 0.1 (10% bonus cap)
```

### 7.3 Platform Integration (Compute Pools)

Integration with existing platform schema:

```typescript
// From COMPUTE_SCHEMA_DESIGN.md
interface RewardIntegration {
  // Link to compute_usage table
  usageId: string;

  // Reward calculation inputs
  hcuHours: number;
  qualityScore: number;

  // Reward outputs
  baseReward: bigint;
  qualityBonus: bigint;
  poaiBonus: bigint;
  totalReward: bigint;

  // Distribution
  destination: ChainId;
  vestingScheduleId: string;
}
```

### 7.4 Teleport Integration (HIP-006)

Rewards distributed via Teleport bridge:

```rust
// From HIP-006 MiningBridge
impl RewardDistributor {
    pub async fn distribute_epoch_rewards(
        &self,
        epoch: u64,
        rewards: Vec<ProviderReward>,
    ) -> Result<Vec<TeleportReceipt>, DistributionError> {
        for reward in rewards {
            match reward.destination {
                ChainId::HanzoEVM => self.teleport.to_hanzo(reward).await?,
                ChainId::ZooEVM => self.teleport.to_zoo(reward).await?,
                ChainId::LuxCChain => self.teleport.to_lux(reward).await?,
                ChainId::Staking => self.stake_and_compound(reward).await?,
            }
        }
    }
}
```

---

## 8. Economic Analysis

### 8.1 Supply Dynamics

**Year 1 Projection** (assuming 100 providers):

| Metric | Value |
|--------|-------|
| Daily Emission | 1,000,000 AI |
| Daily Burn (10%) | 100,000 AI |
| Net Daily Inflation | 900,000 AI |
| Annual Net Inflation | 328,500,000 AI |
| Effective Inflation Rate | ~10% |

**Year 5 Projection** (assuming 1000 providers):

| Metric | Value |
|--------|-------|
| Daily Emission | 250,000 AI |
| Daily Burn (10%) | 25,000 AI |
| Fee Recycling (estimated) | 50,000 AI |
| Net Daily Inflation | 275,000 AI |
| Effective Inflation Rate | ~3% |

### 8.2 Provider Economics

**Break-Even Analysis** for H100 provider:

| Cost Item | Monthly |
|-----------|---------|
| Hardware Depreciation | $2,000 |
| Electricity (0.7kW avg) | $400 |
| Bandwidth (1TB) | $100 |
| Maintenance | $200 |
| **Total Cost** | **$2,700** |

**Revenue at Various Utilization Rates**:

| Utilization | HCU-hours/month | Est. Reward (AI) | USD Value (@$0.10/AI) |
|-------------|-----------------|------------------|----------------------|
| 25% | 180 | 50,000 | $5,000 |
| 50% | 360 | 100,000 | $10,000 |
| 75% | 540 | 150,000 | $15,000 |
| 100% | 720 | 200,000 | $20,000 |

**Conclusion**: Profitable at >15% utilization at current prices.

### 8.3 Game Theory Analysis

**Dominant Strategy**: Provide high-quality compute honestly.

**Rationale**:
1. Gaming quality reduces Quality_multiplier (0.5x vs 2.0x)
2. Low trust hardware reduces Trust_multiplier (0.6x vs 1.0x)
3. Downtime reduces Uptime_multiplier (0.7x vs 1.2x)
4. Sybil attacks require stake (economic cost)
5. Getting caught = stake slashing (50% loss)

**Nash Equilibrium**: All providers maximize honest contribution.

### 8.4 Sustainability Model

**Long-term Equilibrium**:

```
Reward Rate = f(Total Value Created)

Where Value Created = Sum(Job Payments) + Network Utility

Sustainable when:
  Reward Value <= Value Created + Token Price Appreciation
```

**Self-Correcting Mechanisms**:
1. If rewards too high → new providers join → dilution → equilibrium
2. If rewards too low → providers leave → scarcity → price increase → equilibrium
3. Burn rate creates deflationary pressure balancing inflation

---

## 9. Implementation Roadmap

### Phase 1: Foundation (Q1 2026)

**Deliverables**:
- [ ] ContributionRegistry smart contract
- [ ] Basic HCU calculation service
- [ ] Integration with existing compute_usage table
- [ ] Manual epoch distribution (admin-triggered)

**Success Criteria**:
- 10 providers onboarded
- 100,000 AI distributed in first epoch
- No critical bugs in 30-day audit

### Phase 2: Automation (Q2 2026)

**Deliverables**:
- [ ] Automated epoch distribution
- [ ] Quality score integration (PoAI)
- [ ] NVTrust attestation verification
- [ ] Vesting contract deployment

**Success Criteria**:
- 50 providers onboarded
- 99.9% uptime for distribution
- <1% reward calculation variance

### Phase 3: Scaling (Q3 2026)

**Deliverables**:
- [ ] Cross-chain Teleport distribution
- [ ] Auto-compound staking option
- [ ] Advanced anti-gaming ML models
- [ ] Public dashboard and analytics

**Success Criteria**:
- 200 providers onboarded
- $10M+ monthly compute volume
- <0.1% fraudulent claims

### Phase 4: Decentralization (Q4 2026)

**Deliverables**:
- [ ] DAO governance for parameters
- [ ] Decentralized epoch triggers
- [ ] Community-run validators
- [ ] Open-source all components

**Success Criteria**:
- 500+ providers
- 0 admin key dependencies
- Community-approved parameter changes

---

## 10. Security Considerations

### 10.1 Smart Contract Risks

| Risk | Mitigation |
|------|------------|
| Reentrancy | Checks-effects-interactions, ReentrancyGuard |
| Integer Overflow | SafeMath, Solidity 0.8+ |
| Access Control | OpenZeppelin AccessControl |
| Upgrade Risks | Transparent proxy with timelock |

### 10.2 Economic Attacks

| Attack | Mitigation |
|--------|------------|
| Flash Loan Stake | Minimum stake duration (7 days) |
| Reward Manipulation | Epoch-based distribution (24h delay) |
| Front-running | Commit-reveal for claims |
| MEV Extraction | Batch distribution transactions |

### 10.3 Operational Risks

| Risk | Mitigation |
|------|------------|
| Oracle Failure | On-chain contribution records |
| Key Compromise | Multi-sig treasury, timelock |
| Network Congestion | L2 distribution, batching |
| Bug Discovery | Bug bounty program, insurance fund |

### 10.4 Privacy Considerations

- Provider identities can be pseudonymous (wallet addresses)
- Hardware IDs are hashed before storage
- Job details are not stored on-chain (only hashes)
- GDPR compliance for EU providers

---

## References

### Internal HIPs and ZIPs

1. **HIP-004**: Hamiltonian Market Maker - Compute pricing mechanism
2. **HIP-006**: AI Mining Protocol - NVTrust attestation and chain-binding
3. **ZIP-002**: Proof of AI (PoAI) - Quality verification from Zoo Labs
4. **PAT-DEAI-004**: Compute Provider Reputation System - Zoo Labs patent

### External References

5. NVIDIA NVTrust Documentation
6. FIPS 204 ML-DSA Specification
7. Lux Consensus Specification

### Platform Documentation

8. `COMPUTE_SCHEMA_DESIGN.md` - Database schema for compute marketplace
9. `PLATFORM_PHASE1_POOLS_DESIGN.md` - UI and API design for pools

---

## Appendix A: Parameter Reference

| Parameter | Default | Range | Governance |
|-----------|---------|-------|------------|
| EPOCH_DURATION | 86400s (24h) | 3600-604800 | DAO |
| MIN_STAKE | 1000 AI | 100-10000 | DAO |
| PROVIDER_SHARE | 70% | 50-80% | DAO |
| TREASURY_SHARE | 20% | 10-30% | DAO |
| BURN_SHARE | 10% | 5-20% | DAO |
| MIN_QUALITY_SCORE | 5000 | 3000-7000 | DAO |
| MIN_UPTIME_RATIO | 9000 | 8000-9500 | DAO |
| BASE_EMISSION_YEAR1 | 1M AI/day | Fixed | N/A |
| MAX_STAKE_EFFECT | 1M AI | 100K-10M | DAO |

---

## Appendix B: Solidity Interface

```solidity
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

interface IComputeRewards {
    // Events
    event ContributionRecorded(address indexed provider, uint256 hcuHours, uint256 epoch);
    event QualityScoreUpdated(address indexed provider, uint256 newScore);
    event EpochDistributed(uint256 indexed epoch, uint256 totalRewards, uint256 providerCount);
    event RewardClaimed(address indexed provider, uint256 amount, uint256 destination);
    event ProviderSlashed(address indexed provider, uint256 amount, string reason);

    // Views
    function getContribution(address provider) external view returns (ContributionMetrics memory);
    function getEpochReward(address provider, uint256 epoch) external view returns (uint256);
    function getClaimableAmount(address provider) external view returns (uint256);
    function getCurrentEpoch() external view returns (uint256);
    function getEpochEndTime() external view returns (uint256);

    // Actions
    function recordContribution(
        bytes calldata attestationProof,
        uint256 hcuHours,
        uint256 jobsCompleted,
        uint256 jobsFailed
    ) external;

    function updateQualityScore(
        address provider,
        uint256 newScore,
        bytes calldata poaiProof
    ) external;

    function triggerEpochDistribution() external;

    function claimRewards(uint256 destination) external returns (uint256);

    function setRewardDestination(uint256 destination) external;

    // Admin
    function slash(address provider, uint256 amount, string calldata reason) external;
    function setParameter(bytes32 key, uint256 value) external;
}

struct ContributionMetrics {
    uint256 totalCapacityHcu;
    uint256 utilizedHcuHours;
    uint256 qualityScore;
    uint256 trustScore;
    uint256 uptimeRatio;
    uint256 stake;
    uint256 lastUpdateBlock;
    bool nvtrustVerified;
}
```

---

## Copyright

Copyright 2026 Hanzo AI Inc. Released under MIT License.

---

*HIP-XXX Created: January 24, 2026*
*Status: Draft*
*Contact: research@hanzo.ai*
