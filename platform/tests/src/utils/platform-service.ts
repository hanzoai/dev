/**
 * Mock Platform Service for Integration Tests
 *
 * This service simulates the platform backend for testing purposes.
 * In production, this would be replaced with actual API calls.
 */

import type {
  ComputeNode,
  ComputePool,
  ComputeOffer,
  PoolMembership,
  Challenge,
  Reward,
  NodeRegistrationRequest,
  PoolCreationRequest,
  ChallengeData,
  ChallengeResponse,
  ChallengeStatus,
  NodeStatus,
  VerificationResult,
  RewardDistributionRequest,
  QoSScore,
} from '../types';

import {
  generateComputeNode,
  generateComputePool,
  generateChallenge,
  generateReward,
} from '../mocks';

// Generate UUID
function generateId(): string {
  return crypto.randomUUID();
}

// Generate random hex
function generateHex(length: number): string {
  const chars = '0123456789abcdef';
  let result = '';
  for (let i = 0; i < length * 2; i++) {
    result += chars[Math.floor(Math.random() * chars.length)];
  }
  return result;
}

/**
 * In-memory storage for mock platform data
 */
interface PlatformState {
  nodes: Map<string, ComputeNode>;
  pools: Map<string, ComputePool>;
  offers: Map<string, ComputeOffer>;
  memberships: Map<string, PoolMembership>;
  challenges: Map<string, Challenge>;
  rewards: Map<string, Reward>;
  poolSlugs: Map<string, string>; // slug -> pool_id
}

/**
 * Mock Platform Service
 */
export class MockPlatformService {
  private state: PlatformState;

  constructor() {
    this.state = {
      nodes: new Map(),
      pools: new Map(),
      offers: new Map(),
      memberships: new Map(),
      challenges: new Map(),
      rewards: new Map(),
      poolSlugs: new Map(),
    };
  }

  /**
   * Reset all state
   */
  reset(): void {
    this.state = {
      nodes: new Map(),
      pools: new Map(),
      offers: new Map(),
      memberships: new Map(),
      challenges: new Map(),
      rewards: new Map(),
      poolSlugs: new Map(),
    };
  }

  // ==================== Node Registration ====================

  /**
   * Register a new compute node
   */
  async registerNode(request: NodeRegistrationRequest): Promise<ComputeNode> {
    // Validate request
    if (!request.owner_address) {
      throw new Error('Owner address is required');
    }

    if (request.cpu_cores <= 0) {
      throw new Error('CPU cores must be positive');
    }

    if (request.memory_mb <= 0) {
      throw new Error('Memory must be positive');
    }

    if (request.initial_stake < BigInt(100) * BigInt(10) ** BigInt(18)) {
      throw new Error('Minimum stake is 100 tokens');
    }

    // Check for duplicate node from same owner
    for (const node of this.state.nodes.values()) {
      if (
        node.owner_address === request.owner_address &&
        node.status !== 'decommissioned'
      ) {
        throw new Error('Node already registered for this address');
      }
    }

    const node = generateComputeNode({
      owner_address: request.owner_address,
      name: request.name,
      description: request.description,
      cpu_cores: request.cpu_cores,
      cpu_model: request.cpu_model,
      memory_mb: request.memory_mb,
      storage_gb: request.storage_gb,
      gpu_count: request.gpu_count ?? 0,
      gpu_model: request.gpu_model,
      bandwidth_mbps: request.bandwidth_mbps,
      supports_docker: request.supports_docker ?? false,
      supports_k8s: request.supports_k8s ?? false,
      supports_wasm: request.supports_wasm ?? false,
      region: request.region,
      availability_zone: request.availability_zone,
      p2p_address: request.p2p_address,
      tee_type: request.tee_type,
      stake_amount: request.initial_stake,
      status: 'pending',
      sla_score: 100, // Start with perfect score
      uptime_percent: 100,
    });

    this.state.nodes.set(node.id, node);
    return node;
  }

  /**
   * Get a node by ID
   */
  async getNode(nodeId: string): Promise<ComputeNode | null> {
    return this.state.nodes.get(nodeId) ?? null;
  }

  /**
   * Get all nodes
   */
  async getNodes(): Promise<ComputeNode[]> {
    return Array.from(this.state.nodes.values());
  }

  /**
   * Update node status
   */
  async updateNodeStatus(
    nodeId: string,
    status: NodeStatus
  ): Promise<ComputeNode> {
    const node = this.state.nodes.get(nodeId);
    if (!node) {
      throw new Error(`Node not found: ${nodeId}`);
    }

    // Validate state transition
    const validTransitions: Record<NodeStatus, NodeStatus[]> = {
      pending: ['active', 'decommissioned'],
      active: ['suspended', 'offline', 'maintenance', 'decommissioned'],
      suspended: ['active', 'decommissioned'],
      offline: ['active', 'decommissioned'],
      maintenance: ['active', 'decommissioned'],
      decommissioned: [],
    };

    if (!validTransitions[node.status].includes(status)) {
      throw new Error(
        `Invalid status transition: ${node.status} -> ${status}`
      );
    }

    node.status = status;
    node.updated_at = new Date();
    return node;
  }

  /**
   * Activate a pending node
   */
  async activateNode(nodeId: string): Promise<ComputeNode> {
    return this.updateNodeStatus(nodeId, 'active');
  }

  /**
   * Record node heartbeat
   */
  async recordHeartbeat(nodeId: string): Promise<void> {
    const node = this.state.nodes.get(nodeId);
    if (!node) {
      throw new Error(`Node not found: ${nodeId}`);
    }

    node.last_heartbeat_at = new Date();
    node.updated_at = new Date();
  }

  // ==================== Compute Pool Creation ====================

  /**
   * Create a new compute pool
   */
  async createPool(request: PoolCreationRequest): Promise<ComputePool> {
    // Validate request
    if (!request.name) {
      throw new Error('Pool name is required');
    }

    if (!request.slug) {
      throw new Error('Pool slug is required');
    }

    // Check for duplicate slug
    if (this.state.poolSlugs.has(request.slug)) {
      throw new Error(`Pool slug already exists: ${request.slug}`);
    }

    if (request.initial_resource_amount <= BigInt(0)) {
      throw new Error('Initial resource amount must be positive');
    }

    if (request.initial_token_amount <= BigInt(0)) {
      throw new Error('Initial token amount must be positive');
    }

    const feeRate = request.fee_rate ?? 0.003;
    if (feeRate < 0 || feeRate > 1) {
      throw new Error('Fee rate must be between 0 and 1');
    }

    // Calculate initial price and liquidity
    const initialPrice =
      (request.initial_token_amount * BigInt(10) ** BigInt(18)) /
      request.initial_resource_amount;
    const initialLiquidity = this.calculateLiquidityTokens(
      request.initial_resource_amount,
      request.initial_token_amount
    );

    const pool = generateComputePool({
      name: request.name,
      slug: request.slug,
      description: request.description,
      resource_type: request.resource_type,
      resource_amount: request.initial_resource_amount,
      token_amount: request.initial_token_amount,
      total_liquidity: initialLiquidity,
      last_price: initialPrice,
      fee_rate: feeRate,
      min_stake: request.min_stake ?? BigInt(100) * BigInt(10) ** BigInt(18),
      status: 'active',
      volume_24h: BigInt(0),
      volume_7d: BigInt(0),
      volume_total: BigInt(0),
      trade_count_24h: 0,
      trade_count_total: 0,
    });

    this.state.pools.set(pool.id, pool);
    this.state.poolSlugs.set(pool.slug, pool.id);
    return pool;
  }

  /**
   * Calculate LP tokens for initial liquidity
   */
  private calculateLiquidityTokens(
    resourceAmount: bigint,
    tokenAmount: bigint
  ): bigint {
    // sqrt(resourceAmount * tokenAmount)
    const product = resourceAmount * tokenAmount;
    // Simple integer square root approximation
    let x = product;
    let y = (x + BigInt(1)) / BigInt(2);
    while (y < x) {
      x = y;
      y = (x + product / x) / BigInt(2);
    }
    return x;
  }

  /**
   * Get a pool by ID
   */
  async getPool(poolId: string): Promise<ComputePool | null> {
    return this.state.pools.get(poolId) ?? null;
  }

  /**
   * Get a pool by slug
   */
  async getPoolBySlug(slug: string): Promise<ComputePool | null> {
    const poolId = this.state.poolSlugs.get(slug);
    if (!poolId) return null;
    return this.state.pools.get(poolId) ?? null;
  }

  /**
   * Get all pools
   */
  async getPools(): Promise<ComputePool[]> {
    return Array.from(this.state.pools.values());
  }

  /**
   * Add liquidity to a pool
   */
  async addLiquidity(
    poolId: string,
    userAddress: string,
    resourceAmount: bigint,
    tokenAmount: bigint,
    nodeId?: string
  ): Promise<PoolMembership> {
    const pool = this.state.pools.get(poolId);
    if (!pool) {
      throw new Error(`Pool not found: ${poolId}`);
    }

    if (pool.status !== 'active') {
      throw new Error(`Pool is not active: ${pool.status}`);
    }

    // Calculate LP tokens to mint
    const lpTokens = this.calculateLpTokensForDeposit(
      pool,
      resourceAmount,
      tokenAmount
    );

    // Update pool balances
    pool.resource_amount += resourceAmount;
    pool.token_amount += tokenAmount;
    pool.total_liquidity += lpTokens;
    pool.updated_at = new Date();

    // Create or update membership
    let membership = this.findMembership(poolId, userAddress);

    if (membership) {
      membership.liquidity_shares += lpTokens;
      membership.resource_deposited += resourceAmount;
      membership.token_deposited += tokenAmount;
      membership.updated_at = new Date();
    } else {
      membership = {
        id: generateId(),
        pool_id: poolId,
        node_id: nodeId,
        user_address: userAddress,
        role: nodeId ? 'provider' : 'liquidity',
        liquidity_shares: lpTokens,
        resource_deposited: resourceAmount,
        token_deposited: tokenAmount,
        entry_price: pool.last_price,
        pending_rewards: BigInt(0),
        claimed_rewards: BigInt(0),
        last_reward_claim_at: undefined,
        committed_resources: BigInt(0),
        utilized_resources: BigInt(0),
        member_sla_score: 100,
        is_active: true,
        joined_at: new Date(),
        left_at: undefined,
        created_at: new Date(),
        updated_at: new Date(),
      };
      this.state.memberships.set(membership.id, membership);
    }

    return membership;
  }

  /**
   * Calculate LP tokens for a deposit
   */
  private calculateLpTokensForDeposit(
    pool: ComputePool,
    resourceAmount: bigint,
    tokenAmount: bigint
  ): bigint {
    if (pool.total_liquidity === BigInt(0)) {
      return this.calculateLiquidityTokens(resourceAmount, tokenAmount);
    }

    // Proportional to existing liquidity
    const resourceShare =
      (resourceAmount * pool.total_liquidity) / pool.resource_amount;
    const tokenShare =
      (tokenAmount * pool.total_liquidity) / pool.token_amount;

    // Return the minimum to prevent manipulation
    return resourceShare < tokenShare ? resourceShare : tokenShare;
  }

  /**
   * Find membership by pool and user
   */
  private findMembership(
    poolId: string,
    userAddress: string
  ): PoolMembership | undefined {
    for (const membership of this.state.memberships.values()) {
      if (
        membership.pool_id === poolId &&
        membership.user_address === userAddress &&
        membership.is_active
      ) {
        return membership;
      }
    }
    return undefined;
  }

  /**
   * Get membership by ID
   */
  async getMembership(membershipId: string): Promise<PoolMembership | null> {
    return this.state.memberships.get(membershipId) ?? null;
  }

  /**
   * Get memberships for a pool
   */
  async getPoolMemberships(poolId: string): Promise<PoolMembership[]> {
    return Array.from(this.state.memberships.values()).filter(
      (m) => m.pool_id === poolId && m.is_active
    );
  }

  // ==================== QoS Challenge Verification ====================

  /**
   * Issue a challenge to a node
   */
  async issueChallenge(
    nodeId: string,
    challengeType: Challenge['challenge_type'],
    poolId?: string
  ): Promise<Challenge> {
    const node = this.state.nodes.get(nodeId);
    if (!node) {
      throw new Error(`Node not found: ${nodeId}`);
    }

    if (node.status !== 'active') {
      throw new Error(`Node is not active: ${node.status}`);
    }

    const challenge = generateChallenge(nodeId, poolId, {
      challenge_type: challengeType,
    });

    this.state.challenges.set(challenge.id, challenge);
    return challenge;
  }

  /**
   * Submit a challenge response
   */
  async submitChallengeResponse(
    challengeId: string,
    response: ChallengeResponse
  ): Promise<Challenge> {
    const challenge = this.state.challenges.get(challengeId);
    if (!challenge) {
      throw new Error(`Challenge not found: ${challengeId}`);
    }

    if (challenge.status !== 'pending') {
      throw new Error(`Challenge is not pending: ${challenge.status}`);
    }

    const now = new Date();
    if (now > challenge.deadline) {
      challenge.status = 'timeout';
      challenge.updated_at = now;
      throw new Error('Challenge deadline exceeded');
    }

    challenge.response_data = response;
    challenge.response_at = now;
    challenge.updated_at = now;

    return challenge;
  }

  /**
   * Verify a challenge response
   */
  async verifyChallenge(
    challengeId: string,
    verifierAddress: string
  ): Promise<VerificationResult> {
    const challenge = this.state.challenges.get(challengeId);
    if (!challenge) {
      throw new Error(`Challenge not found: ${challengeId}`);
    }

    if (!challenge.response_data) {
      throw new Error('No response submitted for challenge');
    }

    const node = this.state.nodes.get(challenge.node_id);
    if (!node) {
      throw new Error(`Node not found: ${challenge.node_id}`);
    }

    // Perform verification based on challenge type
    const result = this.performVerification(challenge, node);

    // Update challenge
    challenge.verified_by = verifierAddress;
    challenge.verified_at = new Date();
    challenge.verification_proof = new Uint8Array(
      Buffer.from(generateHex(32), 'hex')
    );
    challenge.status = result.passed ? 'passed' : 'failed';
    challenge.score = result.score;
    challenge.penalty_amount = result.penalty_amount ?? BigInt(0);
    challenge.updated_at = new Date();

    // Update node SLA score
    this.updateNodeSlaScore(node, result);

    return result;
  }

  /**
   * Perform verification logic based on challenge type
   */
  private performVerification(
    challenge: Challenge,
    node: ComputeNode
  ): VerificationResult {
    const response = challenge.response_data!;

    switch (challenge.challenge_type) {
      case 'availability': {
        // Check if signed nonce is present
        const passed = !!response.signed_nonce;
        return {
          challenge_id: challenge.id,
          passed,
          score: passed ? 100 : 0,
          failure_reason: passed ? undefined : 'No signed nonce provided',
        };
      }

      case 'latency': {
        const expectedLatency =
          challenge.challenge_data.expected_latency_ms ?? 50;
        const actualLatency = response.latency_ms ?? Infinity;
        const passed = actualLatency <= expectedLatency;
        const score = passed
          ? Math.min(100, Math.round((expectedLatency / actualLatency) * 100))
          : Math.max(0, 100 - (actualLatency - expectedLatency) * 2);

        return {
          challenge_id: challenge.id,
          passed,
          score,
          failure_reason: passed
            ? undefined
            : `Latency ${actualLatency}ms exceeds threshold ${expectedLatency}ms`,
        };
      }

      case 'compute':
      case 'performance': {
        const hasResult = !!response.result_hash;
        const hasHashes =
          response.intermediate_hashes && response.intermediate_hashes.length > 0;
        const hasFlops = response.actual_flops && response.actual_flops > 50;

        const passed = hasResult && hasHashes && hasFlops;
        const score = passed ? Math.min(100, (response.actual_flops ?? 0) / 10) : 0;

        return {
          challenge_id: challenge.id,
          passed,
          score,
          bonus: score > 80 ? 0.1 : undefined,
          failure_reason: passed
            ? undefined
            : 'Compute verification failed',
        };
      }

      case 'bandwidth': {
        const expectedThroughput =
          challenge.challenge_data.expected_throughput_mbps ?? 1000;
        const actualThroughput = response.throughput_mbps ?? 0;
        const passed = actualThroughput >= expectedThroughput;
        const score = passed
          ? Math.min(100, Math.round((actualThroughput / expectedThroughput) * 100))
          : Math.max(0, Math.round((actualThroughput / expectedThroughput) * 100));

        return {
          challenge_id: challenge.id,
          passed,
          score,
          failure_reason: passed
            ? undefined
            : `Throughput ${actualThroughput}Mbps below threshold ${expectedThroughput}Mbps`,
        };
      }

      case 'attestation': {
        // TEE attestation verification (simplified)
        const hasQuote = !!node.tee_type;
        const passed = hasQuote;

        return {
          challenge_id: challenge.id,
          passed,
          score: passed ? 100 : 0,
          bonus: passed ? 0.25 : undefined,
          failure_reason: passed
            ? undefined
            : 'TEE attestation not available',
        };
      }

      default:
        return {
          challenge_id: challenge.id,
          passed: false,
          score: 0,
          failure_reason: `Unknown challenge type: ${challenge.challenge_type}`,
        };
    }
  }

  /**
   * Update node SLA score based on verification result
   */
  private updateNodeSlaScore(
    node: ComputeNode,
    result: VerificationResult
  ): void {
    // Weighted moving average
    const weight = 0.1;
    node.sla_score =
      node.sla_score * (1 - weight) + result.score * weight;
    node.sla_score = parseFloat(node.sla_score.toFixed(2));

    // Update task counts
    if (result.passed) {
      node.total_tasks_completed += 1;
    } else {
      node.total_tasks_failed += 1;
    }

    node.updated_at = new Date();
  }

  /**
   * Get challenge by ID
   */
  async getChallenge(challengeId: string): Promise<Challenge | null> {
    return this.state.challenges.get(challengeId) ?? null;
  }

  /**
   * Get challenges for a node
   */
  async getNodeChallenges(nodeId: string): Promise<Challenge[]> {
    return Array.from(this.state.challenges.values()).filter(
      (c) => c.node_id === nodeId
    );
  }

  /**
   * Get QoS score for a node
   */
  async getNodeQoSScore(nodeId: string): Promise<QoSScore> {
    const node = this.state.nodes.get(nodeId);
    if (!node) {
      throw new Error(`Node not found: ${nodeId}`);
    }

    const challenges = await this.getNodeChallenges(nodeId);
    const recentChallenges = challenges.filter(
      (c) =>
        c.status === 'passed' || c.status === 'failed'
    ).slice(-100);

    // Calculate component scores
    const computeChallenges = recentChallenges.filter(
      (c) => c.challenge_type === 'compute' || c.challenge_type === 'performance'
    );
    const latencyChallenges = recentChallenges.filter(
      (c) => c.challenge_type === 'latency'
    );
    const bandwidthChallenges = recentChallenges.filter(
      (c) => c.challenge_type === 'bandwidth'
    );
    const availabilityChallenges = recentChallenges.filter(
      (c) => c.challenge_type === 'availability'
    );

    const avgScore = (challenges: Challenge[]) =>
      challenges.length > 0
        ? challenges.reduce((sum, c) => sum + (c.score ?? 0), 0) / challenges.length
        : 50;

    const computeScore = Math.round(avgScore(computeChallenges) * 10);
    const latencyScore = Math.round(avgScore(latencyChallenges) * 10);
    const bandwidthScore = Math.round(avgScore(bandwidthChallenges) * 10);
    const availabilityScore = Math.round(node.uptime_percent * 10);

    // Calculate consistency score
    const scores = recentChallenges.map((c) => c.score ?? 0);
    const mean = scores.length > 0 ? scores.reduce((a, b) => a + b, 0) / scores.length : 50;
    const variance =
      scores.length > 1
        ? scores.reduce((sum, s) => sum + (s - mean) ** 2, 0) / scores.length
        : 0;
    const stdDev = Math.sqrt(variance);
    const cv = mean > 0 ? stdDev / mean : 0;
    const consistencyScore = Math.round((1 - Math.min(cv / 0.5, 1)) * 1000);

    // Composite score (weighted geometric mean)
    const composite = Math.round(
      Math.pow(
        Math.pow(computeScore || 500, 0.3) *
          Math.pow(latencyScore || 500, 0.2) *
          Math.pow(bandwidthScore || 500, 0.15) *
          Math.pow(availabilityScore || 500, 0.25) *
          Math.pow(consistencyScore || 500, 0.1),
        1
      )
    );

    return {
      composite,
      compute_score: computeScore,
      latency_score: latencyScore,
      bandwidth_score: bandwidthScore,
      availability_score: availabilityScore,
      consistency_score: consistencyScore,
      confidence: Math.min(recentChallenges.length / 100, 1),
      updated_at: new Date(),
      trend: 0,
    };
  }

  // ==================== Reward Distribution ====================

  /**
   * Calculate and distribute rewards for an epoch
   */
  async distributeRewards(
    request: RewardDistributionRequest
  ): Promise<Reward[]> {
    const pool = this.state.pools.get(request.pool_id);
    if (!pool) {
      throw new Error(`Pool not found: ${request.pool_id}`);
    }

    const memberships = await this.getPoolMemberships(request.pool_id);
    if (memberships.length === 0) {
      return [];
    }

    const rewards: Reward[] = [];

    // Calculate total liquidity shares
    const totalShares = memberships.reduce(
      (sum, m) => sum + m.liquidity_shares,
      BigInt(0)
    );

    if (totalShares === BigInt(0)) {
      return [];
    }

    for (const membership of memberships) {
      const shareRatio = membership.liquidity_shares * BigInt(10) ** BigInt(18) / totalShares;

      // Trading fee rewards
      if (request.total_trading_fees > BigInt(0)) {
        const feeReward =
          (request.total_trading_fees * shareRatio) / BigInt(10) ** BigInt(18);
        if (feeReward > BigInt(0)) {
          const reward = generateReward(
            request.pool_id,
            membership.id,
            membership.user_address,
            {
              reward_type: 'trading_fee',
              amount: feeReward,
              epoch_number: request.epoch_number,
              epoch_start: request.epoch_start,
              epoch_end: request.epoch_end,
            }
          );
          this.state.rewards.set(reward.id, reward);
          rewards.push(reward);

          // Update pending rewards
          membership.pending_rewards += feeReward;
        }
      }

      // Liquidity mining rewards
      if (request.total_liquidity_rewards > BigInt(0)) {
        const liquidityReward =
          (request.total_liquidity_rewards * shareRatio) /
          BigInt(10) ** BigInt(18);
        if (liquidityReward > BigInt(0)) {
          const reward = generateReward(
            request.pool_id,
            membership.id,
            membership.user_address,
            {
              reward_type: 'liquidity_mining',
              amount: liquidityReward,
              epoch_number: request.epoch_number,
              epoch_start: request.epoch_start,
              epoch_end: request.epoch_end,
            }
          );
          this.state.rewards.set(reward.id, reward);
          rewards.push(reward);

          membership.pending_rewards += liquidityReward;
        }
      }

      // SLA bonus rewards for providers
      if (
        membership.role === 'provider' &&
        request.total_sla_bonuses > BigInt(0)
      ) {
        // Get node's SLA score
        const node = membership.node_id
          ? this.state.nodes.get(membership.node_id)
          : null;
        if (node && node.sla_score >= 95) {
          const slaBonusFactor = BigInt(Math.round((node.sla_score - 90) * 10));
          const slaReward =
            (request.total_sla_bonuses * slaBonusFactor) / BigInt(100);
          if (slaReward > BigInt(0)) {
            const reward = generateReward(
              request.pool_id,
              membership.id,
              membership.user_address,
              {
                reward_type: 'sla_bonus',
                amount: slaReward,
                epoch_number: request.epoch_number,
                epoch_start: request.epoch_start,
                epoch_end: request.epoch_end,
              }
            );
            this.state.rewards.set(reward.id, reward);
            rewards.push(reward);

            membership.pending_rewards += slaReward;
          }
        }
      }

      membership.updated_at = new Date();
    }

    return rewards;
  }

  /**
   * Claim pending rewards
   */
  async claimRewards(
    membershipId: string,
    userAddress: string
  ): Promise<{ claimed_amount: bigint; claim_tx_hash: string }> {
    const membership = this.state.memberships.get(membershipId);
    if (!membership) {
      throw new Error(`Membership not found: ${membershipId}`);
    }

    if (membership.user_address !== userAddress) {
      throw new Error('Not authorized to claim rewards');
    }

    if (membership.pending_rewards <= BigInt(0)) {
      throw new Error('No pending rewards to claim');
    }

    const claimedAmount = membership.pending_rewards;
    const claimTxHash = `0x${generateHex(32)}`;

    // Update unclaimed rewards
    for (const reward of this.state.rewards.values()) {
      if (
        reward.membership_id === membershipId &&
        !reward.is_claimed
      ) {
        reward.is_claimed = true;
        reward.claimed_at = new Date();
        reward.claim_tx_hash = claimTxHash;
      }
    }

    // Update membership
    membership.claimed_rewards += claimedAmount;
    membership.pending_rewards = BigInt(0);
    membership.last_reward_claim_at = new Date();
    membership.updated_at = new Date();

    return {
      claimed_amount: claimedAmount,
      claim_tx_hash: claimTxHash,
    };
  }

  /**
   * Get rewards for a user
   */
  async getUserRewards(userAddress: string): Promise<Reward[]> {
    return Array.from(this.state.rewards.values()).filter(
      (r) => r.user_address === userAddress
    );
  }

  /**
   * Get unclaimed rewards for a user
   */
  async getUnclaimedRewards(userAddress: string): Promise<Reward[]> {
    return Array.from(this.state.rewards.values()).filter(
      (r) => r.user_address === userAddress && !r.is_claimed
    );
  }

  /**
   * Get reward by ID
   */
  async getReward(rewardId: string): Promise<Reward | null> {
    return this.state.rewards.get(rewardId) ?? null;
  }

  /**
   * Get all memberships
   */
  async getMemberships(): Promise<PoolMembership[]> {
    return Array.from(this.state.memberships.values());
  }
}

/**
 * Create a singleton instance
 */
export const platformService = new MockPlatformService();
