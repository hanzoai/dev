/**
 * Mock Data Generators for Platform Integration Tests
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
  ResourceType,
  NodeStatus,
  PoolStatus,
  ChallengeStatus,
  ChallengeType,
  MembershipRole,
  RewardType,
} from '../types';

// Simple random ID generator
function generateId(): string {
  return crypto.randomUUID();
}

// Generate random Ethereum address
function generateAddress(): string {
  const chars = '0123456789abcdef';
  let addr = '0x';
  for (let i = 0; i < 40; i++) {
    addr += chars[Math.floor(Math.random() * chars.length)];
  }
  return addr;
}

// Generate random bytes as hex
function generateHex(length: number): string {
  const chars = '0123456789abcdef';
  let result = '';
  for (let i = 0; i < length * 2; i++) {
    result += chars[Math.floor(Math.random() * chars.length)];
  }
  return result;
}

// Generate random number in range
function randomInt(min: number, max: number): number {
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

// Resource types for random selection
const RESOURCE_TYPES: ResourceType[] = [
  'CPU',
  'GPU',
  'Memory',
  'Storage',
  'Bandwidth',
  'WASM',
  'Docker',
  'K8S',
];

const GPU_MODELS = [
  'NVIDIA A100',
  'NVIDIA H100',
  'NVIDIA RTX 4090',
  'AMD MI300X',
  'NVIDIA L40S',
];

const CPU_MODELS = [
  'AMD EPYC 9654',
  'Intel Xeon Platinum 8480+',
  'AMD EPYC 9754',
  'Intel Xeon w9-3595X',
];

const REGIONS = [
  'us-east-1',
  'us-west-2',
  'eu-west-1',
  'ap-northeast-1',
  'ap-southeast-1',
];

const TEE_TYPES = ['SGX', 'TDX', 'SEV', 'TrustZone', null];

/**
 * Generate a mock ComputeNode
 */
export function generateComputeNode(
  overrides: Partial<ComputeNode> = {}
): ComputeNode {
  const hasGpu = Math.random() > 0.3;
  const now = new Date();

  return {
    id: generateId(),
    owner_address: generateAddress(),
    name: `Node-${generateHex(4)}`,
    description: 'High-performance compute node for AI workloads',
    cpu_cores: randomInt(8, 128),
    cpu_model: CPU_MODELS[randomInt(0, CPU_MODELS.length - 1)],
    memory_mb: randomInt(32768, 1048576), // 32GB to 1TB
    storage_gb: randomInt(500, 10000),
    gpu_count: hasGpu ? randomInt(1, 8) : 0,
    gpu_model: hasGpu
      ? GPU_MODELS[randomInt(0, GPU_MODELS.length - 1)]
      : undefined,
    bandwidth_mbps: randomInt(1000, 100000),
    supports_docker: Math.random() > 0.1,
    supports_k8s: Math.random() > 0.3,
    supports_wasm: Math.random() > 0.5,
    region: REGIONS[randomInt(0, REGIONS.length - 1)],
    availability_zone: `az-${randomInt(1, 3)}`,
    ip_address: `${randomInt(10, 192)}.${randomInt(0, 255)}.${randomInt(0, 255)}.${randomInt(1, 254)}`,
    p2p_address: `/ip4/0.0.0.0/tcp/${randomInt(30000, 40000)}/p2p/${generateHex(32)}`,
    tee_type: TEE_TYPES[randomInt(0, TEE_TYPES.length - 1)] ?? undefined,
    attestation_quote: undefined,
    attestation_verified_at: undefined,
    sla_score: parseFloat((85 + Math.random() * 15).toFixed(2)),
    uptime_percent: parseFloat((95 + Math.random() * 5).toFixed(2)),
    avg_response_time_ms: randomInt(1, 100),
    total_tasks_completed: randomInt(0, 10000),
    total_tasks_failed: randomInt(0, 100),
    stake_amount: BigInt(randomInt(100, 10000)) * BigInt(10) ** BigInt(18),
    stake_locked_until: undefined,
    status: 'pending' as NodeStatus,
    last_heartbeat_at: now,
    created_at: now,
    updated_at: now,
    ...overrides,
  };
}

/**
 * Generate a mock NodeRegistrationRequest
 */
export function generateNodeRegistrationRequest(
  overrides: Partial<NodeRegistrationRequest> = {}
): NodeRegistrationRequest {
  const hasGpu = Math.random() > 0.3;

  return {
    owner_address: generateAddress(),
    name: `Node-${generateHex(4)}`,
    description: 'High-performance compute node',
    cpu_cores: randomInt(8, 128),
    cpu_model: CPU_MODELS[randomInt(0, CPU_MODELS.length - 1)],
    memory_mb: randomInt(32768, 524288),
    storage_gb: randomInt(500, 5000),
    gpu_count: hasGpu ? randomInt(1, 8) : 0,
    gpu_model: hasGpu
      ? GPU_MODELS[randomInt(0, GPU_MODELS.length - 1)]
      : undefined,
    bandwidth_mbps: randomInt(1000, 25000),
    supports_docker: true,
    supports_k8s: Math.random() > 0.5,
    supports_wasm: Math.random() > 0.5,
    region: REGIONS[randomInt(0, REGIONS.length - 1)],
    availability_zone: `az-${randomInt(1, 3)}`,
    p2p_address: `/ip4/0.0.0.0/tcp/${randomInt(30000, 40000)}/p2p/${generateHex(32)}`,
    tee_type: TEE_TYPES[randomInt(0, TEE_TYPES.length - 1)] ?? undefined,
    initial_stake: BigInt(randomInt(100, 1000)) * BigInt(10) ** BigInt(18),
    ...overrides,
  };
}

/**
 * Generate a mock ComputePool
 */
export function generateComputePool(
  overrides: Partial<ComputePool> = {}
): ComputePool {
  const now = new Date();
  const resourceType = RESOURCE_TYPES[randomInt(0, RESOURCE_TYPES.length - 1)];

  return {
    id: generateId(),
    name: `${resourceType} Pool - ${generateHex(4)}`,
    slug: `${resourceType.toLowerCase()}-pool-${generateHex(4)}`,
    description: `AMM liquidity pool for ${resourceType} compute resources`,
    resource_type: resourceType,
    resource_amount:
      BigInt(randomInt(1000, 100000)) * BigInt(10) ** BigInt(18),
    token_amount: BigInt(randomInt(10000, 1000000)) * BigInt(10) ** BigInt(18),
    total_liquidity: BigInt(randomInt(5000, 500000)) * BigInt(10) ** BigInt(18),
    last_price: BigInt(randomInt(1, 100)) * BigInt(10) ** BigInt(16),
    fee_rate: 0.003,
    volume_24h: BigInt(randomInt(1000, 50000)) * BigInt(10) ** BigInt(18),
    volume_7d: BigInt(randomInt(7000, 350000)) * BigInt(10) ** BigInt(18),
    volume_total: BigInt(randomInt(100000, 5000000)) * BigInt(10) ** BigInt(18),
    trade_count_24h: randomInt(10, 500),
    trade_count_total: randomInt(1000, 50000),
    price_change_1h: parseFloat((Math.random() * 10 - 5).toFixed(6)),
    price_change_24h: parseFloat((Math.random() * 20 - 10).toFixed(6)),
    price_change_7d: parseFloat((Math.random() * 30 - 15).toFixed(6)),
    utilization: parseFloat((Math.random() * 80 + 10).toFixed(2)),
    avg_sla_score: parseFloat((85 + Math.random() * 15).toFixed(2)),
    apr: parseFloat((Math.random() * 50 + 5).toFixed(4)),
    status: 'active' as PoolStatus,
    min_stake: BigInt(100) * BigInt(10) ** BigInt(18),
    created_at: now,
    updated_at: now,
    ...overrides,
  };
}

/**
 * Generate a mock PoolCreationRequest
 */
export function generatePoolCreationRequest(
  overrides: Partial<PoolCreationRequest> = {}
): PoolCreationRequest {
  const resourceType = RESOURCE_TYPES[randomInt(0, RESOURCE_TYPES.length - 1)];
  const slug = `${resourceType.toLowerCase()}-pool-${generateHex(4)}`;

  return {
    name: `${resourceType} Pool - ${generateHex(4)}`,
    slug,
    description: `AMM liquidity pool for ${resourceType} compute resources`,
    resource_type: resourceType,
    initial_resource_amount:
      BigInt(randomInt(1000, 10000)) * BigInt(10) ** BigInt(18),
    initial_token_amount:
      BigInt(randomInt(10000, 100000)) * BigInt(10) ** BigInt(18),
    fee_rate: 0.003,
    min_stake: BigInt(100) * BigInt(10) ** BigInt(18),
    ...overrides,
  };
}

/**
 * Generate a mock ComputeOffer
 */
export function generateComputeOffer(
  nodeId: string,
  poolId?: string,
  overrides: Partial<ComputeOffer> = {}
): ComputeOffer {
  const now = new Date();
  const expiresAt = new Date(now.getTime() + randomInt(1, 30) * 24 * 60 * 60 * 1000);

  return {
    id: generateId(),
    node_id: nodeId,
    pool_id: poolId,
    resource_type: RESOURCE_TYPES[randomInt(0, RESOURCE_TYPES.length - 1)],
    amount: BigInt(randomInt(10, 1000)) * BigInt(10) ** BigInt(18),
    amount_remaining: BigInt(randomInt(5, 500)) * BigInt(10) ** BigInt(18),
    price_per_unit: BigInt(randomInt(1, 50)) * BigInt(10) ** BigInt(16),
    min_duration_hours: 1,
    max_duration_hours: randomInt(24, 720),
    guaranteed_uptime: parseFloat((95 + Math.random() * 5).toFixed(2)),
    max_latency_ms: randomInt(10, 100),
    status: 'active',
    expires_at: expiresAt,
    created_at: now,
    updated_at: now,
    ...overrides,
  };
}

/**
 * Generate a mock PoolMembership
 */
export function generatePoolMembership(
  poolId: string,
  userAddress?: string,
  nodeId?: string,
  overrides: Partial<PoolMembership> = {}
): PoolMembership {
  const now = new Date();
  const role: MembershipRole = nodeId
    ? 'provider'
    : Math.random() > 0.5
      ? 'liquidity'
      : 'consumer';

  return {
    id: generateId(),
    pool_id: poolId,
    node_id: nodeId,
    user_address: userAddress ?? generateAddress(),
    role,
    liquidity_shares: BigInt(randomInt(100, 10000)) * BigInt(10) ** BigInt(18),
    resource_deposited: BigInt(randomInt(10, 1000)) * BigInt(10) ** BigInt(18),
    token_deposited: BigInt(randomInt(100, 10000)) * BigInt(10) ** BigInt(18),
    entry_price: BigInt(randomInt(1, 100)) * BigInt(10) ** BigInt(16),
    pending_rewards: BigInt(randomInt(0, 500)) * BigInt(10) ** BigInt(18),
    claimed_rewards: BigInt(randomInt(0, 1000)) * BigInt(10) ** BigInt(18),
    last_reward_claim_at: undefined,
    committed_resources:
      role === 'provider'
        ? BigInt(randomInt(100, 1000)) * BigInt(10) ** BigInt(18)
        : BigInt(0),
    utilized_resources:
      role === 'provider'
        ? BigInt(randomInt(0, 500)) * BigInt(10) ** BigInt(18)
        : BigInt(0),
    member_sla_score: parseFloat((85 + Math.random() * 15).toFixed(2)),
    is_active: true,
    joined_at: now,
    left_at: undefined,
    created_at: now,
    updated_at: now,
    ...overrides,
  };
}

/**
 * Generate a mock Challenge
 */
export function generateChallenge(
  nodeId: string,
  poolId?: string,
  overrides: Partial<Challenge> = {}
): Challenge {
  const now = new Date();
  const deadline = new Date(now.getTime() + randomInt(60, 600) * 1000);
  const challengeTypes: ChallengeType[] = [
    'availability',
    'performance',
    'attestation',
    'compute',
    'latency',
    'bandwidth',
  ];
  const challengeType =
    challengeTypes[randomInt(0, challengeTypes.length - 1)];

  const challengeData: ChallengeData = {
    seed: generateHex(32),
    difficulty: randomInt(1, 10),
    nonce: generateHex(16),
    rounds: randomInt(1, 10),
    expected_latency_ms: randomInt(10, 100),
    expected_throughput_mbps: randomInt(100, 10000),
  };

  return {
    id: generateId(),
    node_id: nodeId,
    pool_id: poolId,
    membership_id: undefined,
    challenge_type: challengeType,
    challenge_data: challengeData,
    response_data: undefined,
    response_at: undefined,
    verified_by: undefined,
    verification_proof: undefined,
    verified_at: undefined,
    status: 'pending' as ChallengeStatus,
    score: undefined,
    penalty_amount: BigInt(0),
    deadline,
    created_at: now,
    updated_at: now,
    ...overrides,
  };
}

/**
 * Generate a mock Challenge Response
 */
export function generateChallengeResponse(
  challenge: Challenge,
  passed: boolean = true
): ChallengeResponse {
  if (challenge.challenge_type === 'availability') {
    return {
      signed_nonce: generateHex(64),
    };
  }

  if (challenge.challenge_type === 'latency') {
    const expectedLatency = challenge.challenge_data.expected_latency_ms ?? 50;
    return {
      latency_ms: passed
        ? randomInt(1, expectedLatency)
        : randomInt(expectedLatency + 1, expectedLatency * 2),
    };
  }

  if (
    challenge.challenge_type === 'compute' ||
    challenge.challenge_type === 'performance'
  ) {
    return {
      result_hash: generateHex(32),
      intermediate_hashes: Array.from({ length: 5 }, () => generateHex(32)),
      actual_flops: passed ? randomInt(100, 1000) : randomInt(1, 50),
    };
  }

  if (challenge.challenge_type === 'bandwidth') {
    const expectedThroughput =
      challenge.challenge_data.expected_throughput_mbps ?? 1000;
    return {
      throughput_mbps: passed
        ? randomInt(expectedThroughput, expectedThroughput * 2)
        : randomInt(1, expectedThroughput - 1),
    };
  }

  return {
    result_hash: generateHex(32),
  };
}

/**
 * Generate a mock Reward
 */
export function generateReward(
  poolId: string,
  membershipId: string,
  userAddress: string,
  overrides: Partial<Reward> = {}
): Reward {
  const now = new Date();
  const rewardTypes: RewardType[] = [
    'trading_fee',
    'liquidity_mining',
    'sla_bonus',
  ];
  const rewardType = rewardTypes[randomInt(0, rewardTypes.length - 1)];
  const epochStart = new Date(now.getTime() - 7 * 24 * 60 * 60 * 1000);
  const epochEnd = now;

  return {
    id: generateId(),
    pool_id: poolId,
    membership_id: membershipId,
    user_address: userAddress,
    reward_type: rewardType,
    amount: BigInt(randomInt(1, 100)) * BigInt(10) ** BigInt(18),
    source_tx_hash: Math.random() > 0.5 ? `0x${generateHex(32)}` : undefined,
    source_trade_id: Math.random() > 0.5 ? generateId() : undefined,
    epoch_number: randomInt(1, 100),
    epoch_start: epochStart,
    epoch_end: epochEnd,
    is_claimed: false,
    claimed_at: undefined,
    claim_tx_hash: undefined,
    created_at: now,
    ...overrides,
  };
}

/**
 * Generate multiple nodes for testing
 */
export function generateNodes(count: number): ComputeNode[] {
  return Array.from({ length: count }, () => generateComputeNode());
}

/**
 * Generate multiple pools for testing
 */
export function generatePools(count: number): ComputePool[] {
  return Array.from({ length: count }, () => generateComputePool());
}
