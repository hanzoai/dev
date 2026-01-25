/**
 * @fileoverview Pool Types for ComputeDEX Integration
 * Based on PLATFORM_PHASE1_POOLS_DESIGN.md specification
 */

/** Resource types supported by the DEX */
export enum ResourceType {
  CPU = 0,
  GPU = 1,
  Memory = 2,
  Storage = 3,
  Bandwidth = 4,
  WASM = 5,
  Docker = 6,
  K8S = 7,
}

/** Human-readable labels for resource types */
export const ResourceTypeLabels: Record<ResourceType, string> = {
  [ResourceType.CPU]: 'CPU',
  [ResourceType.GPU]: 'GPU',
  [ResourceType.Memory]: 'Memory',
  [ResourceType.Storage]: 'Storage',
  [ResourceType.Bandwidth]: 'Bandwidth',
  [ResourceType.WASM]: 'WASM',
  [ResourceType.Docker]: 'Docker',
  [ResourceType.K8S]: 'Kubernetes',
};

/** Units for each resource type */
export const ResourceTypeUnits: Record<ResourceType, string> = {
  [ResourceType.CPU]: 'cores',
  [ResourceType.GPU]: 'units',
  [ResourceType.Memory]: 'GB',
  [ResourceType.Storage]: 'GB',
  [ResourceType.Bandwidth]: 'Mbps',
  [ResourceType.WASM]: 'instances',
  [ResourceType.Docker]: 'containers',
  [ResourceType.K8S]: 'pods',
};

/** Pool information structure */
export interface Pool {
  id: string;
  resourceType: ResourceType;
  resourceAmount: bigint;
  tokenAmount: bigint;
  totalLiquidity: bigint;
  feeRate: number;
  utilization: number;
  lastPrice: bigint;
  volume24h: bigint;
  priceChange24h: number;
  apr: number;
  createdAt: Date;
  updatedAt: Date;
}

/** Serializable pool for API transport */
export interface PoolDTO {
  id: string;
  resourceType: ResourceType;
  resourceAmount: string;
  tokenAmount: string;
  totalLiquidity: string;
  feeRate: number;
  utilization: number;
  lastPrice: string;
  volume24h: string;
  priceChange24h: number;
  apr: number;
  createdAt: string;
  updatedAt: string;
}

/** User's liquidity position */
export interface LiquidityPosition {
  poolId: string;
  resourceType: ResourceType;
  liquidityShares: bigint;
  resourceAmount: bigint;
  tokenAmount: bigint;
  pendingRewards: bigint;
  entryPrice: bigint;
  impermanentLoss: number;
  createdAt: Date;
}

/** Serializable liquidity position for API transport */
export interface LiquidityPositionDTO {
  poolId: string;
  resourceType: ResourceType;
  liquidityShares: string;
  resourceAmount: string;
  tokenAmount: string;
  pendingRewards: string;
  entryPrice: string;
  impermanentLoss: number;
  createdAt: string;
}

/** Order book entry */
export interface OrderBookEntry {
  orderId: string;
  provider: string;
  resourceType: ResourceType;
  amount: bigint;
  pricePerUnit: bigint;
  duration: number;
  slaScore: number;
  attestation?: string;
  createdAt: Date;
  expiresAt: Date;
}

/** Order book depth level */
export interface OrderBookLevel {
  price: bigint;
  amount: bigint;
  orderCount: number;
}

/** Complete order book */
export interface OrderBook {
  resourceType: ResourceType;
  bids: OrderBookLevel[];
  asks: OrderBookLevel[];
  spread: number;
  midPrice: bigint;
}

/** Price history data point */
export interface PricePoint {
  timestamp: Date;
  open: bigint;
  high: bigint;
  low: bigint;
  close: bigint;
  volume: bigint;
}

/** Timeframe options for charts */
export type Timeframe = '1H' | '24H' | '7D' | '30D' | 'ALL';

/** Swap quote result */
export interface SwapQuote {
  amountIn: bigint;
  amountOut: bigint;
  priceImpact: number;
  fee: bigint;
  route: 'AMM' | 'ORDERBOOK' | 'HYBRID';
  minAmountOut: bigint;
  executionPrice: bigint;
}

/** Deployment target types */
export type DeploymentTargetType = 'local' | 'cloud' | 'pool';

/** Deployment target configuration */
export interface DeploymentTarget {
  type: DeploymentTargetType;
  poolId?: string;
  region?: string;
  serverId?: string;
}

/** Resource requirements for deployment */
export interface ResourceRequirements {
  cpu: number;
  memory: number;
  storage: number;
  gpu?: number;
}

/** Scaling configuration */
export interface ScalingConfig {
  minReplicas: number;
  maxReplicas: number;
  targetCpuUtilization: number;
}

/** Budget configuration */
export interface BudgetConfig {
  maxTokensPerHour: bigint;
  maxTotalTokens: bigint;
}

/** Complete deployment configuration */
export interface DeploymentConfig {
  applicationId: string;
  target: DeploymentTarget;
  resources: ResourceRequirements;
  scaling: ScalingConfig;
  budget: BudgetConfig;
}

/** Pool deployment states */
export enum PoolDeploymentState {
  CREATED = 'created',
  MATCHING = 'matching',
  MATCHED = 'matched',
  PROVISIONING = 'provisioning',
  RUNNING = 'running',
  SCALING = 'scaling',
  STOPPING = 'stopping',
  STOPPED = 'stopped',
  FAILED = 'failed',
}

/** Provider information */
export interface ProviderInfo {
  address: string;
  slaScore: number;
  availability: number;
  pricePerUnit: bigint;
  attestation?: string;
}

/** Deployment status */
export interface DeploymentStatus {
  orderId: string;
  state: PoolDeploymentState;
  progress: number;
  message: string;
  providers?: ProviderInfo[];
  costSoFar?: bigint;
  estimatedHourlyCost?: bigint;
  startedAt?: Date;
  updatedAt: Date;
}

/** Compute offer for pool selection */
export interface ComputeOffer {
  poolId: string;
  resourceType: ResourceType;
  pricePerUnit: bigint;
  availability: number;
  avgSlaScore: number;
  totalProviders: number;
}

/** Add liquidity parameters */
export interface AddLiquidityParams {
  resourceType: ResourceType;
  resourceAmount: bigint;
  tokenAmount: bigint;
  slippageTolerance: number;
}

/** Remove liquidity parameters */
export interface RemoveLiquidityParams {
  resourceType: ResourceType;
  liquidityAmount: bigint;
  minResourceAmount: bigint;
  minTokenAmount: bigint;
}

/** Swap parameters */
export interface SwapParams {
  resourceType: ResourceType;
  isBuying: boolean;
  amountIn: bigint;
  minAmountOut: bigint;
  deadline: Date;
}

/** Convert PoolDTO to Pool */
export function poolFromDTO(dto: PoolDTO): Pool {
  return {
    ...dto,
    resourceAmount: BigInt(dto.resourceAmount),
    tokenAmount: BigInt(dto.tokenAmount),
    totalLiquidity: BigInt(dto.totalLiquidity),
    lastPrice: BigInt(dto.lastPrice),
    volume24h: BigInt(dto.volume24h),
    createdAt: new Date(dto.createdAt),
    updatedAt: new Date(dto.updatedAt),
  };
}

/** Convert Pool to PoolDTO */
export function poolToDTO(pool: Pool): PoolDTO {
  return {
    ...pool,
    resourceAmount: pool.resourceAmount.toString(),
    tokenAmount: pool.tokenAmount.toString(),
    totalLiquidity: pool.totalLiquidity.toString(),
    lastPrice: pool.lastPrice.toString(),
    volume24h: pool.volume24h.toString(),
    createdAt: pool.createdAt.toISOString(),
    updatedAt: pool.updatedAt.toISOString(),
  };
}

/** Convert LiquidityPositionDTO to LiquidityPosition */
export function positionFromDTO(dto: LiquidityPositionDTO): LiquidityPosition {
  return {
    ...dto,
    liquidityShares: BigInt(dto.liquidityShares),
    resourceAmount: BigInt(dto.resourceAmount),
    tokenAmount: BigInt(dto.tokenAmount),
    pendingRewards: BigInt(dto.pendingRewards),
    entryPrice: BigInt(dto.entryPrice),
    createdAt: new Date(dto.createdAt),
  };
}
