// Hanzo Node API Types
// Generated from OpenAPI spec at /Users/z/work/hanzo/dev/hanzo-node/api/openapi.yaml

export type HealthStatus = 'HEALTHY' | 'DEGRADED' | 'UNHEALTHY'
export type NodeState = 'STARTING' | 'READY' | 'DRAINING' | 'STOPPED'
export type NodeStatus = 'pending' | 'active' | 'suspended' | 'offline' | 'maintenance' | 'decommissioned'
export type DeploymentStatus = 'running' | 'stopped' | 'failed' | 'pending'
export type ChallengeStatus = 'pending' | 'completed' | 'expired' | 'failed'
export type ChallengeType = 'compute' | 'latency' | 'bandwidth' | 'availability' | 'model'

export interface ConsensusStatus {
  synced: boolean
  block_height: number
  network_id: string
}

export interface NodeMetrics {
  cpu_usage: number
  memory_usage: number
  disk_usage: number
  network_rx_bytes: number
  network_tx_bytes: number
  active_containers: number
}

export interface GpuInfo {
  model: string
  memory_bytes: number
  driver_version: string
}

export interface NodeCapabilities {
  cpu_cores: number
  memory_bytes: number
  disk_bytes: number
  gpus: GpuInfo[]
  mlx_enabled: boolean
  supported_runtimes: string[]
  region: string
  zone: string
}

export interface HealthResponse {
  status: HealthStatus
  version: string
  mlx_enabled: boolean
  models: string[]
  consensus: ConsensusStatus
}

export interface StatusResponse {
  state: NodeState
  uptime_seconds: number
  metrics: NodeMetrics
  deployments: Deployment[]
}

export interface Deployment {
  deployment_id: string
  name: string
  status: DeploymentStatus
  replicas: number
  ready_replicas: number
}

export interface DeploymentDetails extends Deployment {
  spec: ComposeSpec
  created_at: number
  updated_at: number
  services: ServiceStatus[]
}

export interface ServiceStatus {
  name: string
  status: string
  replicas: number
  ready: number
  containers: ContainerInfo[]
}

export interface ContainerInfo {
  id: string
  name: string
  image: string
  status: string
  state: 'created' | 'running' | 'paused' | 'restarting' | 'removing' | 'exited' | 'dead'
  created: number
  ports: string[]
  labels: Record<string, string>
}

export interface ComposeSpec {
  version: string
  name: string
  services: Record<string, ServiceSpec>
  networks?: Record<string, NetworkSpec>
  volumes?: Record<string, VolumeSpec>
}

export interface ServiceSpec {
  image: string
  command?: string[]
  entrypoint?: string[]
  environment?: Record<string, string>
  ports?: string[]
  volumes?: string[]
  networks?: string[]
  deploy?: DeploySpec
  healthcheck?: HealthcheckSpec
  restart?: 'no' | 'always' | 'on-failure' | 'unless-stopped'
}

export interface DeploySpec {
  replicas?: number
  resources?: ResourceSpec
}

export interface ResourceSpec {
  limits?: ResourceLimits
  reservations?: ResourceLimits
}

export interface ResourceLimits {
  cpus?: string
  memory?: string
}

export interface HealthcheckSpec {
  test: string[]
  interval?: string
  timeout?: string
  retries?: number
  start_period?: string
}

export interface NetworkSpec {
  driver?: string
  external?: boolean
}

export interface VolumeSpec {
  driver?: string
  driver_opts?: Record<string, string>
}

export interface ComputePool {
  pool_id: string
  name: string
  description: string
  region: string
  total_nodes: number
  active_nodes: number
  total_gpus: number
  available_gpus: number
  min_stake: number
  average_qos_score: number
}

export interface PoolNode {
  node_id: string
  operator_address: string
  capabilities: NodeCapabilities
  status: 'active' | 'draining' | 'offline'
  qos_score: number
  stake: number
  uptime_percentage: number
}

export interface Challenge {
  id: string
  challenge_type: ChallengeTypeDetails
  seed: string
  difficulty: number
  deadline_ms: number
  issued_at: number
  signature: string
}

export interface ChallengeTypeDetails {
  type: ChallengeType
  compute_metric?: string
  transfer_direction?: string
  model_capability?: ModelCapability
}

export interface ModelCapability {
  model_id: string
  required_vram_gb: number
  expected_tps: number
}

export interface QoSScore {
  composite: number
  components: QoSComponents
  confidence: number
  updated_at: number
  trend: number
}

export interface QoSComponents {
  compute_score: number
  latency_score: number
  bandwidth_score: number
  availability_score: number
  consistency_score: number
}

export interface LogEntry {
  timestamp: number
  service: string
  container_id: string
  stream: 'stdout' | 'stderr'
  message: string
}

export interface NodeEvent {
  timestamp: number
  event_type: string
  resource_type: string
  resource_id: string
  message: string
  attributes: Record<string, string>
}

// API Client Types
export interface ApiError {
  code: string
  message: string
  details?: Record<string, unknown>
}

export interface PaginatedResponse<T> {
  items: T[]
  total: number
  page: number
  per_page: number
}
