// Hanzo Node API Client
import type {
  HealthResponse,
  StatusResponse,
  Deployment,
  DeploymentDetails,
  ComputePool,
  PoolNode,
  Challenge,
  QoSScore,
  LogEntry,
  NodeEvent,
  ContainerInfo,
  ApiError,
  PaginatedResponse,
  ComposeSpec,
} from '@/types'

const API_BASE = process.env.NEXT_PUBLIC_HANZO_NODE_URL || '/api/node'

class HanzoNodeClient {
  private baseUrl: string
  private headers: HeadersInit

  constructor(baseUrl: string = API_BASE) {
    this.baseUrl = baseUrl
    this.headers = {
      'Content-Type': 'application/json',
    }
  }

  private async request<T>(
    path: string,
    options: RequestInit = {}
  ): Promise<T> {
    const url = `${this.baseUrl}${path}`
    const response = await fetch(url, {
      ...options,
      headers: {
        ...this.headers,
        ...options.headers,
      },
    })

    if (!response.ok) {
      const error: ApiError = await response.json().catch(() => ({
        code: 'UNKNOWN_ERROR',
        message: response.statusText,
      }))
      throw new Error(error.message || 'API request failed')
    }

    return response.json()
  }

  // Health endpoints
  async getHealth(): Promise<HealthResponse> {
    return this.request<HealthResponse>('/health')
  }

  async getReadiness(): Promise<{ ready: boolean }> {
    return this.request<{ ready: boolean }>('/readiness')
  }

  async getLiveness(): Promise<{ alive: boolean }> {
    return this.request<{ alive: boolean }>('/liveness')
  }

  async getStatus(nodeId?: string): Promise<StatusResponse> {
    const params = nodeId ? `?node_id=${nodeId}` : ''
    return this.request<StatusResponse>(`/v1/status${params}`)
  }

  async getMetrics(): Promise<string> {
    const response = await fetch(`${this.baseUrl}/v1/metrics`)
    return response.text()
  }

  // Deployment endpoints
  async listDeployments(filters?: {
    namespace?: string
    environment?: string
    status?: string
  }): Promise<{ deployments: Deployment[] }> {
    const params = new URLSearchParams()
    if (filters?.namespace) params.set('namespace', filters.namespace)
    if (filters?.environment) params.set('environment', filters.environment)
    if (filters?.status) params.set('status', filters.status)
    const query = params.toString() ? `?${params}` : ''
    return this.request<{ deployments: Deployment[] }>(`/v1/deployments${query}`)
  }

  async getDeployment(deploymentId: string): Promise<DeploymentDetails> {
    return this.request<DeploymentDetails>(`/v1/deployments/${deploymentId}`)
  }

  async createDeployment(data: {
    name: string
    spec: ComposeSpec
    environment?: Record<string, string>
    options?: {
      namespace?: string
      environment?: 'production' | 'staging' | 'development'
      labels?: Record<string, string>
    }
  }): Promise<{ success: boolean; deployment_id: string; message: string; endpoints: string[] }> {
    return this.request('/v1/deployments', {
      method: 'POST',
      body: JSON.stringify(data),
    })
  }

  async stopDeployment(deploymentId: string, force = false): Promise<{ success: boolean; message: string }> {
    return this.request(`/v1/deployments/${deploymentId}/stop`, {
      method: 'POST',
      body: JSON.stringify({ force }),
    })
  }

  async removeDeployment(deploymentId: string, removeVolumes = false): Promise<{ success: boolean; message: string }> {
    const params = removeVolumes ? '?remove_volumes=true' : ''
    return this.request(`/v1/deployments/${deploymentId}${params}`, {
      method: 'DELETE',
    })
  }

  async scaleDeployment(
    deploymentId: string,
    serviceName: string,
    replicas: number
  ): Promise<{ success: boolean; message: string; previous_replicas: number; current_replicas: number }> {
    return this.request(`/v1/deployments/${deploymentId}/scale`, {
      method: 'POST',
      body: JSON.stringify({ service_name: serviceName, replicas }),
    })
  }

  // Container endpoints
  async listContainers(all = false, filters?: string): Promise<{ containers: ContainerInfo[] }> {
    const params = new URLSearchParams()
    if (all) params.set('all', 'true')
    if (filters) params.set('filters', filters)
    const query = params.toString() ? `?${params}` : ''
    return this.request<{ containers: ContainerInfo[] }>(`/v1/containers${query}`)
  }

  async inspectContainer(containerId: string): Promise<ContainerInfo> {
    return this.request<ContainerInfo>(`/v1/containers/${containerId}`)
  }

  async execInContainer(
    containerId: string,
    command: string[],
    options?: { tty?: boolean; stdin?: string }
  ): Promise<{ stdout: string; stderr: string; exit_code: number; done: boolean }> {
    return this.request(`/v1/containers/${containerId}/exec`, {
      method: 'POST',
      body: JSON.stringify({ command, ...options }),
    })
  }

  // Compute Pool endpoints
  async listComputePools(filters?: {
    region?: string
    min_gpu_memory?: number
    capabilities?: string
  }): Promise<{ pools: ComputePool[] }> {
    const params = new URLSearchParams()
    if (filters?.region) params.set('region', filters.region)
    if (filters?.min_gpu_memory) params.set('min_gpu_memory', filters.min_gpu_memory.toString())
    if (filters?.capabilities) params.set('capabilities', filters.capabilities)
    const query = params.toString() ? `?${params}` : ''
    return this.request<{ pools: ComputePool[] }>(`/v1/compute/pools${query}`)
  }

  async getComputePool(poolId: string): Promise<ComputePool> {
    return this.request<ComputePool>(`/v1/compute/pools/${poolId}`)
  }

  async listPoolNodes(
    poolId: string,
    options?: { status?: string; page?: number; per_page?: number }
  ): Promise<PaginatedResponse<PoolNode>> {
    const params = new URLSearchParams()
    if (options?.status) params.set('status', options.status)
    if (options?.page) params.set('page', options.page.toString())
    if (options?.per_page) params.set('per_page', options.per_page.toString())
    const query = params.toString() ? `?${params}` : ''
    const response = await this.request<{ nodes: PoolNode[]; total: number; page: number; per_page: number }>(
      `/v1/compute/pools/${poolId}/nodes${query}`
    )
    return {
      items: response.nodes,
      total: response.total,
      page: response.page,
      per_page: response.per_page,
    }
  }

  // QoS Challenge endpoints
  async listChallenges(filters?: {
    provider_id?: string
    status?: string
    type?: string
  }): Promise<{ challenges: Challenge[] }> {
    const params = new URLSearchParams()
    if (filters?.provider_id) params.set('provider_id', filters.provider_id)
    if (filters?.status) params.set('status', filters.status)
    if (filters?.type) params.set('type', filters.type)
    const query = params.toString() ? `?${params}` : ''
    return this.request<{ challenges: Challenge[] }>(`/v1/qos/challenges${query}`)
  }

  async getProviderScore(providerId: string): Promise<QoSScore> {
    return this.request<QoSScore>(`/v1/qos/providers/${providerId}/score`)
  }

  // Logs endpoints
  async getDeploymentLogs(
    deploymentId: string,
    options?: { service_name?: string; tail?: number; since?: number; until?: number }
  ): Promise<{ entries: LogEntry[] }> {
    const params = new URLSearchParams()
    if (options?.service_name) params.set('service_name', options.service_name)
    if (options?.tail) params.set('tail', options.tail.toString())
    if (options?.since) params.set('since', options.since.toString())
    if (options?.until) params.set('until', options.until.toString())
    const query = params.toString() ? `?${params}` : ''
    return this.request<{ entries: LogEntry[] }>(`/v1/deployments/${deploymentId}/logs${query}`)
  }

  async getEvents(options?: {
    node_id?: string
    event_types?: string
    since?: number
    limit?: number
  }): Promise<{ events: NodeEvent[] }> {
    const params = new URLSearchParams()
    if (options?.node_id) params.set('node_id', options.node_id)
    if (options?.event_types) params.set('event_types', options.event_types)
    if (options?.since) params.set('since', options.since.toString())
    if (options?.limit) params.set('limit', options.limit.toString())
    const query = params.toString() ? `?${params}` : ''
    return this.request<{ events: NodeEvent[] }>(`/v1/events${query}`)
  }
}

export const hanzoNode = new HanzoNodeClient()
export default hanzoNode
