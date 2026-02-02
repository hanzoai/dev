'use client'

import { useQuery } from '@tanstack/react-query'
import hanzoNode from '@/lib/api'
import type { ComputePool, PoolNode, Challenge, QoSScore, PaginatedResponse } from '@/types'

export function useComputePools(filters?: {
  region?: string
  min_gpu_memory?: number
  capabilities?: string
}) {
  return useQuery<{ pools: ComputePool[] }>({
    queryKey: ['compute-pools', filters],
    queryFn: () => hanzoNode.listComputePools(filters),
    refetchInterval: 30000,
  })
}

export function useComputePool(poolId: string | undefined) {
  return useQuery<ComputePool>({
    queryKey: ['compute-pool', poolId],
    queryFn: () => hanzoNode.getComputePool(poolId!),
    enabled: !!poolId,
    refetchInterval: 15000,
  })
}

export function usePoolNodes(
  poolId: string | undefined,
  options?: { status?: string; page?: number; per_page?: number }
) {
  return useQuery<PaginatedResponse<PoolNode>>({
    queryKey: ['pool-nodes', poolId, options],
    queryFn: () => hanzoNode.listPoolNodes(poolId!, options),
    enabled: !!poolId,
    refetchInterval: 15000,
  })
}

export function useChallenges(filters?: {
  provider_id?: string
  status?: string
  type?: string
}) {
  return useQuery<{ challenges: Challenge[] }>({
    queryKey: ['challenges', filters],
    queryFn: () => hanzoNode.listChallenges(filters),
    refetchInterval: 10000,
  })
}

export function useProviderScore(providerId: string | undefined) {
  return useQuery<QoSScore>({
    queryKey: ['provider-score', providerId],
    queryFn: () => hanzoNode.getProviderScore(providerId!),
    enabled: !!providerId,
    refetchInterval: 30000,
  })
}
