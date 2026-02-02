'use client'

import { useQuery } from '@tanstack/react-query'
import hanzoNode from '@/lib/api'
import type { HealthResponse, StatusResponse } from '@/types'

export function useNodeHealth() {
  return useQuery<HealthResponse>({
    queryKey: ['node', 'health'],
    queryFn: () => hanzoNode.getHealth(),
    refetchInterval: 10000, // Refresh every 10 seconds
  })
}

export function useNodeStatus(nodeId?: string) {
  return useQuery<StatusResponse>({
    queryKey: ['node', 'status', nodeId],
    queryFn: () => hanzoNode.getStatus(nodeId),
    refetchInterval: 5000, // Refresh every 5 seconds
  })
}

export function useNodeMetrics() {
  return useQuery<string>({
    queryKey: ['node', 'metrics'],
    queryFn: () => hanzoNode.getMetrics(),
    refetchInterval: 10000,
  })
}

export function useReadiness() {
  return useQuery<{ ready: boolean }>({
    queryKey: ['node', 'readiness'],
    queryFn: () => hanzoNode.getReadiness(),
    refetchInterval: 5000,
  })
}

export function useLiveness() {
  return useQuery<{ alive: boolean }>({
    queryKey: ['node', 'liveness'],
    queryFn: () => hanzoNode.getLiveness(),
    refetchInterval: 5000,
  })
}
