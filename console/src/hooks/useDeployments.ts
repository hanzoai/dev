'use client'

import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query'
import hanzoNode from '@/lib/api'
import type { Deployment, DeploymentDetails, ComposeSpec } from '@/types'

export function useDeployments(filters?: {
  namespace?: string
  environment?: string
  status?: string
}) {
  return useQuery<{ deployments: Deployment[] }>({
    queryKey: ['deployments', filters],
    queryFn: () => hanzoNode.listDeployments(filters),
    refetchInterval: 10000,
  })
}

export function useDeployment(deploymentId: string | undefined) {
  return useQuery<DeploymentDetails>({
    queryKey: ['deployment', deploymentId],
    queryFn: () => hanzoNode.getDeployment(deploymentId!),
    enabled: !!deploymentId,
    refetchInterval: 5000,
  })
}

export function useCreateDeployment() {
  const queryClient = useQueryClient()

  return useMutation({
    mutationFn: (data: {
      name: string
      spec: ComposeSpec
      environment?: Record<string, string>
      options?: {
        namespace?: string
        environment?: 'production' | 'staging' | 'development'
        labels?: Record<string, string>
      }
    }) => hanzoNode.createDeployment(data),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['deployments'] })
    },
  })
}

export function useStopDeployment() {
  const queryClient = useQueryClient()

  return useMutation({
    mutationFn: ({ deploymentId, force }: { deploymentId: string; force?: boolean }) =>
      hanzoNode.stopDeployment(deploymentId, force),
    onSuccess: (_, { deploymentId }) => {
      queryClient.invalidateQueries({ queryKey: ['deployments'] })
      queryClient.invalidateQueries({ queryKey: ['deployment', deploymentId] })
    },
  })
}

export function useRemoveDeployment() {
  const queryClient = useQueryClient()

  return useMutation({
    mutationFn: ({ deploymentId, removeVolumes }: { deploymentId: string; removeVolumes?: boolean }) =>
      hanzoNode.removeDeployment(deploymentId, removeVolumes),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['deployments'] })
    },
  })
}

export function useScaleDeployment() {
  const queryClient = useQueryClient()

  return useMutation({
    mutationFn: ({
      deploymentId,
      serviceName,
      replicas,
    }: {
      deploymentId: string
      serviceName: string
      replicas: number
    }) => hanzoNode.scaleDeployment(deploymentId, serviceName, replicas),
    onSuccess: (_, { deploymentId }) => {
      queryClient.invalidateQueries({ queryKey: ['deployments'] })
      queryClient.invalidateQueries({ queryKey: ['deployment', deploymentId] })
    },
  })
}

export function useDeploymentLogs(
  deploymentId: string | undefined,
  options?: { service_name?: string; tail?: number }
) {
  return useQuery({
    queryKey: ['deployment', deploymentId, 'logs', options],
    queryFn: () => hanzoNode.getDeploymentLogs(deploymentId!, options),
    enabled: !!deploymentId,
    refetchInterval: 3000,
  })
}
