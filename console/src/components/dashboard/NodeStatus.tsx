'use client'

import { useNodeHealth, useNodeStatus } from '@/hooks'
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card'
import { Badge } from '@/components/ui/badge'
import { Progress } from '@/components/ui/progress'
import { formatBytes, formatUptime, formatPercent, getStatusColor } from '@/lib/utils'
import {
  Activity,
  Cpu,
  HardDrive,
  MemoryStick,
  Network,
  Server,
  Zap,
} from 'lucide-react'

export function NodeStatus() {
  const { data: health, isLoading: healthLoading } = useNodeHealth()
  const { data: status, isLoading: statusLoading } = useNodeStatus()

  if (healthLoading || statusLoading) {
    return (
      <div className="grid gap-4 md:grid-cols-2 lg:grid-cols-4">
        {[...Array(4)].map((_, i) => (
          <Card key={i} className="animate-pulse">
            <CardHeader className="pb-2">
              <div className="h-4 w-24 bg-muted rounded" />
            </CardHeader>
            <CardContent>
              <div className="h-8 w-16 bg-muted rounded" />
            </CardContent>
          </Card>
        ))}
      </div>
    )
  }

  const metrics = status?.metrics

  return (
    <div className="space-y-6">
      {/* Status Overview */}
      <div className="grid gap-4 md:grid-cols-2 lg:grid-cols-4">
        <Card>
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">Health Status</CardTitle>
            <Activity className="h-4 w-4 text-muted-foreground" />
          </CardHeader>
          <CardContent>
            <div className="flex items-center gap-2">
              <Badge
                variant={
                  health?.status === 'HEALTHY'
                    ? 'success'
                    : health?.status === 'DEGRADED'
                    ? 'warning'
                    : 'error'
                }
              >
                {health?.status || 'UNKNOWN'}
              </Badge>
              <span className="text-sm text-muted-foreground">
                v{health?.version}
              </span>
            </div>
            {health?.mlx_enabled && (
              <div className="mt-2 flex items-center gap-1 text-xs text-muted-foreground">
                <Zap className="h-3 w-3 text-yellow-500" />
                MLX Acceleration
              </div>
            )}
          </CardContent>
        </Card>

        <Card>
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">Node State</CardTitle>
            <Server className="h-4 w-4 text-muted-foreground" />
          </CardHeader>
          <CardContent>
            <div className="flex items-center gap-2">
              <Badge
                variant={
                  status?.state === 'READY'
                    ? 'success'
                    : status?.state === 'DRAINING'
                    ? 'warning'
                    : 'secondary'
                }
              >
                {status?.state || 'UNKNOWN'}
              </Badge>
            </div>
            <p className="mt-2 text-xs text-muted-foreground">
              Uptime: {formatUptime(status?.uptime_seconds || 0)}
            </p>
          </CardContent>
        </Card>

        <Card>
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">Consensus</CardTitle>
            <Network className="h-4 w-4 text-muted-foreground" />
          </CardHeader>
          <CardContent>
            <div className="flex items-center gap-2">
              <Badge variant={health?.consensus?.synced ? 'success' : 'warning'}>
                {health?.consensus?.synced ? 'Synced' : 'Syncing'}
              </Badge>
            </div>
            <p className="mt-2 text-xs text-muted-foreground">
              Block: {health?.consensus?.block_height?.toLocaleString() || '0'}
            </p>
          </CardContent>
        </Card>

        <Card>
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">Containers</CardTitle>
            <Server className="h-4 w-4 text-muted-foreground" />
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold">
              {metrics?.active_containers || 0}
            </div>
            <p className="text-xs text-muted-foreground">Active containers</p>
          </CardContent>
        </Card>
      </div>

      {/* Resource Metrics */}
      <div className="grid gap-4 md:grid-cols-3">
        <Card>
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">CPU Usage</CardTitle>
            <Cpu className="h-4 w-4 text-muted-foreground" />
          </CardHeader>
          <CardContent>
            <div className="space-y-2">
              <div className="flex items-center justify-between">
                <span className="text-2xl font-bold">
                  {formatPercent(metrics?.cpu_usage || 0)}
                </span>
              </div>
              <Progress value={(metrics?.cpu_usage || 0) * 100} />
            </div>
          </CardContent>
        </Card>

        <Card>
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">Memory Usage</CardTitle>
            <MemoryStick className="h-4 w-4 text-muted-foreground" />
          </CardHeader>
          <CardContent>
            <div className="space-y-2">
              <div className="flex items-center justify-between">
                <span className="text-2xl font-bold">
                  {formatPercent(metrics?.memory_usage || 0)}
                </span>
              </div>
              <Progress value={(metrics?.memory_usage || 0) * 100} />
            </div>
          </CardContent>
        </Card>

        <Card>
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">Disk Usage</CardTitle>
            <HardDrive className="h-4 w-4 text-muted-foreground" />
          </CardHeader>
          <CardContent>
            <div className="space-y-2">
              <div className="flex items-center justify-between">
                <span className="text-2xl font-bold">
                  {formatPercent(metrics?.disk_usage || 0)}
                </span>
              </div>
              <Progress value={(metrics?.disk_usage || 0) * 100} />
            </div>
          </CardContent>
        </Card>
      </div>

      {/* Network Stats */}
      <div className="grid gap-4 md:grid-cols-2">
        <Card>
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">Network RX</CardTitle>
            <Network className="h-4 w-4 text-green-500" />
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold">
              {formatBytes(metrics?.network_rx_bytes || 0)}/s
            </div>
            <p className="text-xs text-muted-foreground">Incoming traffic</p>
          </CardContent>
        </Card>

        <Card>
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">Network TX</CardTitle>
            <Network className="h-4 w-4 text-blue-500" />
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold">
              {formatBytes(metrics?.network_tx_bytes || 0)}/s
            </div>
            <p className="text-xs text-muted-foreground">Outgoing traffic</p>
          </CardContent>
        </Card>
      </div>

      {/* Available Models */}
      {health?.models && health.models.length > 0 && (
        <Card>
          <CardHeader>
            <CardTitle className="text-sm font-medium">Available Models</CardTitle>
          </CardHeader>
          <CardContent>
            <div className="flex flex-wrap gap-2">
              {health.models.map((model) => (
                <Badge key={model} variant="outline">
                  {model}
                </Badge>
              ))}
            </div>
          </CardContent>
        </Card>
      )}
    </div>
  )
}
