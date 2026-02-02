'use client'

import { useComputePools } from '@/hooks'
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card'
import { Badge } from '@/components/ui/badge'
import { Progress } from '@/components/ui/progress'
import { Button } from '@/components/ui/button'
import { formatNumber } from '@/lib/utils'
import { Cpu, Server, ChevronRight, Zap, MapPin } from 'lucide-react'
import Link from 'next/link'

export function PoolList() {
  const { data, isLoading } = useComputePools()
  const pools = data?.pools || []

  if (isLoading) {
    return (
      <div className="grid gap-4 md:grid-cols-2 lg:grid-cols-3">
        {[...Array(6)].map((_, i) => (
          <Card key={i} className="animate-pulse">
            <CardHeader className="pb-2">
              <div className="h-5 w-32 bg-muted rounded" />
            </CardHeader>
            <CardContent>
              <div className="space-y-3">
                <div className="h-4 w-48 bg-muted rounded" />
                <div className="h-4 w-24 bg-muted rounded" />
              </div>
            </CardContent>
          </Card>
        ))}
      </div>
    )
  }

  if (pools.length === 0) {
    return (
      <Card>
        <CardContent className="flex flex-col items-center justify-center py-12">
          <Server className="h-12 w-12 text-muted-foreground mb-4" />
          <h3 className="text-lg font-medium mb-2">No compute pools available</h3>
          <p className="text-sm text-muted-foreground">
            Check back later or register a new node
          </p>
        </CardContent>
      </Card>
    )
  }

  return (
    <div className="grid gap-4 md:grid-cols-2 lg:grid-cols-3">
      {pools.map((pool) => {
        const utilization = (pool.active_nodes / pool.total_nodes) * 100
        const gpuUtilization =
          pool.total_gpus > 0
            ? ((pool.total_gpus - pool.available_gpus) / pool.total_gpus) * 100
            : 0

        return (
          <Card key={pool.pool_id} className="hover:bg-accent/5 transition-colors">
            <CardHeader className="pb-2">
              <div className="flex items-center justify-between">
                <CardTitle className="text-lg">{pool.name}</CardTitle>
                <Button variant="ghost" size="icon" asChild>
                  <Link href={`/pools/${pool.pool_id}`}>
                    <ChevronRight className="h-4 w-4" />
                  </Link>
                </Button>
              </div>
              <p className="text-sm text-muted-foreground line-clamp-2">
                {pool.description}
              </p>
            </CardHeader>
            <CardContent className="space-y-4">
              <div className="flex items-center gap-2 text-sm text-muted-foreground">
                <MapPin className="h-4 w-4" />
                <span>{pool.region}</span>
              </div>

              <div className="space-y-3">
                <div className="flex items-center justify-between text-sm">
                  <div className="flex items-center gap-2">
                    <Server className="h-4 w-4 text-muted-foreground" />
                    <span>Nodes</span>
                  </div>
                  <span className="font-medium">
                    {pool.active_nodes}/{pool.total_nodes}
                  </span>
                </div>
                <Progress value={utilization} className="h-2" />
              </div>

              {pool.total_gpus > 0 && (
                <div className="space-y-3">
                  <div className="flex items-center justify-between text-sm">
                    <div className="flex items-center gap-2">
                      <Cpu className="h-4 w-4 text-muted-foreground" />
                      <span>GPUs</span>
                    </div>
                    <span className="font-medium">
                      {pool.available_gpus}/{pool.total_gpus} available
                    </span>
                  </div>
                  <Progress value={gpuUtilization} className="h-2" />
                </div>
              )}

              <div className="flex items-center justify-between pt-2 border-t">
                <div className="flex items-center gap-2">
                  <Zap className="h-4 w-4 text-yellow-500" />
                  <span className="text-sm">QoS Score</span>
                </div>
                <Badge
                  variant={
                    pool.average_qos_score >= 800
                      ? 'success'
                      : pool.average_qos_score >= 500
                      ? 'warning'
                      : 'error'
                  }
                >
                  {pool.average_qos_score}/1000
                </Badge>
              </div>

              <div className="flex items-center justify-between text-sm text-muted-foreground">
                <span>Min Stake</span>
                <span>{formatNumber(pool.min_stake)} HANZO</span>
              </div>
            </CardContent>
          </Card>
        )
      })}
    </div>
  )
}
