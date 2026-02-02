'use client'

import { useState } from 'react'
import { useDeployments, useStopDeployment, useRemoveDeployment } from '@/hooks'
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card'
import { Badge } from '@/components/ui/badge'
import { Button } from '@/components/ui/button'
import { formatRelativeTime } from '@/lib/utils'
import {
  Play,
  Square,
  Trash2,
  RefreshCw,
  ChevronRight,
  Container,
  AlertCircle,
} from 'lucide-react'
import Link from 'next/link'

export function DeploymentList() {
  const [filter, setFilter] = useState<string | undefined>()
  const { data, isLoading, refetch } = useDeployments({ status: filter })
  const stopMutation = useStopDeployment()
  const removeMutation = useRemoveDeployment()

  const deployments = data?.deployments || []

  const statusVariant = (status: string) => {
    switch (status) {
      case 'running':
        return 'success'
      case 'pending':
        return 'warning'
      case 'stopped':
        return 'secondary'
      case 'failed':
        return 'error'
      default:
        return 'outline'
    }
  }

  return (
    <div className="space-y-4">
      <div className="flex items-center justify-between">
        <div className="flex gap-2">
          <Button
            variant={filter === undefined ? 'default' : 'outline'}
            size="sm"
            onClick={() => setFilter(undefined)}
          >
            All
          </Button>
          <Button
            variant={filter === 'running' ? 'default' : 'outline'}
            size="sm"
            onClick={() => setFilter('running')}
          >
            Running
          </Button>
          <Button
            variant={filter === 'stopped' ? 'default' : 'outline'}
            size="sm"
            onClick={() => setFilter('stopped')}
          >
            Stopped
          </Button>
          <Button
            variant={filter === 'failed' ? 'default' : 'outline'}
            size="sm"
            onClick={() => setFilter('failed')}
          >
            Failed
          </Button>
        </div>
        <Button variant="outline" size="sm" onClick={() => refetch()}>
          <RefreshCw className="h-4 w-4 mr-2" />
          Refresh
        </Button>
      </div>

      {isLoading ? (
        <div className="space-y-4">
          {[...Array(3)].map((_, i) => (
            <Card key={i} className="animate-pulse">
              <CardHeader className="pb-2">
                <div className="h-5 w-32 bg-muted rounded" />
              </CardHeader>
              <CardContent>
                <div className="h-4 w-48 bg-muted rounded" />
              </CardContent>
            </Card>
          ))}
        </div>
      ) : deployments.length === 0 ? (
        <Card>
          <CardContent className="flex flex-col items-center justify-center py-12">
            <Container className="h-12 w-12 text-muted-foreground mb-4" />
            <h3 className="text-lg font-medium mb-2">No deployments found</h3>
            <p className="text-sm text-muted-foreground mb-4">
              {filter ? `No ${filter} deployments` : 'Create your first deployment to get started'}
            </p>
            <Button asChild>
              <Link href="/deployments/new">Create Deployment</Link>
            </Button>
          </CardContent>
        </Card>
      ) : (
        <div className="space-y-4">
          {deployments.map((deployment) => (
            <Card key={deployment.deployment_id} className="hover:bg-accent/5 transition-colors">
              <CardHeader className="pb-2">
                <div className="flex items-center justify-between">
                  <div className="flex items-center gap-3">
                    <CardTitle className="text-lg">{deployment.name}</CardTitle>
                    <Badge variant={statusVariant(deployment.status)}>
                      {deployment.status}
                    </Badge>
                  </div>
                  <div className="flex items-center gap-2">
                    {deployment.status === 'running' && (
                      <Button
                        variant="ghost"
                        size="icon"
                        onClick={() =>
                          stopMutation.mutate({ deploymentId: deployment.deployment_id })
                        }
                        disabled={stopMutation.isPending}
                      >
                        <Square className="h-4 w-4" />
                      </Button>
                    )}
                    {deployment.status === 'stopped' && (
                      <Button variant="ghost" size="icon">
                        <Play className="h-4 w-4" />
                      </Button>
                    )}
                    <Button
                      variant="ghost"
                      size="icon"
                      className="text-destructive"
                      onClick={() =>
                        removeMutation.mutate({ deploymentId: deployment.deployment_id })
                      }
                      disabled={removeMutation.isPending}
                    >
                      <Trash2 className="h-4 w-4" />
                    </Button>
                    <Button variant="ghost" size="icon" asChild>
                      <Link href={`/deployments/${deployment.deployment_id}`}>
                        <ChevronRight className="h-4 w-4" />
                      </Link>
                    </Button>
                  </div>
                </div>
              </CardHeader>
              <CardContent>
                <div className="flex items-center gap-6 text-sm text-muted-foreground">
                  <div className="flex items-center gap-1">
                    <Container className="h-4 w-4" />
                    <span>
                      {deployment.ready_replicas}/{deployment.replicas} replicas
                    </span>
                  </div>
                  <div>ID: {deployment.deployment_id.slice(0, 8)}...</div>
                </div>
                {deployment.status === 'failed' && (
                  <div className="mt-2 flex items-center gap-2 text-sm text-destructive">
                    <AlertCircle className="h-4 w-4" />
                    Deployment failed - check logs for details
                  </div>
                )}
              </CardContent>
            </Card>
          ))}
        </div>
      )}
    </div>
  )
}
