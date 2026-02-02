import { DeploymentList } from '@/components/deployments/DeploymentList'
import { Button } from '@/components/ui/button'
import Link from 'next/link'
import { Plus } from 'lucide-react'

export default function DeploymentsPage() {
  return (
    <div className="space-y-6">
      <div className="flex items-center justify-between">
        <div>
          <h1 className="text-3xl font-bold tracking-tight">Deployments</h1>
          <p className="text-muted-foreground">
            Manage container deployments on your node
          </p>
        </div>
        <Button asChild>
          <Link href="/deployments/new">
            <Plus className="h-4 w-4 mr-2" />
            New Deployment
          </Link>
        </Button>
      </div>
      <DeploymentList />
    </div>
  )
}
