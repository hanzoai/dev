import { NodeStatus } from '@/components/dashboard/NodeStatus'

export default function DashboardPage() {
  return (
    <div className="space-y-6">
      <div>
        <h1 className="text-3xl font-bold tracking-tight">Dashboard</h1>
        <p className="text-muted-foreground">
          Monitor your Hanzo Node status and performance
        </p>
      </div>
      <NodeStatus />
    </div>
  )
}
