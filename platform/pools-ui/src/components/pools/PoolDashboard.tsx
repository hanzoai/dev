import * as React from 'react';
import {
  RefreshCw,
  Plus,
  Wallet,
  TrendingUp,
  Activity,
  DollarSign,
  AlertCircle,
  ExternalLink,
  MoreVertical,
  Gift,
} from 'lucide-react';
import { cn } from '@/lib/utils';
import {
  formatTokenAmount,
  formatUSD,
  formatPercentage,
  calculatePoolShare,
} from '@/lib/utils';
import {
  Card,
  CardHeader,
  CardTitle,
  CardDescription,
  CardContent,
} from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import { Progress } from '@/components/ui/progress';
import { PoolList } from './PoolList';
import {
  Pool,
  LiquidityPosition,
  ResourceType,
  ResourceTypeLabels,
  ResourceTypeUnits,
} from '@/types';

export interface PoolDashboardProps {
  pools: Pool[];
  positions: LiquidityPosition[];
  totalValueLocked: bigint;
  volume24h: bigint;
  userTotalValue: bigint;
  userTotalRewards: bigint;
  loading?: boolean;
  onRefresh?: () => void;
  onTrade?: (pool: Pool) => void;
  onAddLiquidity?: (pool: Pool) => void;
  onRemoveLiquidity?: (position: LiquidityPosition) => void;
  onClaimRewards?: (position: LiquidityPosition) => void;
  onClaimAllRewards?: () => void;
  onCreatePool?: () => void;
  onViewPoolDetails?: (pool: Pool) => void;
  className?: string;
}

export function PoolDashboard({
  pools,
  positions,
  totalValueLocked,
  volume24h,
  userTotalValue,
  userTotalRewards,
  loading = false,
  onRefresh,
  onTrade,
  onAddLiquidity,
  onRemoveLiquidity,
  onClaimRewards,
  onClaimAllRewards,
  onCreatePool,
  onViewPoolDetails,
  className,
}: PoolDashboardProps) {
  const [activeTab, setActiveTab] = React.useState<'pools' | 'positions'>('pools');

  // Calculate aggregated stats
  const avgApr = React.useMemo(() => {
    if (pools.length === 0) return 0;
    return pools.reduce((sum, p) => sum + p.apr, 0) / pools.length;
  }, [pools]);

  const avgUtilization = React.useMemo(() => {
    if (pools.length === 0) return 0;
    return pools.reduce((sum, p) => sum + p.utilization, 0) / pools.length;
  }, [pools]);

  // Get pool for position
  const getPoolForPosition = (position: LiquidityPosition): Pool | undefined => {
    return pools.find((p) => p.id === position.poolId);
  };

  return (
    <div className={cn('space-y-6', className)}>
      {/* Header */}
      <div className="flex flex-col gap-4 sm:flex-row sm:items-center sm:justify-between">
        <div>
          <h1 className="text-2xl font-bold">Compute Pools</h1>
          <p className="text-muted-foreground">
            Trade and provide liquidity for compute resources
          </p>
        </div>
        <div className="flex items-center gap-2">
          {onRefresh && (
            <Button
              variant="outline"
              size="icon"
              onClick={onRefresh}
              disabled={loading}
              aria-label="Refresh"
            >
              <RefreshCw className={cn('h-4 w-4', loading && 'animate-spin')} />
            </Button>
          )}
          {onCreatePool && (
            <Button onClick={onCreatePool}>
              <Plus className="mr-2 h-4 w-4" />
              Create Pool
            </Button>
          )}
        </div>
      </div>

      {/* Stats Cards */}
      <div className="grid gap-4 sm:grid-cols-2 lg:grid-cols-4">
        <Card>
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">Total Value Locked</CardTitle>
            <DollarSign className="h-4 w-4 text-muted-foreground" />
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold">{formatUSD(totalValueLocked)}</div>
            <p className="text-xs text-muted-foreground">
              Across {pools.length} pools
            </p>
          </CardContent>
        </Card>

        <Card>
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">24h Volume</CardTitle>
            <Activity className="h-4 w-4 text-muted-foreground" />
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold">{formatUSD(volume24h)}</div>
            <p className="text-xs text-muted-foreground">
              Trading activity
            </p>
          </CardContent>
        </Card>

        <Card>
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">Avg APR</CardTitle>
            <TrendingUp className="h-4 w-4 text-muted-foreground" />
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold text-green-500">
              {formatPercentage(avgApr, 1, false)}
            </div>
            <p className="text-xs text-muted-foreground">
              Liquidity provider yield
            </p>
          </CardContent>
        </Card>

        <Card>
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">Avg Utilization</CardTitle>
            <AlertCircle className="h-4 w-4 text-muted-foreground" />
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold">{avgUtilization.toFixed(0)}%</div>
            <Progress
              value={avgUtilization}
              className="mt-2"
              variant={avgUtilization > 80 ? 'warning' : 'default'}
            />
          </CardContent>
        </Card>
      </div>

      {/* User Summary (if has positions) */}
      {positions.length > 0 && (
        <Card>
          <CardHeader>
            <div className="flex items-center justify-between">
              <div>
                <CardTitle className="flex items-center gap-2">
                  <Wallet className="h-5 w-5" />
                  My Portfolio
                </CardTitle>
                <CardDescription>
                  Your liquidity positions across all pools
                </CardDescription>
              </div>
              {userTotalRewards > 0n && onClaimAllRewards && (
                <Button onClick={onClaimAllRewards} variant="outline">
                  <Gift className="mr-2 h-4 w-4" />
                  Claim All ({formatTokenAmount(userTotalRewards)} HANZO)
                </Button>
              )}
            </div>
          </CardHeader>
          <CardContent>
            <div className="mb-4 grid gap-4 sm:grid-cols-3">
              <div>
                <p className="text-sm text-muted-foreground">Total Value</p>
                <p className="text-2xl font-bold">{formatUSD(userTotalValue)}</p>
              </div>
              <div>
                <p className="text-sm text-muted-foreground">Pending Rewards</p>
                <p className="text-2xl font-bold text-green-500">
                  {formatTokenAmount(userTotalRewards)} HANZO
                </p>
              </div>
              <div>
                <p className="text-sm text-muted-foreground">Active Positions</p>
                <p className="text-2xl font-bold">{positions.length}</p>
              </div>
            </div>
          </CardContent>
        </Card>
      )}

      {/* Tab Navigation */}
      <div className="flex gap-2 border-b">
        <button
          className={cn(
            'px-4 py-2 text-sm font-medium transition-colors',
            activeTab === 'pools'
              ? 'border-b-2 border-primary text-primary'
              : 'text-muted-foreground hover:text-foreground'
          )}
          onClick={() => setActiveTab('pools')}
        >
          All Pools ({pools.length})
        </button>
        <button
          className={cn(
            'px-4 py-2 text-sm font-medium transition-colors',
            activeTab === 'positions'
              ? 'border-b-2 border-primary text-primary'
              : 'text-muted-foreground hover:text-foreground'
          )}
          onClick={() => setActiveTab('positions')}
        >
          My Positions ({positions.length})
        </button>
      </div>

      {/* Tab Content */}
      {activeTab === 'pools' ? (
        <PoolList
          pools={pools}
          loading={loading}
          onTrade={onTrade}
          onAddLiquidity={onAddLiquidity}
          onViewDetails={onViewPoolDetails}
          onRefresh={onRefresh}
        />
      ) : (
        <PositionsList
          positions={positions}
          pools={pools}
          onRemoveLiquidity={onRemoveLiquidity}
          onClaimRewards={onClaimRewards}
          onViewPool={(position) => {
            const pool = getPoolForPosition(position);
            if (pool) onViewPoolDetails?.(pool);
          }}
        />
      )}
    </div>
  );
}

/** Positions list component */
interface PositionsListProps {
  positions: LiquidityPosition[];
  pools: Pool[];
  onRemoveLiquidity?: (position: LiquidityPosition) => void;
  onClaimRewards?: (position: LiquidityPosition) => void;
  onViewPool?: (position: LiquidityPosition) => void;
}

function PositionsList({
  positions,
  pools,
  onRemoveLiquidity,
  onClaimRewards,
  onViewPool,
}: PositionsListProps) {
  if (positions.length === 0) {
    return (
      <div className="flex flex-col items-center justify-center rounded-lg border border-dashed p-8">
        <Wallet className="mb-4 h-12 w-12 text-muted-foreground" />
        <p className="text-lg font-medium">No positions yet</p>
        <p className="text-sm text-muted-foreground">
          Add liquidity to a pool to start earning rewards
        </p>
      </div>
    );
  }

  const getPool = (poolId: string) => pools.find((p) => p.id === poolId);

  return (
    <div className="space-y-4">
      {positions.map((position) => {
        const pool = getPool(position.poolId);
        if (!pool) return null;

        const label = ResourceTypeLabels[position.resourceType];
        const unit = ResourceTypeUnits[position.resourceType];
        const poolShare = calculatePoolShare(
          position.liquidityShares,
          pool.totalLiquidity
        );
        const ilClass =
          position.impermanentLoss < 0 ? 'text-red-500' : 'text-green-500';

        return (
          <Card key={position.poolId} className="overflow-hidden">
            <CardContent className="p-4">
              <div className="flex flex-col gap-4 sm:flex-row sm:items-center sm:justify-between">
                {/* Pool Info */}
                <div
                  className="flex cursor-pointer items-center gap-3"
                  onClick={() => onViewPool?.(position)}
                >
                  <Badge variant="secondary" className="h-10 w-10 justify-center">
                    {label.slice(0, 3).toUpperCase()}
                  </Badge>
                  <div>
                    <h3 className="font-semibold">{label} Pool</h3>
                    <p className="text-sm text-muted-foreground">
                      {formatTokenAmount(position.tokenAmount)} HANZO +{' '}
                      {formatTokenAmount(position.resourceAmount)} {unit}
                    </p>
                  </div>
                </div>

                {/* Stats */}
                <div className="grid grid-cols-3 gap-6 text-sm sm:gap-8">
                  <div>
                    <p className="text-muted-foreground">Pool Share</p>
                    <p className="font-medium">{poolShare.toFixed(2)}%</p>
                  </div>
                  <div>
                    <p className="text-muted-foreground">Pending Rewards</p>
                    <p className="font-medium text-green-500">
                      {formatTokenAmount(position.pendingRewards)} HANZO
                    </p>
                  </div>
                  <div>
                    <p className="text-muted-foreground">IL</p>
                    <p className={cn('font-medium', ilClass)}>
                      {formatPercentage(position.impermanentLoss)}
                    </p>
                  </div>
                </div>

                {/* Actions */}
                <div className="flex gap-2">
                  {position.pendingRewards > 0n && onClaimRewards && (
                    <Button
                      variant="outline"
                      size="sm"
                      onClick={() => onClaimRewards(position)}
                    >
                      <Gift className="mr-1 h-3 w-3" />
                      Claim
                    </Button>
                  )}
                  {onRemoveLiquidity && (
                    <Button
                      variant="outline"
                      size="sm"
                      onClick={() => onRemoveLiquidity(position)}
                    >
                      Manage
                    </Button>
                  )}
                </div>
              </div>
            </CardContent>
          </Card>
        );
      })}
    </div>
  );
}

export default PoolDashboard;
