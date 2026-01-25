import * as React from 'react';
import {
  Cpu,
  HardDrive,
  Database,
  Server,
  Wifi,
  Box,
  Container,
  Cloud,
  TrendingUp,
  TrendingDown,
  AlertCircle,
} from 'lucide-react';
import { cn } from '@/lib/utils';
import {
  formatTokenAmount,
  formatPercentage,
  formatUSD,
} from '@/lib/utils';
import {
  Card,
  CardHeader,
  CardContent,
  CardFooter,
} from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import { Progress } from '@/components/ui/progress';
import {
  Pool,
  ResourceType,
  ResourceTypeLabels,
  ResourceTypeUnits,
} from '@/types';

/** Icon mapping for resource types */
const ResourceIcons: Record<ResourceType, React.ComponentType<{ className?: string }>> = {
  [ResourceType.CPU]: Cpu,
  [ResourceType.GPU]: HardDrive,
  [ResourceType.Memory]: Database,
  [ResourceType.Storage]: Server,
  [ResourceType.Bandwidth]: Wifi,
  [ResourceType.WASM]: Box,
  [ResourceType.Docker]: Container,
  [ResourceType.K8S]: Cloud,
};

/** Color classes for resource types */
const ResourceColors: Record<ResourceType, string> = {
  [ResourceType.CPU]: 'text-blue-500 bg-blue-500/10',
  [ResourceType.GPU]: 'text-purple-500 bg-purple-500/10',
  [ResourceType.Memory]: 'text-green-500 bg-green-500/10',
  [ResourceType.Storage]: 'text-orange-500 bg-orange-500/10',
  [ResourceType.Bandwidth]: 'text-cyan-500 bg-cyan-500/10',
  [ResourceType.WASM]: 'text-pink-500 bg-pink-500/10',
  [ResourceType.Docker]: 'text-sky-500 bg-sky-500/10',
  [ResourceType.K8S]: 'text-indigo-500 bg-indigo-500/10',
};

export interface PoolCardProps {
  pool: Pool;
  onTrade?: (pool: Pool) => void;
  onAddLiquidity?: (pool: Pool) => void;
  onViewDetails?: (pool: Pool) => void;
  className?: string;
  compact?: boolean;
}

export function PoolCard({
  pool,
  onTrade,
  onAddLiquidity,
  onViewDetails,
  className,
  compact = false,
}: PoolCardProps) {
  const Icon = ResourceIcons[pool.resourceType];
  const colorClass = ResourceColors[pool.resourceType];
  const label = ResourceTypeLabels[pool.resourceType];
  const unit = ResourceTypeUnits[pool.resourceType];

  const priceFormatted = formatTokenAmount(pool.lastPrice);
  const isPositiveChange = pool.priceChange24h >= 0;
  const utilizationVariant =
    pool.utilization > 90 ? 'danger' : pool.utilization > 70 ? 'warning' : 'default';

  return (
    <Card
      className={cn(
        'transition-all duration-200 hover:shadow-md',
        onViewDetails && 'cursor-pointer',
        className
      )}
      onClick={() => onViewDetails?.(pool)}
    >
      <CardHeader className="pb-2">
        <div className="flex items-start justify-between">
          <div className="flex items-center gap-3">
            <div className={cn('rounded-lg p-2', colorClass)}>
              <Icon className="h-5 w-5" />
            </div>
            <div>
              <h3 className="font-semibold">{label} Pool</h3>
              {!compact && (
                <p className="text-sm text-muted-foreground">
                  {formatTokenAmount(pool.resourceAmount, 0, 0)} {unit} available
                </p>
              )}
            </div>
          </div>
          {pool.utilization > 90 && (
            <Badge variant="warning" className="gap-1">
              <AlertCircle className="h-3 w-3" />
              High Demand
            </Badge>
          )}
        </div>
      </CardHeader>

      <CardContent className="space-y-4">
        {/* Price and Change */}
        <div>
          <div className="flex items-baseline gap-2">
            <span className="text-2xl font-bold">{priceFormatted}</span>
            <span className="text-sm text-muted-foreground">HANZO/{unit}</span>
          </div>
          <div
            className={cn(
              'flex items-center gap-1 text-sm',
              isPositiveChange ? 'text-green-500' : 'text-red-500'
            )}
          >
            {isPositiveChange ? (
              <TrendingUp className="h-4 w-4" />
            ) : (
              <TrendingDown className="h-4 w-4" />
            )}
            {formatPercentage(pool.priceChange24h)} (24h)
          </div>
        </div>

        {!compact && (
          <>
            {/* Stats Grid */}
            <div className="grid grid-cols-2 gap-4 text-sm">
              <div>
                <p className="text-muted-foreground">Liquidity</p>
                <p className="font-medium">{formatUSD(pool.totalLiquidity)}</p>
              </div>
              <div>
                <p className="text-muted-foreground">Volume 24h</p>
                <p className="font-medium">{formatUSD(pool.volume24h)}</p>
              </div>
              <div>
                <p className="text-muted-foreground">APR</p>
                <p className="font-medium text-green-500">
                  {formatPercentage(pool.apr, 1, false)}
                </p>
              </div>
              <div>
                <p className="text-muted-foreground">Fee</p>
                <p className="font-medium">
                  {formatPercentage(pool.feeRate * 100, 1, false)}
                </p>
              </div>
            </div>

            {/* Utilization Bar */}
            <div className="space-y-1">
              <div className="flex justify-between text-sm">
                <span className="text-muted-foreground">Utilization</span>
                <span className="font-medium">{pool.utilization.toFixed(0)}%</span>
              </div>
              <Progress value={pool.utilization} variant={utilizationVariant} />
            </div>
          </>
        )}
      </CardContent>

      <CardFooter className="gap-2">
        <Button
          variant="default"
          className="flex-1"
          onClick={(e) => {
            e.stopPropagation();
            onTrade?.(pool);
          }}
        >
          Trade
        </Button>
        <Button
          variant="outline"
          className="flex-1"
          onClick={(e) => {
            e.stopPropagation();
            onAddLiquidity?.(pool);
          }}
        >
          Add Liquidity
        </Button>
      </CardFooter>
    </Card>
  );
}

export default PoolCard;
