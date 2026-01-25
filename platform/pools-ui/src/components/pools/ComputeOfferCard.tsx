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
  Users,
  Shield,
  Clock,
  CheckCircle,
} from 'lucide-react';
import { cn } from '@/lib/utils';
import { formatTokenAmount, formatPercentage } from '@/lib/utils';
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
  ComputeOffer,
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
  [ResourceType.CPU]: 'text-blue-500 bg-blue-500/10 border-blue-500/20',
  [ResourceType.GPU]: 'text-purple-500 bg-purple-500/10 border-purple-500/20',
  [ResourceType.Memory]: 'text-green-500 bg-green-500/10 border-green-500/20',
  [ResourceType.Storage]: 'text-orange-500 bg-orange-500/10 border-orange-500/20',
  [ResourceType.Bandwidth]: 'text-cyan-500 bg-cyan-500/10 border-cyan-500/20',
  [ResourceType.WASM]: 'text-pink-500 bg-pink-500/10 border-pink-500/20',
  [ResourceType.Docker]: 'text-sky-500 bg-sky-500/10 border-sky-500/20',
  [ResourceType.K8S]: 'text-indigo-500 bg-indigo-500/10 border-indigo-500/20',
};

export interface ComputeOfferCardProps {
  offer: ComputeOffer;
  selected?: boolean;
  onSelect?: (offer: ComputeOffer) => void;
  className?: string;
  recommended?: boolean;
}

export function ComputeOfferCard({
  offer,
  selected = false,
  onSelect,
  className,
  recommended = false,
}: ComputeOfferCardProps) {
  const Icon = ResourceIcons[offer.resourceType];
  const colorClass = ResourceColors[offer.resourceType];
  const label = ResourceTypeLabels[offer.resourceType];
  const unit = ResourceTypeUnits[offer.resourceType];

  const priceFormatted = formatTokenAmount(offer.pricePerUnit, 18, 4);
  const availabilityVariant =
    offer.availability > 80
      ? 'success'
      : offer.availability > 50
      ? 'warning'
      : 'danger';

  const slaVariant =
    offer.avgSlaScore >= 98
      ? 'success'
      : offer.avgSlaScore >= 95
      ? 'warning'
      : 'danger';

  return (
    <Card
      className={cn(
        'relative cursor-pointer transition-all duration-200',
        'hover:shadow-md',
        selected && 'ring-2 ring-primary',
        className
      )}
      onClick={() => onSelect?.(offer)}
    >
      {/* Recommended Badge */}
      {recommended && (
        <div className="absolute -top-2 left-4">
          <Badge variant="default" className="gap-1">
            <CheckCircle className="h-3 w-3" />
            Recommended
          </Badge>
        </div>
      )}

      {/* Selection Indicator */}
      {selected && (
        <div className="absolute right-3 top-3">
          <div className="flex h-5 w-5 items-center justify-center rounded-full bg-primary text-primary-foreground">
            <CheckCircle className="h-3 w-3" />
          </div>
        </div>
      )}

      <CardHeader className="pb-2">
        <div className="flex items-center gap-3">
          <div className={cn('rounded-lg border p-2', colorClass)}>
            <Icon className="h-5 w-5" />
          </div>
          <div>
            <h3 className="font-semibold">{label} Pool</h3>
            <p className="text-sm text-muted-foreground">
              {offer.totalProviders} providers
            </p>
          </div>
        </div>
      </CardHeader>

      <CardContent className="space-y-4">
        {/* Price */}
        <div className="space-y-1">
          <p className="text-sm text-muted-foreground">Price</p>
          <div className="flex items-baseline gap-1">
            <span className="text-xl font-bold">{priceFormatted}</span>
            <span className="text-sm text-muted-foreground">
              HANZO/{unit}/hr
            </span>
          </div>
        </div>

        {/* Stats */}
        <div className="grid grid-cols-2 gap-4">
          {/* Availability */}
          <div className="space-y-1">
            <div className="flex items-center gap-1 text-sm text-muted-foreground">
              <Clock className="h-3 w-3" />
              Availability
            </div>
            <p className="font-medium">
              {offer.availability.toFixed(0)}%
            </p>
            <Progress
              value={offer.availability}
              variant={availabilityVariant}
              className="h-1"
            />
          </div>

          {/* SLA Score */}
          <div className="space-y-1">
            <div className="flex items-center gap-1 text-sm text-muted-foreground">
              <Shield className="h-3 w-3" />
              Avg SLA
            </div>
            <p className="font-medium">
              {offer.avgSlaScore.toFixed(1)}%
            </p>
            <Progress
              value={offer.avgSlaScore}
              variant={slaVariant}
              className="h-1"
            />
          </div>
        </div>

        {/* Provider Count */}
        <div className="flex items-center gap-2 text-sm text-muted-foreground">
          <Users className="h-4 w-4" />
          <span>
            {offer.totalProviders} active provider{offer.totalProviders !== 1 && 's'}
          </span>
        </div>
      </CardContent>

      <CardFooter>
        <Button
          variant={selected ? 'default' : 'outline'}
          className="w-full"
          onClick={(e) => {
            e.stopPropagation();
            onSelect?.(offer);
          }}
        >
          {selected ? 'Selected' : 'Select Pool'}
        </Button>
      </CardFooter>
    </Card>
  );
}

/** Grid of compute offers with selection */
export interface ComputeOfferGridProps {
  offers: ComputeOffer[];
  selectedPoolId?: string;
  onSelect?: (offer: ComputeOffer) => void;
  className?: string;
}

export function ComputeOfferGrid({
  offers,
  selectedPoolId,
  onSelect,
  className,
}: ComputeOfferGridProps) {
  // Sort offers by price and availability for recommendation
  const sortedOffers = React.useMemo(() => {
    return [...offers].sort((a, b) => {
      // Recommend based on balance of price and SLA
      const scoreA = (a.avgSlaScore / 100) * (a.availability / 100);
      const scoreB = (b.avgSlaScore / 100) * (b.availability / 100);
      return scoreB - scoreA;
    });
  }, [offers]);

  const recommendedId = sortedOffers[0]?.poolId;

  return (
    <div className={cn('grid gap-4 sm:grid-cols-2 lg:grid-cols-3', className)}>
      {sortedOffers.map((offer) => (
        <ComputeOfferCard
          key={offer.poolId}
          offer={offer}
          selected={selectedPoolId === offer.poolId}
          onSelect={onSelect}
          recommended={offer.poolId === recommendedId}
        />
      ))}
    </div>
  );
}

export default ComputeOfferCard;
