import * as React from 'react';
import { Search, Grid3X3, List, ArrowUpDown, RefreshCw } from 'lucide-react';
import { cn } from '@/lib/utils';
import { Input } from '@/components/ui/input';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import { PoolCard } from './PoolCard';
import {
  Pool,
  ResourceType,
  ResourceTypeLabels,
} from '@/types';

type SortField = 'price' | 'liquidity' | 'volume' | 'apr' | 'utilization';
type SortDirection = 'asc' | 'desc';
type ViewMode = 'grid' | 'list';

export interface PoolListProps {
  pools: Pool[];
  onTrade?: (pool: Pool) => void;
  onAddLiquidity?: (pool: Pool) => void;
  onViewDetails?: (pool: Pool) => void;
  onRefresh?: () => void;
  loading?: boolean;
  className?: string;
}

export function PoolList({
  pools,
  onTrade,
  onAddLiquidity,
  onViewDetails,
  onRefresh,
  loading = false,
  className,
}: PoolListProps) {
  const [searchQuery, setSearchQuery] = React.useState('');
  const [selectedTypes, setSelectedTypes] = React.useState<Set<ResourceType>>(new Set());
  const [sortField, setSortField] = React.useState<SortField>('liquidity');
  const [sortDirection, setSortDirection] = React.useState<SortDirection>('desc');
  const [viewMode, setViewMode] = React.useState<ViewMode>('grid');

  // Filter pools
  const filteredPools = React.useMemo(() => {
    let result = pools;

    // Filter by search query
    if (searchQuery) {
      const query = searchQuery.toLowerCase();
      result = result.filter((pool) => {
        const label = ResourceTypeLabels[pool.resourceType].toLowerCase();
        return label.includes(query) || pool.id.toLowerCase().includes(query);
      });
    }

    // Filter by selected types
    if (selectedTypes.size > 0) {
      result = result.filter((pool) => selectedTypes.has(pool.resourceType));
    }

    // Sort
    result = [...result].sort((a, b) => {
      let comparison = 0;
      switch (sortField) {
        case 'price':
          comparison = Number(a.lastPrice - b.lastPrice);
          break;
        case 'liquidity':
          comparison = Number(a.totalLiquidity - b.totalLiquidity);
          break;
        case 'volume':
          comparison = Number(a.volume24h - b.volume24h);
          break;
        case 'apr':
          comparison = a.apr - b.apr;
          break;
        case 'utilization':
          comparison = a.utilization - b.utilization;
          break;
      }
      return sortDirection === 'desc' ? -comparison : comparison;
    });

    return result;
  }, [pools, searchQuery, selectedTypes, sortField, sortDirection]);

  const toggleResourceType = (type: ResourceType) => {
    const newSet = new Set(selectedTypes);
    if (newSet.has(type)) {
      newSet.delete(type);
    } else {
      newSet.add(type);
    }
    setSelectedTypes(newSet);
  };

  const toggleSort = (field: SortField) => {
    if (sortField === field) {
      setSortDirection((d) => (d === 'asc' ? 'desc' : 'asc'));
    } else {
      setSortField(field);
      setSortDirection('desc');
    }
  };

  const availableTypes = React.useMemo(() => {
    const types = new Set<ResourceType>();
    pools.forEach((pool) => types.add(pool.resourceType));
    return Array.from(types).sort();
  }, [pools]);

  return (
    <div className={cn('space-y-4', className)}>
      {/* Header Controls */}
      <div className="flex flex-col gap-4 sm:flex-row sm:items-center sm:justify-between">
        {/* Search */}
        <div className="relative w-full sm:w-64">
          <Input
            placeholder="Search pools..."
            value={searchQuery}
            onChange={(e) => setSearchQuery(e.target.value)}
            leftElement={<Search className="h-4 w-4 text-muted-foreground" />}
          />
        </div>

        {/* Controls */}
        <div className="flex items-center gap-2">
          {/* Refresh Button */}
          {onRefresh && (
            <Button
              variant="outline"
              size="icon"
              onClick={onRefresh}
              disabled={loading}
              aria-label="Refresh pools"
            >
              <RefreshCw className={cn('h-4 w-4', loading && 'animate-spin')} />
            </Button>
          )}

          {/* Sort Dropdown */}
          <div className="flex items-center gap-1 rounded-md border p-1">
            {(['liquidity', 'volume', 'apr'] as SortField[]).map((field) => (
              <Button
                key={field}
                variant={sortField === field ? 'secondary' : 'ghost'}
                size="sm"
                className="h-7 px-2 text-xs"
                onClick={() => toggleSort(field)}
              >
                {field.charAt(0).toUpperCase() + field.slice(1)}
                {sortField === field && (
                  <ArrowUpDown className="ml-1 h-3 w-3" />
                )}
              </Button>
            ))}
          </div>

          {/* View Mode Toggle */}
          <div className="flex items-center gap-1 rounded-md border p-1">
            <Button
              variant={viewMode === 'grid' ? 'secondary' : 'ghost'}
              size="icon"
              className="h-7 w-7"
              onClick={() => setViewMode('grid')}
              aria-label="Grid view"
            >
              <Grid3X3 className="h-4 w-4" />
            </Button>
            <Button
              variant={viewMode === 'list' ? 'secondary' : 'ghost'}
              size="icon"
              className="h-7 w-7"
              onClick={() => setViewMode('list')}
              aria-label="List view"
            >
              <List className="h-4 w-4" />
            </Button>
          </div>
        </div>
      </div>

      {/* Type Filters */}
      <div className="flex flex-wrap gap-2">
        {availableTypes.map((type) => (
          <Badge
            key={type}
            variant={selectedTypes.has(type) ? 'default' : 'outline'}
            className="cursor-pointer"
            onClick={() => toggleResourceType(type)}
          >
            {ResourceTypeLabels[type]}
          </Badge>
        ))}
        {selectedTypes.size > 0 && (
          <Button
            variant="ghost"
            size="sm"
            className="h-6 px-2 text-xs"
            onClick={() => setSelectedTypes(new Set())}
          >
            Clear filters
          </Button>
        )}
      </div>

      {/* Results Count */}
      <p className="text-sm text-muted-foreground">
        Showing {filteredPools.length} of {pools.length} pools
      </p>

      {/* Pool Grid/List */}
      {loading ? (
        <div className="grid gap-4 sm:grid-cols-2 lg:grid-cols-3 xl:grid-cols-4">
          {Array.from({ length: 4 }).map((_, i) => (
            <div
              key={i}
              className="h-64 animate-pulse rounded-lg border bg-muted"
            />
          ))}
        </div>
      ) : filteredPools.length === 0 ? (
        <div className="flex flex-col items-center justify-center rounded-lg border border-dashed p-8">
          <p className="text-lg font-medium">No pools found</p>
          <p className="text-sm text-muted-foreground">
            Try adjusting your search or filters
          </p>
        </div>
      ) : (
        <div
          className={cn(
            viewMode === 'grid'
              ? 'grid gap-4 sm:grid-cols-2 lg:grid-cols-3 xl:grid-cols-4'
              : 'flex flex-col gap-4'
          )}
        >
          {filteredPools.map((pool) => (
            <PoolCard
              key={pool.id}
              pool={pool}
              onTrade={onTrade}
              onAddLiquidity={onAddLiquidity}
              onViewDetails={onViewDetails}
              compact={viewMode === 'list'}
            />
          ))}
        </div>
      )}
    </div>
  );
}

export default PoolList;
