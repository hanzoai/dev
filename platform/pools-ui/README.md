# @hanzo/pools-ui

Platform Pools UI Components for Hanzo ComputeDEX integration.

## Overview

This package provides React components for managing compute resource pools, liquidity provision, and application deployment targeting. It integrates with the hanzo-node ComputeDEX smart contracts as specified in the Platform Phase 1 design.

## Installation

```bash
pnpm add @hanzo/pools-ui
```

## Components

### PoolCard

Displays individual pool information including price, liquidity, volume, APR, and utilization.

```tsx
import { PoolCard } from '@hanzo/pools-ui';

<PoolCard
  pool={pool}
  onTrade={(pool) => handleTrade(pool)}
  onAddLiquidity={(pool) => handleAddLiquidity(pool)}
  onViewDetails={(pool) => handleViewDetails(pool)}
/>
```

### PoolList

Filterable and sortable list of pools with search, type filtering, and view mode toggle.

```tsx
import { PoolList } from '@hanzo/pools-ui';

<PoolList
  pools={pools}
  loading={loading}
  onTrade={handleTrade}
  onAddLiquidity={handleAddLiquidity}
  onViewDetails={handleViewDetails}
  onRefresh={handleRefresh}
/>
```

### CreatePoolModal

Multi-step modal for creating new liquidity pools.

```tsx
import { CreatePoolModal } from '@hanzo/pools-ui';

<CreatePoolModal
  isOpen={isOpen}
  onClose={() => setIsOpen(false)}
  onSubmit={handleCreatePool}
  userResourceBalances={balances}
  userTokenBalance={tokenBalance}
  existingPools={existingPoolTypes}
/>
```

### ComputeOfferCard / ComputeOfferGrid

Display compute offers for deployment pool selection.

```tsx
import { ComputeOfferGrid } from '@hanzo/pools-ui';

<ComputeOfferGrid
  offers={offers}
  selectedPoolId={selectedPoolId}
  onSelect={(offer) => setSelectedPoolId(offer.poolId)}
/>
```

### PoolDashboard

Complete dashboard with stats, pool list, and user positions.

```tsx
import { PoolDashboard, usePools, usePoolPositions } from '@hanzo/pools-ui';

function App() {
  const { pools, loading, refetch } = usePools();
  const { positions, totalValue, totalRewards } = usePoolPositions();

  return (
    <PoolDashboard
      pools={pools}
      positions={positions}
      totalValueLocked={tvl}
      volume24h={volume}
      userTotalValue={totalValue}
      userTotalRewards={totalRewards}
      loading={loading}
      onRefresh={refetch}
      onTrade={handleTrade}
      onAddLiquidity={handleAddLiquidity}
      onRemoveLiquidity={handleRemoveLiquidity}
      onClaimRewards={handleClaimRewards}
      onCreatePool={handleCreatePool}
      onViewPoolDetails={handleViewPoolDetails}
    />
  );
}
```

## Hooks

### usePools

Fetch all pools with automatic refresh.

```tsx
const { pools, loading, error, refetch } = usePools({
  refetchInterval: 10000, // 10 seconds
});
```

### usePool

Fetch a single pool by ID.

```tsx
const { pool, loading, error, refetch } = usePool({
  poolId: 'gpu-pool',
});
```

### usePoolPositions

Fetch user's liquidity positions.

```tsx
const { positions, totalValue, totalRewards, loading } = usePoolPositions();
```

### useSwapQuote

Get swap quotes with automatic debouncing.

```tsx
const { quote, loading } = useSwapQuote({
  resourceType: ResourceType.GPU,
  isBuying: true,
  amount: 1000n,
});
```

### useAddLiquidity / useRemoveLiquidity

Mutations for liquidity operations.

```tsx
const { addLiquidity, loading } = useAddLiquidity({
  onSuccess: () => toast.success('Liquidity added!'),
});

await addLiquidity({
  resourceType: ResourceType.GPU,
  resourceAmount: 10n,
  tokenAmount: 1000n,
  slippageTolerance: 0.005,
});
```

## Types

### ResourceType

```typescript
enum ResourceType {
  CPU = 0,
  GPU = 1,
  Memory = 2,
  Storage = 3,
  Bandwidth = 4,
  WASM = 5,
  Docker = 6,
  K8S = 7,
}
```

### Pool

```typescript
interface Pool {
  id: string;
  resourceType: ResourceType;
  resourceAmount: bigint;
  tokenAmount: bigint;
  totalLiquidity: bigint;
  feeRate: number;
  utilization: number;
  lastPrice: bigint;
  volume24h: bigint;
  priceChange24h: number;
  apr: number;
  createdAt: Date;
  updatedAt: Date;
}
```

### LiquidityPosition

```typescript
interface LiquidityPosition {
  poolId: string;
  resourceType: ResourceType;
  liquidityShares: bigint;
  resourceAmount: bigint;
  tokenAmount: bigint;
  pendingRewards: bigint;
  entryPrice: bigint;
  impermanentLoss: number;
  createdAt: Date;
}
```

## Utilities

```typescript
import {
  formatTokenAmount,
  formatPercentage,
  formatUSD,
  calculateSwapOutput,
  calculatePoolShare,
  calculateImpermanentLoss,
  applySlippage,
  parseBigInt,
} from '@hanzo/pools-ui';
```

## Styling

Components use Tailwind CSS with CSS custom properties for theming. Add these CSS variables to your root:

```css
:root {
  --background: 0 0% 100%;
  --foreground: 222.2 84% 4.9%;
  --card: 0 0% 100%;
  --card-foreground: 222.2 84% 4.9%;
  --primary: 222.2 47.4% 11.2%;
  --primary-foreground: 210 40% 98%;
  --secondary: 210 40% 96.1%;
  --secondary-foreground: 222.2 47.4% 11.2%;
  --muted: 210 40% 96.1%;
  --muted-foreground: 215.4 16.3% 46.9%;
  --accent: 210 40% 96.1%;
  --accent-foreground: 222.2 47.4% 11.2%;
  --destructive: 0 84.2% 60.2%;
  --destructive-foreground: 210 40% 98%;
  --border: 214.3 31.8% 91.4%;
  --input: 214.3 31.8% 91.4%;
  --ring: 222.2 84% 4.9%;
  --radius: 0.5rem;
}
```

## Development

```bash
# Install dependencies
pnpm install

# Run tests
pnpm test

# Build
pnpm build

# Type check
pnpm type-check
```

## License

Apache-2.0
