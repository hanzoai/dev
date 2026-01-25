/**
 * @hanzo/pools-ui
 *
 * Platform Pools UI Components for Hanzo ComputeDEX
 *
 * This package provides React components for managing compute resource pools,
 * liquidity provision, and application deployment targeting.
 *
 * @example
 * ```tsx
 * import { PoolDashboard, usePools, usePoolPositions } from '@hanzo/pools-ui';
 *
 * function App() {
 *   const { pools, loading, refetch } = usePools();
 *   const { positions, totalRewards } = usePoolPositions();
 *
 *   return (
 *     <PoolDashboard
 *       pools={pools}
 *       positions={positions}
 *       loading={loading}
 *       onRefresh={refetch}
 *     />
 *   );
 * }
 * ```
 */

// Components
export * from './components';

// Hooks
export * from './hooks';

// Types
export * from './types';

// Utilities
export {
  cn,
  formatTokenAmount,
  formatPrice,
  formatPercentage,
  formatCompact,
  formatUSD,
  calculatePrice,
  calculateSwapOutput,
  calculatePoolShare,
  calculateImpermanentLoss,
  truncateAddress,
  formatDuration,
  formatRelativeTime,
  validateSlippage,
  applySlippage,
  parseBigInt,
} from './lib/utils';
