import * as React from 'react';
import {
  LiquidityPosition,
  LiquidityPositionDTO,
  positionFromDTO,
  AddLiquidityParams,
  RemoveLiquidityParams,
} from '@/types';

export interface UsePoolPositionsOptions {
  enabled?: boolean;
  refetchInterval?: number;
}

export interface UsePoolPositionsReturn {
  positions: LiquidityPosition[];
  totalValue: bigint;
  totalRewards: bigint;
  loading: boolean;
  error: Error | null;
  refetch: () => Promise<void>;
}

/**
 * Hook for fetching user's liquidity positions
 *
 * @example
 * ```tsx
 * const { positions, totalValue, totalRewards, loading } = usePoolPositions();
 * ```
 */
export function usePoolPositions({
  enabled = true,
  refetchInterval,
}: UsePoolPositionsOptions = {}): UsePoolPositionsReturn {
  const [positions, setPositions] = React.useState<LiquidityPosition[]>([]);
  const [loading, setLoading] = React.useState(false);
  const [error, setError] = React.useState<Error | null>(null);

  const fetchPositions = React.useCallback(async () => {
    if (!enabled) return;

    setLoading(true);
    setError(null);

    try {
      // This would be replaced with actual API call
      // const response = await api.pools.getUserPositions();
      // setPositions(response.map(positionFromDTO));

      console.log('[usePoolPositions] Fetching user positions');
    } catch (err) {
      setError(err instanceof Error ? err : new Error('Failed to fetch positions'));
    } finally {
      setLoading(false);
    }
  }, [enabled]);

  React.useEffect(() => {
    fetchPositions();
  }, [fetchPositions]);

  React.useEffect(() => {
    if (!refetchInterval || !enabled) return;

    const interval = setInterval(fetchPositions, refetchInterval);
    return () => clearInterval(interval);
  }, [fetchPositions, refetchInterval, enabled]);

  // Calculate totals
  const totalValue = React.useMemo(() => {
    return positions.reduce(
      (sum, pos) => sum + pos.tokenAmount + pos.resourceAmount,
      0n
    );
  }, [positions]);

  const totalRewards = React.useMemo(() => {
    return positions.reduce((sum, pos) => sum + pos.pendingRewards, 0n);
  }, [positions]);

  return {
    positions,
    totalValue,
    totalRewards,
    loading,
    error,
    refetch: fetchPositions,
  };
}

export interface UseAddLiquidityOptions {
  onSuccess?: () => void;
  onError?: (error: Error) => void;
}

export interface UseAddLiquidityReturn {
  addLiquidity: (params: AddLiquidityParams) => Promise<void>;
  loading: boolean;
  error: Error | null;
}

/**
 * Hook for adding liquidity to a pool
 *
 * @example
 * ```tsx
 * const { addLiquidity, loading, error } = useAddLiquidity({
 *   onSuccess: () => console.log('Liquidity added!'),
 * });
 *
 * await addLiquidity({
 *   resourceType: ResourceType.GPU,
 *   resourceAmount: 10n,
 *   tokenAmount: 1000n,
 *   slippageTolerance: 0.005,
 * });
 * ```
 */
export function useAddLiquidity({
  onSuccess,
  onError,
}: UseAddLiquidityOptions = {}): UseAddLiquidityReturn {
  const [loading, setLoading] = React.useState(false);
  const [error, setError] = React.useState<Error | null>(null);

  const addLiquidity = React.useCallback(
    async (params: AddLiquidityParams) => {
      setLoading(true);
      setError(null);

      try {
        // This would be replaced with actual API call
        // await api.pools.addLiquidity({
        //   resourceType: params.resourceType,
        //   resourceAmount: params.resourceAmount.toString(),
        //   tokenAmount: params.tokenAmount.toString(),
        // });

        console.log('[useAddLiquidity] Adding liquidity:', params);
        onSuccess?.();
      } catch (err) {
        const error = err instanceof Error ? err : new Error('Failed to add liquidity');
        setError(error);
        onError?.(error);
        throw error;
      } finally {
        setLoading(false);
      }
    },
    [onSuccess, onError]
  );

  return {
    addLiquidity,
    loading,
    error,
  };
}

export interface UseRemoveLiquidityOptions {
  onSuccess?: () => void;
  onError?: (error: Error) => void;
}

export interface UseRemoveLiquidityReturn {
  removeLiquidity: (params: RemoveLiquidityParams) => Promise<void>;
  loading: boolean;
  error: Error | null;
}

/**
 * Hook for removing liquidity from a pool
 *
 * @example
 * ```tsx
 * const { removeLiquidity, loading, error } = useRemoveLiquidity({
 *   onSuccess: () => console.log('Liquidity removed!'),
 * });
 *
 * await removeLiquidity({
 *   resourceType: ResourceType.GPU,
 *   liquidityAmount: 100n,
 *   minResourceAmount: 9n,
 *   minTokenAmount: 900n,
 * });
 * ```
 */
export function useRemoveLiquidity({
  onSuccess,
  onError,
}: UseRemoveLiquidityOptions = {}): UseRemoveLiquidityReturn {
  const [loading, setLoading] = React.useState(false);
  const [error, setError] = React.useState<Error | null>(null);

  const removeLiquidity = React.useCallback(
    async (params: RemoveLiquidityParams) => {
      setLoading(true);
      setError(null);

      try {
        // This would be replaced with actual API call
        // await api.pools.removeLiquidity({
        //   resourceType: params.resourceType,
        //   liquidityAmount: params.liquidityAmount.toString(),
        // });

        console.log('[useRemoveLiquidity] Removing liquidity:', params);
        onSuccess?.();
      } catch (err) {
        const error = err instanceof Error ? err : new Error('Failed to remove liquidity');
        setError(error);
        onError?.(error);
        throw error;
      } finally {
        setLoading(false);
      }
    },
    [onSuccess, onError]
  );

  return {
    removeLiquidity,
    loading,
    error,
  };
}

export interface UseClaimRewardsOptions {
  onSuccess?: () => void;
  onError?: (error: Error) => void;
}

export interface UseClaimRewardsReturn {
  claimRewards: (poolId: string) => Promise<void>;
  claimAllRewards: () => Promise<void>;
  loading: boolean;
  error: Error | null;
}

/**
 * Hook for claiming rewards from pools
 *
 * @example
 * ```tsx
 * const { claimRewards, claimAllRewards, loading } = useClaimRewards();
 *
 * await claimRewards('gpu-pool');
 * // or
 * await claimAllRewards();
 * ```
 */
export function useClaimRewards({
  onSuccess,
  onError,
}: UseClaimRewardsOptions = {}): UseClaimRewardsReturn {
  const [loading, setLoading] = React.useState(false);
  const [error, setError] = React.useState<Error | null>(null);

  const claimRewards = React.useCallback(
    async (poolId: string) => {
      setLoading(true);
      setError(null);

      try {
        // This would be replaced with actual API call
        // await api.pools.claimRewards({ poolId });

        console.log('[useClaimRewards] Claiming rewards for:', poolId);
        onSuccess?.();
      } catch (err) {
        const error = err instanceof Error ? err : new Error('Failed to claim rewards');
        setError(error);
        onError?.(error);
        throw error;
      } finally {
        setLoading(false);
      }
    },
    [onSuccess, onError]
  );

  const claimAllRewards = React.useCallback(async () => {
    setLoading(true);
    setError(null);

    try {
      // This would be replaced with actual API call
      // await api.pools.claimAllRewards();

      console.log('[useClaimRewards] Claiming all rewards');
      onSuccess?.();
    } catch (err) {
      const error = err instanceof Error ? err : new Error('Failed to claim all rewards');
      setError(error);
      onError?.(error);
      throw error;
    } finally {
      setLoading(false);
    }
  }, [onSuccess, onError]);

  return {
    claimRewards,
    claimAllRewards,
    loading,
    error,
  };
}
