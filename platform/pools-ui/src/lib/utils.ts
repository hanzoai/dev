import { clsx, type ClassValue } from 'clsx';
import { twMerge } from 'tailwind-merge';

/** Merge Tailwind CSS classes */
export function cn(...inputs: ClassValue[]) {
  return twMerge(clsx(inputs));
}

/** Format bigint as currency with proper decimals */
export function formatTokenAmount(
  amount: bigint,
  decimals: number = 18,
  displayDecimals: number = 2
): string {
  const divisor = BigInt(10 ** decimals);
  const integerPart = amount / divisor;
  const fractionalPart = amount % divisor;

  const fractionalStr = fractionalPart.toString().padStart(decimals, '0');
  const displayFraction = fractionalStr.slice(0, displayDecimals);

  return `${integerPart.toLocaleString()}.${displayFraction}`;
}

/** Format bigint price per unit */
export function formatPrice(
  price: bigint,
  decimals: number = 18,
  displayDecimals: number = 2
): string {
  return formatTokenAmount(price, decimals, displayDecimals);
}

/** Format percentage with sign */
export function formatPercentage(
  value: number,
  decimals: number = 2,
  showSign: boolean = true
): string {
  const formatted = Math.abs(value).toFixed(decimals);
  const sign = value >= 0 ? '+' : '-';
  return showSign ? `${sign}${formatted}%` : `${formatted}%`;
}

/** Format large numbers with K, M, B suffixes */
export function formatCompact(value: number): string {
  if (value >= 1_000_000_000) {
    return `${(value / 1_000_000_000).toFixed(2)}B`;
  }
  if (value >= 1_000_000) {
    return `${(value / 1_000_000).toFixed(2)}M`;
  }
  if (value >= 1_000) {
    return `${(value / 1_000).toFixed(2)}K`;
  }
  return value.toFixed(2);
}

/** Format bigint to compact USD string */
export function formatUSD(amount: bigint, decimals: number = 18): string {
  const divisor = BigInt(10 ** decimals);
  const value = Number(amount) / Number(divisor);
  return `$${formatCompact(value)}`;
}

/** Calculate price from pool reserves */
export function calculatePrice(
  tokenAmount: bigint,
  resourceAmount: bigint,
  tokenDecimals: number = 18,
  resourceDecimals: number = 18
): bigint {
  if (resourceAmount === 0n) return 0n;
  const scaleFactor = BigInt(10 ** tokenDecimals);
  return (tokenAmount * scaleFactor) / resourceAmount;
}

/** Calculate swap output using constant product formula */
export function calculateSwapOutput(
  amountIn: bigint,
  reserveIn: bigint,
  reserveOut: bigint,
  feeRate: number = 0.003
): { amountOut: bigint; priceImpact: number } {
  const feeMultiplier = BigInt(Math.floor((1 - feeRate) * 10000));
  const amountInWithFee = amountIn * feeMultiplier;
  const numerator = amountInWithFee * reserveOut;
  const denominator = reserveIn * 10000n + amountInWithFee;
  const amountOut = numerator / denominator;

  // Calculate price impact
  const spotPrice = Number(reserveOut) / Number(reserveIn);
  const executionPrice = Number(amountOut) / Number(amountIn);
  const priceImpact = Math.abs((executionPrice - spotPrice) / spotPrice);

  return { amountOut, priceImpact };
}

/** Calculate liquidity share percentage */
export function calculatePoolShare(
  userLiquidity: bigint,
  totalLiquidity: bigint
): number {
  if (totalLiquidity === 0n) return 0;
  return (Number(userLiquidity) / Number(totalLiquidity)) * 100;
}

/** Calculate impermanent loss */
export function calculateImpermanentLoss(
  initialPrice: bigint,
  currentPrice: bigint
): number {
  const priceRatio = Number(currentPrice) / Number(initialPrice);
  const sqrtRatio = Math.sqrt(priceRatio);
  const il = (2 * sqrtRatio) / (1 + priceRatio) - 1;
  return il * 100;
}

/** Truncate address for display */
export function truncateAddress(address: string, chars: number = 4): string {
  if (address.length <= chars * 2 + 2) return address;
  return `${address.slice(0, chars + 2)}...${address.slice(-chars)}`;
}

/** Format duration in seconds to human readable */
export function formatDuration(seconds: number): string {
  if (seconds < 60) return `${seconds}s`;
  if (seconds < 3600) return `${Math.floor(seconds / 60)}m`;
  if (seconds < 86400) return `${Math.floor(seconds / 3600)}h`;
  return `${Math.floor(seconds / 86400)}d`;
}

/** Format date relative to now */
export function formatRelativeTime(date: Date): string {
  const now = new Date();
  const diffMs = now.getTime() - date.getTime();
  const diffSecs = Math.floor(diffMs / 1000);

  if (diffSecs < 60) return 'just now';
  if (diffSecs < 3600) return `${Math.floor(diffSecs / 60)}m ago`;
  if (diffSecs < 86400) return `${Math.floor(diffSecs / 3600)}h ago`;
  return `${Math.floor(diffSecs / 86400)}d ago`;
}

/** Validate slippage tolerance */
export function validateSlippage(slippage: number): boolean {
  return slippage > 0 && slippage <= 50;
}

/** Calculate minimum output with slippage */
export function applySlippage(amount: bigint, slippagePercent: number): bigint {
  const slippageBps = BigInt(Math.floor(slippagePercent * 100));
  return (amount * (10000n - slippageBps)) / 10000n;
}

/** Parse string to bigint safely */
export function parseBigInt(value: string, decimals: number = 18): bigint {
  const parts = value.split('.');
  const integerPart = parts[0] || '0';
  let fractionalPart = parts[1] || '';

  // Pad or truncate fractional part
  fractionalPart = fractionalPart.padEnd(decimals, '0').slice(0, decimals);

  return BigInt(integerPart + fractionalPart);
}
