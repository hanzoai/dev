import { describe, it, expect } from 'vitest';
import {
  cn,
  formatTokenAmount,
  formatPercentage,
  formatCompact,
  calculateSwapOutput,
  calculatePoolShare,
  calculateImpermanentLoss,
  applySlippage,
  parseBigInt,
  truncateAddress,
} from '../lib/utils';

describe('cn', () => {
  it('should merge class names', () => {
    expect(cn('foo', 'bar')).toBe('foo bar');
  });

  it('should handle conditional classes', () => {
    expect(cn('foo', false && 'bar', 'baz')).toBe('foo baz');
  });

  it('should merge tailwind classes', () => {
    expect(cn('px-2', 'px-4')).toBe('px-4');
  });
});

describe('formatTokenAmount', () => {
  it('should format bigint with decimals', () => {
    const amount = BigInt('1000000000000000000'); // 1e18 = 1.00
    expect(formatTokenAmount(amount, 18, 2)).toBe('1.00');
  });

  it('should format large amounts', () => {
    const amount = BigInt('123456789000000000000'); // 123.456789
    expect(formatTokenAmount(amount, 18, 2)).toBe('123.45');
  });

  it('should handle zero', () => {
    expect(formatTokenAmount(0n, 18, 2)).toBe('0.00');
  });
});

describe('formatPercentage', () => {
  it('should format positive percentage with sign', () => {
    expect(formatPercentage(12.5)).toBe('+12.50%');
  });

  it('should format negative percentage with sign', () => {
    expect(formatPercentage(-5.2)).toBe('-5.20%');
  });

  it('should format without sign when specified', () => {
    expect(formatPercentage(12.5, 2, false)).toBe('12.50%');
  });
});

describe('formatCompact', () => {
  it('should format thousands', () => {
    expect(formatCompact(1500)).toBe('1.50K');
  });

  it('should format millions', () => {
    expect(formatCompact(2500000)).toBe('2.50M');
  });

  it('should format billions', () => {
    expect(formatCompact(1200000000)).toBe('1.20B');
  });

  it('should not compact small numbers', () => {
    expect(formatCompact(999)).toBe('999.00');
  });
});

describe('calculateSwapOutput', () => {
  it('should calculate correct output amount with fee', () => {
    const amountIn = BigInt('1000000000000000000'); // 1
    const reserveIn = BigInt('100000000000000000000'); // 100
    const reserveOut = BigInt('100000000000000000000'); // 100

    const result = calculateSwapOutput(amountIn, reserveIn, reserveOut, 0.003);

    // Expected: ~0.987 (accounting for 0.3% fee and price impact)
    expect(result.amountOut).toBeLessThan(amountIn);
    expect(result.priceImpact).toBeGreaterThan(0);
    expect(result.priceImpact).toBeLessThan(0.02); // Should be small for this amount
  });

  it('should calculate higher price impact for larger trades', () => {
    const reserveIn = BigInt('100000000000000000000'); // 100
    const reserveOut = BigInt('100000000000000000000'); // 100

    const smallTrade = calculateSwapOutput(
      BigInt('1000000000000000000'), // 1
      reserveIn,
      reserveOut
    );

    const largeTrade = calculateSwapOutput(
      BigInt('10000000000000000000'), // 10
      reserveIn,
      reserveOut
    );

    expect(largeTrade.priceImpact).toBeGreaterThan(smallTrade.priceImpact);
  });
});

describe('calculatePoolShare', () => {
  it('should calculate correct pool share', () => {
    const userLiquidity = BigInt('100');
    const totalLiquidity = BigInt('10000');

    expect(calculatePoolShare(userLiquidity, totalLiquidity)).toBe(1);
  });

  it('should handle zero total liquidity', () => {
    expect(calculatePoolShare(100n, 0n)).toBe(0);
  });
});

describe('calculateImpermanentLoss', () => {
  it('should return 0 when price unchanged', () => {
    const il = calculateImpermanentLoss(1000n, 1000n);
    expect(Math.abs(il)).toBeLessThan(0.01);
  });

  it('should calculate IL for 2x price change', () => {
    // 2x price change should result in ~5.7% IL
    const il = calculateImpermanentLoss(1000n, 2000n);
    expect(il).toBeLessThan(0);
    expect(Math.abs(il)).toBeCloseTo(5.72, 0);
  });
});

describe('applySlippage', () => {
  it('should reduce amount by slippage percentage', () => {
    const amount = BigInt('1000000000000000000'); // 1
    const result = applySlippage(amount, 1); // 1% slippage

    // Expected: 0.99
    expect(result).toBe(BigInt('990000000000000000'));
  });
});

describe('parseBigInt', () => {
  it('should parse integer string', () => {
    expect(parseBigInt('1', 18)).toBe(BigInt('1000000000000000000'));
  });

  it('should parse decimal string', () => {
    expect(parseBigInt('1.5', 18)).toBe(BigInt('1500000000000000000'));
  });

  it('should handle small decimals', () => {
    expect(parseBigInt('0.001', 18)).toBe(BigInt('1000000000000000'));
  });
});

describe('truncateAddress', () => {
  it('should truncate ethereum address', () => {
    const address = '0x1234567890abcdef1234567890abcdef12345678';
    expect(truncateAddress(address)).toBe('0x1234...5678');
  });

  it('should handle short addresses', () => {
    const address = '0x1234';
    expect(truncateAddress(address)).toBe('0x1234');
  });
});
