import { describe, it, expect } from 'vitest';
import {
  ResourceType,
  ResourceTypeLabels,
  ResourceTypeUnits,
  poolFromDTO,
  poolToDTO,
  positionFromDTO,
  Pool,
  PoolDTO,
  LiquidityPositionDTO,
} from '../types';

describe('ResourceType', () => {
  it('should have all resource types defined', () => {
    expect(ResourceType.CPU).toBe(0);
    expect(ResourceType.GPU).toBe(1);
    expect(ResourceType.Memory).toBe(2);
    expect(ResourceType.Storage).toBe(3);
    expect(ResourceType.Bandwidth).toBe(4);
    expect(ResourceType.WASM).toBe(5);
    expect(ResourceType.Docker).toBe(6);
    expect(ResourceType.K8S).toBe(7);
  });

  it('should have labels for all resource types', () => {
    Object.values(ResourceType)
      .filter((v) => typeof v === 'number')
      .forEach((type) => {
        expect(ResourceTypeLabels[type as ResourceType]).toBeDefined();
      });
  });

  it('should have units for all resource types', () => {
    Object.values(ResourceType)
      .filter((v) => typeof v === 'number')
      .forEach((type) => {
        expect(ResourceTypeUnits[type as ResourceType]).toBeDefined();
      });
  });
});

describe('poolFromDTO', () => {
  it('should convert PoolDTO to Pool', () => {
    const dto: PoolDTO = {
      id: 'gpu-pool',
      resourceType: ResourceType.GPU,
      resourceAmount: '1000000000000000000',
      tokenAmount: '5000000000000000000',
      totalLiquidity: '3000000000000000000',
      feeRate: 0.003,
      utilization: 75.5,
      lastPrice: '1234567890000000000',
      volume24h: '50000000000000000000',
      priceChange24h: 2.5,
      apr: 18.7,
      createdAt: '2024-01-01T00:00:00Z',
      updatedAt: '2024-01-15T12:00:00Z',
    };

    const pool = poolFromDTO(dto);

    expect(pool.id).toBe('gpu-pool');
    expect(pool.resourceType).toBe(ResourceType.GPU);
    expect(pool.resourceAmount).toBe(BigInt('1000000000000000000'));
    expect(pool.tokenAmount).toBe(BigInt('5000000000000000000'));
    expect(pool.totalLiquidity).toBe(BigInt('3000000000000000000'));
    expect(pool.feeRate).toBe(0.003);
    expect(pool.utilization).toBe(75.5);
    expect(pool.lastPrice).toBe(BigInt('1234567890000000000'));
    expect(pool.volume24h).toBe(BigInt('50000000000000000000'));
    expect(pool.priceChange24h).toBe(2.5);
    expect(pool.apr).toBe(18.7);
    expect(pool.createdAt).toBeInstanceOf(Date);
    expect(pool.updatedAt).toBeInstanceOf(Date);
  });
});

describe('poolToDTO', () => {
  it('should convert Pool to PoolDTO', () => {
    const pool: Pool = {
      id: 'cpu-pool',
      resourceType: ResourceType.CPU,
      resourceAmount: BigInt('2000000000000000000'),
      tokenAmount: BigInt('4000000000000000000'),
      totalLiquidity: BigInt('2500000000000000000'),
      feeRate: 0.003,
      utilization: 60,
      lastPrice: BigInt('890000000000000000'),
      volume24h: BigInt('30000000000000000000'),
      priceChange24h: -1.2,
      apr: 15.3,
      createdAt: new Date('2024-01-01T00:00:00Z'),
      updatedAt: new Date('2024-01-15T12:00:00Z'),
    };

    const dto = poolToDTO(pool);

    expect(dto.id).toBe('cpu-pool');
    expect(dto.resourceType).toBe(ResourceType.CPU);
    expect(dto.resourceAmount).toBe('2000000000000000000');
    expect(dto.tokenAmount).toBe('4000000000000000000');
    expect(dto.totalLiquidity).toBe('2500000000000000000');
    expect(dto.feeRate).toBe(0.003);
    expect(dto.utilization).toBe(60);
    expect(dto.lastPrice).toBe('890000000000000000');
    expect(dto.volume24h).toBe('30000000000000000000');
    expect(dto.priceChange24h).toBe(-1.2);
    expect(dto.apr).toBe(15.3);
    expect(dto.createdAt).toBe('2024-01-01T00:00:00.000Z');
    expect(dto.updatedAt).toBe('2024-01-15T12:00:00.000Z');
  });
});

describe('positionFromDTO', () => {
  it('should convert LiquidityPositionDTO to LiquidityPosition', () => {
    const dto: LiquidityPositionDTO = {
      poolId: 'memory-pool',
      resourceType: ResourceType.Memory,
      liquidityShares: '500000000000000000',
      resourceAmount: '1000000000000000000',
      tokenAmount: '2000000000000000000',
      pendingRewards: '100000000000000000',
      entryPrice: '800000000000000000',
      impermanentLoss: -1.5,
      createdAt: '2024-01-10T08:00:00Z',
    };

    const position = positionFromDTO(dto);

    expect(position.poolId).toBe('memory-pool');
    expect(position.resourceType).toBe(ResourceType.Memory);
    expect(position.liquidityShares).toBe(BigInt('500000000000000000'));
    expect(position.resourceAmount).toBe(BigInt('1000000000000000000'));
    expect(position.tokenAmount).toBe(BigInt('2000000000000000000'));
    expect(position.pendingRewards).toBe(BigInt('100000000000000000'));
    expect(position.entryPrice).toBe(BigInt('800000000000000000'));
    expect(position.impermanentLoss).toBe(-1.5);
    expect(position.createdAt).toBeInstanceOf(Date);
  });
});
