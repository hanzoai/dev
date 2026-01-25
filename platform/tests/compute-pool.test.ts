/**
 * Compute Pool Creation - Integration Tests
 *
 * Tests the complete flow of creating and managing compute pools:
 * 1. Pool creation with initial liquidity
 * 2. Validation of pool parameters
 * 3. Adding liquidity by providers and LPs
 * 4. Pool membership management
 * 5. AMM pricing mechanics
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { MockPlatformService } from './src/utils/platform-service';
import {
  generatePoolCreationRequest,
  generateNodeRegistrationRequest,
} from './src/mocks/generators';
import type { PoolCreationRequest, ResourceType } from './src/types';

describe('Compute Pool Creation', () => {
  let service: MockPlatformService;

  beforeEach(() => {
    service = new MockPlatformService();
  });

  afterEach(() => {
    service.reset();
  });

  describe('Basic Pool Creation', () => {
    it('should create a new GPU compute pool', async () => {
      const request: PoolCreationRequest = {
        name: 'GPU Premium Pool',
        slug: 'gpu-premium-pool',
        description: 'High-performance GPU compute pool for AI workloads',
        resource_type: 'GPU',
        initial_resource_amount: BigInt(1000) * BigInt(10) ** BigInt(18),
        initial_token_amount: BigInt(100000) * BigInt(10) ** BigInt(18),
        fee_rate: 0.003,
        min_stake: BigInt(500) * BigInt(10) ** BigInt(18),
      };

      const pool = await service.createPool(request);

      expect(pool).toBeDefined();
      expect(pool.id).toBeDefined();
      expect(pool.name).toBe('GPU Premium Pool');
      expect(pool.slug).toBe('gpu-premium-pool');
      expect(pool.description).toBe(
        'High-performance GPU compute pool for AI workloads'
      );
      expect(pool.resource_type).toBe('GPU');
      expect(pool.resource_amount).toBe(request.initial_resource_amount);
      expect(pool.token_amount).toBe(request.initial_token_amount);
      expect(pool.fee_rate).toBe(0.003);
      expect(pool.min_stake).toBe(request.min_stake);
      expect(pool.status).toBe('active');
      expect(pool.total_liquidity).toBeGreaterThan(BigInt(0));
    });

    it('should create pools for all resource types', async () => {
      const resourceTypes: ResourceType[] = [
        'CPU',
        'GPU',
        'Memory',
        'Storage',
        'Bandwidth',
        'WASM',
        'Docker',
        'K8S',
      ];

      for (const resourceType of resourceTypes) {
        const svc = new MockPlatformService();
        const request = generatePoolCreationRequest({
          name: `${resourceType} Pool`,
          slug: `${resourceType.toLowerCase()}-pool`,
          resource_type: resourceType,
        });

        const pool = await svc.createPool(request);

        expect(pool.resource_type).toBe(resourceType);
        expect(pool.status).toBe('active');
      }
    });

    it('should calculate initial liquidity tokens correctly', async () => {
      const request: PoolCreationRequest = {
        name: 'Test Pool',
        slug: 'test-pool',
        resource_type: 'CPU',
        initial_resource_amount: BigInt(10000) * BigInt(10) ** BigInt(18),
        initial_token_amount: BigInt(10000) * BigInt(10) ** BigInt(18),
        fee_rate: 0.003,
      };

      const pool = await service.createPool(request);

      // For equal amounts, sqrt(10000 * 10000) = 10000 (scaled by 10^18)
      expect(pool.total_liquidity).toBe(
        BigInt(10000) * BigInt(10) ** BigInt(18)
      );
    });

    it('should calculate initial price correctly', async () => {
      const request: PoolCreationRequest = {
        name: 'Priced Pool',
        slug: 'priced-pool',
        resource_type: 'GPU',
        initial_resource_amount: BigInt(100) * BigInt(10) ** BigInt(18), // 100 GPU units
        initial_token_amount: BigInt(10000) * BigInt(10) ** BigInt(18), // 10000 tokens
        fee_rate: 0.003,
      };

      const pool = await service.createPool(request);

      // Price = tokens / resources = 10000 / 100 = 100 tokens per GPU unit
      expect(pool.last_price).toBe(BigInt(100) * BigInt(10) ** BigInt(18));
    });

    it('should use default fee rate when not specified', async () => {
      const request: PoolCreationRequest = {
        name: 'Default Fee Pool',
        slug: 'default-fee-pool',
        resource_type: 'Memory',
        initial_resource_amount: BigInt(1000) * BigInt(10) ** BigInt(18),
        initial_token_amount: BigInt(50000) * BigInt(10) ** BigInt(18),
      };

      const pool = await service.createPool(request);

      expect(pool.fee_rate).toBe(0.003); // Default 0.3%
    });
  });

  describe('Validation Rules', () => {
    it('should reject pool without name', async () => {
      const request = generatePoolCreationRequest({
        name: '',
      });

      await expect(service.createPool(request)).rejects.toThrow(
        'Pool name is required'
      );
    });

    it('should reject pool without slug', async () => {
      const request = generatePoolCreationRequest({
        slug: '',
      });

      await expect(service.createPool(request)).rejects.toThrow(
        'Pool slug is required'
      );
    });

    it('should reject duplicate slug', async () => {
      const request1 = generatePoolCreationRequest({
        slug: 'unique-slug',
      });
      const request2 = generatePoolCreationRequest({
        slug: 'unique-slug',
      });

      await service.createPool(request1);

      await expect(service.createPool(request2)).rejects.toThrow(
        'Pool slug already exists'
      );
    });

    it('should reject zero initial resource amount', async () => {
      const request = generatePoolCreationRequest({
        initial_resource_amount: BigInt(0),
      });

      await expect(service.createPool(request)).rejects.toThrow(
        'Initial resource amount must be positive'
      );
    });

    it('should reject zero initial token amount', async () => {
      const request = generatePoolCreationRequest({
        initial_token_amount: BigInt(0),
      });

      await expect(service.createPool(request)).rejects.toThrow(
        'Initial token amount must be positive'
      );
    });

    it('should reject invalid fee rate', async () => {
      const request1 = generatePoolCreationRequest({
        fee_rate: -0.01,
      });

      await expect(service.createPool(request1)).rejects.toThrow(
        'Fee rate must be between 0 and 1'
      );

      const request2 = generatePoolCreationRequest({
        fee_rate: 1.5,
      });

      await expect(service.createPool(request2)).rejects.toThrow(
        'Fee rate must be between 0 and 1'
      );
    });
  });

  describe('Liquidity Management', () => {
    it('should add liquidity to existing pool', async () => {
      const poolRequest = generatePoolCreationRequest({
        initial_resource_amount: BigInt(1000) * BigInt(10) ** BigInt(18),
        initial_token_amount: BigInt(100000) * BigInt(10) ** BigInt(18),
      });
      const pool = await service.createPool(poolRequest);

      const initialResourceAmount = pool.resource_amount;
      const initialTokenAmount = pool.token_amount;
      const initialLiquidity = pool.total_liquidity;

      const userAddress = '0x' + 'a'.repeat(40);
      const resourceDeposit = BigInt(100) * BigInt(10) ** BigInt(18);
      const tokenDeposit = BigInt(10000) * BigInt(10) ** BigInt(18);

      const membership = await service.addLiquidity(
        pool.id,
        userAddress,
        resourceDeposit,
        tokenDeposit
      );

      expect(membership).toBeDefined();
      expect(membership.user_address).toBe(userAddress);
      expect(membership.pool_id).toBe(pool.id);
      expect(membership.role).toBe('liquidity');
      expect(membership.resource_deposited).toBe(resourceDeposit);
      expect(membership.token_deposited).toBe(tokenDeposit);
      expect(membership.liquidity_shares).toBeGreaterThan(BigInt(0));
      expect(membership.is_active).toBe(true);

      const updatedPool = await service.getPool(pool.id);
      expect(updatedPool!.resource_amount).toBe(
        initialResourceAmount + resourceDeposit
      );
      expect(updatedPool!.token_amount).toBe(
        initialTokenAmount + tokenDeposit
      );
      expect(updatedPool!.total_liquidity).toBeGreaterThan(initialLiquidity);
    });

    it('should add provider liquidity with node reference', async () => {
      // Register a node first
      const nodeRequest = generateNodeRegistrationRequest();
      const node = await service.registerNode(nodeRequest);
      await service.activateNode(node.id);

      // Create pool
      const poolRequest = generatePoolCreationRequest();
      const pool = await service.createPool(poolRequest);

      // Add liquidity as provider
      const membership = await service.addLiquidity(
        pool.id,
        node.owner_address,
        BigInt(500) * BigInt(10) ** BigInt(18),
        BigInt(50000) * BigInt(10) ** BigInt(18),
        node.id
      );

      expect(membership.node_id).toBe(node.id);
      expect(membership.role).toBe('provider');
    });

    it('should accumulate liquidity for same user', async () => {
      const poolRequest = generatePoolCreationRequest();
      const pool = await service.createPool(poolRequest);

      const userAddress = '0x' + 'b'.repeat(40);
      const deposit1 = BigInt(100) * BigInt(10) ** BigInt(18);
      const deposit2 = BigInt(200) * BigInt(10) ** BigInt(18);

      await service.addLiquidity(pool.id, userAddress, deposit1, deposit1 * BigInt(100));

      const membership2 = await service.addLiquidity(
        pool.id,
        userAddress,
        deposit2,
        deposit2 * BigInt(100)
      );

      expect(membership2.resource_deposited).toBe(deposit1 + deposit2);
    });

    it('should reject adding liquidity to non-existent pool', async () => {
      await expect(
        service.addLiquidity(
          'non-existent-pool',
          '0x' + 'a'.repeat(40),
          BigInt(100) * BigInt(10) ** BigInt(18),
          BigInt(10000) * BigInt(10) ** BigInt(18)
        )
      ).rejects.toThrow('Pool not found');
    });
  });

  describe('Pool Retrieval', () => {
    it('should retrieve pool by ID', async () => {
      const request = generatePoolCreationRequest();
      const createdPool = await service.createPool(request);

      const retrievedPool = await service.getPool(createdPool.id);

      expect(retrievedPool).toEqual(createdPool);
    });

    it('should retrieve pool by slug', async () => {
      const request = generatePoolCreationRequest({
        slug: 'my-unique-pool',
      });
      const createdPool = await service.createPool(request);

      const retrievedPool = await service.getPoolBySlug('my-unique-pool');

      expect(retrievedPool).toEqual(createdPool);
    });

    it('should return null for non-existent pool', async () => {
      const pool = await service.getPool('non-existent-id');
      expect(pool).toBeNull();

      const poolBySlug = await service.getPoolBySlug('non-existent-slug');
      expect(poolBySlug).toBeNull();
    });

    it('should retrieve all pools', async () => {
      const requests = Array.from({ length: 5 }, (_, i) =>
        generatePoolCreationRequest({
          slug: `pool-${i}`,
        })
      );

      for (const request of requests) {
        await service.createPool(request);
      }

      const pools = await service.getPools();
      expect(pools).toHaveLength(5);
    });
  });

  describe('Membership Queries', () => {
    it('should get all memberships for a pool', async () => {
      const poolRequest = generatePoolCreationRequest();
      const pool = await service.createPool(poolRequest);

      // Add multiple LPs
      for (let i = 0; i < 5; i++) {
        await service.addLiquidity(
          pool.id,
          `0x${'0'.repeat(38)}${i.toString(16).padStart(2, '0')}`,
          BigInt(100) * BigInt(10) ** BigInt(18),
          BigInt(10000) * BigInt(10) ** BigInt(18)
        );
      }

      const memberships = await service.getPoolMemberships(pool.id);

      expect(memberships).toHaveLength(5);
      memberships.forEach((m) => {
        expect(m.pool_id).toBe(pool.id);
        expect(m.is_active).toBe(true);
      });
    });

    it('should get membership by ID', async () => {
      const poolRequest = generatePoolCreationRequest();
      const pool = await service.createPool(poolRequest);

      const userAddress = '0x' + 'c'.repeat(40);
      const membership = await service.addLiquidity(
        pool.id,
        userAddress,
        BigInt(100) * BigInt(10) ** BigInt(18),
        BigInt(10000) * BigInt(10) ** BigInt(18)
      );

      const retrieved = await service.getMembership(membership.id);

      expect(retrieved).toEqual(membership);
    });
  });

  describe('Edge Cases', () => {
    it('should handle large liquidity amounts', async () => {
      const request: PoolCreationRequest = {
        name: 'Whale Pool',
        slug: 'whale-pool',
        resource_type: 'GPU',
        initial_resource_amount: BigInt(10) ** BigInt(24), // 1 million units
        initial_token_amount: BigInt(10) ** BigInt(27), // 1 billion tokens
        fee_rate: 0.003,
      };

      const pool = await service.createPool(request);

      expect(pool.resource_amount).toBe(request.initial_resource_amount);
      expect(pool.token_amount).toBe(request.initial_token_amount);
    });

    it('should handle minimum liquidity amounts', async () => {
      const request: PoolCreationRequest = {
        name: 'Tiny Pool',
        slug: 'tiny-pool',
        resource_type: 'CPU',
        initial_resource_amount: BigInt(1), // 1 wei
        initial_token_amount: BigInt(1), // 1 wei
        fee_rate: 0.003,
      };

      const pool = await service.createPool(request);

      expect(pool.resource_amount).toBe(BigInt(1));
      expect(pool.token_amount).toBe(BigInt(1));
      expect(pool.total_liquidity).toBe(BigInt(1)); // sqrt(1*1) = 1
    });

    it('should handle zero fee rate', async () => {
      const request = generatePoolCreationRequest({
        fee_rate: 0,
      });

      const pool = await service.createPool(request);

      expect(pool.fee_rate).toBe(0);
    });

    it('should handle maximum fee rate', async () => {
      const request = generatePoolCreationRequest({
        fee_rate: 1, // 100%
      });

      const pool = await service.createPool(request);

      expect(pool.fee_rate).toBe(1);
    });

    it('should handle special characters in pool name', async () => {
      const request = generatePoolCreationRequest({
        name: 'GPU Pool #1 - High Performance (Beta)',
        slug: 'gpu-pool-1-beta',
      });

      const pool = await service.createPool(request);

      expect(pool.name).toBe('GPU Pool #1 - High Performance (Beta)');
    });

    it('should initialize metrics to zero', async () => {
      const request = generatePoolCreationRequest();
      const pool = await service.createPool(request);

      expect(pool.volume_24h).toBe(BigInt(0));
      expect(pool.volume_7d).toBe(BigInt(0));
      expect(pool.volume_total).toBe(BigInt(0));
      expect(pool.trade_count_24h).toBe(0);
      expect(pool.trade_count_total).toBe(0);
    });
  });
});
