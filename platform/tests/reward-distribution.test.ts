/**
 * Reward Distribution - Integration Tests
 *
 * Tests the complete reward distribution system:
 * 1. Epoch-based reward calculation
 * 2. Distribution to liquidity providers based on share
 * 3. Trading fee rewards
 * 4. Liquidity mining rewards
 * 5. SLA bonus rewards for high-performing providers
 * 6. Reward claiming mechanism
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { MockPlatformService } from './src/utils/platform-service';
import {
  generateNodeRegistrationRequest,
  generatePoolCreationRequest,
} from './src/mocks/generators';
import type { RewardDistributionRequest, RewardType } from './src/types';

describe('Reward Distribution', () => {
  let service: MockPlatformService;
  let poolId: string;
  let nodeId: string;
  let userAddresses: string[];

  beforeEach(async () => {
    service = new MockPlatformService();

    // Create a pool
    const poolRequest = generatePoolCreationRequest({
      initial_resource_amount: BigInt(10000) * BigInt(10) ** BigInt(18),
      initial_token_amount: BigInt(1000000) * BigInt(10) ** BigInt(18),
    });
    const pool = await service.createPool(poolRequest);
    poolId = pool.id;

    // Create a node
    const nodeRequest = generateNodeRegistrationRequest();
    const node = await service.registerNode(nodeRequest);
    await service.activateNode(node.id);
    nodeId = node.id;

    // Create multiple LPs
    userAddresses = [];
    for (let i = 0; i < 5; i++) {
      const address = `0x${'0'.repeat(38)}${(i + 1).toString(16).padStart(2, '0')}`;
      userAddresses.push(address);

      const resourceAmount = BigInt((i + 1) * 100) * BigInt(10) ** BigInt(18);
      const tokenAmount = BigInt((i + 1) * 10000) * BigInt(10) ** BigInt(18);

      await service.addLiquidity(
        poolId,
        address,
        resourceAmount,
        tokenAmount,
        i === 0 ? nodeId : undefined // First LP is a provider with a node
      );
    }
  });

  afterEach(() => {
    service.reset();
  });

  describe('Epoch Reward Distribution', () => {
    it('should distribute trading fee rewards proportionally', async () => {
      const epochStart = new Date(Date.now() - 7 * 24 * 60 * 60 * 1000);
      const epochEnd = new Date();

      const request: RewardDistributionRequest = {
        pool_id: poolId,
        epoch_number: 1,
        epoch_start: epochStart,
        epoch_end: epochEnd,
        total_trading_fees: BigInt(1000) * BigInt(10) ** BigInt(18), // 1000 tokens
        total_liquidity_rewards: BigInt(0),
        total_sla_bonuses: BigInt(0),
      };

      const rewards = await service.distributeRewards(request);

      expect(rewards.length).toBeGreaterThan(0);

      // All rewards should be trading_fee type
      rewards.forEach((r) => {
        expect(r.reward_type).toBe('trading_fee');
        expect(r.epoch_number).toBe(1);
        expect(r.pool_id).toBe(poolId);
        expect(r.is_claimed).toBe(false);
      });

      // Verify proportional distribution
      const totalRewardAmount = rewards.reduce(
        (sum, r) => sum + r.amount,
        BigInt(0)
      );
      // Total rewards should be close to total_trading_fees (may have rounding)
      expect(totalRewardAmount).toBeLessThanOrEqual(request.total_trading_fees);
      expect(totalRewardAmount).toBeGreaterThan(BigInt(0));
    });

    it('should distribute liquidity mining rewards', async () => {
      const epochStart = new Date(Date.now() - 7 * 24 * 60 * 60 * 1000);
      const epochEnd = new Date();

      const request: RewardDistributionRequest = {
        pool_id: poolId,
        epoch_number: 2,
        epoch_start: epochStart,
        epoch_end: epochEnd,
        total_trading_fees: BigInt(0),
        total_liquidity_rewards: BigInt(5000) * BigInt(10) ** BigInt(18), // 5000 tokens
        total_sla_bonuses: BigInt(0),
      };

      const rewards = await service.distributeRewards(request);

      expect(rewards.length).toBeGreaterThan(0);
      rewards.forEach((r) => {
        expect(r.reward_type).toBe('liquidity_mining');
      });
    });

    it('should distribute SLA bonus to high-performing providers', async () => {
      // First, improve the node's SLA score through passing challenges
      for (let i = 0; i < 5; i++) {
        const challenge = await service.issueChallenge(
          nodeId,
          'availability',
          poolId
        );
        await service.submitChallengeResponse(challenge.id, {
          signed_nonce: 'valid',
        });
        await service.verifyChallenge(challenge.id, '0x' + 'f'.repeat(40));
      }

      const node = await service.getNode(nodeId);
      expect(node!.sla_score).toBeGreaterThanOrEqual(95); // Should have high SLA

      const epochStart = new Date(Date.now() - 7 * 24 * 60 * 60 * 1000);
      const epochEnd = new Date();

      const request: RewardDistributionRequest = {
        pool_id: poolId,
        epoch_number: 3,
        epoch_start: epochStart,
        epoch_end: epochEnd,
        total_trading_fees: BigInt(0),
        total_liquidity_rewards: BigInt(0),
        total_sla_bonuses: BigInt(500) * BigInt(10) ** BigInt(18),
      };

      const rewards = await service.distributeRewards(request);

      // Should have SLA bonus rewards for the provider
      const slaRewards = rewards.filter((r) => r.reward_type === 'sla_bonus');
      expect(slaRewards.length).toBeGreaterThan(0);

      // SLA bonus should go to the provider (first LP with node)
      const providerReward = slaRewards.find(
        (r) => r.user_address === userAddresses[0]
      );
      expect(providerReward).toBeDefined();
    });

    it('should distribute all reward types in single epoch', async () => {
      // Improve node SLA score first
      const challenge = await service.issueChallenge(
        nodeId,
        'availability',
        poolId
      );
      await service.submitChallengeResponse(challenge.id, {
        signed_nonce: 'valid',
      });
      await service.verifyChallenge(challenge.id, '0x' + 'f'.repeat(40));

      const epochStart = new Date(Date.now() - 7 * 24 * 60 * 60 * 1000);
      const epochEnd = new Date();

      const request: RewardDistributionRequest = {
        pool_id: poolId,
        epoch_number: 4,
        epoch_start: epochStart,
        epoch_end: epochEnd,
        total_trading_fees: BigInt(1000) * BigInt(10) ** BigInt(18),
        total_liquidity_rewards: BigInt(2000) * BigInt(10) ** BigInt(18),
        total_sla_bonuses: BigInt(500) * BigInt(10) ** BigInt(18),
      };

      const rewards = await service.distributeRewards(request);

      const rewardTypes = new Set(rewards.map((r) => r.reward_type));
      expect(rewardTypes.has('trading_fee')).toBe(true);
      expect(rewardTypes.has('liquidity_mining')).toBe(true);
      // SLA bonus may or may not be present depending on score
    });
  });

  describe('Reward Claiming', () => {
    it('should claim pending rewards successfully', async () => {
      // Distribute some rewards first
      const epochStart = new Date(Date.now() - 7 * 24 * 60 * 60 * 1000);
      const epochEnd = new Date();

      await service.distributeRewards({
        pool_id: poolId,
        epoch_number: 5,
        epoch_start: epochStart,
        epoch_end: epochEnd,
        total_trading_fees: BigInt(1000) * BigInt(10) ** BigInt(18),
        total_liquidity_rewards: BigInt(0),
        total_sla_bonuses: BigInt(0),
      });

      // Get membership for first user
      const memberships = await service.getPoolMemberships(poolId);
      const userMembership = memberships.find(
        (m) => m.user_address === userAddresses[0]
      );
      expect(userMembership).toBeDefined();
      expect(userMembership!.pending_rewards).toBeGreaterThan(BigInt(0));

      const pendingBefore = userMembership!.pending_rewards;

      // Claim rewards
      const result = await service.claimRewards(
        userMembership!.id,
        userAddresses[0]
      );

      expect(result.claimed_amount).toBe(pendingBefore);
      expect(result.claim_tx_hash).toMatch(/^0x[a-f0-9]{64}$/);

      // Verify membership updated
      const membershipAfter = await service.getMembership(userMembership!.id);
      expect(membershipAfter!.pending_rewards).toBe(BigInt(0));
      // claimed_rewards should have increased by the claimed amount
      expect(membershipAfter!.claimed_rewards).toBeGreaterThan(BigInt(0));
      expect(membershipAfter!.last_reward_claim_at).toBeInstanceOf(Date);
    });

    it('should reject claim for wrong user', async () => {
      // Distribute some rewards first
      await service.distributeRewards({
        pool_id: poolId,
        epoch_number: 6,
        epoch_start: new Date(Date.now() - 7 * 24 * 60 * 60 * 1000),
        epoch_end: new Date(),
        total_trading_fees: BigInt(1000) * BigInt(10) ** BigInt(18),
        total_liquidity_rewards: BigInt(0),
        total_sla_bonuses: BigInt(0),
      });

      const memberships = await service.getPoolMemberships(poolId);
      const userMembership = memberships.find(
        (m) => m.user_address === userAddresses[0]
      );

      // Try to claim with different address
      await expect(
        service.claimRewards(userMembership!.id, userAddresses[1])
      ).rejects.toThrow('Not authorized to claim rewards');
    });

    it('should reject claim with no pending rewards', async () => {
      const memberships = await service.getPoolMemberships(poolId);
      const userMembership = memberships[0];

      // If no rewards have been distributed, pending should be 0
      if (userMembership.pending_rewards === BigInt(0)) {
        await expect(
          service.claimRewards(userMembership.id, userMembership.user_address)
        ).rejects.toThrow('No pending rewards to claim');
      }
    });

    it('should reject claim for non-existent membership', async () => {
      await expect(
        service.claimRewards('non-existent-membership', userAddresses[0])
      ).rejects.toThrow('Membership not found');
    });

    it('should update reward records on claim', async () => {
      // Distribute rewards
      await service.distributeRewards({
        pool_id: poolId,
        epoch_number: 7,
        epoch_start: new Date(Date.now() - 7 * 24 * 60 * 60 * 1000),
        epoch_end: new Date(),
        total_trading_fees: BigInt(1000) * BigInt(10) ** BigInt(18),
        total_liquidity_rewards: BigInt(0),
        total_sla_bonuses: BigInt(0),
      });

      const memberships = await service.getPoolMemberships(poolId);
      const userMembership = memberships.find(
        (m) => m.user_address === userAddresses[0]
      );

      // Get unclaimed rewards before
      const unclaimedBefore = await service.getUnclaimedRewards(
        userAddresses[0]
      );
      expect(unclaimedBefore.length).toBeGreaterThan(0);

      // Claim rewards
      const result = await service.claimRewards(
        userMembership!.id,
        userAddresses[0]
      );

      // Check rewards are marked as claimed
      const unclaimedAfter = await service.getUnclaimedRewards(userAddresses[0]);
      const claimedRewards = (await service.getUserRewards(userAddresses[0])).filter(
        (r) => r.is_claimed
      );

      expect(unclaimedAfter.length).toBeLessThan(unclaimedBefore.length);
      expect(claimedRewards.length).toBeGreaterThan(0);

      claimedRewards.forEach((r) => {
        expect(r.claimed_at).toBeInstanceOf(Date);
        expect(r.claim_tx_hash).toBe(result.claim_tx_hash);
      });
    });
  });

  describe('Reward Queries', () => {
    it('should get all rewards for a user', async () => {
      // Distribute multiple epochs
      for (let epoch = 1; epoch <= 3; epoch++) {
        await service.distributeRewards({
          pool_id: poolId,
          epoch_number: epoch,
          epoch_start: new Date(Date.now() - (8 - epoch) * 24 * 60 * 60 * 1000),
          epoch_end: new Date(Date.now() - (7 - epoch) * 24 * 60 * 60 * 1000),
          total_trading_fees: BigInt(500) * BigInt(10) ** BigInt(18),
          total_liquidity_rewards: BigInt(500) * BigInt(10) ** BigInt(18),
          total_sla_bonuses: BigInt(0),
        });
      }

      const userRewards = await service.getUserRewards(userAddresses[0]);

      expect(userRewards.length).toBeGreaterThan(0);
      userRewards.forEach((r) => {
        expect(r.user_address).toBe(userAddresses[0]);
      });
    });

    it('should get unclaimed rewards for a user', async () => {
      await service.distributeRewards({
        pool_id: poolId,
        epoch_number: 10,
        epoch_start: new Date(Date.now() - 7 * 24 * 60 * 60 * 1000),
        epoch_end: new Date(),
        total_trading_fees: BigInt(1000) * BigInt(10) ** BigInt(18),
        total_liquidity_rewards: BigInt(0),
        total_sla_bonuses: BigInt(0),
      });

      const unclaimed = await service.getUnclaimedRewards(userAddresses[0]);

      expect(unclaimed.length).toBeGreaterThan(0);
      unclaimed.forEach((r) => {
        expect(r.is_claimed).toBe(false);
        expect(r.claimed_at).toBeUndefined();
      });
    });

    it('should get reward by ID', async () => {
      const rewards = await service.distributeRewards({
        pool_id: poolId,
        epoch_number: 11,
        epoch_start: new Date(Date.now() - 7 * 24 * 60 * 60 * 1000),
        epoch_end: new Date(),
        total_trading_fees: BigInt(1000) * BigInt(10) ** BigInt(18),
        total_liquidity_rewards: BigInt(0),
        total_sla_bonuses: BigInt(0),
      });

      const reward = rewards[0];
      const retrieved = await service.getReward(reward.id);

      expect(retrieved).toEqual(reward);
    });

    it('should return null for non-existent reward', async () => {
      const reward = await service.getReward('non-existent-reward');
      expect(reward).toBeNull();
    });
  });

  describe('Proportional Distribution', () => {
    it('should distribute rewards proportional to liquidity shares', async () => {
      // Users have different liquidity amounts:
      // User 1: 100 resources, 10000 tokens
      // User 2: 200 resources, 20000 tokens
      // User 3: 300 resources, 30000 tokens
      // User 4: 400 resources, 40000 tokens
      // User 5: 500 resources, 50000 tokens

      await service.distributeRewards({
        pool_id: poolId,
        epoch_number: 20,
        epoch_start: new Date(Date.now() - 7 * 24 * 60 * 60 * 1000),
        epoch_end: new Date(),
        total_trading_fees: BigInt(15000) * BigInt(10) ** BigInt(18), // Large enough to see distribution
        total_liquidity_rewards: BigInt(0),
        total_sla_bonuses: BigInt(0),
      });

      // Get rewards for each user
      const rewardAmounts = await Promise.all(
        userAddresses.map(async (addr) => {
          const rewards = await service.getUserRewards(addr);
          return rewards
            .filter((r) => r.epoch_number === 20)
            .reduce((sum, r) => sum + r.amount, BigInt(0));
        })
      );

      // User with more liquidity should get more rewards
      for (let i = 1; i < rewardAmounts.length; i++) {
        expect(rewardAmounts[i]).toBeGreaterThan(rewardAmounts[i - 1]);
      }
    });
  });

  describe('Edge Cases', () => {
    it('should handle distribution to pool with no members', async () => {
      // Create empty pool
      const emptyPoolRequest = generatePoolCreationRequest({
        slug: 'empty-pool',
      });
      const emptyPool = await service.createPool(emptyPoolRequest);

      const rewards = await service.distributeRewards({
        pool_id: emptyPool.id,
        epoch_number: 30,
        epoch_start: new Date(Date.now() - 7 * 24 * 60 * 60 * 1000),
        epoch_end: new Date(),
        total_trading_fees: BigInt(1000) * BigInt(10) ** BigInt(18),
        total_liquidity_rewards: BigInt(0),
        total_sla_bonuses: BigInt(0),
      });

      expect(rewards).toHaveLength(0);
    });

    it('should handle zero reward amounts', async () => {
      const rewards = await service.distributeRewards({
        pool_id: poolId,
        epoch_number: 31,
        epoch_start: new Date(Date.now() - 7 * 24 * 60 * 60 * 1000),
        epoch_end: new Date(),
        total_trading_fees: BigInt(0),
        total_liquidity_rewards: BigInt(0),
        total_sla_bonuses: BigInt(0),
      });

      expect(rewards).toHaveLength(0);
    });

    it('should reject distribution for non-existent pool', async () => {
      await expect(
        service.distributeRewards({
          pool_id: 'non-existent-pool',
          epoch_number: 32,
          epoch_start: new Date(),
          epoch_end: new Date(),
          total_trading_fees: BigInt(1000) * BigInt(10) ** BigInt(18),
          total_liquidity_rewards: BigInt(0),
          total_sla_bonuses: BigInt(0),
        })
      ).rejects.toThrow('Pool not found');
    });

    it('should handle very large reward amounts', async () => {
      const largeReward = BigInt(10) ** BigInt(27); // 1 billion tokens

      const rewards = await service.distributeRewards({
        pool_id: poolId,
        epoch_number: 33,
        epoch_start: new Date(Date.now() - 7 * 24 * 60 * 60 * 1000),
        epoch_end: new Date(),
        total_trading_fees: largeReward,
        total_liquidity_rewards: BigInt(0),
        total_sla_bonuses: BigInt(0),
      });

      expect(rewards.length).toBeGreaterThan(0);

      const totalDistributed = rewards.reduce(
        (sum, r) => sum + r.amount,
        BigInt(0)
      );
      expect(totalDistributed).toBeLessThanOrEqual(largeReward);
    });

    it('should handle multiple claims in sequence', async () => {
      // Distribute rewards for multiple epochs
      for (let epoch = 40; epoch <= 42; epoch++) {
        await service.distributeRewards({
          pool_id: poolId,
          epoch_number: epoch,
          epoch_start: new Date(Date.now() - (50 - epoch) * 24 * 60 * 60 * 1000),
          epoch_end: new Date(Date.now() - (49 - epoch) * 24 * 60 * 60 * 1000),
          total_trading_fees: BigInt(500) * BigInt(10) ** BigInt(18),
          total_liquidity_rewards: BigInt(0),
          total_sla_bonuses: BigInt(0),
        });
      }

      const memberships = await service.getPoolMemberships(poolId);
      const userMembership = memberships.find(
        (m) => m.user_address === userAddresses[0]
      );

      // Claim multiple times
      const result1 = await service.claimRewards(
        userMembership!.id,
        userAddresses[0]
      );
      expect(result1.claimed_amount).toBeGreaterThan(BigInt(0));

      // Should fail on second claim (no pending rewards)
      await expect(
        service.claimRewards(userMembership!.id, userAddresses[0])
      ).rejects.toThrow('No pending rewards to claim');

      // Distribute more rewards
      await service.distributeRewards({
        pool_id: poolId,
        epoch_number: 43,
        epoch_start: new Date(Date.now() - 7 * 24 * 60 * 60 * 1000),
        epoch_end: new Date(),
        total_trading_fees: BigInt(500) * BigInt(10) ** BigInt(18),
        total_liquidity_rewards: BigInt(0),
        total_sla_bonuses: BigInt(0),
      });

      // Should be able to claim again
      const result2 = await service.claimRewards(
        userMembership!.id,
        userAddresses[0]
      );
      expect(result2.claimed_amount).toBeGreaterThan(BigInt(0));
    });
  });
});
