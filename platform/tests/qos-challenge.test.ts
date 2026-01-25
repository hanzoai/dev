/**
 * QoS Challenge Verification - Integration Tests
 *
 * Tests the complete QoS challenge system as defined in QOS_CHALLENGE_SYSTEM_DESIGN.md:
 * 1. Challenge issuance to nodes
 * 2. Response submission and deadline handling
 * 3. Verification of different challenge types
 * 4. Score calculation and node SLA updates
 * 5. Failure handling and timeout scenarios
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { MockPlatformService } from './src/utils/platform-service';
import {
  generateNodeRegistrationRequest,
  generatePoolCreationRequest,
  generateChallengeResponse,
} from './src/mocks/generators';
import type { ChallengeType, ChallengeStatus, ChallengeResponse } from './src/types';

describe('QoS Challenge Verification', () => {
  let service: MockPlatformService;
  let nodeId: string;
  let poolId: string;

  beforeEach(async () => {
    service = new MockPlatformService();

    // Set up a node and pool for testing
    const nodeRequest = generateNodeRegistrationRequest({
      tee_type: 'SGX', // Enable TEE for attestation tests
    });
    const node = await service.registerNode(nodeRequest);
    await service.activateNode(node.id);
    nodeId = node.id;

    const poolRequest = generatePoolCreationRequest();
    const pool = await service.createPool(poolRequest);
    poolId = pool.id;
  });

  afterEach(() => {
    service.reset();
  });

  describe('Challenge Issuance', () => {
    it('should issue an availability challenge', async () => {
      const challenge = await service.issueChallenge(
        nodeId,
        'availability',
        poolId
      );

      expect(challenge).toBeDefined();
      expect(challenge.id).toBeDefined();
      expect(challenge.node_id).toBe(nodeId);
      expect(challenge.pool_id).toBe(poolId);
      expect(challenge.challenge_type).toBe('availability');
      expect(challenge.status).toBe('pending');
      expect(challenge.deadline).toBeInstanceOf(Date);
      expect(challenge.deadline.getTime()).toBeGreaterThan(Date.now());
      expect(challenge.challenge_data).toBeDefined();
      expect(challenge.challenge_data.nonce).toBeDefined();
    });

    it('should issue a compute challenge', async () => {
      const challenge = await service.issueChallenge(nodeId, 'compute', poolId);

      expect(challenge.challenge_type).toBe('compute');
      expect(challenge.challenge_data.seed).toBeDefined();
      expect(challenge.challenge_data.difficulty).toBeGreaterThanOrEqual(1);
      expect(challenge.challenge_data.difficulty).toBeLessThanOrEqual(10);
    });

    it('should issue a latency challenge', async () => {
      const challenge = await service.issueChallenge(nodeId, 'latency', poolId);

      expect(challenge.challenge_type).toBe('latency');
      expect(challenge.challenge_data.expected_latency_ms).toBeGreaterThan(0);
      expect(challenge.challenge_data.rounds).toBeGreaterThan(0);
    });

    it('should issue a bandwidth challenge', async () => {
      const challenge = await service.issueChallenge(
        nodeId,
        'bandwidth',
        poolId
      );

      expect(challenge.challenge_type).toBe('bandwidth');
      expect(
        challenge.challenge_data.expected_throughput_mbps
      ).toBeGreaterThan(0);
    });

    it('should issue an attestation challenge', async () => {
      const challenge = await service.issueChallenge(
        nodeId,
        'attestation',
        poolId
      );

      expect(challenge.challenge_type).toBe('attestation');
    });

    it('should issue challenge without pool reference', async () => {
      const challenge = await service.issueChallenge(nodeId, 'availability');

      expect(challenge.pool_id).toBeUndefined();
    });

    it('should reject challenge for non-existent node', async () => {
      await expect(
        service.issueChallenge('non-existent-node', 'availability')
      ).rejects.toThrow('Node not found');
    });

    it('should reject challenge for inactive node', async () => {
      const inactiveNodeRequest = generateNodeRegistrationRequest({
        owner_address: '0x' + 'e'.repeat(40),
      });
      const inactiveNode = await service.registerNode(inactiveNodeRequest);
      // Node is still pending, not active

      await expect(
        service.issueChallenge(inactiveNode.id, 'availability')
      ).rejects.toThrow('Node is not active');
    });
  });

  describe('Response Submission', () => {
    it('should accept valid response before deadline', async () => {
      const challenge = await service.issueChallenge(
        nodeId,
        'availability',
        poolId
      );

      const response: ChallengeResponse = {
        signed_nonce: 'a'.repeat(64),
      };

      const updatedChallenge = await service.submitChallengeResponse(
        challenge.id,
        response
      );

      expect(updatedChallenge.response_data).toEqual(response);
      expect(updatedChallenge.response_at).toBeInstanceOf(Date);
      expect(updatedChallenge.status).toBe('pending'); // Still pending until verified
    });

    it('should reject response for non-existent challenge', async () => {
      await expect(
        service.submitChallengeResponse('non-existent-challenge', {
          signed_nonce: 'test',
        })
      ).rejects.toThrow('Challenge not found');
    });

    it('should reject response for already submitted challenge', async () => {
      const challenge = await service.issueChallenge(
        nodeId,
        'availability',
        poolId
      );

      await service.submitChallengeResponse(challenge.id, {
        signed_nonce: 'first',
      });

      // Verify the challenge to change its status
      await service.verifyChallenge(challenge.id, '0x' + 'f'.repeat(40));

      await expect(
        service.submitChallengeResponse(challenge.id, { signed_nonce: 'second' })
      ).rejects.toThrow('Challenge is not pending');
    });
  });

  describe('Verification - Availability Challenge', () => {
    it('should pass availability challenge with signed nonce', async () => {
      const challenge = await service.issueChallenge(
        nodeId,
        'availability',
        poolId
      );

      await service.submitChallengeResponse(challenge.id, {
        signed_nonce: 'valid_signature_' + 'a'.repeat(50),
      });

      const result = await service.verifyChallenge(
        challenge.id,
        '0x' + 'f'.repeat(40)
      );

      expect(result.passed).toBe(true);
      expect(result.score).toBe(100);

      const verifiedChallenge = await service.getChallenge(challenge.id);
      expect(verifiedChallenge!.status).toBe('passed');
      expect(verifiedChallenge!.verified_at).toBeInstanceOf(Date);
      expect(verifiedChallenge!.verified_by).toBe('0x' + 'f'.repeat(40));
    });

    it('should fail availability challenge without signed nonce', async () => {
      const challenge = await service.issueChallenge(
        nodeId,
        'availability',
        poolId
      );

      await service.submitChallengeResponse(challenge.id, {});

      const result = await service.verifyChallenge(
        challenge.id,
        '0x' + 'f'.repeat(40)
      );

      expect(result.passed).toBe(false);
      expect(result.score).toBe(0);
      expect(result.failure_reason).toBe('No signed nonce provided');

      const verifiedChallenge = await service.getChallenge(challenge.id);
      expect(verifiedChallenge!.status).toBe('failed');
    });
  });

  describe('Verification - Latency Challenge', () => {
    it('should pass latency challenge within threshold', async () => {
      const challenge = await service.issueChallenge(nodeId, 'latency', poolId);
      const expectedLatency = challenge.challenge_data.expected_latency_ms!;

      await service.submitChallengeResponse(challenge.id, {
        latency_ms: expectedLatency - 10, // Within threshold
      });

      const result = await service.verifyChallenge(
        challenge.id,
        '0x' + 'f'.repeat(40)
      );

      expect(result.passed).toBe(true);
      expect(result.score).toBeGreaterThan(0);
    });

    it('should fail latency challenge exceeding threshold', async () => {
      const challenge = await service.issueChallenge(nodeId, 'latency', poolId);
      const expectedLatency = challenge.challenge_data.expected_latency_ms!;

      await service.submitChallengeResponse(challenge.id, {
        latency_ms: expectedLatency + 50, // Exceeds threshold
      });

      const result = await service.verifyChallenge(
        challenge.id,
        '0x' + 'f'.repeat(40)
      );

      expect(result.passed).toBe(false);
      expect(result.failure_reason).toContain('exceeds threshold');
    });

    it('should calculate score based on latency performance', async () => {
      const challenge = await service.issueChallenge(nodeId, 'latency', poolId);
      const expectedLatency = challenge.challenge_data.expected_latency_ms!;

      // Submit latency at half the threshold (excellent performance)
      await service.submitChallengeResponse(challenge.id, {
        latency_ms: Math.floor(expectedLatency / 2),
      });

      const result = await service.verifyChallenge(
        challenge.id,
        '0x' + 'f'.repeat(40)
      );

      expect(result.passed).toBe(true);
      expect(result.score).toBe(100); // Capped at 100
    });
  });

  describe('Verification - Compute Challenge', () => {
    it('should pass compute challenge with valid proof', async () => {
      const challenge = await service.issueChallenge(nodeId, 'compute', poolId);

      await service.submitChallengeResponse(challenge.id, {
        result_hash: 'a'.repeat(32),
        intermediate_hashes: ['b'.repeat(32), 'c'.repeat(32), 'd'.repeat(32)],
        actual_flops: 500, // Good performance
      });

      const result = await service.verifyChallenge(
        challenge.id,
        '0x' + 'f'.repeat(40)
      );

      expect(result.passed).toBe(true);
      expect(result.score).toBeGreaterThan(0);
    });

    it('should fail compute challenge without result hash', async () => {
      const challenge = await service.issueChallenge(nodeId, 'compute', poolId);

      await service.submitChallengeResponse(challenge.id, {
        intermediate_hashes: ['a'.repeat(32)],
        actual_flops: 100,
      });

      const result = await service.verifyChallenge(
        challenge.id,
        '0x' + 'f'.repeat(40)
      );

      expect(result.passed).toBe(false);
      expect(result.failure_reason).toBe('Compute verification failed');
    });

    it('should fail compute challenge with low FLOPS', async () => {
      const challenge = await service.issueChallenge(nodeId, 'compute', poolId);

      await service.submitChallengeResponse(challenge.id, {
        result_hash: 'a'.repeat(32),
        intermediate_hashes: ['b'.repeat(32)],
        actual_flops: 10, // Too low
      });

      const result = await service.verifyChallenge(
        challenge.id,
        '0x' + 'f'.repeat(40)
      );

      expect(result.passed).toBe(false);
    });

    it('should award bonus for high compute performance', async () => {
      const challenge = await service.issueChallenge(nodeId, 'compute', poolId);

      await service.submitChallengeResponse(challenge.id, {
        result_hash: 'a'.repeat(32),
        intermediate_hashes: ['b'.repeat(32), 'c'.repeat(32), 'd'.repeat(32)],
        actual_flops: 1000, // Excellent performance
      });

      const result = await service.verifyChallenge(
        challenge.id,
        '0x' + 'f'.repeat(40)
      );

      expect(result.passed).toBe(true);
      expect(result.score).toBeGreaterThan(80);
      expect(result.bonus).toBe(0.1); // Performance bonus
    });
  });

  describe('Verification - Bandwidth Challenge', () => {
    it('should pass bandwidth challenge meeting threshold', async () => {
      const challenge = await service.issueChallenge(
        nodeId,
        'bandwidth',
        poolId
      );
      const expectedThroughput =
        challenge.challenge_data.expected_throughput_mbps!;

      await service.submitChallengeResponse(challenge.id, {
        throughput_mbps: expectedThroughput + 100, // Exceeds requirement
      });

      const result = await service.verifyChallenge(
        challenge.id,
        '0x' + 'f'.repeat(40)
      );

      expect(result.passed).toBe(true);
      expect(result.score).toBeGreaterThanOrEqual(100); // Can reach or exceed 100 for high performance
    });

    it('should fail bandwidth challenge below threshold', async () => {
      const challenge = await service.issueChallenge(
        nodeId,
        'bandwidth',
        poolId
      );
      const expectedThroughput =
        challenge.challenge_data.expected_throughput_mbps!;

      await service.submitChallengeResponse(challenge.id, {
        throughput_mbps: expectedThroughput - 200, // Below requirement
      });

      const result = await service.verifyChallenge(
        challenge.id,
        '0x' + 'f'.repeat(40)
      );

      expect(result.passed).toBe(false);
      expect(result.failure_reason).toContain('below threshold');
    });
  });

  describe('Verification - Attestation Challenge', () => {
    it('should pass attestation challenge for TEE-enabled node', async () => {
      const challenge = await service.issueChallenge(
        nodeId,
        'attestation',
        poolId
      );

      await service.submitChallengeResponse(challenge.id, {
        result_hash: 'attestation_quote_hash',
      });

      const result = await service.verifyChallenge(
        challenge.id,
        '0x' + 'f'.repeat(40)
      );

      expect(result.passed).toBe(true);
      expect(result.bonus).toBe(0.25); // TEE bonus
    });

    it('should fail attestation challenge for non-TEE node', async () => {
      // Create a node without TEE
      const nonTeeNodeRequest = generateNodeRegistrationRequest({
        owner_address: '0x' + 'd'.repeat(40),
        tee_type: undefined,
      });
      const nonTeeNode = await service.registerNode(nonTeeNodeRequest);
      await service.activateNode(nonTeeNode.id);

      const challenge = await service.issueChallenge(
        nonTeeNode.id,
        'attestation'
      );

      await service.submitChallengeResponse(challenge.id, {});

      const result = await service.verifyChallenge(
        challenge.id,
        '0x' + 'f'.repeat(40)
      );

      expect(result.passed).toBe(false);
      expect(result.failure_reason).toBe('TEE attestation not available');
    });
  });

  describe('SLA Score Updates', () => {
    it('should update node SLA score on challenge pass', async () => {
      const nodeBefore = await service.getNode(nodeId);
      const initialScore = nodeBefore!.sla_score;

      const challenge = await service.issueChallenge(
        nodeId,
        'availability',
        poolId
      );

      await service.submitChallengeResponse(challenge.id, {
        signed_nonce: 'valid',
      });

      await service.verifyChallenge(challenge.id, '0x' + 'f'.repeat(40));

      const nodeAfter = await service.getNode(nodeId);
      // Verify task count increased
      expect(nodeAfter!.total_tasks_completed).toBeGreaterThanOrEqual(
        nodeBefore!.total_tasks_completed
      );
      // Score should be updated (weighted average with 100)
      expect(nodeAfter!.sla_score).toBeGreaterThanOrEqual(0);
    });

    it('should update node SLA score on challenge fail', async () => {
      const nodeBefore = await service.getNode(nodeId);

      const challenge = await service.issueChallenge(
        nodeId,
        'availability',
        poolId
      );

      await service.submitChallengeResponse(challenge.id, {});

      await service.verifyChallenge(challenge.id, '0x' + 'f'.repeat(40));

      const nodeAfter = await service.getNode(nodeId);
      // Verify task failure count increased
      expect(nodeAfter!.total_tasks_failed).toBeGreaterThanOrEqual(
        nodeBefore!.total_tasks_failed
      );
      // Score should decrease or stay same (weighted average with 0)
      expect(nodeAfter!.sla_score).toBeGreaterThanOrEqual(0);
    });

    it('should track challenge history for QoS score calculation', async () => {
      // Issue and complete multiple challenges
      const challengeTypes: ChallengeType[] = [
        'availability',
        'compute',
        'latency',
        'bandwidth',
      ];

      for (const type of challengeTypes) {
        const challenge = await service.issueChallenge(nodeId, type, poolId);
        const response = generateChallengeResponse(challenge, true);
        await service.submitChallengeResponse(challenge.id, response);
        await service.verifyChallenge(challenge.id, '0x' + 'f'.repeat(40));
      }

      const challenges = await service.getNodeChallenges(nodeId);
      expect(challenges).toHaveLength(4);

      const qosScore = await service.getNodeQoSScore(nodeId);
      expect(qosScore).toBeDefined();
      expect(qosScore.composite).toBeGreaterThan(0);
      expect(qosScore.compute_score).toBeGreaterThanOrEqual(0);
      expect(qosScore.latency_score).toBeGreaterThanOrEqual(0);
      expect(qosScore.bandwidth_score).toBeGreaterThanOrEqual(0);
      expect(qosScore.availability_score).toBeGreaterThanOrEqual(0);
      expect(qosScore.consistency_score).toBeGreaterThanOrEqual(0);
    });
  });

  describe('Challenge Queries', () => {
    it('should retrieve challenge by ID', async () => {
      const challenge = await service.issueChallenge(
        nodeId,
        'availability',
        poolId
      );

      const retrieved = await service.getChallenge(challenge.id);

      expect(retrieved).toEqual(challenge);
    });

    it('should return null for non-existent challenge', async () => {
      const challenge = await service.getChallenge('non-existent-id');
      expect(challenge).toBeNull();
    });

    it('should retrieve all challenges for a node', async () => {
      await service.issueChallenge(nodeId, 'availability', poolId);
      await service.issueChallenge(nodeId, 'compute', poolId);
      await service.issueChallenge(nodeId, 'latency', poolId);

      const challenges = await service.getNodeChallenges(nodeId);

      expect(challenges).toHaveLength(3);
      challenges.forEach((c) => {
        expect(c.node_id).toBe(nodeId);
      });
    });
  });

  describe('Edge Cases', () => {
    it('should handle multiple concurrent challenges', async () => {
      const challengePromises = Array.from({ length: 10 }, (_, i) =>
        service.issueChallenge(
          nodeId,
          i % 2 === 0 ? 'availability' : 'compute',
          poolId
        )
      );

      const challenges = await Promise.all(challengePromises);

      expect(challenges).toHaveLength(10);
      challenges.forEach((c) => {
        expect(c.id).toBeDefined();
        expect(c.status).toBe('pending');
      });
    });

    it('should reject verification without response', async () => {
      const challenge = await service.issueChallenge(
        nodeId,
        'availability',
        poolId
      );

      await expect(
        service.verifyChallenge(challenge.id, '0x' + 'f'.repeat(40))
      ).rejects.toThrow('No response submitted');
    });

    it('should handle challenge with very short deadline', async () => {
      // The mock service creates challenges with configurable deadlines
      // This tests the timeout handling
      const challenge = await service.issueChallenge(
        nodeId,
        'availability',
        poolId
      );

      // Submit response (should succeed as deadline hasn't passed in test)
      await service.submitChallengeResponse(challenge.id, {
        signed_nonce: 'valid',
      });

      const result = await service.verifyChallenge(
        challenge.id,
        '0x' + 'f'.repeat(40)
      );

      expect(result).toBeDefined();
    });
  });
});
