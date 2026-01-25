/**
 * Node Registration Flow - Integration Tests
 *
 * Tests the complete flow of registering a compute node on the Hanzo Platform:
 * 1. Submit registration request with node capabilities
 * 2. Validate stake requirements
 * 3. Verify node status transitions
 * 4. Test heartbeat mechanism
 * 5. Verify node decommissioning
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { MockPlatformService } from './src/utils/platform-service';
import { generateNodeRegistrationRequest } from './src/mocks/generators';
import type { NodeRegistrationRequest, NodeStatus } from './src/types';

describe('Node Registration Flow', () => {
  let service: MockPlatformService;

  beforeEach(() => {
    service = new MockPlatformService();
  });

  afterEach(() => {
    service.reset();
  });

  describe('Basic Registration', () => {
    it('should register a new compute node with valid request', async () => {
      const request = generateNodeRegistrationRequest({
        name: 'Test Node Alpha',
        cpu_cores: 32,
        memory_mb: 131072, // 128GB
        gpu_count: 4,
        gpu_model: 'NVIDIA H100',
        initial_stake: BigInt(500) * BigInt(10) ** BigInt(18),
      });

      const node = await service.registerNode(request);

      expect(node).toBeDefined();
      expect(node.id).toBeDefined();
      expect(node.name).toBe('Test Node Alpha');
      expect(node.owner_address).toBe(request.owner_address);
      expect(node.cpu_cores).toBe(32);
      expect(node.memory_mb).toBe(131072);
      expect(node.gpu_count).toBe(4);
      expect(node.gpu_model).toBe('NVIDIA H100');
      expect(node.stake_amount).toBe(request.initial_stake);
      expect(node.status).toBe('pending');
      expect(node.sla_score).toBe(100); // New nodes start with perfect score
    });

    it('should register a CPU-only node without GPU', async () => {
      const request = generateNodeRegistrationRequest({
        gpu_count: 0,
        gpu_model: undefined,
        initial_stake: BigInt(200) * BigInt(10) ** BigInt(18),
      });

      const node = await service.registerNode(request);

      expect(node.gpu_count).toBe(0);
      expect(node.gpu_model).toBeUndefined();
      expect(node.status).toBe('pending');
    });

    it('should register a node with container support', async () => {
      const request = generateNodeRegistrationRequest({
        supports_docker: true,
        supports_k8s: true,
        supports_wasm: true,
        initial_stake: BigInt(300) * BigInt(10) ** BigInt(18),
      });

      const node = await service.registerNode(request);

      expect(node.supports_docker).toBe(true);
      expect(node.supports_k8s).toBe(true);
      expect(node.supports_wasm).toBe(true);
    });

    it('should register a node with TEE capabilities', async () => {
      const request = generateNodeRegistrationRequest({
        tee_type: 'SGX',
        initial_stake: BigInt(400) * BigInt(10) ** BigInt(18),
      });

      const node = await service.registerNode(request);

      expect(node.tee_type).toBe('SGX');
    });
  });

  describe('Validation Rules', () => {
    it('should reject registration without owner address', async () => {
      const request = generateNodeRegistrationRequest({
        owner_address: '',
      });

      await expect(service.registerNode(request)).rejects.toThrow(
        'Owner address is required'
      );
    });

    it('should reject registration with zero CPU cores', async () => {
      const request = generateNodeRegistrationRequest({
        cpu_cores: 0,
      });

      await expect(service.registerNode(request)).rejects.toThrow(
        'CPU cores must be positive'
      );
    });

    it('should reject registration with zero memory', async () => {
      const request = generateNodeRegistrationRequest({
        memory_mb: 0,
      });

      await expect(service.registerNode(request)).rejects.toThrow(
        'Memory must be positive'
      );
    });

    it('should reject registration with insufficient stake', async () => {
      const request = generateNodeRegistrationRequest({
        initial_stake: BigInt(50) * BigInt(10) ** BigInt(18), // Less than 100 minimum
      });

      await expect(service.registerNode(request)).rejects.toThrow(
        'Minimum stake is 100 tokens'
      );
    });

    it('should reject duplicate registration from same owner', async () => {
      const ownerAddress = '0x' + 'a'.repeat(40);
      const request1 = generateNodeRegistrationRequest({
        owner_address: ownerAddress,
        initial_stake: BigInt(200) * BigInt(10) ** BigInt(18),
      });
      const request2 = generateNodeRegistrationRequest({
        owner_address: ownerAddress,
        initial_stake: BigInt(200) * BigInt(10) ** BigInt(18),
      });

      await service.registerNode(request1);

      await expect(service.registerNode(request2)).rejects.toThrow(
        'Node already registered for this address'
      );
    });
  });

  describe('Status Transitions', () => {
    it('should transition from pending to active', async () => {
      const request = generateNodeRegistrationRequest();
      const node = await service.registerNode(request);

      expect(node.status).toBe('pending');

      const activatedNode = await service.activateNode(node.id);

      expect(activatedNode.status).toBe('active');
      expect(activatedNode.updated_at.getTime()).toBeGreaterThanOrEqual(
        node.created_at.getTime()
      );
    });

    it('should transition from active to suspended', async () => {
      const request = generateNodeRegistrationRequest();
      const node = await service.registerNode(request);
      await service.activateNode(node.id);

      const suspendedNode = await service.updateNodeStatus(node.id, 'suspended');

      expect(suspendedNode.status).toBe('suspended');
    });

    it('should transition from active to offline', async () => {
      const request = generateNodeRegistrationRequest();
      const node = await service.registerNode(request);
      await service.activateNode(node.id);

      const offlineNode = await service.updateNodeStatus(node.id, 'offline');

      expect(offlineNode.status).toBe('offline');
    });

    it('should transition from active to maintenance', async () => {
      const request = generateNodeRegistrationRequest();
      const node = await service.registerNode(request);
      await service.activateNode(node.id);

      const maintenanceNode = await service.updateNodeStatus(
        node.id,
        'maintenance'
      );

      expect(maintenanceNode.status).toBe('maintenance');
    });

    it('should transition from suspended back to active', async () => {
      const request = generateNodeRegistrationRequest();
      const node = await service.registerNode(request);
      await service.activateNode(node.id);
      await service.updateNodeStatus(node.id, 'suspended');

      const reactivatedNode = await service.updateNodeStatus(node.id, 'active');

      expect(reactivatedNode.status).toBe('active');
    });

    it('should allow decommissioning from any non-decommissioned state', async () => {
      const states: NodeStatus[] = ['pending', 'active', 'suspended', 'offline', 'maintenance'];

      for (const state of states) {
        const svc = new MockPlatformService();
        const request = generateNodeRegistrationRequest();
        const node = await svc.registerNode(request);

        // Transition to target state
        if (state === 'active') {
          await svc.activateNode(node.id);
        } else if (state !== 'pending') {
          await svc.activateNode(node.id);
          await svc.updateNodeStatus(node.id, state);
        }

        const decommissionedNode = await svc.updateNodeStatus(
          node.id,
          'decommissioned'
        );

        expect(decommissionedNode.status).toBe('decommissioned');
      }
    });

    it('should reject invalid state transitions', async () => {
      const request = generateNodeRegistrationRequest();
      const node = await service.registerNode(request);

      // pending -> suspended is invalid
      await expect(
        service.updateNodeStatus(node.id, 'suspended')
      ).rejects.toThrow('Invalid status transition');

      // pending -> offline is invalid
      await expect(
        service.updateNodeStatus(node.id, 'offline')
      ).rejects.toThrow('Invalid status transition');
    });

    it('should reject transitions from decommissioned', async () => {
      const request = generateNodeRegistrationRequest();
      const node = await service.registerNode(request);
      await service.updateNodeStatus(node.id, 'decommissioned');

      await expect(service.activateNode(node.id)).rejects.toThrow(
        'Invalid status transition'
      );
    });
  });

  describe('Heartbeat Mechanism', () => {
    it('should record heartbeat for active node', async () => {
      const request = generateNodeRegistrationRequest();
      const node = await service.registerNode(request);
      await service.activateNode(node.id);

      const initialHeartbeat = node.last_heartbeat_at;

      // Wait a bit and record new heartbeat
      await new Promise((resolve) => setTimeout(resolve, 10));
      await service.recordHeartbeat(node.id);

      const updatedNode = await service.getNode(node.id);
      expect(updatedNode!.last_heartbeat_at!.getTime()).toBeGreaterThan(
        initialHeartbeat!.getTime()
      );
    });

    it('should fail heartbeat for non-existent node', async () => {
      await expect(
        service.recordHeartbeat('non-existent-id')
      ).rejects.toThrow('Node not found');
    });
  });

  describe('Node Retrieval', () => {
    it('should retrieve node by ID', async () => {
      const request = generateNodeRegistrationRequest();
      const registeredNode = await service.registerNode(request);

      const retrievedNode = await service.getNode(registeredNode.id);

      expect(retrievedNode).toEqual(registeredNode);
    });

    it('should return null for non-existent node', async () => {
      const node = await service.getNode('non-existent-id');

      expect(node).toBeNull();
    });

    it('should retrieve all nodes', async () => {
      const requests = Array.from({ length: 5 }, (_, i) =>
        generateNodeRegistrationRequest({
          owner_address: `0x${'0'.repeat(38)}${i.toString(16).padStart(2, '0')}`,
        })
      );

      for (const request of requests) {
        await service.registerNode(request);
      }

      const nodes = await service.getNodes();

      expect(nodes).toHaveLength(5);
    });
  });

  describe('Edge Cases', () => {
    it('should handle maximum resource values', async () => {
      const request = generateNodeRegistrationRequest({
        cpu_cores: 1024,
        memory_mb: 8388608, // 8TB
        storage_gb: 1000000, // 1PB
        gpu_count: 64,
        bandwidth_mbps: 400000, // 400Gbps
        initial_stake: BigInt(1000000) * BigInt(10) ** BigInt(18),
      });

      const node = await service.registerNode(request);

      expect(node.cpu_cores).toBe(1024);
      expect(node.memory_mb).toBe(8388608);
      expect(node.storage_gb).toBe(1000000);
      expect(node.gpu_count).toBe(64);
      expect(node.bandwidth_mbps).toBe(400000);
    });

    it('should handle regional configurations', async () => {
      const regions = ['us-east-1', 'eu-west-1', 'ap-northeast-1'];

      for (const region of regions) {
        const svc = new MockPlatformService();
        const request = generateNodeRegistrationRequest({
          region,
          availability_zone: `${region}-az1`,
          owner_address: `0x${'0'.repeat(38)}${regions.indexOf(region).toString(16).padStart(2, '0')}`,
        });

        const node = await svc.registerNode(request);

        expect(node.region).toBe(region);
        expect(node.availability_zone).toBe(`${region}-az1`);
      }
    });

    it('should preserve p2p address format', async () => {
      const p2pAddress = '/ip4/192.168.1.1/tcp/30303/p2p/QmYyQSo1c1Ym7orWxLYvCrM2EmxFTANf8wXmmE7DWjhx5N';
      const request = generateNodeRegistrationRequest({
        p2p_address: p2pAddress,
      });

      const node = await service.registerNode(request);

      expect(node.p2p_address).toBe(p2pAddress);
    });
  });
});
