import { describe, test, expect, beforeEach, afterEach, jest } from '@jest/globals';
import { PeerAgentNetwork, AgentConfig } from '../src/lib/peer-agent-network';
import * as fs from 'fs';
import * as path from 'path';
import * as os from 'os';

describe('PeerAgentNetwork', () => {
  let network: PeerAgentNetwork;
  let testDir: string;

  beforeEach(() => {
    network = new PeerAgentNetwork();
    testDir = fs.mkdtempSync(path.join(os.tmpdir(), 'peer-network-test-'));
    
    // Create test file structure
    fs.mkdirSync(path.join(testDir, 'src'));
    fs.mkdirSync(path.join(testDir, 'tests'));
    fs.writeFileSync(path.join(testDir, 'src', 'index.js'), 'console.log("Hello");');
    fs.writeFileSync(path.join(testDir, 'src', 'utils.js'), 'export function util() {}');
    fs.writeFileSync(path.join(testDir, 'tests', 'index.test.js'), 'test("sample", () => {});');
  });

  afterEach(() => {
    // Clean up
    fs.rmSync(testDir, { recursive: true, force: true });
  });

  describe('agent spawning', () => {
    test('should spawn agent with configuration', async () => {
      const config: AgentConfig = {
        id: 'test-agent',
        name: 'Test Agent',
        type: 'claude-code',
        responsibility: 'Test file processing',
        tools: ['edit_file', 'view_file']
      };

      await network.spawnAgent(config);
      
      const agents = network.getActiveAgents();
      expect(agents).toHaveLength(1);
      expect(agents[0].id).toBe('test-agent');
      expect(agents[0].status).toBe('active');
    });

    test('should prevent duplicate agent IDs', async () => {
      const config: AgentConfig = {
        id: 'duplicate',
        name: 'Agent 1',
        type: 'claude-code'
      };

      await network.spawnAgent(config);
      
      // Try to spawn with same ID
      await expect(network.spawnAgent({
        ...config,
        name: 'Agent 2'
      })).rejects.toThrow('already exists');
    });
  });

  describe('codebase agent spawning', () => {
    test('should spawn one agent per file', async () => {
      await network.spawnAgentsForCodebase(testDir, 'claude-code', 'one-per-file');
      
      const agents = network.getActiveAgents();
      // Should have 3 agents (3 files)
      expect(agents).toHaveLength(3);
      
      // Check agent responsibilities
      const responsibilities = agents.map(a => a.responsibility);
      expect(responsibilities).toContain(expect.stringContaining('src/index.js'));
      expect(responsibilities).toContain(expect.stringContaining('src/utils.js'));
      expect(responsibilities).toContain(expect.stringContaining('tests/index.test.js'));
    });

    test('should spawn one agent per directory', async () => {
      await network.spawnAgentsForCodebase(testDir, 'claude-code', 'one-per-directory');
      
      const agents = network.getActiveAgents();
      // Should have 2 agents (src and tests directories)
      expect(agents).toHaveLength(2);
      
      const responsibilities = agents.map(a => a.responsibility);
      expect(responsibilities).toContain(expect.stringContaining('src'));
      expect(responsibilities).toContain(expect.stringContaining('tests'));
    });

    test('should respect file patterns', async () => {
      await network.spawnAgentsForCodebase(
        testDir, 
        'claude-code', 
        'one-per-file',
        ['**/*.test.js'] // Only test files
      );
      
      const agents = network.getActiveAgents();
      expect(agents).toHaveLength(1);
      expect(agents[0].responsibility).toContain('tests/index.test.js');
    });
  });

  describe('agent communication', () => {
    test('should enable agent-to-agent messaging', async () => {
      // Spawn two agents
      await network.spawnAgent({
        id: 'agent1',
        name: 'Agent 1',
        type: 'claude-code'
      });
      
      await network.spawnAgent({
        id: 'agent2', 
        name: 'Agent 2',
        type: 'claude-code'
      });

      // Send message from agent1 to agent2
      const response = await network.sendMessage('agent1', 'agent2', {
        type: 'query',
        content: 'What files are you working on?'
      });

      expect(response).toBeDefined();
      expect(response.from).toBe('agent2');
      expect(response.to).toBe('agent1');
    });

    test('should broadcast messages to all agents', async () => {
      // Spawn three agents
      for (let i = 1; i <= 3; i++) {
        await network.spawnAgent({
          id: `agent${i}`,
          name: `Agent ${i}`,
          type: 'claude-code'
        });
      }

      const responses = await network.broadcast('agent1', {
        type: 'announcement',
        content: 'Starting code review'
      });

      expect(responses).toHaveLength(2); // Response from agent2 and agent3
      expect(responses.every(r => r.from !== 'agent1')).toBe(true);
    });
  });

  describe('MCP tool exposure', () => {
    test('should expose agents as MCP tools to each other', async () => {
      await network.spawnAgent({
        id: 'file-agent',
        name: 'File Agent',
        type: 'claude-code',
        responsibility: 'File operations',
        tools: ['edit_file', 'create_file']
      });

      await network.spawnAgent({
        id: 'test-agent',
        name: 'Test Agent', 
        type: 'aider',
        responsibility: 'Test writing'
      });

      // Check that each agent can see the other as a tool
      const fileAgentTools = await network.getAgentTools('file-agent');
      expect(fileAgentTools).toContain(expect.objectContaining({
        name: 'ask_test_agent',
        description: expect.stringContaining('Test Agent')
      }));

      const testAgentTools = await network.getAgentTools('test-agent');
      expect(testAgentTools).toContain(expect.objectContaining({
        name: 'ask_file_agent',
        description: expect.stringContaining('File Agent')
      }));
    });

    test('should allow recursive agent calls via MCP', async () => {
      // Set up agents
      await network.spawnAgent({
        id: 'coordinator',
        name: 'Coordinator',
        type: 'claude-code'
      });

      await network.spawnAgent({
        id: 'worker1',
        name: 'Worker 1',
        type: 'claude-code'
      });

      await network.spawnAgent({
        id: 'worker2',
        name: 'Worker 2',
        type: 'claude-code'
      });

      // Coordinator delegates to workers
      const result = await network.callAgentTool('coordinator', 'delegate_to_worker1', {
        task: 'Process data'
      });

      expect(result).toBeDefined();
      expect(result.success).toBe(true);
    });
  });

  describe('task coordination', () => {
    test('should coordinate parallel tasks across agents', async () => {
      // Create a task that can be parallelized
      const files = [
        'file1.js',
        'file2.js', 
        'file3.js',
        'file4.js'
      ];

      // Spawn agents for parallel processing
      const agents = await network.spawnAgentsForTask(
        'Process multiple files',
        files.map(f => ({
          subtask: `Process ${f}`,
          data: { file: f }
        }))
      );

      expect(agents).toHaveLength(4);
      
      // Execute all tasks in parallel
      const results = await network.executeParallelTasks(
        agents.map(a => ({
          agentId: a.id,
          task: a.config.responsibility!
        }))
      );

      expect(results).toHaveLength(4);
      expect(results.every(r => r.status === 'completed')).toBe(true);
    });

    test('should handle agent failures gracefully', async () => {
      await network.spawnAgent({
        id: 'failing-agent',
        name: 'Failing Agent',
        type: 'claude-code'
      });

      // Make agent fail
      (network as any).agents.get('failing-agent').status = 'error';

      const agents = network.getActiveAgents();
      expect(agents).toHaveLength(0); // Failed agents not in active list

      const allAgents = network.getAllAgents();
      expect(allAgents).toHaveLength(1);
      expect(allAgents[0].status).toBe('error');
    });
  });

  describe('swarm optimization', () => {
    test('should optimize agent allocation based on workload', async () => {
      // Create initial agents
      for (let i = 1; i <= 3; i++) {
        await network.spawnAgent({
          id: `agent${i}`,
          name: `Agent ${i}`,
          type: 'claude-code'
        });
      }

      // Simulate workload
      const metrics = {
        agent1: { tasksCompleted: 10, avgTime: 2.5 },
        agent2: { tasksCompleted: 5, avgTime: 5.0 },
        agent3: { tasksCompleted: 8, avgTime: 3.0 }
      };

      const optimization = network.optimizeSwarm(metrics);
      
      // Should recommend spawning more agents like agent1 (best performance)
      expect(optimization.recommendations).toContain(
        expect.stringContaining('agent1')
      );
    });

    test('should monitor swarm health', async () => {
      // Spawn multiple agents
      for (let i = 1; i <= 5; i++) {
        await network.spawnAgent({
          id: `agent${i}`,
          name: `Agent ${i}`,
          type: i % 2 === 0 ? 'aider' : 'claude-code'
        });
      }

      const health = network.getSwarmHealth();
      
      expect(health.totalAgents).toBe(5);
      expect(health.activeAgents).toBe(5);
      expect(health.agentTypes).toContain('claude-code');
      expect(health.agentTypes).toContain('aider');
    });
  });

  describe('cleanup and lifecycle', () => {
    test('should terminate individual agents', async () => {
      await network.spawnAgent({
        id: 'temp-agent',
        name: 'Temporary Agent',
        type: 'claude-code'
      });

      expect(network.getActiveAgents()).toHaveLength(1);

      await network.terminateAgent('temp-agent');
      
      expect(network.getActiveAgents()).toHaveLength(0);
    });

    test('should terminate all agents on shutdown', async () => {
      // Spawn multiple agents
      for (let i = 1; i <= 3; i++) {
        await network.spawnAgent({
          id: `agent${i}`,
          name: `Agent ${i}`,
          type: 'claude-code'
        });
      }

      expect(network.getActiveAgents()).toHaveLength(3);

      await network.shutdown();
      
      expect(network.getActiveAgents()).toHaveLength(0);
    });
  });
});