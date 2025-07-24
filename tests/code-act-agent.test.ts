import { describe, test, expect, beforeEach, jest } from '@jest/globals';
import { CodeActAgent, AgentState } from '../src/lib/code-act-agent';
import { FunctionCallingSystem } from '../src/lib/function-calling';

describe('CodeActAgent', () => {
  let agent: CodeActAgent;
  let mockFunctionCalling: jest.Mocked<FunctionCallingSystem>;

  beforeEach(() => {
    // Mock function calling system
    mockFunctionCalling = {
      registerTool: jest.fn(),
      callFunctions: jest.fn(),
      getAvailableTools: jest.fn().mockReturnValue([
        { name: 'view_file', description: 'View file contents' },
        { name: 'str_replace', description: 'Replace string in file' },
        { name: 'run_command', description: 'Run shell command' }
      ]),
      getAllToolSchemas: jest.fn().mockReturnValue([])
    } as any;

    agent = new CodeActAgent('test-agent', mockFunctionCalling);
  });

  describe('state management', () => {
    test('should initialize with correct default state', () => {
      const state = agent.getState();
      expect(state.currentTask).toBe('');
      expect(state.plan).toEqual([]);
      expect(state.completedSteps).toEqual([]);
      expect(state.currentStep).toBe(0);
      expect(state.errors).toEqual([]);
      expect(state.observations).toEqual([]);
    });

    test('should update state correctly', () => {
      const newState: Partial<AgentState> = {
        currentTask: 'Fix bug in login',
        plan: ['Locate login file', 'Fix validation', 'Test changes'],
        currentStep: 1
      };

      agent.setState(newState);
      const state = agent.getState();
      
      expect(state.currentTask).toBe('Fix bug in login');
      expect(state.plan).toHaveLength(3);
      expect(state.currentStep).toBe(1);
    });
  });

  describe('planning', () => {
    test('should generate plan for task', async () => {
      const task = 'Add user authentication to the API';
      
      // Mock LLM response for planning
      const mockPlan = [
        'Analyze current API structure',
        'Install authentication dependencies',
        'Create auth middleware',
        'Add login/logout endpoints',
        'Update existing endpoints with auth checks',
        'Write tests for authentication'
      ];

      // The agent should generate a plan based on the task
      await agent.plan(task);
      
      const state = agent.getState();
      expect(state.currentTask).toBe(task);
      expect(state.plan.length).toBeGreaterThan(0);
    });

    test('should handle planning errors gracefully', async () => {
      const task = 'Invalid task that causes error';
      
      // Even with errors, planning should not throw
      await expect(agent.plan(task)).resolves.not.toThrow();
      
      const state = agent.getState();
      expect(state.currentTask).toBe(task);
    });
  });

  describe('task execution', () => {
    test('should execute single step', async () => {
      // Set up agent with a plan
      agent.setState({
        currentTask: 'Fix typo in README',
        plan: ['View README.md', 'Fix typo', 'Verify changes'],
        currentStep: 0
      });

      // Mock function calling for view_file
      mockFunctionCalling.callFunctions.mockResolvedValueOnce([{
        success: true,
        content: '# README\n\nThis is a typpo in the readme.'
      }]);

      const result = await agent.executeStep();
      
      expect(result.completed).toBe(false);
      expect(result.action).toBe('View README.md');
      expect(mockFunctionCalling.callFunctions).toHaveBeenCalled();
    });

    test('should handle step execution errors', async () => {
      agent.setState({
        currentTask: 'Run failing command',
        plan: ['Execute broken command'],
        currentStep: 0
      });

      // Mock function calling to throw error
      mockFunctionCalling.callFunctions.mockRejectedValueOnce(
        new Error('Command not found')
      );

      const result = await agent.executeStep();
      
      expect(result.completed).toBe(false);
      expect(result.error).toBe('Command not found');
      
      const state = agent.getState();
      expect(state.errors).toHaveLength(1);
      expect(state.errors[0]).toContain('Command not found');
    });

    test('should mark task as completed when all steps done', async () => {
      agent.setState({
        currentTask: 'Simple task',
        plan: ['Step 1', 'Step 2'],
        currentStep: 1,
        completedSteps: ['Step 1']
      });

      // Mock successful execution
      mockFunctionCalling.callFunctions.mockResolvedValueOnce([{
        success: true
      }]);

      const result = await agent.executeStep();
      
      expect(result.completed).toBe(true);
      expect(result.action).toBe('Step 2');
      
      const state = agent.getState();
      expect(state.completedSteps).toHaveLength(2);
    });
  });

  describe('parallel execution', () => {
    test('should identify parallelizable steps', () => {
      const plan = [
        'Download file A',
        'Download file B', 
        'Process file A',
        'Process file B',
        'Merge results'
      ];

      const parallel = agent.identifyParallelSteps(plan);
      
      // Downloads can be parallel
      expect(parallel[0]).toEqual([0, 1]);
      // Processing depends on downloads
      expect(parallel[1]).toEqual([2]);
      expect(parallel[2]).toEqual([3]);
      // Merge depends on processing
      expect(parallel[3]).toEqual([4]);
    });

    test('should execute parallel steps concurrently', async () => {
      agent.setState({
        currentTask: 'Parallel downloads',
        plan: ['Download file1.txt', 'Download file2.txt', 'Merge files'],
        currentStep: 0
      });

      // Mock both downloads to succeed
      mockFunctionCalling.callFunctions
        .mockResolvedValueOnce([{ success: true, file: 'file1.txt' }])
        .mockResolvedValueOnce([{ success: true, file: 'file2.txt' }]);

      // Execute should handle parallel steps
      const result1 = await agent.executeStep();
      expect(result1.action).toContain('Download');
      
      // The agent should recognize these can be parallel
      const state = agent.getState();
      expect(state.currentStep).toBeLessThanOrEqual(2);
    });
  });

  describe('self-correction', () => {
    test('should retry failed steps with corrections', async () => {
      agent.setState({
        currentTask: 'Fix syntax error',
        plan: ['Edit file with error'],
        currentStep: 0
      });

      // First attempt fails
      mockFunctionCalling.callFunctions.mockResolvedValueOnce([{
        success: false,
        error: 'Syntax error in edit'
      }]);

      // Agent should detect error and retry
      const result1 = await agent.executeStep();
      expect(result1.error).toBeDefined();

      // Second attempt with correction succeeds
      mockFunctionCalling.callFunctions.mockResolvedValueOnce([{
        success: true
      }]);

      const result2 = await agent.executeStep();
      expect(result2.error).toBeUndefined();
      expect(result2.retryCount).toBeGreaterThan(0);
    });

    test('should give up after max retries', async () => {
      agent.setState({
        currentTask: 'Impossible task',
        plan: ['Do impossible thing'],
        currentStep: 0
      });

      // All attempts fail
      mockFunctionCalling.callFunctions.mockRejectedValue(
        new Error('Cannot do impossible thing')
      );

      let lastResult;
      for (let i = 0; i < 5; i++) {
        lastResult = await agent.executeStep();
      }

      expect(lastResult!.error).toBeDefined();
      expect(lastResult!.aborted).toBe(true);
    });
  });

  describe('observation handling', () => {
    test('should collect and store observations', async () => {
      agent.setState({
        currentTask: 'Analyze codebase',
        plan: ['List files', 'Read main file'],
        currentStep: 0
      });

      // Mock file listing
      mockFunctionCalling.callFunctions.mockResolvedValueOnce([{
        success: true,
        output: 'file1.js\nfile2.js\nindex.js'
      }]);

      await agent.executeStep();
      
      const state = agent.getState();
      expect(state.observations).toHaveLength(1);
      expect(state.observations[0]).toContain('file1.js');
    });

    test('should use observations for context', async () => {
      // Pre-populate observations
      agent.setState({
        currentTask: 'Fix bug',
        plan: ['Find bug location', 'Fix bug'],
        currentStep: 1,
        observations: ['Bug is in auth.js on line 42']
      });

      // The agent should use the observation context
      mockFunctionCalling.callFunctions.mockResolvedValueOnce([{
        success: true,
        result: 'Fixed bug in auth.js'
      }]);

      const result = await agent.executeStep();
      expect(result.completed).toBe(true);
    });
  });

  describe('complete task execution', () => {
    test('should execute entire task from plan to completion', async () => {
      const task = 'Add logging to application';
      
      // Mock successful execution of all steps
      mockFunctionCalling.callFunctions
        .mockResolvedValueOnce([{ success: true }]) // Install logger
        .mockResolvedValueOnce([{ success: true }]) // Create logger config
        .mockResolvedValueOnce([{ success: true }]) // Add logging statements
        .mockResolvedValueOnce([{ success: true }]); // Test logging

      await agent.plan(task);
      const result = await agent.execute(task);
      
      expect(result.success).toBe(true);
      expect(result.completedSteps.length).toBeGreaterThan(0);
      expect(result.errors).toHaveLength(0);
    });
  });
});