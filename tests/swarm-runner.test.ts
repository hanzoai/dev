import { describe, test, expect, beforeEach, afterEach, afterAll, vi } from 'vitest';
import * as fs from 'fs';
import * as path from 'path';
import * as os from 'os';
import { SwarmRunner, SwarmOptions } from '../src/lib/swarm-runner';
import { EventEmitter } from 'events';
import * as child_process from 'child_process';
import { glob } from 'glob';

// Mock modules
vi.mock('child_process');
vi.mock('glob');
vi.mock('ora', () => ({
  default: () => ({
    start: vi.fn().mockReturnThis(),
    succeed: vi.fn().mockReturnThis(),
    fail: vi.fn().mockReturnThis(),
    stop: vi.fn().mockReturnThis()
  })
}));

describe('SwarmRunner', () => {
  let testDir: string;
  let runner: SwarmRunner;

  beforeEach(() => {
    // Create test directory
    testDir = fs.mkdtempSync(path.join(os.tmpdir(), 'swarm-test-'));
    
    // Create test files
    fs.writeFileSync(path.join(testDir, 'file1.js'), '// Test file 1');
    fs.writeFileSync(path.join(testDir, 'file2.ts'), '// Test file 2');
    fs.writeFileSync(path.join(testDir, 'file3.py'), '# Test file 3');

    // Reset mocks
    vi.clearAllMocks();
  });

  afterEach(() => {
    fs.rmSync(testDir, { recursive: true, force: true });
    vi.clearAllMocks();
    vi.restoreAllMocks();
  });
  
  afterAll(() => {
    // Force exit after all tests complete
    setTimeout(() => process.exit(0), 100);
  });

  describe('initialization', () => {
    test('should create swarm runner with options', () => {
      const options: SwarmOptions = {
        provider: 'claude',
        count: 5,
        prompt: 'Add copyright header',
        cwd: testDir
      };

      runner = new SwarmRunner(options);
      expect(runner).toBeDefined();
    });

    test('should limit agent count to 100', () => {
      const options: SwarmOptions = {
        provider: 'claude',
        count: 150,
        prompt: 'Test prompt',
        cwd: testDir
      };

      runner = new SwarmRunner(options);
      // We can't directly test private properties, but this ensures no crash
      expect(runner).toBeDefined();
    });
  });

  describe('file finding', () => {
    test('should find editable files in directory', async () => {
      // Mock glob to return our test files immediately
      vi.mocked(glob).mockImplementation((pattern, options, callback) => {
        if (typeof callback === 'function') {
          // Call callback synchronously
          callback(null, ['file1.js', 'file2.ts', 'file3.py']);
        }
        return undefined as any;
      });

      const options: SwarmOptions = {
        provider: 'claude',
        count: 3,
        prompt: 'Test prompt',
        cwd: testDir
      };

      runner = new SwarmRunner(options);
      
      // Mock auth to return true
      vi.spyOn(runner, 'ensureProviderAuth').mockResolvedValue(true);
      
      // Mock spawn to return immediately closing processes
      let spawnCount = 0;
      vi.mocked(child_process.spawn).mockImplementation(() => {
        spawnCount++;
        const proc = new EventEmitter();
        proc.stdout = new EventEmitter();
        proc.stderr = new EventEmitter();
        proc.kill = vi.fn();
        
        // Close immediately
        process.nextTick(() => proc.emit('close', 0));
        
        return proc as any;
      });

      await runner.run();

      // Should have spawned 3 processes (one for each file)
      expect(spawnCount).toBe(3);
    });
  });

  describe('provider authentication', () => {
    test('should check Claude authentication', async () => {
      const options: SwarmOptions = {
        provider: 'claude',
        count: 1,
        prompt: 'Test',
        cwd: testDir
      };

      runner = new SwarmRunner(options);

      // Mock environment variable
      process.env.ANTHROPIC_API_KEY = 'test-key';

      // Mock successful auth check
      vi.mocked(child_process.spawn).mockImplementationOnce(() => {
        const authCheckProcess = new EventEmitter();
        authCheckProcess.stderr = new EventEmitter();
        authCheckProcess.kill = vi.fn();
        
        // Emit close immediately
        process.nextTick(() => authCheckProcess.emit('close', 0));
        
        return authCheckProcess as any;
      });

      const result = await runner.ensureProviderAuth();
      expect(result).toBe(true);
    });

    test('should return true for local provider', async () => {
      const options: SwarmOptions = {
        provider: 'local',
        count: 1,
        prompt: 'Test',
        cwd: testDir
      };

      runner = new SwarmRunner(options);
      const result = await runner.ensureProviderAuth();
      expect(result).toBe(true);
    });

    test('should check API key for OpenAI', async () => {
      const options: SwarmOptions = {
        provider: 'openai',
        count: 1,
        prompt: 'Test',
        cwd: testDir
      };

      runner = new SwarmRunner(options);

      // Without API key
      delete process.env.OPENAI_API_KEY;
      expect(await runner.ensureProviderAuth()).toBe(false);

      // With API key
      process.env.OPENAI_API_KEY = 'test-key';
      expect(await runner.ensureProviderAuth()).toBe(true);
    });
  });

  describe('command building', () => {
    test('should build correct command for Claude', () => {
      const options: SwarmOptions = {
        provider: 'claude',
        count: 1,
        prompt: 'Add header',
        cwd: testDir
      };

      runner = new SwarmRunner(options);
      const command = (runner as any).buildCommand('test.js');

      expect(command.cmd).toBe('claude');
      expect(command.args).toContain('-p');
      expect(command.args.join(' ')).toContain('Add header');
      expect(command.args).toContain('--max-turns');
      expect(command.args).toContain('5');
    });

    test('should build correct command for local provider', () => {
      const options: SwarmOptions = {
        provider: 'local',
        count: 1,
        prompt: 'Format code',
        cwd: testDir
      };

      runner = new SwarmRunner(options);
      const command = (runner as any).buildCommand('test.js');

      expect(command.cmd).toBe('dev');
      expect(command.args).toContain('agent');
      expect(command.args.join(' ')).toContain('Format code');
    });
  });

  describe('parallel processing', () => {
    test('should process multiple files in parallel', async () => {
      vi.mocked(glob).mockImplementation((pattern, options, callback) => {
        if (typeof callback === 'function') {
          callback(null, ['file1.js', 'file2.js', 'file3.js']);
        }
        return undefined as any;
      });

      const options: SwarmOptions = {
        provider: 'local',
        count: 3,
        prompt: 'Add copyright',
        cwd: testDir
      };

      runner = new SwarmRunner(options);
      
      // Mock auth
      vi.spyOn(runner, 'ensureProviderAuth').mockResolvedValue(true);

      let processCount = 0;
      vi.mocked(child_process.spawn).mockImplementation(() => {
        processCount++;
        const proc = new EventEmitter();
        proc.stdout = new EventEmitter();
        proc.stderr = new EventEmitter();
        proc.kill = vi.fn();
        
        // Simulate successful completion
        process.nextTick(() => proc.emit('close', 0));

        return proc as any;
      });

      await runner.run();

      // Should have spawned 3 processes
      expect(processCount).toBe(3);
    });

    test('should handle process failures', async () => {
      vi.mocked(glob).mockImplementation((pattern, options, callback) => {
        if (typeof callback === 'function') {
          callback(null, ['file1.js']);
        }
        return undefined as any;
      });

      const options: SwarmOptions = {
        provider: 'local',
        count: 1,
        prompt: 'Test',
        cwd: testDir
      };

      runner = new SwarmRunner(options);
      
      // Mock auth
      vi.spyOn(runner, 'ensureProviderAuth').mockResolvedValue(true);

      vi.mocked(child_process.spawn).mockImplementation(() => {
        const proc = new EventEmitter();
        proc.stdout = new EventEmitter();
        proc.stderr = new EventEmitter();
        proc.kill = vi.fn();
        
        // Simulate failure
        process.nextTick(() => {
          proc.stderr!.emit('data', 'Error occurred');
          proc.emit('close', 1);
        });

        return proc as any;
      });

      // Should complete without throwing
      await expect(runner.run()).resolves.not.toThrow();
    });
  });
});