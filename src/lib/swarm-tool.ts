import { spawn, ChildProcess } from 'child_process';
import * as path from 'path';
import chalk from 'chalk';
import { v4 as uuidv4 } from 'uuid';

export interface SwarmTask {
  id: string;
  files: string[];
  instructions: string;
  provider: string;
}

export interface SwarmResult {
  taskId: string;
  success: boolean;
  filesProcessed: string[];
  summary: string;
  errors?: string[];
}

export class SwarmTool {
  private activeWorkers: Map<string, ChildProcess> = new Map();

  /**
   * Execute tasks in parallel using worker agents
   * This tool is called by the main agent loop to distribute work
   */
  async executeParallelTasks(tasks: SwarmTask[]): Promise<SwarmResult[]> {
    console.log(chalk.cyan(`\n🐝 Distributing ${tasks.length} tasks to swarm workers...\n`));
    
    const results: SwarmResult[] = [];
    const promises = tasks.map(task => this.executeTask(task));
    
    try {
      const taskResults = await Promise.all(promises);
      results.push(...taskResults);
    } catch (error) {
      console.error(chalk.red('Swarm execution error:'), error);
    }

    return results;
  }

  /**
   * Execute a single task with a worker agent
   */
  private async executeTask(task: SwarmTask): Promise<SwarmResult> {
    return new Promise((resolve) => {
      const workerId = `worker-${uuidv4().slice(0, 8)}`;
      console.log(chalk.gray(`[${workerId}] Processing ${task.files.length} files...`));

      // Build command based on provider
      const { cmd, args } = this.buildCommand(task);
      
      // Spawn worker process
      const worker = spawn(cmd, args, {
        cwd: process.cwd(),
        env: {
          ...process.env,
          // Ensure worker runs in non-interactive mode
          CLAUDE_CODE_PERMISSION_MODE: 'acceptEdits'
        }
      });

      this.activeWorkers.set(workerId, worker);

      let output = '';
      let errorOutput = '';

      worker.stdout?.on('data', (data) => {
        output += data.toString();
      });

      worker.stderr?.on('data', (data) => {
        errorOutput += data.toString();
      });

      worker.on('close', (code) => {
        this.activeWorkers.delete(workerId);
        
        const result: SwarmResult = {
          taskId: task.id,
          success: code === 0,
          filesProcessed: task.files,
          summary: output || 'No output captured',
          errors: code !== 0 ? [errorOutput || `Process exited with code ${code}`] : undefined
        };

        if (code === 0) {
          console.log(chalk.green(`[${workerId}] ✓ Completed successfully`));
        } else {
          console.log(chalk.red(`[${workerId}] ✗ Failed with code ${code}`));
        }

        resolve(result);
      });

      // Add timeout to prevent hanging
      setTimeout(() => {
        if (this.activeWorkers.has(workerId)) {
          console.log(chalk.yellow(`[${workerId}] Timeout - killing worker`));
          worker.kill();
          this.activeWorkers.delete(workerId);
          
          resolve({
            taskId: task.id,
            success: false,
            filesProcessed: task.files,
            summary: 'Worker timed out',
            errors: ['Task execution timed out after 5 minutes']
          });
        }
      }, 5 * 60 * 1000); // 5 minute timeout per task
    });
  }

  /**
   * Build command and arguments based on provider
   */
  private buildCommand(task: SwarmTask): { cmd: string; args: string[] } {
    const prompt = `${task.instructions}\n\nFiles to process:\n${task.files.join('\n')}`;

    switch (task.provider) {
      case 'claude':
        return {
          cmd: 'claude',
          args: [
            '-p', prompt,
            '--max-turns', '5',
            '--thinking',
            ...task.files
          ]
        };

      case 'openai':
        return {
          cmd: 'openai',
          args: ['chat', '--prompt', prompt, ...task.files]
        };

      case 'gemini':
        return {
          cmd: 'gemini',
          args: ['edit', '--prompt', prompt, ...task.files]
        };

      case 'local':
        return {
          cmd: 'dev',
          args: ['agent', '-p', prompt, ...task.files]
        };

      default:
        return {
          cmd: 'claude',
          args: ['-p', prompt, ...task.files]
        };
    }
  }

  /**
   * Create task batches for parallel execution
   * Called by the main agent to divide work
   */
  createTaskBatches(files: string[], batchSize: number, instructions: string, provider: string): SwarmTask[] {
    const tasks: SwarmTask[] = [];
    
    for (let i = 0; i < files.length; i += batchSize) {
      const batch = files.slice(i, i + batchSize);
      tasks.push({
        id: uuidv4(),
        files: batch,
        instructions,
        provider
      });
    }

    return tasks;
  }

  /**
   * Cleanup any remaining workers
   */
  cleanup(): void {
    for (const [workerId, worker] of this.activeWorkers) {
      console.log(chalk.yellow(`Cleaning up worker ${workerId}`));
      worker.kill();
    }
    this.activeWorkers.clear();
  }
}