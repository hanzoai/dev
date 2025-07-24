import { spawn, ChildProcess } from 'child_process';
import * as path from 'path';
import * as fs from 'fs';
import chalk from 'chalk';
import ora from 'ora';
import { glob } from 'glob';
import { v4 as uuidv4 } from 'uuid';

interface WorkerTask {
  id: string;
  type: 'analyze' | 'edit' | 'verify';
  files: string[];
  instructions: string;
}

interface WorkerResult {
  taskId: string;
  workerId: string;
  success: boolean;
  files: string[];
  summary: string;
  errors?: string[];
}

interface SwarmWorker {
  id: string;
  process?: ChildProcess;
  status: 'idle' | 'busy' | 'error';
  currentTask?: WorkerTask;
}

export interface SwarmCoordinatorOptions {
  provider: 'claude' | 'openai' | 'gemini' | 'grok' | 'local';
  workerCount: number;
  prompt: string;
  cwd?: string;
  pattern?: string;
  maxIterations?: number;
}

export class SwarmCoordinator {
  private options: SwarmCoordinatorOptions;
  private workers: Map<string, SwarmWorker> = new Map();
  private taskQueue: WorkerTask[] = [];
  private results: WorkerResult[] = [];
  private tempDir: string;
  private iteration: number = 0;

  constructor(options: SwarmCoordinatorOptions) {
    this.options = {
      ...options,
      cwd: options.cwd || process.cwd(),
      maxIterations: options.maxIterations || 5
    };
    
    // Create temp directory for worker communication
    this.tempDir = path.join(process.cwd(), '.swarm-tmp', uuidv4());
    fs.mkdirSync(this.tempDir, { recursive: true });
  }

  async run(): Promise<void> {
    const spinner = ora('Starting swarm coordinator...').start();

    try {
      // Phase 1: Initialize main coordinator
      spinner.text = 'Initializing coordinator agent...';
      const coordinator = await this.startCoordinator();
      
      // Phase 2: Analyze the task and create work plan
      spinner.text = 'Analyzing task and creating work plan...';
      const workPlan = await this.analyzeTask(coordinator);
      
      // Phase 3: Initialize worker pool
      spinner.text = `Spawning ${this.options.workerCount} worker agents...`;
      await this.initializeWorkers();
      spinner.succeed('Worker pool initialized');

      // Phase 4: Execute work plan with iterations
      while (this.iteration < this.options.maxIterations && this.taskQueue.length > 0) {
        this.iteration++;
        console.log(chalk.cyan(`\n🔄 Iteration ${this.iteration}/${this.options.maxIterations}`));
        
        // Distribute tasks to workers
        await this.distributeTasks();
        
        // Collect results
        const iterationResults = await this.collectResults();
        
        // Send results back to coordinator for analysis
        const nextSteps = await this.coordinatorAnalyze(coordinator, iterationResults);
        
        if (nextSteps.complete) {
          console.log(chalk.green('\n✅ Task completed successfully!'));
          break;
        }
        
        // Add new tasks to queue
        this.taskQueue.push(...nextSteps.tasks);
      }

      // Phase 5: Final summary
      await this.generateFinalReport(coordinator);
      
    } catch (error) {
      spinner.fail(`Swarm error: ${error}`);
      throw error;
    } finally {
      // Cleanup
      await this.cleanup();
    }
  }

  private async startCoordinator(): Promise<ChildProcess> {
    return new Promise((resolve, reject) => {
      const coordinatorPrompt = `
You are the coordinator of a swarm of AI agents. Your role is to:
1. Analyze the main task: "${this.options.prompt}"
2. Break it down into smaller tasks for worker agents
3. Collect and analyze results from workers
4. Decide on next steps based on results
5. Determine when the overall task is complete

Working directory: ${this.options.cwd}
Number of workers available: ${this.options.workerCount}

First, analyze the task and create a work plan.
`;

      const args = this.buildCoordinatorArgs(coordinatorPrompt);
      const coordinator = spawn(this.getCommand(), args, {
        cwd: this.options.cwd,
        env: this.getEnv()
      });

      // Set up communication via temp files
      const outputPath = path.join(this.tempDir, 'coordinator-output.json');
      let output = '';

      coordinator.stdout?.on('data', (data) => {
        output += data.toString();
      });

      coordinator.stderr?.on('data', (data) => {
        console.error(chalk.red('Coordinator error:'), data.toString());
      });

      coordinator.on('error', (error) => {
        reject(new Error(`Failed to start coordinator: ${error.message}`));
      });

      // Give coordinator time to initialize
      setTimeout(() => resolve(coordinator), 2000);
    });
  }

  private async analyzeTask(coordinator: ChildProcess): Promise<WorkerTask[]> {
    // Send analysis request to coordinator
    const analysisPrompt = `
Analyze the files in the current directory and create a work plan.
Output your plan as a JSON array of tasks in the following format:
{
  "tasks": [
    {
      "type": "analyze|edit|verify",
      "files": ["file1.js", "file2.js"],
      "instructions": "Specific instructions for this task"
    }
  ]
}
`;

    // Write prompt to temp file for coordinator to read
    const promptPath = path.join(this.tempDir, 'analysis-prompt.txt');
    fs.writeFileSync(promptPath, analysisPrompt);

    // Wait for coordinator response
    const responsePath = path.join(this.tempDir, 'analysis-response.json');
    
    // In real implementation, we'd wait for the coordinator to write the response
    // For now, let's create a default work plan based on files found
    const files = await this.findFiles();
    const tasks: WorkerTask[] = [];
    
    // Group files into batches for workers
    const batchSize = Math.ceil(files.length / this.options.workerCount);
    for (let i = 0; i < files.length; i += batchSize) {
      const batch = files.slice(i, i + batchSize);
      tasks.push({
        id: uuidv4(),
        type: 'edit',
        files: batch,
        instructions: this.options.prompt
      });
    }

    this.taskQueue.push(...tasks);
    return tasks;
  }

  private async initializeWorkers(): Promise<void> {
    for (let i = 0; i < this.options.workerCount; i++) {
      const worker: SwarmWorker = {
        id: `worker-${i}`,
        status: 'idle'
      };
      this.workers.set(worker.id, worker);
    }
  }

  private async distributeTasks(): Promise<void> {
    const availableWorkers = Array.from(this.workers.values()).filter(w => w.status === 'idle');
    
    for (const worker of availableWorkers) {
      if (this.taskQueue.length === 0) break;
      
      const task = this.taskQueue.shift()!;
      worker.currentTask = task;
      worker.status = 'busy';
      
      // Start worker process
      await this.startWorker(worker, task);
    }
  }

  private async startWorker(worker: SwarmWorker, task: WorkerTask): Promise<void> {
    const workerPrompt = `
You are worker ${worker.id} in a swarm of AI agents.
Your task: ${task.instructions}
Files to process: ${task.files.join(', ')}

Complete the task and report your results.
`;

    const args = this.buildWorkerArgs(workerPrompt, task.files);
    const process = spawn(this.getCommand(), args, {
      cwd: this.options.cwd,
      env: this.getEnv()
    });

    worker.process = process;

    let output = '';
    process.stdout?.on('data', (data) => {
      output += data.toString();
    });

    process.on('close', (code) => {
      // Save result
      const result: WorkerResult = {
        taskId: task.id,
        workerId: worker.id,
        success: code === 0,
        files: task.files,
        summary: output,
        errors: code !== 0 ? [output] : undefined
      };
      
      this.results.push(result);
      worker.status = 'idle';
      worker.currentTask = undefined;
    });
  }

  private async collectResults(): Promise<WorkerResult[]> {
    // Wait for all busy workers to complete
    await new Promise<void>((resolve) => {
      const checkInterval = setInterval(() => {
        const busyWorkers = Array.from(this.workers.values()).filter(w => w.status === 'busy');
        if (busyWorkers.length === 0) {
          clearInterval(checkInterval);
          resolve();
        }
      }, 1000);
    });

    // Return results from this iteration
    const iterationResults = [...this.results];
    this.results = []; // Clear for next iteration
    return iterationResults;
  }

  private async coordinatorAnalyze(coordinator: ChildProcess, results: WorkerResult[]): Promise<{
    complete: boolean;
    tasks: WorkerTask[];
  }> {
    // Send results to coordinator for analysis
    const analysisPrompt = `
Worker results from iteration ${this.iteration}:
${JSON.stringify(results, null, 2)}

Analyze these results and determine:
1. Is the overall task complete?
2. If not, what additional tasks need to be performed?

Respond with JSON:
{
  "complete": true/false,
  "tasks": [...] // New tasks if not complete
}
`;

    // In real implementation, communicate with coordinator
    // For now, assume task is complete after first iteration
    return {
      complete: true,
      tasks: []
    };
  }

  private async generateFinalReport(coordinator: ChildProcess): Promise<void> {
    console.log(chalk.bold.cyan('\n📊 Swarm Execution Summary\n'));
    console.log(chalk.white('Total iterations:'), this.iteration);
    console.log(chalk.white('Workers used:'), this.options.workerCount);
    console.log(chalk.white('Tasks completed:'), this.results.length);
    
    // Show results summary
    const successful = this.results.filter(r => r.success).length;
    const failed = this.results.filter(r => !r.success).length;
    
    console.log(chalk.green('Successful:'), successful);
    if (failed > 0) {
      console.log(chalk.red('Failed:'), failed);
    }
  }

  private async cleanup(): Promise<void> {
    // Kill any remaining processes
    for (const worker of this.workers.values()) {
      if (worker.process) {
        worker.process.kill();
      }
    }
    
    // Remove temp directory
    if (fs.existsSync(this.tempDir)) {
      fs.rmSync(this.tempDir, { recursive: true, force: true });
    }
  }

  private async findFiles(): Promise<string[]> {
    // Reuse logic from original swarm-runner
    return new Promise((resolve, reject) => {
      const options = {
        cwd: this.options.cwd,
        nodir: true,
        ignore: [
          '**/node_modules/**',
          '**/.git/**',
          '**/dist/**',
          '**/build/**'
        ]
      };

      glob(this.options.pattern || '**/*', options, (err, files) => {
        if (err) {
          reject(err);
        } else {
          const editableFiles = files.filter(file => {
            const ext = path.extname(file);
            return ['.js', '.ts', '.jsx', '.tsx', '.py', '.java', '.go', '.rs', '.md', '.txt', '.json', '.yaml', '.yml'].includes(ext);
          });
          resolve(editableFiles);
        }
      });
    });
  }

  private getCommand(): string {
    switch (this.options.provider) {
      case 'claude': return 'claude';
      case 'openai': return 'openai';
      case 'gemini': return 'gemini';
      case 'grok': return 'grok';
      case 'local': return 'dev';
      default: return 'claude';
    }
  }

  private buildCoordinatorArgs(prompt: string): string[] {
    // Build args specific to each provider for the coordinator
    switch (this.options.provider) {
      case 'claude':
        return ['-p', prompt, '--thinking'];
      case 'local':
        return ['agent', '-p', prompt];
      default:
        return [prompt];
    }
  }

  private buildWorkerArgs(prompt: string, files: string[]): string[] {
    // Build args specific to each provider for workers
    switch (this.options.provider) {
      case 'claude':
        return ['-p', prompt, ...files];
      case 'local':
        return ['agent', '-p', prompt, ...files];
      default:
        return [prompt, ...files];
    }
  }

  private getEnv(): NodeJS.ProcessEnv {
    return {
      ...process.env,
      ANTHROPIC_API_KEY: process.env.ANTHROPIC_API_KEY,
      OPENAI_API_KEY: process.env.OPENAI_API_KEY,
      GOOGLE_API_KEY: process.env.GOOGLE_API_KEY,
      GROK_API_KEY: process.env.GROK_API_KEY
    };
  }
}