import * as fs from 'fs';
import * as path from 'path';
import * as os from 'os';
import * as toml from '@iarna/toml';

export interface LLMConfig {
  model: string;
  apiKey?: string;
  baseUrl?: string;
  temperature?: number;
  maxTokens?: number;
  provider?: 'openai' | 'anthropic' | 'google' | 'azure' | 'local';
}

export interface AgentConfig {
  name: string;
  llmConfig?: LLMConfig;
  memoryEnabled?: boolean;
  microagentsEnabled?: boolean;
}

export interface SecurityConfig {
  confirmationMode: boolean;
  sandboxMode?: boolean;
  allowedCommands?: string[];
}

export interface SandboxConfig {
  workspaceBase: string;
  selectedRepo?: string;
  useHost?: boolean;
}

export interface HanzoDevConfig {
  // Core settings
  defaultAgent: string;
  agents: AgentConfig[];
  llm: LLMConfig;
  security: SecurityConfig;
  sandbox: SandboxConfig;
  
  // CLI specific
  cliMultilineInput?: boolean;
  runtime?: 'cli' | 'docker' | 'local';
  
  // Session
  sessionName?: string;
  resumeSession?: boolean;
}

export class ConfigManager {
  private configPath: string;
  private config: HanzoDevConfig;

  constructor() {
    this.configPath = this.getConfigPath();
    this.config = this.loadConfig();
  }

  private getConfigPath(): string {
    // Check for config in order of precedence
    const locations = [
      path.join(process.cwd(), 'hanzo-dev.toml'),
      path.join(process.cwd(), '.hanzo-dev', 'config.toml'),
      path.join(os.homedir(), '.config', 'hanzo-dev', 'config.toml'),
    ];

    for (const loc of locations) {
      if (fs.existsSync(loc)) {
        return loc;
      }
    }

    // Default location
    return path.join(os.homedir(), '.config', 'hanzo-dev', 'config.toml');
  }

  private loadConfig(): HanzoDevConfig {
    // Default configuration
    const defaultConfig: HanzoDevConfig = {
      defaultAgent: 'CodeActAgent',
      agents: [{
        name: 'CodeActAgent',
        memoryEnabled: true,
        microagentsEnabled: true,
      }],
      llm: {
        model: 'gpt-4',
        provider: 'openai',
        temperature: 0.7,
      },
      security: {
        confirmationMode: true,
        sandboxMode: true,
      },
      sandbox: {
        workspaceBase: process.cwd(),
        useHost: false,
      },
      runtime: 'cli',
      cliMultilineInput: false,
    };

    // Load from file if exists
    if (fs.existsSync(this.configPath)) {
      try {
        const content = fs.readFileSync(this.configPath, 'utf-8');
        const parsed = toml.parse(content) as Partial<HanzoDevConfig>;
        return { ...defaultConfig, ...parsed };
      } catch (error) {
        console.error('Error loading config:', error);
        return defaultConfig;
      }
    }

    return defaultConfig;
  }

  public getConfig(): HanzoDevConfig {
    return this.config;
  }

  public updateConfig(updates: Partial<HanzoDevConfig>): void {
    this.config = { ...this.config, ...updates };
    this.saveConfig();
  }

  private saveConfig(): void {
    const configDir = path.dirname(this.configPath);
    if (!fs.existsSync(configDir)) {
      fs.mkdirSync(configDir, { recursive: true });
    }

    const content = toml.stringify(this.config as any);
    fs.writeFileSync(this.configPath, content);
  }

  // Load LLM config from environment
  public loadLLMConfigFromEnv(): LLMConfig {
    const config = this.config.llm;

    // Check for API keys in environment
    if (process.env.OPENAI_API_KEY) {
      config.apiKey = process.env.OPENAI_API_KEY;
      config.provider = 'openai';
    } else if (process.env.ANTHROPIC_API_KEY) {
      config.apiKey = process.env.ANTHROPIC_API_KEY;
      config.provider = 'anthropic';
      config.model = 'claude-3-sonnet-20240229';
    } else if (process.env.GOOGLE_API_KEY || process.env.GEMINI_API_KEY) {
      config.apiKey = process.env.GOOGLE_API_KEY || process.env.GEMINI_API_KEY;
      config.provider = 'google';
      config.model = 'gemini-pro';
    }

    // Check for base URL
    if (process.env.LLM_BASE_URL) {
      config.baseUrl = process.env.LLM_BASE_URL;
    }

    return config;
  }
}