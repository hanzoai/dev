/**
 * Tool Registry - Central registry for all MCP tools
 */

import { Tool, ToolResult, ToolParams, ToolCategory } from './types';
import { McpBridge } from './bridge';

export class ToolRegistry {
  private tools: Map<string, Tool> = new Map();
  private bridge: McpBridge;

  constructor() {
    this.bridge = new McpBridge();
    this.registerAllTools();
  }

  /**
   * Register all available tools
   */
  private registerAllTools(): void {
    // This will be populated from the Rust bridge
    const tools = this.bridge.listTools();
    tools.forEach(tool => {
      this.tools.set(tool.name, tool);
    });
  }

  /**
   * List all available tools
   */
  listTools(): Tool[] {
    return Array.from(this.tools.values());
  }

  /**
   * List tools by category
   */
  listToolsByCategory(category: ToolCategory): Tool[] {
    return this.listTools().filter(tool => tool.category === category);
  }

  /**
   * Get a specific tool
   */
  getTool(name: string): Tool | undefined {
    return this.tools.get(name);
  }

  /**
   * Execute a tool
   */
  async execute(name: string, params: ToolParams): Promise<ToolResult> {
    const tool = this.tools.get(name);
    if (!tool) {
      return {
        success: false,
        error: {
          code: 'TOOL_NOT_FOUND',
          message: `Tool '${name}' not found`
        }
      };
    }

    try {
      // Validate params against schema
      this.validateParams(tool, params);
      
      // Execute via bridge
      const result = await this.bridge.execute(name, params);
      return {
        success: true,
        data: result
      };
    } catch (error: any) {
      return {
        success: false,
        error: {
          code: 'EXECUTION_ERROR',
          message: error.message || 'Tool execution failed',
          details: error
        }
      };
    }
  }

  /**
   * Validate parameters against tool schema
   */
  private validateParams(tool: Tool, params: ToolParams): void {
    const schema = tool.schema;
    
    // Check required parameters
    if (schema.required) {
      for (const required of schema.required) {
        if (!(required in params)) {
          throw new Error(`Missing required parameter: ${required}`);
        }
      }
    }

    // Validate parameter types
    for (const [key, value] of Object.entries(params)) {
      if (schema.properties && key in schema.properties) {
        const propSchema = schema.properties[key];
        this.validateParamType(key, value, propSchema);
      }
    }
  }

  /**
   * Validate individual parameter type
   */
  private validateParamType(key: string, value: any, schema: any): void {
    if (schema.type) {
      const actualType = Array.isArray(value) ? 'array' : typeof value;
      if (schema.type !== actualType) {
        throw new Error(`Parameter '${key}' must be of type ${schema.type}, got ${actualType}`);
      }
    }
  }
}