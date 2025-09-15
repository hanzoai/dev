/**
 * @hanzo/mcp - Model Context Protocol tools for Hanzo Dev
 * 
 * This package provides TypeScript bindings for the MCP tools implemented in Rust.
 * It includes both native Rust bindings (when available) and JavaScript fallbacks.
 */

import { ToolRegistry } from './registry';
import { McpBridge } from './bridge';
import * as tools from './tools';

export { ToolRegistry, McpBridge, tools };

// Export individual tool categories
export { FileTools } from './tools/file';
export { SearchTools } from './tools/search';
export { ShellTools } from './tools/shell';
export { EditTools } from './tools/edit';
export { GitTools } from './tools/git';
export { AstTools } from './tools/ast';
export { BrowserTools } from './tools/browser';
export { AiTools } from './tools/ai';
export { ProjectTools } from './tools/project';

// Export types
export type {
  Tool,
  ToolResult,
  ToolError,
  ToolSchema,
  ToolCategory
} from './types';

// Default export
export default {
  ToolRegistry,
  McpBridge,
  tools
};