/**
 * Core types for MCP tools
 */

export interface Tool {
  name: string;
  description: string;
  category: ToolCategory;
  schema: ToolSchema;
}

export interface ToolSchema {
  type: 'object';
  properties: Record<string, any>;
  required?: string[];
}

export type ToolCategory = 
  | 'file'
  | 'search'
  | 'shell'
  | 'edit'
  | 'git'
  | 'ast'
  | 'browser'
  | 'ai'
  | 'project';

export interface ToolResult {
  success: boolean;
  data?: any;
  error?: ToolError;
}

export interface ToolError {
  code: string;
  message: string;
  details?: any;
}

export interface ToolParams {
  [key: string]: any;
}