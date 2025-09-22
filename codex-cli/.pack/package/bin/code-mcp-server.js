#!/usr/bin/env node

process.env.CODE_NATIVE_BINARY = process.env.CODE_NATIVE_BINARY || 'code-mcp-server';
process.env.CODE_NATIVE_COMMAND = process.env.CODE_NATIVE_COMMAND || 'code-mcp-server';

await import('./coder.js');
