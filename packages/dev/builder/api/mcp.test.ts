import { describe, expect, it } from 'vitest'

import { Refusal } from './call.ts'
import { nativesOf } from './mcp.ts'

const list = {
  jsonrpc: '2.0',
  id: 1,
  result: {
    tools: [
      { name: 'describe', description: 'one schema' },
      {
        name: 'git',
        description: 'git: 2 operations.',
        inputSchema: { properties: { op: { enum: ['list_git_repos', 'get_git_repo', ''] } } },
      },
      { name: 'code', description: 'code', inputSchema: { properties: { op: { enum: ['ask'] } } } },
    ],
  },
}

describe('native MCP servers', () => {
  it('reads each server and the operations it names', () => {
    expect(nativesOf(list)).toEqual([
      { name: 'git', description: 'git: 2 operations.', ops: ['list_git_repos', 'get_git_repo'] },
      { name: 'code', description: 'code', ops: ['ask'] },
    ])
  })

  it('refuses an answer that is not a tool list', () => {
    expect(() => nativesOf({ jsonrpc: '2.0', error: { message: 'no' } })).toThrow(Refusal)
    expect(() => nativesOf({ result: {} })).toThrow(Refusal)
  })
})
