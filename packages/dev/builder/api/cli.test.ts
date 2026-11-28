/**
 * A run's transcript reads as `dev` prints it in its terminal. The exploration cases
 * are hanzoai/dev's own TUI snapshots, word for word
 * (codex-rs/tui/src/history_cell/snapshots/codex_tui__history_cell__tests__*.snap);
 * the rest are the TUI's headers for a command, a patch and an MCP call, from the
 * same exec stream a coding run in the cloud narrates in.
 */
import { describe, expect, it } from 'vitest'

import { shown } from './cli.ts'
import { cards, printed } from './turn.ts'

let seq = 0
/** One item of `dev exec --json`, completed, as a run's session records it. */
const done = (item: Record<string, unknown>) => ({ seq: ++seq, kind: 'event', payload: { type: 'item.completed', item: { id: `i${seq}`, ...item } } })
const cmd = (command_actions: unknown[], extra: Record<string, unknown> = {}) =>
  done({ type: 'command_execution', command: "/bin/zsh -lc 'echo'", aggregated_output: '', exit_code: 0, status: 'completed', command_actions, ...extra })
const read = (name: string) => ({ type: 'read', command: `cat ${name}`, name, path: name })
const search = (query: string, path: string | null = null) => ({ type: 'search', command: `rg ${query}`, query, path })

describe('a run’s transcript, as dev prints it', () => {
  it('coalesces reads across calls (coalesces_reads_across_multiple_calls)', () => {
    expect(printed(cards([cmd([search('shimmer_spans')]), cmd([read('shimmer.rs')]), cmd([read('status_indicator_widget.rs')])]))).toBe(
      ['• Explored', '  └ Search shimmer_spans', '    Read shimmer.rs, status_indicator_widget.rs'].join('\n'),
    )
  })

  it('says each action of one call on its own line (coalesces_sequential_reads_within_one_call)', () => {
    expect(printed(cards([cmd([search('shimmer_spans'), read('shimmer.rs'), read('status_indicator_widget.rs')])]))).toBe(
      ['• Explored', '  └ Search shimmer_spans', '    Read shimmer.rs', '    Read status_indicator_widget.rs'].join('\n'),
    )
  })

  it('names each file read once (coalesced_reads_dedupe_names)', () => {
    expect(printed(cards([cmd([read('auth.rs'), read('auth.rs'), read('shimmer.rs')])]))).toBe(['• Explored', '  └ Read auth.rs, shimmer.rs'].join('\n'))
  })

  it('keeps one block across the reasoning between explorations, and says a failed one’s exit', () => {
    const think = done({ type: 'reasoning', text: 'Looking for the widget' })
    const list = { type: 'listFiles', command: 'ls src', path: 'src' }
    expect(printed(cards([cmd([list]), think, cmd([search('widget', 'src')], { exit_code: 1 })]))).toBe(
      ['• Explored', '  └ List src', '    Search widget in src (exit 1)'].join('\n'),
    )
  })

  it('says Ran with the script the shell ran, its output, and Edited and Called as the CLI does', () => {
    const run = done({ type: 'command_execution', command: "/bin/zsh -lc 'cargo test -p widget'", aggregated_output: 'ok\n', exit_code: 0, status: 'completed', command_actions: [{ type: 'unknown', command: 'cargo test -p widget' }] })
    const quiet = done({ type: 'command_execution', command: 'bash -lc "make fmt"', aggregated_output: '', exit_code: 0, status: 'completed', command_actions: [] })
    const one = done({ type: 'file_change', status: 'completed', changes: [{ path: '/work/src/lib.rs', kind: 'update', added: 2, removed: 1 }] })
    const two = done({
      type: 'file_change',
      status: 'completed',
      changes: [
        { path: 'a.rs', kind: 'add', added: 1, removed: 0 },
        { path: 'b.rs', kind: 'update', added: 2, removed: 1 },
      ],
    })
    const call = done({ type: 'mcp_tool_call', server: 'hanzo', tool: 'search', arguments: { q: 'widget' }, status: 'completed', result: { content: [{ type: 'text', text: '3 hits' }] } })
    expect(printed(cards([run, quiet, one, two, call]))).toBe(
      [
        '• Ran cargo test -p widget',
        '  └ ok',
        '• Ran make fmt',
        '  └ (no output)',
        '• Edited src/lib.rs (+2 -1)',
        '• Edited 2 files (+3 -1)',
        '  └ a.rs (+1 -0)',
        '  └ b.rs (+2 -1)',
        '• Called hanzo.search({"q":"widget"})',
      ].join('\n'),
    )
  })

  it('says a file added or deleted alone with its own verb', () => {
    const added = done({ type: 'file_change', status: 'completed', changes: [{ path: 'new.rs', kind: 'add', added: 4, removed: 0 }] })
    const gone = done({ type: 'file_change', status: 'completed', changes: [{ path: 'old.rs', kind: 'delete', added: 0, removed: 9 }] })
    expect(printed(cards([added, gone]))).toBe(['• Added new.rs (+4 -0)', '• Deleted old.rs (+0 -9)'].join('\n'))
  })

  it('takes a shell’s wrapper off and nothing else', () => {
    expect(shown("/bin/zsh -lc 'ls -la'")).toBe('ls -la')
    expect(shown(`bash -lc 'echo '"'"'hi'"'"''`)).toBe("echo 'hi'")
    expect(shown('sh -c "make test"')).toBe('make test')
    expect(shown('git status')).toBe('git status')
  })
})
