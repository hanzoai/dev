# Skills

> Warning: this is an experimental feature and may change.

Hanzo Dev can discover reusable "skills" on disk. A skill is a small bundle with a name, short description, and optional body instructions. Runtime context includes only name, description, and file path; the full body stays on disk.

## Enable skills

Skills are behind the `skills` feature flag and enabled by default.

- Disable in config (preferred): add to `$HANZO_HOME/config.toml` (usually `~/.hanzo/config.toml`):

  ```toml
  [features]
  skills = false
  ```

- Override for a single run:

  ```bash
  dev -c features.skills=true
  ```

## Where skills live

Discovery roots (highest precedence first):

- Repo: nearest `.hanzo/skills/**/SKILL.md` from current directory up to repo root.
- User: `$HANZO_HOME/skills/**/SKILL.md` (default: `~/.hanzo/skills/**/SKILL.md`).
- System: bundled skills under `$HANZO_HOME/skills/.system/**/SKILL.md`.

Behavior:

- Recursive scan; only files named exactly `SKILL.md` are loaded.
- Hidden entries are skipped.
- Symlinks are skipped.
- Duplicate skill names are deduped by precedence above (first match wins).
- Render order is by name, then path.

## File format

- YAML frontmatter + body.
- Required frontmatter fields:
  - `name` (non-empty, <= 64 chars, single-line normalized)
  - `description` (non-empty, <= 1024 chars, single-line normalized)
- Extra keys are ignored.

## Loading and rendering

- Skills load once at startup.
- When valid skills exist, Hanzo Dev appends a runtime-only `## Skills` section after `AGENTS.md`.
- Invalid skills are ignored and surfaced as startup errors in the TUI.

## Create a skill

1. Create `~/.hanzo/skills/<skill-name>/`.
2. Add `SKILL.md`:

   ```markdown
   ---
   name: your-skill-name
   description: what it does and when to use it
   ---

   # Optional body
   Add instructions, references, examples, or scripts (kept on disk).
   ```

3. Restart Hanzo Dev.
