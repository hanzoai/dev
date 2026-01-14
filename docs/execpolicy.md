# Execution policy

Hanzo Dev can enforce your own rules-based execution policy before it runs shell commands. Policies live in Starlark `.codexpolicy` files under `~/.hanzo/policy` (Hanzo Dev still reads legacy `~/.code/policy` or `~/.codex/policy` for backward compatibility).

## How to create and edit rules

### TUI interactions

Hanzo Dev CLI will present the option to whitelist commands when a command causes a prompt.

<img width="513" height="168" alt="Screenshot 2025-12-04 at 9 23 54 AM" src="https://github.com/user-attachments/assets/4c8ee8ea-3101-4a81-bb13-3f4a9aa02502" />

Whitelisted commands will no longer require your permission to run in current and subsequent sessions.

Under the hood, when you approve and whitelist a command, Hanzo Dev will edit `~/.hanzo/rules/default.rules` (legacy: `~/.code/rules/default.rules` or `~/.codex/rules/default.rules`).

### Editing `.rules` files

1. Create a policy directory: `mkdir -p ~/.hanzo/policy`.
2. Add one or more `.codexpolicy` files in that folder. Hanzo Dev automatically loads every `.codexpolicy` file in there on startup.
3. Write `prefix_rule` entries to describe the commands you want to allow, prompt, or block:

```starlark
prefix_rule(
    pattern = ["git", ["push", "fetch"]],
    decision = "prompt",  # allow | prompt | forbidden
    match = [["git", "push", "origin", "main"]],  # examples that must match
    not_match = [["git", "status"]],              # examples that must not match
)
```

- `pattern` is a list of shell tokens, evaluated from left to right; wrap tokens in a nested list to express alternatives (for example, match both `push` and `fetch`).
- `decision` sets the severity; Hanzo Dev picks the strictest decision when multiple rules match (forbidden > prompt > allow).
- `match` and `not_match` act as optional unit tests. Hanzo Dev validates them when it loads your policy, so you get feedback if an example has unexpected behavior.

In this example rule, if Hanzo Dev wants to run commands with the prefix `git push` or `git fetch`, it will first ask for user approval.

## Preview decisions

Use the `dev execpolicy check` subcommand to preview decisions before you save a rule (see the [`dev-execpolicy` README](../code-rs/execpolicy/README.md) for syntax details):

```shell
dev execpolicy check --policy ~/.hanzo/policy/default.codexpolicy git push origin main
```

Pass multiple `--policy` flags to test how several files combine, and use `--pretty` for formatted JSON output. See the [`code-rs/execpolicy` README](../code-rs/execpolicy/README.md) for a more detailed walkthrough of the available syntax.

Example output when a rule matches:

```json
{
  "matchedRules": [
    {
      "prefixRuleMatch": {
        "matchedPrefix": ["git", "push"],
        "decision": "prompt"
      }
    }
  ],
  "decision": "prompt"
}
```

When no rules match, `matchedRules` is an empty array and `decision` is omitted.

```json
{
  "matchedRules": []
}
```

## Status

`execpolicy` commands are still in preview. The API may have breaking changes in the future.
