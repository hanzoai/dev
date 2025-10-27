# Issue Responses

## #207 — Remove Codex Requirement

Thanks for raising this! The CLI no longer assumes the Codex-only GPT-5 tier:

- The default `model` is now `gpt-5`, so a fresh install works with general OpenAI access or any configured provider. The ChatGPT onboarding wizard also stops rewriting the model back to the Codex tier.
- Multi-agent defaults promote non-Codex providers first (`code-gpt-5`, Claude, Gemini, Qwen) while keeping `code-gpt-5-codex` available for legacy accounts.
- Docs and prompts were updated to reflect the new defaults and highlight that Claude/Gemini are first-class options.

If you already have a `config.toml`, you do not need to change anything—your configured `model` continues to win. New installs and empty configs now pick the provider-agnostic defaults.

Let us know if anything else still suggests Codex is mandatory, and we can sweep those spots too.

## #343 — OPENAI_WIRE_API support was removed

We’ve reinstated the documented `OPENAI_WIRE_API` override. Setting `OPENAI_WIRE_API=chat` now forces the built-in OpenAI provider to use the Chat Completions endpoint, while `responses` (or the default) stays on the Responses API. The new regression tests in `code-rs/core/tests/openai_wire_api_env.rs` cover chat/responses/default/invalid cases so this stays verified going forward.
