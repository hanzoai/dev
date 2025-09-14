## @just-every/code v0.2.145

A small maintenance release that improves the reliability and safety of our issue‑comment automation.

### Changes

- CI/Issue comments: ensure proxy script is checked out in both jobs; align with upstream flows.
- CI: gate issue-comment job on OPENAI_API_KEY via env and avoid secrets in if conditions.

### Install

```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.144...v0.2.145
