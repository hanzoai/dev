## @just-every/code v0.2.137

This release updates the Responses proxy defaults for better reliability and adds a developer script to probe the API.

### Changes
- Dev: add `scripts/test-responses.js` to probe Responses API with ChatGPT/API key auth; includes schema/tools/store tests.
- Proxy: default Responses v1; fail-fast on 5xx; add STRICT_HEADERS and RESPONSES_BETA override.

### Install
```
npm install -g @just-every/code@latest
code
```

Compare: https://github.com/just-every/code/compare/v0.2.136...v0.2.137

