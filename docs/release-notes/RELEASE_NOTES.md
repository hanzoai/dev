## @hanzo/dev v0.6.71

This release adds Zen model support and ZAP-native tool wiring while carrying forward the latest upstream fixes.

### Changes
- Models: add `zen-4-coder` and `zen-4` presets in picker and model family routing.
- Core: expose ZAP native tool schemas to tool selection and route `zap_*` calls through the in-process dispatcher.
- Agent Defaults: add Zen aliases and tuned read/write presets for agent workflows.
- Upstream: includes latest release metadata and app-server protocol/schema updates.

### Install
```
npm install -g @hanzo/dev@latest
code
```
