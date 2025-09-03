## Unreleased

- TUI: Support pasting images from the clipboard and data‑URI/base64 into the composer. Pasted images now appear as `[image: filename.png]` and are attached to the message. Fixes #10.
- TUI/Input: Normalize key events on non‑enhanced terminals (e.g., Git Bash/mintty on Windows) so typing and Ctrl+C work reliably. Closes #18, #14.
- NPM: Make `@vscode/ripgrep` an optional dependency to prevent global installs from failing when its postinstall cannot download. Closes #16.
- Core/HTTP: Honor extra CA certs via `SSL_CERT_FILE`, `REQUESTS_CA_BUNDLE`, `NODE_EXTRA_CA_CERTS`, and `SSL_CERT_DIR` to work behind corporate/mitm proxies. Closes #17.
- Docs: Clarify npm package name; `@just-every/coder` is deprecated in favor of `@just-every/code`. Closes #12.
- Docs: Add Homebrew formula generator and instructions for maintainers. Refs #19.
