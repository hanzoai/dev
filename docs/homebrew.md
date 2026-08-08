Homebrew (macOS)

`scripts/generate-homebrew-formula.sh` generates a Homebrew formula from the
latest GitHub release artifacts. It writes `Formula/hanzo-dev.rb` in the repo
root; the release workflow copies that into the `hanzoai/homebrew-tap` tap.

Users install with:

```
brew tap hanzoai/tap
brew install hanzo-dev
```

Notes

- The formula expects release assets named `dev-<target-triple>.tar.gz`, e.g.
  `dev-aarch64-apple-darwin.tar.gz` and `dev-x86_64-unknown-linux-musl.tar.gz`.
- The CLI is installed as `dev`. The formula deliberately installs no other
  shim, so it cannot collide with `hanzo.rb` (the Hanzo CLI, a different
  program).
