Homebrew (macOS)

This repository now includes a helper script to generate a Homebrew formula
from the latest GitHub release artifacts. Publishing to Homebrew requires a
tap repository (for example, `just-every/homebrew-tap`). Once a tap exists,
you can generate and publish the formula like so:

1) Generate the formula for the latest version:

```
scripts/generate-homebrew-formula.sh
```

2) Copy the generated `Hanzo Dev.rb` into your tap repo under `Formula/Hanzo Dev.rb`
and update the `url`/`sha256` if needed.

3) Users can then install with:

```
brew tap just-every/tap
brew install dev
```

Notes

- The formula expects release assets named like:
  - `dev-aarch64-apple-darwin.tar.gz`
  - `dev-x86_64-apple-darwin.tar.gz`
- The CLI is installed as `dev` and `coder` shims for compatibility.

