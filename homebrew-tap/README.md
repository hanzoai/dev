# just-every/homebrew-tap

Unofficial tap for Code (terminal coding agent).

## Usage

```bash
brew tap just-every/tap
brew install code
```

## Updating

- Regenerate the formula in the main repo:
  ```bash
  scripts/generate-homebrew-formula.sh
  ```
- Copy `Formula/Code.rb` into this tap at `Formula/Code.rb` and push a commit.
