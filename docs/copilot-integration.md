# GitHub Copilot Integration for Hanzo Dev CLI

The Hanzo Dev CLI now includes comprehensive GitHub Copilot integration, making it a powerful CLI editing and agent backend. This integration provides AI-powered code assistance directly from the command line.

## Features

### 🤖 Interactive Chat

- Real-time conversations with GitHub Copilot
- Context-aware responses with file integration
- Continuous session support

### 💡 Code Suggestions

- AI-powered code completions and suggestions
- Language-aware context understanding
- Multiple suggestion alternatives

### 🔍 Code Review & Analysis

- Automated code review and suggestions
- Git diff analysis and feedback
- Quality improvement recommendations

### 📚 Documentation Generation

- Auto-generate comments, README files, and API docs
- Multiple documentation formats
- Context-aware explanations

### 🐚 Shell Command Assistance

- Natural language to shell commands
- OS-specific command suggestions
- Command explanations and safety tips

### 🎯 Git Integration

- Auto-generate commit messages from diffs
- Conventional commit format support
- Change analysis and categorization

## Installation & Setup

### 1. Install GitHub CLI and Copilot Extension

```bash
# Install GitHub CLI (if not already installed)
brew install gh

# Authenticate with GitHub
gh auth login

# Install GitHub Copilot extension
gh extension install github/copilot

# Verify installation
dev copilot setup --check
```

### 2. Auto-setup (Recommended)

```bash
# Install and configure everything automatically
dev copilot setup --install --auth
```

## Command Reference

### Basic Usage

```bash
# Interactive chat with Copilot
dev copilot chat

# Get code suggestions
dev copilot suggest "function to sort array"

# Review code in a file
dev copilot review main.rs

# Explain code functionality
dev copilot explain "const result = arr.map(x => x * 2)"
```

### Chat Commands

```bash
# Start interactive chat
dev copilot chat

# Chat with file context
dev copilot chat --file src/main.rs

# Continue previous session
dev copilot chat --continue

# One-shot question
dev copilot chat "How do I implement async/await in Rust?"
```

### Code Suggestions

```bash
# Get suggestions for code context
dev copilot suggest --file main.rs --language rust

# Suggest from stdin
echo "def fibonacci(" | dev copilot suggest

# Multiple suggestions
dev copilot suggest --count 5 "sort algorithm"
```

### Code Review

```bash
# Review a specific file
dev copilot review src/lib.rs

# Review git diff
dev copilot review main.rs --diff

# Output as JSON
dev copilot review main.rs --format json

# Output as Markdown
dev copilot review main.rs --format markdown
```

### Documentation Generation

```bash
# Generate code comments
dev copilot docs "fn calculate_sum(a: i32, b: i32) -> i32" --type comment

# Generate README
dev copilot docs src/ --type readme --output README.md

# Generate API documentation
dev copilot docs api.rs --type api
```

### Code Explanation

```bash
# Explain code snippet
dev copilot explain "async fn fetch_data() -> Result<String, Error>"

# Explain file contents
dev copilot explain --file complex_algorithm.rs

# Different explanation levels
dev copilot explain --level expert "impl Iterator for MyStruct"

# Read from stdin
cat main.rs | dev copilot explain -
```

### Shell Commands

```bash
# Get shell commands for tasks
dev copilot shell "find all Rust files and check them with clippy"

# OS-specific commands
dev copilot shell --os macos "compress directory into tar.gz"

# With explanations
dev copilot shell --explain "backup database and compress"
```

### Git Integration

```bash
# Generate commit message for staged changes
dev copilot commit --staged

# Generate commit message for specific diff
git diff | dev copilot commit --diff -

# Different commit styles
dev copilot commit --style conventional
dev copilot commit --style simple
dev copilot commit --style detailed
```

### Auto-completion

```bash
# Auto-complete at specific position
dev copilot complete main.rs --line 42 --column 15

# With custom context window
dev copilot complete main.rs --line 42 --column 15 --context 20
```

## Integration with Existing Workflows

### IDE Integration

The Copilot CLI commands can be integrated into your favorite editor:

#### Vim/Neovim

```vim
" Add to your .vimrc or init.vim
command! CopilotChat !dev copilot chat
command! CopilotReview !dev copilot review %
command! CopilotExplain !dev copilot explain --file %
```

#### VS Code

```json
{
  "terminal.integrated.shellArgs.osx": ["-c", "alias cop='dev copilot'"]
}
```

### Git Hooks

#### Pre-commit Hook

```bash
#!/bin/sh
# .git/hooks/pre-commit
dev copilot review --diff $(git diff --cached --name-only)
```

#### Commit Message Hook

```bash
#!/bin/sh
# .git/hooks/prepare-commit-msg
if [ -z "$2" ]; then
  dev copilot commit --staged > "$1"
fi
```

### CI/CD Integration

#### GitHub Actions

```yaml
name: AI Code Review
on: [pull_request]
jobs:
  copilot-review:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Install Hanzo Dev
        run: npm install -g @hanzo/dev
      - name: Review Changes
        run: |
          dev copilot review --diff --format markdown > review.md
        env:
          GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
```

## Configuration

### Global Configuration

Add to your `~/.hanzo/config.toml`:

```toml
[copilot]
enabled = true
auto_suggest = false
code_review = true
chat_enabled = true
model = "gpt-4"
max_tokens = 4096
context_window = 10000
```

### Project-specific Configuration

Add to your project's `.dev/config.toml`:

```toml
[copilot]
# Enable auto-suggestions for this project
auto_suggest = true
# Use larger context window for complex codebases
context_window = 20000
```

## Advanced Usage

### Custom Workflows

#### Code Refactoring Workflow

```bash
#!/bin/bash
# refactor.sh - AI-assisted refactoring workflow

FILE="$1"
echo "Starting AI-assisted refactoring for $FILE"

# 1. Get initial review
echo "=== Initial Review ==="
dev copilot review "$FILE"

# 2. Get suggestions
echo "=== Suggestions ==="
dev copilot suggest --file "$FILE" --count 3

# 3. Explain complex parts
echo "=== Code Explanation ==="
dev copilot explain --file "$FILE"

# 4. Generate documentation
echo "=== Documentation ==="
dev copilot docs "$FILE" --type comment
```

#### Documentation Generation Pipeline

```bash
#!/bin/bash
# docs-gen.sh - Comprehensive documentation generation

# Generate README
dev copilot docs src/ --type readme --output README.md

# Generate API docs for all public modules
find src/ -name "*.rs" -exec dev copilot docs {} --type api \;

# Generate inline comments for complex functions
find src/ -name "*.rs" -exec dev copilot docs {} --type comment \;
```

## Tips and Best Practices

### 1. Effective Prompting

- **Be specific**: Instead of "help with code", say "optimize this sorting algorithm for large datasets"
- **Provide context**: Include relevant file paths and project information
- **Ask follow-up questions**: Build on previous responses for deeper insights

### 2. Code Review Integration

- Use `--diff` flag for reviewing changes before committing
- Combine with your existing code review process
- Focus on security, performance, and maintainability suggestions

### 3. Documentation Workflow

- Generate docs incrementally as you write code
- Use different doc types for different audiences
- Review and edit AI-generated documentation for accuracy

### 4. Shell Command Safety

- Always review suggested commands before execution
- Use `--explain` flag to understand what commands do
- Test in safe environments first

## Troubleshooting

### Common Issues

#### "Copilot is not available"

```bash
# Check installation status
dev copilot setup --check

# Reinstall if needed
dev copilot setup --install --auth
```

#### Authentication Issues

```bash
# Re-authenticate with GitHub
gh auth login

# Configure Copilot access
gh auth refresh -s copilot
```

#### Performance Issues

- Reduce context window size for faster responses
- Use shorter prompts for simple questions
- Check internet connectivity

### Debug Mode

```bash
# Enable verbose logging
RUST_LOG=debug dev copilot chat "debug prompt"
```

## Examples

### Real-world Scenarios

#### 1. Bug Investigation

```bash
# Get suggestions for fixing a bug
dev copilot chat --file buggy_function.rs "This function sometimes returns None unexpectedly. What could be wrong?"

# Review the fix
dev copilot review buggy_function.rs --diff
```

#### 2. Performance Optimization

```bash
# Get optimization suggestions
dev copilot suggest --file slow_algorithm.rs "optimize for performance"

# Explain the current implementation
dev copilot explain --file slow_algorithm.rs --level expert
```

#### 3. Testing Strategy

```bash
# Generate test suggestions
dev copilot chat "What tests should I write for this authentication module?" --file auth.rs

# Review test coverage
dev copilot review tests/ --format markdown
```

#### 4. Documentation Sprint

```bash
# Document entire project
find src/ -name "*.rs" | xargs -I {} dev copilot docs {} --type comment

# Generate comprehensive README
dev copilot docs . --type readme --output README.md
```

## Contributing

The Copilot integration is part of the open-source Hanzo Dev project. Contributions are welcome!

### Development

```bash
# Build from source
git clone https://github.com/hanzoai/dev
cd dev
cargo build --release

# Test Copilot integration
cargo test copilot
```

### Feature Requests

- File issues on the [Hanzo Dev repository](https://github.com/hanzoai/dev/issues)
- Use the "enhancement" label for new feature requests
- Provide use cases and examples

## License

This integration is part of Hanzo Dev and follows the same licensing terms as the main project.

---

**Need Help?**

- 📖 [Full Documentation](https://hanzo.ai/dev/docs)
- 💬 [Community Discord](https://discord.gg/hanzo)
- 🐛 [Report Issues](https://github.com/hanzoai/dev/issues)
- ✨ [Feature Requests](https://github.com/hanzoai/dev/discussions)
