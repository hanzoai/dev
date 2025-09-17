#!/bin/bash

# Script to resolve merge conflicts preserving Hanzo branding while incorporating Codex updates

echo "Resolving merge conflicts..."

# For most Rust files, take upstream version (Codex improvements)
for file in $(git status --porcelain | grep "^UU " | grep -E "\.(rs|toml)$" | cut -d' ' -f2); do
    echo "Taking upstream version for: $file"
    git checkout --theirs "$file"
    # Replace OpenAI/Codex branding with Hanzo
    sed -i '' 's/OpenAI Codex/Hanzo Dev/g' "$file" 2>/dev/null || true
    sed -i '' 's/Codex CLI/Hanzo Dev/g' "$file" 2>/dev/null || true
    sed -i '' 's/@openai\/codex/@hanzo\/dev/g' "$file" 2>/dev/null || true
    sed -i '' 's/codex-cli/hanzo-dev/g' "$file" 2>/dev/null || true
    git add "$file"
done

# For documentation files, take upstream and rebrand
for file in $(git status --porcelain | grep "^UU " | grep "\.md$" | cut -d' ' -f2); do
    if [ "$file" != "README.md" ]; then
        echo "Rebranding documentation: $file"
        git checkout --theirs "$file"
        sed -i '' 's/OpenAI Codex/Hanzo Dev/g' "$file" 2>/dev/null || true
        sed -i '' 's/Codex CLI/Hanzo Dev/g' "$file" 2>/dev/null || true
        sed -i '' 's/@openai\/codex/@hanzo\/dev/g' "$file" 2>/dev/null || true
        sed -i '' 's/codex/hanzo/g' "$file" 2>/dev/null || true
        sed -i '' 's/\.hanzo\//~\/.hanzo\//g' "$file" 2>/dev/null || true
        git add "$file"
    fi
done

# For package.json files, keep Hanzo naming
for file in $(git status --porcelain | grep "^UU " | grep "package\.json$" | cut -d' ' -f2); do
    echo "Preserving Hanzo package naming in: $file"
    git checkout --ours "$file"
    git add "$file"
done

echo "Conflicts resolved!"
git status --short | grep "^UU " | wc -l | xargs echo "Remaining conflicts:"