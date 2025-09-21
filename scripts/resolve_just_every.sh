#!/bin/bash

echo "Resolving just-every fork conflicts..."

# For renamed file locations, take the suggestions
for file in $(git status | grep "suggesting it should perhaps be moved" | sed -n 's/.*moved to \(.*\)\./\1/p'); do
    if [ -f "$file" ]; then
        echo "Already exists: $file"
    else
        echo "Moving to suggested location: $file"
        # Extract source from conflict message
        source=$(git status | grep "$file" | sed -n 's/.*: \(.*\) added.*/\1/p' | head -1)
        if [ -n "$source" ]; then
            git checkout --theirs "$source" 2>/dev/null && mv "$source" "$file" && git add "$file"
        fi
    fi
done

# For most conflicts, prefer just-every features but keep Hanzo branding
for file in $(git diff --name-only --diff-filter=U); do
    echo "Processing: $file"
    
    # Special handling for specific files
    case "$file" in
        README.md)
            # Keep our Hanzo branding
            git checkout --ours "$file"
            ;;
        docs/advanced.md)
            # Merge both sets of changes
            git checkout --theirs "$file"
            sed -i '' 's/@openai\/codex/@hanzo\/dev/g' "$file"
            sed -i '' 's/codex login/hanzo login/g' "$file"
            sed -i '' 's/codex exec/hanzo exec/g' "$file"
            sed -i '' 's/codex mcp/hanzo mcp/g' "$file"
            ;;
        */message_processor.rs|*/chatgpt_client.rs|*/chatgpt_token.rs|*/apply_command.rs)
            # These have complex auth changes - take just-every version
            git checkout --theirs "$file"
            ;;
        */Cargo.toml)
            # Take just-every version for dependencies
            git checkout --theirs "$file"
            ;;
        *.snap)
            # Test snapshots - take just-every version
            git checkout --theirs "$file"
            ;;
        *)
            # Default: take just-every version
            git checkout --theirs "$file"
            ;;
    esac
    
    git add "$file"
done

# Handle deleted files
for file in $(git status | grep "deleted by us" | cut -d':' -f2); do
    echo "Removing deleted file: $file"
    git rm "$file" 2>/dev/null
done

for file in $(git status | grep "deleted by them" | cut -d':' -f2); do
    echo "Keeping our file: $file"
    git add "$file" 2>/dev/null
done

echo "Conflicts resolved!"
git status --short | grep "^UU " | wc -l | xargs echo "Remaining conflicts:"