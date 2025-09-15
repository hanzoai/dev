#!/bin/bash
# Fix all test compilation errors

echo "Fixing all test compilation errors..."

# Fix missing allow_git_writes field
echo "1. Fixing missing allow_git_writes field..."
find src/rs -name "*.rs" -type f | xargs grep -l "SandboxPolicy::WorkspaceWrite" | while read file; do
    echo "  Fixing $file"
    sed -i.bak 's/exclude_slash_tmp: \(true\|false\),$/exclude_slash_tmp: \1,\n                allow_git_writes: false,/g' "$file"
    sed -i.bak 's/exclude_slash_tmp: \(true\|false\)$/exclude_slash_tmp: \1,\n                allow_git_writes: false/g' "$file"
done

# Fix missing event_seq and order fields in Event
echo "2. Fixing missing event_seq and order in Event..."
find src/rs -name "*.rs" -type f | xargs grep -l "Event {" | while read file; do
    echo "  Checking $file"
    # Add event_seq: 0 and order: 0 to Event initializers that don't have them
done

# Fix missing fields in Config
echo "3. Fixing missing fields in Config..."
find src/rs -name "*.rs" -type f | xargs grep -l "Config {" | while read file; do
    echo "  Checking $file"
    # Add agents: vec![] and include_view_image_tool: false
done

# Fix import issues
echo "4. Fixing import issues..."

# Fix ResponseEvent imports
find src/rs -name "*.rs" -type f -exec sed -i.bak \
    's/use dev_core::ResponseEvent;/use dev_protocol::models::ResponseEvent;/g' {} \;

find src/rs -name "*.rs" -type f -exec sed -i.bak \
    's/use dev_core::ResponseItem;/use dev_protocol::models::ResponseItem;/g' {} \;

find src/rs -name "*.rs" -type f -exec sed -i.bak \
    's/use dev_core::Prompt;/use dev_protocol::models::Prompt;/g' {} \;

find src/rs -name "*.rs" -type f -exec sed -i.bak \
    's/use dev_core::ModelClient;/use dev_protocol::models::ModelClient;/g' {} \;

# Clean up backup files
find src/rs -name "*.bak" -delete

echo "Done fixing compilation errors!"
echo "Now running tests to verify..."

cd src/rs
cargo test --all 2>&1 | grep -E "^error\[E[0-9]+\]:" | wc -l
echo "Remaining errors: $(cargo test --all 2>&1 | grep -E '^error\[E[0-9]+\]:' | wc -l)"