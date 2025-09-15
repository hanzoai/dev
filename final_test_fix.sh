#!/bin/bash
# Final comprehensive test fix

cd /Users/z/work/hanzo/dev/src/rs

echo "Fixing all remaining test compilation errors..."

# Fix all SandboxPolicy::WorkspaceWrite missing allow_git_writes
echo "1. Fixing SandboxPolicy missing allow_git_writes..."
find . -name "*.rs" -type f -exec grep -l "SandboxPolicy::WorkspaceWrite" {} \; | while read file; do
    perl -i -pe 's/(SandboxPolicy::WorkspaceWrite\s*\{[^}]*exclude_slash_tmp:\s*(?:true|false))(\s*\})/\1,\n                allow_git_writes: false\2/g' "$file"
done

# Fix FileChange::Delete having content field
echo "2. Fixing FileChange::Delete content field..."
perl -i -pe 's/FileChange::Delete\s*\{[^}]*content:[^}]*\}/FileChange::Delete { path: path.clone() }/g' core/src/turn_diff_tracker.rs

# Fix Event missing event_seq and order
echo "3. Fixing Event missing fields..."
find . -name "*.rs" -type f -exec grep -l "Event {" {} \; | while read file; do
    perl -i -pe 's/(Event\s*\{\s*id:\s*[^,]+,)(\s*msg:)/\1\n            event_seq: 0,\n            order: 0,\2/g' "$file"
done

# Fix Config missing fields
echo "4. Fixing Config missing fields..."
perl -i -pe 's/(Config\s*\{[^}]*)(agents:[^,]*,)?([^}]*)\}/
    my $start = $1;
    my $agents = $2 || "            agents: vec![],\n";
    my $rest = $3;
    my $result = $start . $agents . $rest;
    if ($result !~ \/include_view_image_tool:\/){
        $result = $start . $agents . "            include_view_image_tool: false,\n" . $rest;
    }
    $result . "}"/gse' core/src/config.rs

echo "Running tests to check status..."
cargo test --all 2>&1 | grep -E "test result:|error:" | head -20