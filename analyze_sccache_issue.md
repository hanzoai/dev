# Sccache Cache Hit Investigation Report

## Issue Summary
The GitHub Actions workflow is showing 0% cache hits despite using `mozilla-actions/sccache-action@v0.0.9` with proper configuration. The cache location shows `ghac` (GitHub Actions Cache) with a consistent hash: `8a6df4cb573bcfbf8c8e0bad63dc2287eedbfcc340fd86fbbdfa3b71d52e1c25`.

## Current Configuration Analysis

### Workflow Setup (release.yml)
```yaml
# Line 124-128: Setup sccache action
- name: Setup sccache (GHA backend)
  uses: mozilla-actions/sccache-action@v0.0.9
  with:
    version: v0.10.0
    token: ${{ secrets.GITHUB_TOKEN }}

# Line 131-148: Environment configuration
- Sets SCCACHE_GHA_ENABLED=true
- Sets SCCACHE_IDLE_TIMEOUT=1800
- Sets RUSTC_WRAPPER=sccache
- Configures OS-specific SCCACHE_DIR

# Line 151-155: Export GH cache credentials
- Exports ACTIONS_CACHE_URL
- Exports ACTIONS_RUNTIME_TOKEN

# Line 228-240: Build process
- Restarts sccache server to avoid race conditions
- Falls back to local cache if GHAC fails
- Shows stats before and after build
```

## Root Cause Analysis

### 1. **Hash Consistency Issue**
The hash `8a6df4cb573bcfbf8c8e0bad63dc2287eedbfcc340fd86fbbdfa3b71d52e1c25` appears to be the same across all builds. This suggests it's NOT a compilation hash but rather a **cache namespace/version identifier**.

### 2. **Missing Cache Key Configuration**
The workflow is missing critical cache key configuration:
- **SCCACHE_GHA_CACHE_TO**: Not set (determines where to write cache)
- **SCCACHE_GHA_CACHE_FROM**: Not set (determines where to read cache)
- **SCCACHE_GHA_VERSION**: Not set (used for cache busting)

Without these, sccache might be:
- Writing to a default location that changes per run
- Unable to read from previous cache entries
- Using a cache key that's too specific (includes run-specific data)

### 3. **GitHub Actions Cache Service Behavior**
The GitHub Actions cache service is **immutable** - once a (key, version) pair is reserved, it cannot be overwritten. This means:
- If the cache key is too specific (includes timestamps, run IDs), every build creates a new cache entry
- Subsequent builds can't find matching cache entries

### 4. **Path Dependencies**
Sccache requires **absolute paths to match** for cache hits. In GitHub Actions:
- The workspace path includes the run number: `/home/runner/work/code/code`
- If paths vary between runs, cache hits won't occur

### 5. **Incremental Compilation Interference**
The workflow sets `CARGO_INCREMENTAL="0"` (line 215), which is correct. However, certain crate types cannot be cached:
- bin crates (which is what's being built)
- proc-macro crates
- cdylib/dylib crates

## Recommended Solutions

### Solution 1: Configure Cache Keys Explicitly
```yaml
- name: Setup sccache (GHA backend)
  uses: mozilla-actions/sccache-action@v0.0.9
  with:
    version: v0.10.0
    token: ${{ secrets.GITHUB_TOKEN }}

- name: Configure sccache cache keys
  shell: bash
  run: |
    # Use a stable cache key based on target and Rust version
    CACHE_KEY="sccache-${{ matrix.target }}-rust-1.89"
    echo "SCCACHE_GHA_CACHE_TO=${CACHE_KEY}" >> "$GITHUB_ENV"
    echo "SCCACHE_GHA_CACHE_FROM=${CACHE_KEY}" >> "$GITHUB_ENV"
    # Version for cache busting when needed
    echo "SCCACHE_GHA_VERSION=v1" >> "$GITHUB_ENV"
```

### Solution 2: Use Stable Build Paths
```yaml
- name: Setup stable build path
  shell: bash
  run: |
    # Create a stable build directory
    sudo mkdir -p /opt/rust-build
    sudo chown -R $USER:$USER /opt/rust-build
    # Symlink or copy project there
    cp -r . /opt/rust-build/
    cd /opt/rust-build
```

### Solution 3: Focus on Library Caching
Since binary crates have limited caching potential, focus on caching dependencies:
```yaml
env:
  # Only cache dependencies, not workspace members
  SCCACHE_CACHE_MULTIARCH: "1"
  # Increase cache size for better hit rates
  SCCACHE_CACHE_SIZE: "10G"
```

### Solution 4: Diagnostic Improvements
Add better diagnostics to understand what's happening:
```yaml
- name: Detailed sccache diagnostics
  shell: bash
  run: |
    # Enable verbose logging
    export RUST_LOG=sccache=debug
    export SCCACHE_LOG=debug
    
    # Show detailed stats
    sccache --show-stats --stats-format json > sccache-stats.json
    
    # Check what's being hashed
    echo "Checking hash inputs..."
    env | grep -E "^(CARGO_|RUST|SCCACHE_)" | sort
```

## The Consistent Hash Mystery

The hash `8a6df4cb573bcfbf8c8e0bad63dc2287eedbfcc340fd86fbbdfa3b71d52e1c25` is likely:
1. **A namespace identifier** derived from the repository or workflow context
2. **Not changing between builds** because it's based on stable inputs
3. **Not the compilation hash** but rather the cache storage prefix

This suggests the cache backend IS working, but cache entries aren't being matched due to key mismatches.

## Immediate Action Items

1. **Add cache key configuration** (SCCACHE_GHA_CACHE_TO/FROM)
2. **Enable debug logging** to see why cache misses occur
3. **Consider using Swatinem/rust-cache** alongside sccache for dependency caching
4. **Test with a simple library crate** to verify sccache works at all

## Alternative: Rely More on Swatinem/rust-cache

The workflow already uses `Swatinem/rust-cache` (line 111-117) which handles:
- Cargo registry caching
- Target directory caching
- Proper cache key generation

Consider removing sccache temporarily and optimizing Swatinem/rust-cache configuration instead, as it's specifically designed for Rust and handles GitHub Actions caching properly.

## Conclusion

The 0% cache hit rate is most likely due to missing `SCCACHE_GHA_CACHE_TO` and `SCCACHE_GHA_CACHE_FROM` configuration, causing sccache to use dynamic cache keys that never match between runs. The consistent hash in the output is a red herring - it's a namespace identifier, not the cache key being used for compilation artifacts.