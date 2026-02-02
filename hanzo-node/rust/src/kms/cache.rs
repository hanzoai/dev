//! Key caching layer with TTL

use std::collections::HashMap;
use std::sync::RwLock;
use std::time::{Duration, Instant};

use crate::kms::types::KeyMetadata;

/// Cached key entry
struct CacheEntry {
    metadata: KeyMetadata,
    cached_at: Instant,
}

/// Key cache with TTL support
pub struct KeyCache {
    entries: RwLock<HashMap<String, CacheEntry>>,
    ttl: Duration,
}

impl KeyCache {
    /// Create a new cache with the specified TTL
    pub fn new(ttl_secs: u64) -> Self {
        Self {
            entries: RwLock::new(HashMap::new()),
            ttl: Duration::from_secs(ttl_secs),
        }
    }

    /// Get a cached key if it exists and hasn't expired
    pub fn get(&self, key_id: &str) -> Option<KeyMetadata> {
        let entries = self.entries.read().ok()?;
        let entry = entries.get(key_id)?;

        if entry.cached_at.elapsed() < self.ttl {
            Some(entry.metadata.clone())
        } else {
            None
        }
    }

    /// Insert or update a key in the cache
    pub fn set(&self, metadata: KeyMetadata) {
        if let Ok(mut entries) = self.entries.write() {
            entries.insert(
                metadata.key_id.clone(),
                CacheEntry {
                    metadata,
                    cached_at: Instant::now(),
                },
            );
        }
    }

    /// Remove a key from the cache
    pub fn remove(&self, key_id: &str) {
        if let Ok(mut entries) = self.entries.write() {
            entries.remove(key_id);
        }
    }

    /// Clear all cached entries
    pub fn clear(&self) {
        if let Ok(mut entries) = self.entries.write() {
            entries.clear();
        }
    }

    /// Remove expired entries
    pub fn cleanup_expired(&self) {
        if let Ok(mut entries) = self.entries.write() {
            entries.retain(|_, entry| entry.cached_at.elapsed() < self.ttl);
        }
    }

    /// Get cache statistics
    pub fn stats(&self) -> CacheStats {
        let entries = self.entries.read().map(|e| e.len()).unwrap_or(0);
        CacheStats {
            entry_count: entries,
            ttl_secs: self.ttl.as_secs(),
        }
    }
}

/// Cache statistics
#[derive(Debug, Clone)]
pub struct CacheStats {
    pub entry_count: usize,
    pub ttl_secs: u64,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::kms::types::{KeyType, KeyUsage};

    #[test]
    fn test_cache_set_get() {
        let cache = KeyCache::new(60);
        let metadata = KeyMetadata::new("test-key", KeyType::Aes256Gcm, KeyUsage::EncryptDecrypt);

        cache.set(metadata.clone());

        let cached = cache.get("test-key");
        assert!(cached.is_some());
        assert_eq!(cached.unwrap().key_id, "test-key");
    }

    #[test]
    fn test_cache_miss() {
        let cache = KeyCache::new(60);
        assert!(cache.get("nonexistent").is_none());
    }

    #[test]
    fn test_cache_remove() {
        let cache = KeyCache::new(60);
        let metadata = KeyMetadata::new("test-key", KeyType::Aes256Gcm, KeyUsage::EncryptDecrypt);

        cache.set(metadata);
        assert!(cache.get("test-key").is_some());

        cache.remove("test-key");
        assert!(cache.get("test-key").is_none());
    }
}
