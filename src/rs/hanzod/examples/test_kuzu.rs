//! Test KuzuDB directly

use anyhow::Result;

#[cfg(feature = "kuzu")]
use kuzu::{Database, SystemConfig, Connection};

fn main() -> Result<()> {
    #[cfg(feature = "kuzu")]
    {
        println!("🚀 Testing KuzuDB native ledger...");

        // Create temp database
        let temp_dir = tempfile::tempdir()?;
        let db_path = temp_dir.path().join("test_kuzu");

        // Initialize database
        let db = Database::new(&db_path, SystemConfig::default())?;
        println!("✅ KuzuDB database created at: {:?}", db_path);

        // Create connection
        let conn = Connection::new(&db)?;
        println!("✅ Connection established");

        // Create schema
        conn.query("CREATE NODE TABLE Block(
            height UINT64 PRIMARY KEY,
            hash STRING,
            timestamp TIMESTAMP
        )")?;
        println!("✅ Block table created");

        // Insert test data
        conn.query("CREATE (:Block {height: 1, hash: 'genesis', timestamp: '2024-01-01 00:00:00'})")?;
        conn.query("CREATE (:Block {height: 2, hash: 'block2', timestamp: '2024-01-01 00:01:00'})")?;
        println!("✅ Test blocks inserted");

        // Query data
        let mut result = conn.query("MATCH (b:Block) RETURN b.height, b.hash ORDER BY b.height")?;
        println!("\n📊 Blockchain data in KuzuDB:");
        while result.has_next() {
            let height = result.get_value(0)?;
            let hash = result.get_value(1)?;
            println!("  Block {}: {}", height, hash);
            result.next()?;
        }

        println!("\n✨ KuzuDB native ledger is working!");
    }

    #[cfg(not(feature = "kuzu"))]
    {
        println!("❌ KuzuDB feature not enabled. Run with: cargo run --features kuzu --example test_kuzu");
    }

    Ok(())
}