//! Demonstrate the blockchain with native KuzuDB ledger

use anyhow::Result;

#[cfg(feature = "kuzu")]
fn main() -> Result<()> {
    println!("🚀 Running Blockchain with Native KuzuDB Ledger");
    println!("================================================\n");

    // Create temp database
    let temp_dir = tempfile::tempdir()?;
    let db_path = temp_dir.path().join("blockchain_ledger");

    println!("📂 Creating KuzuDB at: {:?}", db_path);

    // Initialize native KuzuDB ledger
    let db = kuzu::Database::new(&db_path, kuzu::SystemConfig::default())?;
    println!("✅ Native KuzuDB ledger initialized");

    // Create connection
    let mut conn = kuzu::Connection::new(&db)?;
    println!("✅ Connected to ledger");

    // Create blockchain schema
    println!("\n📊 Setting up blockchain schema...");

    // Blocks table
    conn.query("CREATE NODE TABLE Block(
        height UINT64 PRIMARY KEY,
        hash STRING,
        parent_hash STRING,
        merkle_root STRING,
        timestamp TIMESTAMP,
        validator STRING,
        state_root STRING
    )")?;
    println!("  ✓ Block table created");

    // Transactions table
    conn.query("CREATE NODE TABLE Transaction(
        hash STRING PRIMARY KEY,
        block_height UINT64,
        from_addr STRING,
        to_addr STRING,
        value DOUBLE,
        gas_used UINT64,
        timestamp TIMESTAMP
    )")?;
    println!("  ✓ Transaction table created");

    // AI operations table (vectors + metadata)
    conn.query("CREATE NODE TABLE AIOperation(
        id STRING PRIMARY KEY,
        embedding DOUBLE[1536],
        content STRING,
        model STRING,
        timestamp TIMESTAMP
    )")?;
    println!("  ✓ AIOperation table created");

    // Relationships
    conn.query("CREATE REL TABLE CONTAINS(
        FROM Block TO Transaction
    )")?;
    conn.query("CREATE REL TABLE EXECUTES(
        FROM Transaction TO AIOperation,
        inference_time_ms INT32
    )")?;
    println!("  ✓ Relationships created");

    // Insert genesis block
    println!("\n⛓️  Creating genesis block...");
    conn.query("CREATE (:Block {
        height: 0,
        hash: 'genesis_hash_000000',
        parent_hash: '',
        merkle_root: 'merkle_root_000',
        timestamp: '2025-01-01 00:00:00',
        validator: 'genesis',
        state_root: 'state_000'
    })")?;
    println!("  ✓ Genesis block created");

    // Insert some transactions
    println!("\n💰 Adding transactions...");

    // Transaction 1
    conn.query("CREATE (:Transaction {
        hash: 'tx_001',
        block_height: 0,
        from_addr: '0x0000',
        to_addr: '0x1234',
        value: 100.0,
        gas_used: 21000,
        timestamp: '2025-01-01 00:00:01'
    })")?;

    // Transaction 2 with AI operation
    conn.query("CREATE (:Transaction {
        hash: 'tx_002',
        block_height: 0,
        from_addr: '0x1234',
        to_addr: '0x5678',
        value: 50.0,
        gas_used: 45000,
        timestamp: '2025-01-01 00:00:02'
    })")?;

    // Link transactions to block
    conn.query("MATCH (b:Block {height: 0}), (t:Transaction {block_height: 0})
                CREATE (b)-[:CONTAINS]->(t)")?;
    println!("  ✓ 2 transactions added to genesis block");

    // Add AI operation
    println!("\n🤖 Adding AI operations...");
    let embedding = vec![0.1_f64; 1536]; // Simulated embedding
    let embedding_str: Vec<String> = embedding.iter().map(|v| v.to_string()).collect();
    let embedding_array = format!("[{}]", embedding_str.join(","));

    conn.query(&format!("CREATE (:AIOperation {{
        id: 'ai_op_001',
        embedding: {},
        content: 'Test inference for blockchain validation',
        model: 'qwen3:8b',
        timestamp: '2025-01-01 00:00:03'
    }})", embedding_array))?;

    // Link AI operation to transaction
    conn.query("MATCH (t:Transaction {hash: 'tx_002'}), (a:AIOperation {id: 'ai_op_001'})
                CREATE (t)-[:EXECUTES {inference_time_ms: 250}]->(a)")?;
    println!("  ✓ AI operation linked to transaction");

    // Query the blockchain
    println!("\n🔍 Querying the blockchain ledger:");

    // Get block info
    let mut result = conn.query("MATCH (b:Block) RETURN b.height, b.hash, b.validator ORDER BY b.height")?;
    println!("\n📦 Blocks:");
    while result.has_next() {
        let height: i64 = result.get_value(0)?;
        let hash: String = result.get_value(1)?;
        let validator: String = result.get_value(2)?;
        println!("  Block #{}: {} (validated by: {})", height, hash, validator);
        result.next()?;
    }

    // Get transaction count
    let mut result = conn.query("MATCH (t:Transaction) RETURN count(t) as tx_count")?;
    if result.has_next() {
        let count: i64 = result.get_value(0)?;
        println!("\n💸 Total transactions: {}", count);
    }

    // Get AI operations
    let mut result = conn.query("MATCH (a:AIOperation) RETURN a.id, a.model, a.content")?;
    println!("\n🧠 AI Operations:");
    while result.has_next() {
        let id: String = result.get_value(0)?;
        let model: String = result.get_value(1)?;
        let content: String = result.get_value(2)?;
        println!("  {}: {} - '{}'", id, model, content);
        result.next()?;
    }

    // Graph query - show relationships
    println!("\n🔗 Graph Relationships:");
    let mut result = conn.query("
        MATCH (b:Block)-[:CONTAINS]->(t:Transaction)
        OPTIONAL MATCH (t)-[e:EXECUTES]->(a:AIOperation)
        RETURN b.height, t.hash, a.id, e.inference_time_ms
        ORDER BY b.height, t.hash
    ")?;

    while result.has_next() {
        let block_height: i64 = result.get_value(0)?;
        let tx_hash: String = result.get_value(1)?;

        // Check if AI operation exists (could be null)
        let has_ai = !result.is_null(2)?;

        if has_ai {
            let ai_id: String = result.get_value(2)?;
            let inference_time: i32 = result.get_value(3)?;
            println!("  Block #{} -> Tx {} -> AI {} ({}ms)",
                     block_height, tx_hash, ai_id, inference_time);
        } else {
            println!("  Block #{} -> Tx {}", block_height, tx_hash);
        }

        result.next()?;
    }

    // Create a new block
    println!("\n⛓️  Mining new block...");
    conn.query("CREATE (:Block {
        height: 1,
        hash: 'block_hash_001',
        parent_hash: 'genesis_hash_000000',
        merkle_root: 'merkle_root_001',
        timestamp: '2025-01-01 00:01:00',
        validator: 'validator_node_1',
        state_root: 'state_001'
    })")?;
    println!("  ✓ Block #1 mined");

    // Final statistics
    println!("\n📈 Ledger Statistics:");
    let mut result = conn.query("MATCH (n) RETURN labels(n)[0] as type, count(n) as count ORDER BY type")?;
    while result.has_next() {
        let node_type: String = result.get_value(0)?;
        let count: i64 = result.get_value(1)?;
        println!("  {}: {}", node_type, count);
        result.next()?;
    }

    println!("\n✨ Blockchain with native KuzuDB ledger is running successfully!");
    println!("🎯 KuzuDB provides:");
    println!("  • Graph database for blockchain relationships");
    println!("  • Vector storage for AI embeddings (1536-dim)");
    println!("  • ACID transactions for ledger integrity");
    println!("  • Cypher queries for complex analysis");
    println!("  • In-process execution (no network overhead)");

    Ok(())
}

#[cfg(not(feature = "kuzu"))]
fn main() {
    println!("❌ KuzuDB feature not enabled.");
    println!("Run with: cargo run --example native_kuzu_ledger --features kuzu");
}