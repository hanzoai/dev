/// Comprehensive tests for MCP tool registry
use dev_mcp_server::tool_handlers::{ToolRegistry, ToolError};
use serde_json::json;
use tempfile::TempDir;
use std::fs;

#[tokio::test]
async fn test_tool_registry_initialization() {
    let registry = ToolRegistry::new();
    
    // Check that tools are registered
    let tools = registry.list_tools();
    assert!(tools.len() > 30, "Should have at least 30 tools registered");
    
    // Check categories exist
    let categories: Vec<String> = tools.iter()
        .map(|t| t.category.clone())
        .collect::<std::collections::HashSet<_>>()
        .into_iter()
        .collect();
    
    assert!(categories.contains(&"file".to_string()));
    assert!(categories.contains(&"search".to_string()));
    assert!(categories.contains(&"edit".to_string()));
    assert!(categories.contains(&"git".to_string()));
    assert!(categories.contains(&"ai".to_string()));
    assert!(categories.contains(&"project".to_string()));
}

#[tokio::test]
async fn test_read_write_file() {
    let registry = ToolRegistry::new();
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("test.txt");
    
    // Test write_file
    let write_params = json!({
        "path": file_path.to_str().unwrap(),
        "content": "Hello from MCP test"
    });
    
    let result = registry.execute("write_file", write_params).await;
    assert!(result.is_ok());
    
    // Test read_file
    let read_params = json!({
        "path": file_path.to_str().unwrap()
    });
    
    let result = registry.execute("read_file", read_params).await.unwrap();
    assert_eq!(result["content"], "Hello from MCP test");
}

#[tokio::test]
async fn test_list_files() {
    let registry = ToolRegistry::new();
    let temp_dir = TempDir::new().unwrap();
    
    // Create some test files
    fs::write(temp_dir.path().join("file1.txt"), "content1").unwrap();
    fs::write(temp_dir.path().join("file2.txt"), "content2").unwrap();
    fs::create_dir(temp_dir.path().join("subdir")).unwrap();
    fs::write(temp_dir.path().join("subdir/file3.txt"), "content3").unwrap();
    
    // Test non-recursive listing
    let params = json!({
        "path": temp_dir.path().to_str().unwrap(),
        "recursive": false
    });
    
    let result = registry.execute("list_files", params).await.unwrap();
    let file_count = result["count"].as_u64().unwrap();
    assert_eq!(file_count, 3); // 2 files + 1 directory
    
    // Test recursive listing
    let params = json!({
        "path": temp_dir.path().to_str().unwrap(),
        "recursive": true
    });
    
    let result = registry.execute("list_files", params).await.unwrap();
    let file_count = result["count"].as_u64().unwrap();
    assert_eq!(file_count, 4); // 3 files + 1 directory
}

#[tokio::test]
async fn test_file_operations() {
    let registry = ToolRegistry::new();
    let temp_dir = TempDir::new().unwrap();
    let source_path = temp_dir.path().join("source.txt");
    let dest_path = temp_dir.path().join("dest.txt");
    
    // Create source file
    fs::write(&source_path, "test content").unwrap();
    
    // Test copy_file
    let params = json!({
        "source": source_path.to_str().unwrap(),
        "destination": dest_path.to_str().unwrap()
    });
    
    let result = registry.execute("copy_file", params).await;
    assert!(result.is_ok());
    assert!(dest_path.exists());
    
    // Test file_info
    let params = json!({
        "path": dest_path.to_str().unwrap()
    });
    
    let result = registry.execute("file_info", params).await.unwrap();
    assert_eq!(result["is_file"], true);
    assert_eq!(result["size"], 12); // "test content" is 12 bytes
    
    // Test move_file
    let new_path = temp_dir.path().join("moved.txt");
    let params = json!({
        "source": dest_path.to_str().unwrap(),
        "destination": new_path.to_str().unwrap()
    });
    
    let result = registry.execute("move_file", params).await;
    assert!(result.is_ok());
    assert!(!dest_path.exists());
    assert!(new_path.exists());
    
    // Test delete_file
    let params = json!({
        "path": new_path.to_str().unwrap()
    });
    
    let result = registry.execute("delete_file", params).await;
    assert!(result.is_ok());
    assert!(!new_path.exists());
}

#[tokio::test]
async fn test_bash_execution() {
    let registry = ToolRegistry::new();
    
    // Test simple command
    let params = json!({
        "command": "echo 'Hello World'"
    });
    
    let result = registry.execute("bash", params).await.unwrap();
    assert_eq!(result["stdout"].as_str().unwrap().trim(), "Hello World");
    assert_eq!(result["exit_code"], 0);
    
    // Test command with error
    let params = json!({
        "command": "exit 1"
    });
    
    let result = registry.execute("bash", params).await.unwrap();
    assert_eq!(result["exit_code"], 1);
}

#[tokio::test]
async fn test_edit_file() {
    let registry = ToolRegistry::new();
    let temp_dir = TempDir::new().unwrap();
    let file_path = temp_dir.path().join("edit_test.txt");
    
    // Create initial file
    fs::write(&file_path, "Line 1\nLine 2\nLine 3").unwrap();
    
    // Test edit_file
    let params = json!({
        "path": file_path.to_str().unwrap(),
        "old_text": "Line 2",
        "new_text": "Modified Line 2"
    });
    
    let result = registry.execute("edit_file", params).await;
    assert!(result.is_ok());
    
    // Verify the edit
    let content = fs::read_to_string(&file_path).unwrap();
    assert!(content.contains("Modified Line 2"));
    assert!(content.contains("Line 1"));
    assert!(content.contains("Line 3"));
}

#[tokio::test]
async fn test_git_operations() {
    let registry = ToolRegistry::new();
    
    // Test git_status (should work in current repo)
    let params = json!({});
    let result = registry.execute("git_status", params).await;
    
    // Should succeed if in a git repo
    if result.is_ok() {
        let status = result.unwrap();
        assert!(status.get("status").is_some());
        assert!(status.get("clean").is_some());
    }
    
    // Test git_diff
    let params = json!({
        "staged": false
    });
    
    let result = registry.execute("git_diff", params).await;
    if result.is_ok() {
        let diff = result.unwrap();
        assert!(diff.get("diff").is_some());
    }
}

#[tokio::test]
async fn test_find_files() {
    let registry = ToolRegistry::new();
    let temp_dir = TempDir::new().unwrap();
    
    // Create test files with pattern
    fs::write(temp_dir.path().join("test1.rs"), "").unwrap();
    fs::write(temp_dir.path().join("test2.rs"), "").unwrap();
    fs::write(temp_dir.path().join("other.txt"), "").unwrap();
    
    // Find .rs files
    let params = json!({
        "pattern": "*.rs",
        "path": temp_dir.path().to_str().unwrap()
    });
    
    let result = registry.execute("find_files", params).await.unwrap();
    let files = result["files"].as_array().unwrap();
    assert_eq!(files.len(), 2);
}

#[tokio::test]
async fn test_tool_not_found() {
    let registry = ToolRegistry::new();
    let params = json!({});
    
    let result = registry.execute("non_existent_tool", params).await;
    assert!(result.is_err());
    
    match result.unwrap_err() {
        ToolError::NotFound(name) => {
            assert_eq!(name, "non_existent_tool");
        }
        _ => panic!("Expected NotFound error"),
    }
}

#[tokio::test]
async fn test_invalid_params() {
    let registry = ToolRegistry::new();
    
    // Missing required parameter
    let params = json!({});
    let result = registry.execute("read_file", params).await;
    assert!(result.is_err());
    
    match result.unwrap_err() {
        ToolError::InvalidParams(_) => {
            // Expected error
        }
        _ => panic!("Expected InvalidParams error"),
    }
}

#[tokio::test]
async fn test_all_tools_have_schema() {
    let registry = ToolRegistry::new();
    let tools = registry.list_tools();
    
    for tool in tools {
        // Check that each tool has required schema fields
        assert!(!tool.name.is_empty());
        assert!(!tool.description.is_empty());
        assert!(!tool.category.is_empty());
        
        // Check schema is valid JSON object
        assert!(tool.schema.is_object());
        let schema = tool.schema.as_object().unwrap();
        assert_eq!(schema.get("type").unwrap(), "object");
        assert!(schema.contains_key("properties"));
    }
}