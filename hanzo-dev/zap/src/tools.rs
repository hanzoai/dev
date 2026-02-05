//! Native tool definitions for the complete agentic tool surface.
//!
//! This module defines all standard tools that agents can invoke through ZAP.
//! Tools are organized by category and provide typed schemas for arguments and results.
//!
//! ## Tool Categories
//!
//! 1. **Computer/OS** - Window, input, screen, process, filesystem, packaging
//! 2. **Browser** - Navigation, DOM, network, storage, auth, extraction, render
//! 3. **Vision** - UI understanding, OCR, layout detection
//! 4. **LSP/IDE** - Language server protocol, project model, refactoring
//! 5. **VCS** - Git operations, code review, PR management
//! 6. **Build/Test** - Compilation, testing, validation, coverage
//! 7. **Debug** - Debugger, profiling, tracing
//! 8. **Containers** - Docker, Kubernetes, VM management
//! 9. **Cloud** - IaC, secrets, deploy, DNS/CDN
//! 10. **Network** - HTTP, gRPC, SSH, port scanning
//! 11. **Data** - Database clients, migrations, queries
//! 12. **Security** - Policy, secrets detection, SAST/DAST, signing
//! 13. **Knowledge** - Search, docs, embeddings, citations
//! 14. **Plan** - Intent, routing, DAG composition, scheduling

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

// ============================================================================
// 1. COMPUTER / OS AUTOMATION
// ============================================================================

/// Window manager operations
pub mod window {
    use super::*;

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct ListWindowsArgs {}

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Window {
        pub id: String,
        pub title: String,
        pub app: String,
        pub x: i32,
        pub y: i32,
        pub width: u32,
        pub height: u32,
        pub focused: bool,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct FocusWindowArgs {
        pub window_id: String,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct MoveWindowArgs {
        pub window_id: String,
        pub x: i32,
        pub y: i32,
        pub width: Option<u32>,
        pub height: Option<u32>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct ScreenshotWindowArgs {
        pub window_id: Option<String>, // None = entire screen
        pub format: Option<String>,    // "png", "jpeg"
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct ClipboardArgs {
        pub action: ClipboardAction,
        pub content: Option<String>,
        pub mime_type: Option<String>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    #[serde(rename_all = "snake_case")]
    pub enum ClipboardAction {
        Get,
        Set,
        Clear,
    }
}

/// Input simulation
pub mod input {
    use super::*;

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct KeyPressArgs {
        pub key: String,            // "a", "Enter", "Ctrl+C"
        pub modifiers: Vec<String>, // ["ctrl", "shift", "alt", "meta"]
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct TextInsertArgs {
        pub text: String,
        pub delay_ms: Option<u32>, // Typing delay between chars
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct MouseMoveArgs {
        pub x: i32,
        pub y: i32,
        pub duration_ms: Option<u32>, // Smooth movement duration
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct MouseClickArgs {
        pub x: i32,
        pub y: i32,
        pub button: MouseButton,
        pub clicks: Option<u32>, // 1=single, 2=double, 3=triple
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    #[serde(rename_all = "snake_case")]
    pub enum MouseButton {
        Left,
        Right,
        Middle,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct MouseDragArgs {
        pub start_x: i32,
        pub start_y: i32,
        pub end_x: i32,
        pub end_y: i32,
        pub button: MouseButton,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct ScrollArgs {
        pub x: i32,
        pub y: i32,
        pub delta_x: i32,
        pub delta_y: i32,
    }
}

/// Screen operations
pub mod screen {
    use super::*;

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct ScreenshotArgs {
        pub region: Option<Region>,
        pub format: Option<String>, // "png", "jpeg", "webp"
        pub quality: Option<u8>,    // 1-100 for lossy formats
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Region {
        pub x: i32,
        pub y: i32,
        pub width: u32,
        pub height: u32,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct VideoCaptureArgs {
        pub region: Option<Region>,
        pub fps: Option<u32>,
        pub duration_secs: Option<u32>,
        pub format: Option<String>, // "mp4", "webm", "gif"
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct PixelSampleArgs {
        pub x: i32,
        pub y: i32,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct PixelColor {
        pub r: u8,
        pub g: u8,
        pub b: u8,
        pub a: u8,
    }
}

/// Process management
pub mod process {
    use super::*;

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct ExecArgs {
        pub command: String,
        pub args: Option<Vec<String>>,
        pub cwd: Option<String>,
        pub env: Option<HashMap<String, String>>,
        pub timeout_ms: Option<u64>,
        pub stdin: Option<String>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct ExecResult {
        pub exit_code: i32,
        pub stdout: String,
        pub stderr: String,
        pub duration_ms: u64,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct PsArgs {
        pub filter: Option<String>, // Process name filter
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct ProcessInfo {
        pub pid: u32,
        pub ppid: Option<u32>,
        pub name: String,
        pub cpu_percent: Option<f32>,
        pub memory_percent: Option<f32>,
        pub status: Option<String>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct KillProcessArgs {
        pub pid: u32,
        pub signal: Option<i32>, // Signal number (15=SIGTERM, 9=SIGKILL)
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct GetEnvArgs {
        pub name: Option<String>, // None = return all env vars
    }
}

/// Filesystem operations
pub mod filesystem {
    use super::*;

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct ReadFileArgs {
        pub path: String,
        pub offset: Option<u64>,
        pub limit: Option<u64>,
        pub encoding: Option<String>, // "utf-8", "base64", "binary"
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct WriteFileArgs {
        pub path: String,
        pub content: String,
        pub encoding: Option<String>,
        pub create_dirs: Option<bool>,
        pub overwrite: Option<bool>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct EditFileArgs {
        pub path: String,
        pub edits: Vec<Edit>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Edit {
        pub old_text: String,
        pub new_text: String,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct MkdirArgs {
        pub path: String,
        pub recursive: Option<bool>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct RmArgs {
        pub path: String,
        pub recursive: Option<bool>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct StatArgs {
        pub path: String,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct FileStat {
        pub path: String,
        pub size: u64,
        pub is_file: bool,
        pub is_dir: bool,
        pub is_symlink: bool,
        pub created: Option<i64>,
        pub modified: Option<i64>,
        pub accessed: Option<i64>,
        pub permissions: String,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct WatchArgs {
        pub paths: Vec<String>,
        pub recursive: Option<bool>,
        pub events: Option<Vec<String>>, // "create", "modify", "delete", "rename"
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct GlobArgs {
        pub pattern: String,
        pub base_path: Option<String>,
        pub max_results: Option<u32>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct GrepArgs {
        pub pattern: String,
        pub path: String,
        pub recursive: Option<bool>,
        pub ignore_case: Option<bool>,
        pub max_results: Option<u32>,
        pub context_lines: Option<u32>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct GrepMatch {
        pub path: String,
        pub line_number: u32,
        pub line: String,
        pub context_before: Option<Vec<String>>,
        pub context_after: Option<Vec<String>>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct ListDirArgs {
        pub path: String,
        pub recursive: Option<bool>,
        pub show_hidden: Option<bool>,
        pub max_depth: Option<u32>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct DirEntry {
        pub name: String,
        pub path: String,
        pub is_dir: bool,
        pub size: Option<u64>,
        pub modified: Option<String>,
    }
}

/// Archive/packaging operations
pub mod packaging {
    use super::*;

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct ArchiveArgs {
        pub paths: Vec<String>,
        pub output: String,
        pub format: ArchiveFormat,
        pub compression: Option<CompressionLevel>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    #[serde(rename_all = "snake_case")]
    pub enum ArchiveFormat {
        Zip,
        TarGz,
        TarXz,
        TarBz2,
        SevenZip,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    #[serde(rename_all = "snake_case")]
    pub enum CompressionLevel {
        None,
        Fast,
        Normal,
        Best,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct UnarchiveArgs {
        pub path: String,
        pub output_dir: String,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct ChecksumArgs {
        pub path: String,
        pub algorithm: ChecksumAlgorithm,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    #[serde(rename_all = "snake_case")]
    pub enum ChecksumAlgorithm {
        Md5,
        Sha1,
        Sha256,
        Sha512,
        Blake3,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct FileTypeArgs {
        pub path: String,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct FileType {
        pub mime_type: String,
        pub extension: Option<String>,
        pub description: String,
    }
}

// ============================================================================
// 2. BROWSER AUTOMATION
// ============================================================================

pub mod browser {
    use super::*;

    /// Navigation
    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct NavigateArgs {
        pub url: String,
        pub wait_until: Option<WaitUntil>,
        pub timeout_ms: Option<u64>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    #[serde(rename_all = "snake_case")]
    pub enum WaitUntil {
        Load,
        DomContentLoaded,
        NetworkIdle,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct BackForwardArgs {
        pub direction: Direction,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    #[serde(rename_all = "snake_case")]
    pub enum Direction {
        Back,
        Forward,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct NewTabArgs {
        pub url: Option<String>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct SetViewportArgs {
        pub width: u32,
        pub height: u32,
        pub device_scale_factor: Option<f32>,
        pub is_mobile: Option<bool>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct SetUserAgentArgs {
        pub user_agent: String,
    }

    /// DOM automation
    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct QuerySelectorArgs {
        pub selector: String,
        pub all: Option<bool>,
        pub timeout_ms: Option<u64>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Element {
        pub selector: String,
        pub tag: String,
        pub text: Option<String>,
        pub attributes: HashMap<String, String>,
        pub bounding_box: Option<BoundingBox>,
        pub visible: bool,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct BoundingBox {
        pub x: f32,
        pub y: f32,
        pub width: f32,
        pub height: f32,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct ClickArgs {
        pub selector: String,
        pub button: Option<String>, // "left", "right", "middle"
        pub click_count: Option<u32>,
        pub delay_ms: Option<u32>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct FillArgs {
        pub selector: String,
        pub value: String,
        pub clear_first: Option<bool>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct SubmitArgs {
        pub selector: String, // Form selector
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct WaitForSelectorArgs {
        pub selector: String,
        pub state: Option<WaitState>,
        pub timeout_ms: Option<u64>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    #[serde(rename_all = "snake_case")]
    pub enum WaitState {
        Attached,
        Detached,
        Visible,
        Hidden,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct ScrollIntoViewArgs {
        pub selector: String,
        pub behavior: Option<String>, // "auto", "smooth"
    }

    /// Network
    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct InterceptRequestArgs {
        pub url_pattern: String,
        pub action: InterceptAction,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    #[serde(rename_all = "snake_case")]
    pub enum InterceptAction {
        Block,
        Modify { headers: HashMap<String, String> },
        Mock { status: u16, body: String },
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct HarExportArgs {
        pub path: String,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct ThrottleArgs {
        pub download_kbps: Option<u32>,
        pub upload_kbps: Option<u32>,
        pub latency_ms: Option<u32>,
    }

    /// Storage
    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct CookiesArgs {
        pub action: StorageAction,
        pub cookies: Option<Vec<Cookie>>,
        pub url: Option<String>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    #[serde(rename_all = "snake_case")]
    pub enum StorageAction {
        Get,
        Set,
        Clear,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Cookie {
        pub name: String,
        pub value: String,
        pub domain: Option<String>,
        pub path: Option<String>,
        pub expires: Option<i64>,
        pub http_only: Option<bool>,
        pub secure: Option<bool>,
        pub same_site: Option<String>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct LocalStorageArgs {
        pub action: StorageAction,
        pub key: Option<String>,
        pub value: Option<String>,
    }

    /// Extraction
    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct ExtractDataArgs {
        pub selector: String,
        pub schema: serde_json::Value, // JSON schema for extraction
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct ExtractTableArgs {
        pub selector: String,
        pub headers: Option<bool>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct DownloadArgs {
        pub url: String,
        pub path: String,
    }

    /// Render
    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct BrowserScreenshotArgs {
        pub selector: Option<String>, // Element or full page
        pub format: Option<String>,
        pub quality: Option<u8>,
        pub full_page: Option<bool>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct PdfPrintArgs {
        pub path: String,
        pub format: Option<String>, // "A4", "Letter"
        pub margin: Option<PdfMargin>,
        pub print_background: Option<bool>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct PdfMargin {
        pub top: String,
        pub right: String,
        pub bottom: String,
        pub left: String,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct A11yTreeArgs {
        pub selector: Option<String>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct A11yNode {
        pub role: String,
        pub name: Option<String>,
        pub value: Option<String>,
        pub description: Option<String>,
        pub children: Vec<A11yNode>,
    }
}

// ============================================================================
// 3. COMPUTER VISION / UI UNDERSTANDING
// ============================================================================

pub mod vision {
    use super::*;

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct DetectElementsArgs {
        pub screenshot: Option<String>,         // Base64 or path
        pub element_types: Option<Vec<String>>, // "button", "input", "link"
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct DetectedElement {
        pub element_type: String,
        pub bounding_box: screen::Region,
        pub confidence: f32,
        pub text: Option<String>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct RegionDiffArgs {
        pub image1: String,
        pub image2: String,
        pub threshold: Option<f32>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct DiffResult {
        pub changed: bool,
        pub diff_percent: f32,
        pub diff_regions: Vec<screen::Region>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct DetectLayoutArgs {
        pub screenshot: Option<String>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct LayoutLandmarks {
        pub navigation: Option<screen::Region>,
        pub content: Option<screen::Region>,
        pub sidebar: Option<screen::Region>,
        pub footer: Option<screen::Region>,
        pub forms: Vec<screen::Region>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct OcrArgs {
        pub screenshot: Option<String>,
        pub region: Option<screen::Region>,
        pub language: Option<String>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct OcrResult {
        pub text: String,
        pub blocks: Vec<OcrBlock>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct OcrBlock {
        pub text: String,
        pub bounding_box: screen::Region,
        pub confidence: f32,
    }
}

// ============================================================================
// 4. LSP / IDE TOOLCHAIN
// ============================================================================

pub mod lsp {
    use super::*;

    /// Initialize LSP
    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct InitializeArgs {
        pub root_uri: String,
        pub workspace_folders: Vec<WorkspaceFolder>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct WorkspaceFolder {
        pub uri: String,
        pub name: String,
    }

    /// Document operations
    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct DidOpenArgs {
        pub uri: String,
        pub language_id: String,
        pub version: u32,
        pub text: String,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct DidChangeArgs {
        pub uri: String,
        pub version: u32,
        pub changes: Vec<TextChange>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct TextChange {
        pub range: Option<Range>,
        pub text: String,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Range {
        pub start: Position,
        pub end: Position,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Position {
        pub line: u32,
        pub character: u32,
    }

    /// Completion
    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct CompletionArgs {
        pub uri: String,
        pub position: Position,
        pub trigger_character: Option<String>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct CompletionItem {
        pub label: String,
        pub kind: Option<String>,
        pub detail: Option<String>,
        pub documentation: Option<String>,
        pub insert_text: Option<String>,
        pub text_edit: Option<TextEdit>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct TextEdit {
        pub range: Range,
        pub new_text: String,
    }

    /// Hover
    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct HoverArgs {
        pub uri: String,
        pub position: Position,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct HoverResult {
        pub contents: String,
        pub range: Option<Range>,
    }

    /// Navigation
    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct DefinitionArgs {
        pub uri: String,
        pub position: Position,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Location {
        pub uri: String,
        pub range: Range,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct ReferencesArgs {
        pub uri: String,
        pub position: Position,
        pub include_declaration: Option<bool>,
    }

    /// Symbols
    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct DocumentSymbolsArgs {
        pub uri: String,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Symbol {
        pub name: String,
        pub kind: String,
        pub range: Range,
        pub selection_range: Range,
        pub children: Option<Vec<Symbol>>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct WorkspaceSymbolsArgs {
        pub query: String,
    }

    /// Refactoring
    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct RenameArgs {
        pub uri: String,
        pub position: Position,
        pub new_name: String,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct WorkspaceEdit {
        pub changes: HashMap<String, Vec<TextEdit>>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct CodeActionArgs {
        pub uri: String,
        pub range: Range,
        pub diagnostics: Vec<Diagnostic>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct CodeAction {
        pub title: String,
        pub kind: Option<String>,
        pub diagnostics: Option<Vec<Diagnostic>>,
        pub edit: Option<WorkspaceEdit>,
    }

    /// Formatting
    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct FormatArgs {
        pub uri: String,
        pub range: Option<Range>,
        pub options: FormatOptions,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct FormatOptions {
        pub tab_size: u32,
        pub insert_spaces: bool,
    }

    /// Diagnostics
    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Diagnostic {
        pub range: Range,
        pub severity: Option<DiagnosticSeverity>,
        pub code: Option<String>,
        pub source: Option<String>,
        pub message: String,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    #[serde(rename_all = "snake_case")]
    pub enum DiagnosticSeverity {
        Error,
        Warning,
        Information,
        Hint,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct InlayHintsArgs {
        pub uri: String,
        pub range: Range,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct InlayHint {
        pub position: Position,
        pub label: String,
        pub kind: Option<String>,
    }
}

/// Project model (build graph, dependencies)
pub mod project {
    use super::*;

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct BuildGraphArgs {
        pub root: String,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct BuildGraph {
        pub targets: Vec<BuildTarget>,
        pub edges: Vec<(String, String)>, // (from, to)
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct BuildTarget {
        pub name: String,
        pub kind: String,
        pub path: String,
        pub sources: Vec<String>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct DependencyGraphArgs {
        pub root: String,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Dependency {
        pub name: String,
        pub version: String,
        pub dependencies: Vec<String>,
    }
}

/// Code transformation (pure codemod)
pub mod codemod {
    use super::*;

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct ParseArgs {
        pub source: String,
        pub language: String,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Ast {
        pub root: AstNode,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct AstNode {
        pub kind: String,
        pub text: Option<String>,
        pub range: lsp::Range,
        pub children: Vec<AstNode>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct TransformArgs {
        pub ast: Ast,
        pub transforms: Vec<Transform>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Transform {
        pub kind: String,
        pub target: String, // Selector for nodes
        pub replacement: String,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct SerializeArgs {
        pub ast: Ast,
    }
}

// ============================================================================
// 5. VCS / CODE REVIEW
// ============================================================================

pub mod vcs {
    use super::*;

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct StatusArgs {
        pub path: Option<String>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct StatusResult {
        pub branch: String,
        pub ahead: Option<u32>,
        pub behind: Option<u32>,
        pub staged: Vec<String>,
        pub unstaged: Vec<String>,
        pub untracked: Vec<String>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct GitStatus {
        pub branch: String,
        pub ahead: u32,
        pub behind: u32,
        pub staged: Vec<FileChange>,
        pub unstaged: Vec<FileChange>,
        pub untracked: Vec<String>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct CommitResult {
        pub commit: String,
        pub message: String,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct FileChange {
        pub path: String,
        pub status: ChangeStatus,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    #[serde(rename_all = "snake_case")]
    pub enum ChangeStatus {
        Added,
        Modified,
        Deleted,
        Renamed,
        Copied,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct DiffArgs {
        pub path: Option<String>,
        pub staged: Option<bool>,
        pub commit: Option<String>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct ApplyPatchArgs {
        pub patch: String,
        pub reverse: Option<bool>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct CommitArgs {
        pub message: String,
        pub files: Option<Vec<String>>,
        pub amend: Option<bool>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct LogArgs {
        pub path: Option<String>,
        pub limit: Option<u32>,
        pub since: Option<String>,
        pub until: Option<String>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct LogEntry {
        pub commit: String,
        pub author: String,
        pub email: String,
        pub timestamp: String,
        pub message: String,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct BlameArgs {
        pub path: String,
        pub start_line: Option<u32>,
        pub end_line: Option<u32>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct BlameLine {
        pub line_number: u32,
        pub commit: String,
        pub author: String,
        pub date: String,
        pub content: String,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct BranchArgs {
        pub action: BranchAction,
        pub name: Option<String>,
        pub from: Option<String>,
        pub force: Option<bool>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    #[serde(rename_all = "snake_case")]
    pub enum BranchAction {
        List,
        Create,
        Delete,
        Switch,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct BranchInfo {
        pub name: String,
        pub upstream: Option<String>,
        pub is_current: bool,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct MergeArgs {
        pub branch: String,
        pub strategy: Option<String>,
        pub no_commit: Option<bool>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct RebaseArgs {
        pub onto: String,
        pub interactive: Option<bool>,
    }

    /// Pull request operations
    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct PrCreateArgs {
        pub title: String,
        pub body: String,
        pub base: String,
        pub head: String,
        pub draft: Option<bool>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct PrUpdateArgs {
        pub pr_number: u32,
        pub title: Option<String>,
        pub body: Option<String>,
        pub state: Option<String>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct PrCommentArgs {
        pub pr_number: u32,
        pub body: String,
        pub path: Option<String>,
        pub line: Option<u32>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct PrReviewArgs {
        pub pr_number: u32,
        pub event: ReviewEvent,
        pub body: Option<String>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    #[serde(rename_all = "snake_case")]
    pub enum ReviewEvent {
        Approve,
        RequestChanges,
        Comment,
    }
}

// ============================================================================
// 6. BUILD / TEST / VALIDATE
// ============================================================================

pub mod build {
    use super::*;

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct BuildArgs {
        pub target: Option<String>,
        pub release: Option<bool>,
        pub features: Option<Vec<String>>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct BuildResult {
        pub success: bool,
        pub duration_ms: u64,
        pub stdout: String,
        pub stderr: String,
        pub artifacts: Vec<String>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct BuildError {
        pub path: String,
        pub line: Option<u32>,
        pub column: Option<u32>,
        pub message: String,
        pub code: Option<String>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct TestArgs {
        pub filter: Option<String>,
        pub verbose: Option<bool>,
        pub coverage: Option<bool>,
        pub timeout_ms: Option<u64>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct TestResult {
        pub success: bool,
        pub duration_ms: u64,
        pub passed: u32,
        pub failed: u32,
        pub skipped: u32,
        pub stdout: String,
        pub stderr: String,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct TestCase {
        pub name: String,
        pub status: TestStatus,
        pub duration_ms: u64,
        pub message: Option<String>,
        pub stdout: Option<String>,
        pub stderr: Option<String>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    #[serde(rename_all = "snake_case")]
    pub enum TestStatus {
        Passed,
        Failed,
        Skipped,
        Error,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Coverage {
        pub line_percent: f32,
        pub branch_percent: Option<f32>,
        pub files: Vec<FileCoverage>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct FileCoverage {
        pub path: String,
        pub line_percent: f32,
        pub lines_covered: u32,
        pub lines_total: u32,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct LintArgs {
        pub path: Option<String>,
        pub fix: Option<bool>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct LintResult {
        pub success: bool,
        pub warnings: Vec<String>,
        pub errors: Vec<String>,
        pub stdout: String,
        pub stderr: String,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct TypecheckArgs {
        pub path: Option<String>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct TypecheckResult {
        pub success: bool,
        pub errors: Vec<String>,
        pub stdout: String,
        pub stderr: String,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct FormatCheckArgs {
        pub path: Option<String>,
        pub write: Option<bool>,
    }
}

// ============================================================================
// 7. RUNTIME / DEBUGGING
// ============================================================================

pub mod debug {
    use super::*;

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct AttachArgs {
        pub pid: Option<u32>,
        pub command: Option<Vec<String>>,
        pub debugger: Option<String>, // "gdb", "lldb", "node"
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct BreakpointArgs {
        pub action: BreakpointAction,
        pub location: Option<BreakpointLocation>,
        pub id: Option<u32>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    #[serde(rename_all = "snake_case")]
    pub enum BreakpointAction {
        Set,
        Remove,
        Enable,
        Disable,
        List,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct BreakpointLocation {
        pub file: String,
        pub line: u32,
        pub condition: Option<String>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct StepArgs {
        pub action: StepAction,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    #[serde(rename_all = "snake_case")]
    pub enum StepAction {
        Into,
        Over,
        Out,
        Continue,
        Pause,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct StackTraceArgs {}

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct StackFrame {
        pub id: u32,
        pub name: String,
        pub file: Option<String>,
        pub line: Option<u32>,
        pub column: Option<u32>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct VariablesArgs {
        pub scope: VariableScope,
        pub frame_id: Option<u32>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    #[serde(rename_all = "snake_case")]
    pub enum VariableScope {
        Local,
        Arguments,
        Global,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Variable {
        pub name: String,
        pub value: String,
        pub var_type: String,
        pub children: Option<Vec<Variable>>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct ProfileArgs {
        pub kind: ProfileKind,
        pub duration_secs: Option<u32>,
        pub output: Option<String>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    #[serde(rename_all = "snake_case")]
    pub enum ProfileKind {
        Cpu,
        Heap,
        Allocations,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct TraceArgs {
        pub spans: Option<Vec<String>>,
        pub output: Option<String>,
    }
}

// ============================================================================
// 8. CONTAINERS / VIRTUALIZATION
// ============================================================================

pub mod container {
    use super::*;

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct DockerBuildArgs {
        pub context: String,
        pub dockerfile: Option<String>,
        pub tag: String,
        pub build_args: Option<HashMap<String, String>>,
        pub no_cache: Option<bool>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct DockerRunArgs {
        pub image: String,
        pub command: Option<Vec<String>>,
        pub env: Option<HashMap<String, String>>,
        pub volumes: Option<Vec<String>>,
        pub ports: Option<Vec<String>>,
        pub detach: Option<bool>,
        pub rm: Option<bool>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct DockerExecArgs {
        pub container: String,
        pub command: Vec<String>,
        pub interactive: Option<bool>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct DockerLogsArgs {
        pub container: String,
        pub follow: Option<bool>,
        pub tail: Option<u32>,
        pub since: Option<String>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct ComposeArgs {
        pub action: ComposeAction,
        pub file: Option<String>,
        pub services: Option<Vec<String>>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    #[serde(rename_all = "snake_case")]
    pub enum ComposeAction {
        Up,
        Down,
        Restart,
        Logs,
        Ps,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct ImageScanArgs {
        pub image: String,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct ScanResult {
        pub vulnerabilities: Vec<Vulnerability>,
        pub summary: VulnSummary,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Vulnerability {
        pub id: String,
        pub package: String,
        pub version: String,
        pub severity: String,
        pub description: String,
        pub fix_version: Option<String>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct VulnSummary {
        pub critical: u32,
        pub high: u32,
        pub medium: u32,
        pub low: u32,
    }

    /// Kubernetes
    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct KubeApplyArgs {
        pub manifest: String,
        pub namespace: Option<String>,
        pub dry_run: Option<bool>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct KubeRolloutArgs {
        pub action: RolloutAction,
        pub resource: String,
        pub namespace: Option<String>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    #[serde(rename_all = "snake_case")]
    pub enum RolloutAction {
        Status,
        History,
        Undo,
        Restart,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct KubeLogsArgs {
        pub pod: String,
        pub container: Option<String>,
        pub namespace: Option<String>,
        pub follow: Option<bool>,
        pub tail: Option<u32>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct KubePortForwardArgs {
        pub resource: String,
        pub ports: Vec<String>,
        pub namespace: Option<String>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct KubeExecArgs {
        pub pod: String,
        pub command: Vec<String>,
        pub container: Option<String>,
        pub namespace: Option<String>,
    }
}

// ============================================================================
// 9. CLOUD / INFRA
// ============================================================================

pub mod cloud {
    use super::*;

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct IacPlanArgs {
        pub tool: IacTool,
        pub path: String,
        pub vars: Option<HashMap<String, String>>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    #[serde(rename_all = "snake_case")]
    pub enum IacTool {
        Terraform,
        Pulumi,
        Helm,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct IacApplyArgs {
        pub tool: IacTool,
        pub path: String,
        pub auto_approve: Option<bool>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct SecretRefArgs {
        pub provider: SecretProvider,
        pub path: String,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    #[serde(rename_all = "snake_case")]
    pub enum SecretProvider {
        AwsSecretsManager,
        GcpSecretManager,
        AzureKeyVault,
        Vault,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct DeployArgs {
        pub service: String,
        pub image: String,
        pub environment: String,
        pub canary_percent: Option<u32>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct RollbackArgs {
        pub service: String,
        pub environment: String,
        pub revision: Option<String>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct DnsRecordArgs {
        pub action: DnsAction,
        pub zone: String,
        pub name: String,
        pub record_type: Option<String>,
        pub value: Option<String>,
        pub ttl: Option<u32>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    #[serde(rename_all = "snake_case")]
    pub enum DnsAction {
        Get,
        Create,
        Update,
        Delete,
        List,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct CdnPurgeArgs {
        pub distribution: String,
        pub paths: Vec<String>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct CostArgs {
        pub service: Option<String>,
        pub start_date: String,
        pub end_date: String,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct CostReport {
        pub total: f64,
        pub currency: String,
        pub by_service: HashMap<String, f64>,
    }
}

// ============================================================================
// 10. NETWORKING
// ============================================================================

pub mod network {
    use super::*;

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct HttpRequestArgs {
        pub method: String,
        pub url: String,
        pub headers: Option<HashMap<String, String>>,
        pub body: Option<String>,
        pub json: Option<serde_json::Value>,
        pub timeout_ms: Option<u64>,
        pub follow_redirects: Option<bool>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct HttpResponse {
        pub status: u16,
        pub headers: HashMap<String, String>,
        pub body: String,
        pub duration_ms: u64,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct FetchUrlArgs {
        pub url: String,
        pub headers: Option<HashMap<String, String>>,
        pub binary: Option<bool>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct FetchResult {
        pub status: u16,
        pub content: String,
        pub duration_ms: u64,
        pub content_type: Option<String>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct PortCheckArgs {
        pub host: String,
        pub port: u16,
        pub timeout_ms: Option<u64>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct PortCheckResult {
        pub host: String,
        pub port: u16,
        pub is_open: bool,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct DnsLookupArgs {
        pub host: String,
        pub record_type: Option<String>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct DnsLookupResult {
        pub host: String,
        pub addresses: Vec<String>,
        pub record_type: String,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct GrpcCallArgs {
        pub service: String,
        pub method: String,
        pub message: serde_json::Value,
        pub metadata: Option<HashMap<String, String>>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct SshExecArgs {
        pub host: String,
        pub command: String,
        pub user: Option<String>,
        pub key_path: Option<String>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct SftpArgs {
        pub action: SftpAction,
        pub host: String,
        pub remote_path: String,
        pub local_path: Option<String>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    #[serde(rename_all = "snake_case")]
    pub enum SftpAction {
        Get,
        Put,
        List,
        Delete,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct PortScanArgs {
        pub host: String,
        pub ports: Vec<u16>,
        pub timeout_ms: Option<u64>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct PortScanResult {
        pub host: String,
        pub open_ports: Vec<PortInfo>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct PortInfo {
        pub port: u16,
        pub protocol: String,
        pub service: Option<String>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct TcpdumpArgs {
        pub interface: Option<String>,
        pub filter: Option<String>,
        pub count: Option<u32>,
        pub output: Option<String>,
    }
}

// ============================================================================
// 11. DATA LAYER
// ============================================================================

pub mod data {
    use super::*;

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct DbConnectArgs {
        pub connection_string: String,
        pub database: Option<String>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct DbQueryArgs {
        pub query: String,
        pub params: Option<Vec<serde_json::Value>>,
        pub timeout_ms: Option<u64>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct DbQueryResult {
        pub columns: Vec<String>,
        pub rows: Vec<Vec<serde_json::Value>>,
        pub rows_affected: u64,
        pub duration_ms: u64,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct DbExplainArgs {
        pub query: String,
        pub analyze: Option<bool>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct ExplainResult {
        pub plan: String,
        pub cost: Option<f64>,
        pub actual_time_ms: Option<f64>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct MigrationArgs {
        pub action: MigrationAction,
        pub path: Option<String>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    #[serde(rename_all = "snake_case")]
    pub enum MigrationAction {
        Status,
        Plan,
        Apply,
        Rollback,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct BackupArgs {
        pub output: String,
        pub tables: Option<Vec<String>>,
        pub compress: Option<bool>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct RestoreArgs {
        pub input: String,
        pub tables: Option<Vec<String>>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct DataDiffArgs {
        pub table: String,
        pub source_query: String,
        pub target_query: String,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct DiffResult {
        pub added: u64,
        pub removed: u64,
        pub modified: u64,
        pub unchanged: u64,
    }
}

// ============================================================================
// 12. SECURITY / COMPLIANCE
// ============================================================================

pub mod security {
    use super::*;

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct PolicyCheckArgs {
        pub policy: String,
        pub input: serde_json::Value,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct PolicyResult {
        pub allowed: bool,
        pub reasons: Vec<String>,
        pub violations: Vec<PolicyViolation>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct PolicyViolation {
        pub rule: String,
        pub message: String,
        pub severity: String,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct SecretScanArgs {
        pub path: String,
        pub recursive: Option<bool>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct SecretFinding {
        pub file: String,
        pub line: u32,
        pub secret_type: String,
        pub redacted: String,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct SastArgs {
        pub path: String,
        pub rules: Option<Vec<String>>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct SastFinding {
        pub file: String,
        pub line: u32,
        pub rule: String,
        pub severity: String,
        pub message: String,
        pub cwe: Option<String>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct DastArgs {
        pub target_url: String,
        pub auth: Option<DastAuth>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct DastAuth {
        pub login_url: String,
        pub username: String,
        pub password_ref: String, // Reference, never raw
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct DastFinding {
        pub url: String,
        pub vulnerability: String,
        pub severity: String,
        pub evidence: String,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct DepScanArgs {
        pub path: String,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct DepVulnerability {
        pub package: String,
        pub version: String,
        pub cve: String,
        pub severity: String,
        pub fixed_in: Option<String>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct SignArgs {
        pub artifact: String,
        pub key_ref: String,
        pub format: Option<String>, // "cosign", "gpg", "sigstore"
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct VerifyArgs {
        pub artifact: String,
        pub signature: String,
        pub key_ref: Option<String>,
    }
}

// ============================================================================
// 13. KNOWLEDGE / SEARCH
// ============================================================================

pub mod knowledge {
    use super::*;

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct LocalSearchArgs {
        pub query: String,
        pub path: Option<String>,
        pub file_types: Option<Vec<String>>,
        pub max_results: Option<u32>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct SearchResult {
        pub path: String,
        pub line: Option<u32>,
        pub snippet: String,
        pub score: f32,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct SemanticSearchArgs {
        pub query: String,
        pub index: String,
        pub top_k: Option<u32>,
        pub threshold: Option<f32>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct SemanticResult {
        pub id: String,
        pub content: String,
        pub metadata: HashMap<String, String>,
        pub score: f32,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct FetchDocArgs {
        pub url: String,
        pub format: Option<DocFormat>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    #[serde(rename_all = "snake_case")]
    pub enum DocFormat {
        Html,
        Pdf,
        Markdown,
        Text,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct FetchedDoc {
        pub url: String,
        pub title: Option<String>,
        pub content: String,
        pub format: String,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct CitationArgs {
        pub action: CitationAction,
        pub citation: Option<Citation>,
        pub query: Option<String>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    #[serde(rename_all = "snake_case")]
    pub enum CitationAction {
        Add,
        Get,
        Search,
        List,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Citation {
        pub id: String,
        pub url: String,
        pub title: String,
        pub excerpt: String,
        pub accessed: String,
        pub tags: Vec<String>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct EmbedArgs {
        pub texts: Vec<String>,
        pub model: Option<String>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Embeddings {
        pub vectors: Vec<Vec<f32>>,
        pub model: String,
        pub dimensions: u32,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct IndexArgs {
        pub action: IndexAction,
        pub index: String,
        pub documents: Option<Vec<Document>>,
        pub ids: Option<Vec<String>>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    #[serde(rename_all = "snake_case")]
    pub enum IndexAction {
        Create,
        Upsert,
        Delete,
        Clear,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Document {
        pub id: String,
        pub content: String,
        pub metadata: HashMap<String, String>,
    }
}

// ============================================================================
// 14. PLANNING / ORCHESTRATION
// ============================================================================

pub mod plan {
    use super::*;

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct IntentArgs {
        pub description: String,
        pub constraints: Option<Vec<String>>,
        pub context: Option<serde_json::Value>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Intent {
        pub id: String,
        pub description: String,
        pub goals: Vec<Goal>,
        pub context: serde_json::Value,
        pub created_at: String,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Goal {
        pub id: String,
        pub description: String,
        pub priority: u32,
        pub constraints: Option<Vec<String>>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct RouteArgs {
        pub intent: serde_json::Value,
        pub available_tools: Option<Vec<String>>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Route {
        pub id: String,
        pub intent_id: String,
        pub steps: Vec<Step>,
        pub estimated_duration_ms: Option<u64>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Step {
        pub id: String,
        pub tool: String,
        pub args: serde_json::Value,
        pub depends_on: Vec<String>,
        pub description: String,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct ComposeArgs {
        pub routes: serde_json::Value,
        pub parallel: Option<bool>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct Dag {
        pub id: String,
        pub nodes: Vec<DagNode>,
        pub edges: Vec<DagEdge>,
        pub parallel: bool,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct DagNode {
        pub id: String,
        pub tool: String,
        pub args: serde_json::Value,
        pub route_id: String,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct DagEdge {
        pub from: String,
        pub to: String,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    #[serde(rename_all = "snake_case")]
    pub enum DagStatus {
        Pending,
        Running,
        Completed,
        Failed,
        Skipped,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct ExecuteArgs {
        pub dag: Dag,
        pub concurrency: Option<u32>,
        pub retry_policy: Option<RetryPolicy>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct RetryPolicy {
        pub max_attempts: u32,
        pub backoff_ms: u64,
        pub backoff_multiplier: f32,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct ExecutionResult {
        pub dag_id: String,
        pub status: DagStatus,
        pub results: HashMap<String, StepResult>,
        pub duration_ms: u64,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct StepResult {
        pub step_id: String,
        pub status: DagStatus,
        pub output: Option<serde_json::Value>,
        pub error: Option<String>,
        pub attempts: u32,
        pub duration_ms: u64,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct CacheLookupArgs {
        pub key: String,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct CacheResult {
        pub hit: bool,
        pub key: String,
        pub value: Option<serde_json::Value>,
        pub ttl_remaining_ms: Option<u64>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct CacheArgs {
        pub key: String,
        pub action: CacheAction,
        pub value: Option<serde_json::Value>,
        pub ttl_secs: Option<u64>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    #[serde(rename_all = "snake_case")]
    pub enum CacheAction {
        Get,
        Set,
        Delete,
        Has,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct AuditLogArgs {
        pub action: String,
        pub tool: Option<String>,
        pub args: Option<serde_json::Value>,
        pub result: Option<serde_json::Value>,
        pub user: Option<String>,
        pub session_id: Option<String>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct AuditEntry {
        pub id: String,
        pub timestamp: String,
        pub action: String,
        pub tool: Option<String>,
        pub args: Option<serde_json::Value>,
        pub result: Option<serde_json::Value>,
        pub user: Option<String>,
        pub session_id: Option<String>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct AuditFilter {
        pub since: Option<String>,
        pub until: Option<String>,
        pub tool: Option<String>,
        pub user: Option<String>,
    }
}

// ============================================================================
// TOOL REGISTRY
// ============================================================================

/// All available tool categories
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ToolCategory {
    /// Computer/OS automation
    Computer,
    /// Browser automation
    Browser,
    /// Computer vision / UI understanding
    Vision,
    /// LSP/IDE operations
    Lsp,
    /// Version control
    Vcs,
    /// Build/test/validate
    Build,
    /// Debugging/profiling
    Debug,
    /// Container/VM operations
    Container,
    /// Cloud/infra operations
    Cloud,
    /// Network operations
    Network,
    /// Database operations
    Data,
    /// Security/compliance
    Security,
    /// Knowledge/search
    Knowledge,
    /// Planning/orchestration
    Plan,
}

/// Tool definition with schema
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ToolDef {
    pub name: String,
    pub category: ToolCategory,
    pub description: String,
    pub input_schema: serde_json::Value,
    pub output_schema: serde_json::Value,
    pub requires_approval: bool,
    pub idempotent: bool,
}

/// Generate the default tool registry
pub fn default_tools() -> Vec<ToolDef> {
    vec![
        // Filesystem tools (most commonly used)
        ToolDef {
            name: "read_file".to_string(),
            category: ToolCategory::Computer,
            description: "Read contents of a file".to_string(),
            input_schema: serde_json::json!({
                "type": "object",
                "properties": {
                    "path": {"type": "string"},
                    "offset": {"type": "integer"},
                    "limit": {"type": "integer"},
                    "encoding": {"type": "string"}
                },
                "required": ["path"]
            }),
            output_schema: serde_json::json!({"type": "string"}),
            requires_approval: false,
            idempotent: true,
        },
        ToolDef {
            name: "write_file".to_string(),
            category: ToolCategory::Computer,
            description: "Write contents to a file".to_string(),
            input_schema: serde_json::json!({
                "type": "object",
                "properties": {
                    "path": {"type": "string"},
                    "content": {"type": "string"},
                    "encoding": {"type": "string"},
                    "create_dirs": {"type": "boolean"},
                    "overwrite": {"type": "boolean"}
                },
                "required": ["path", "content"]
            }),
            output_schema: serde_json::json!({"type": "boolean"}),
            requires_approval: false,
            idempotent: false,
        },
        ToolDef {
            name: "edit_file".to_string(),
            category: ToolCategory::Computer,
            description: "Apply edits to a file".to_string(),
            input_schema: serde_json::json!({
                "type": "object",
                "properties": {
                    "path": {"type": "string"},
                    "edits": {
                        "type": "array",
                        "items": {
                            "type": "object",
                            "properties": {
                                "old_text": {"type": "string"},
                                "new_text": {"type": "string"}
                            },
                            "required": ["old_text", "new_text"]
                        }
                    }
                },
                "required": ["path", "edits"]
            }),
            output_schema: serde_json::json!({"type": "boolean"}),
            requires_approval: false,
            idempotent: false,
        },
        ToolDef {
            name: "glob".to_string(),
            category: ToolCategory::Computer,
            description: "Find files matching a glob pattern".to_string(),
            input_schema: serde_json::json!({
                "type": "object",
                "properties": {
                    "pattern": {"type": "string"},
                    "base_path": {"type": "string"},
                    "max_results": {"type": "integer"}
                },
                "required": ["pattern"]
            }),
            output_schema: serde_json::json!({"type": "array", "items": {"type": "string"}}),
            requires_approval: false,
            idempotent: true,
        },
        ToolDef {
            name: "grep".to_string(),
            category: ToolCategory::Computer,
            description: "Search file contents with regex".to_string(),
            input_schema: serde_json::json!({
                "type": "object",
                "properties": {
                    "pattern": {"type": "string"},
                    "path": {"type": "string"},
                    "recursive": {"type": "boolean"},
                    "ignore_case": {"type": "boolean"},
                    "max_results": {"type": "integer"},
                    "context_lines": {"type": "integer"}
                },
                "required": ["pattern", "path"]
            }),
            output_schema: serde_json::json!({
                "type": "array",
                "items": {
                    "type": "object",
                    "properties": {
                        "path": {"type": "string"},
                        "line_number": {"type": "integer"},
                        "line": {"type": "string"}
                    }
                }
            }),
            requires_approval: false,
            idempotent: true,
        },
        // Process tools
        ToolDef {
            name: "exec".to_string(),
            category: ToolCategory::Computer,
            description: "Execute a shell command".to_string(),
            input_schema: serde_json::json!({
                "type": "object",
                "properties": {
                    "command": {"type": "string"},
                    "args": {"type": "array", "items": {"type": "string"}},
                    "cwd": {"type": "string"},
                    "env": {"type": "object"},
                    "timeout_ms": {"type": "integer"},
                    "stdin": {"type": "string"}
                },
                "required": ["command"]
            }),
            output_schema: serde_json::json!({
                "type": "object",
                "properties": {
                    "exit_code": {"type": "integer"},
                    "stdout": {"type": "string"},
                    "stderr": {"type": "string"},
                    "duration_ms": {"type": "integer"}
                }
            }),
            requires_approval: true,
            idempotent: false,
        },
        // VCS tools
        ToolDef {
            name: "git_status".to_string(),
            category: ToolCategory::Vcs,
            description: "Get git repository status".to_string(),
            input_schema: serde_json::json!({
                "type": "object",
                "properties": {
                    "path": {"type": "string"}
                }
            }),
            output_schema: serde_json::json!({
                "type": "object",
                "properties": {
                    "branch": {"type": "string"},
                    "staged": {"type": "array"},
                    "unstaged": {"type": "array"},
                    "untracked": {"type": "array"}
                }
            }),
            requires_approval: false,
            idempotent: true,
        },
        ToolDef {
            name: "git_diff".to_string(),
            category: ToolCategory::Vcs,
            description: "Get diff of changes".to_string(),
            input_schema: serde_json::json!({
                "type": "object",
                "properties": {
                    "path": {"type": "string"},
                    "staged": {"type": "boolean"},
                    "commit": {"type": "string"}
                }
            }),
            output_schema: serde_json::json!({"type": "string"}),
            requires_approval: false,
            idempotent: true,
        },
        ToolDef {
            name: "git_commit".to_string(),
            category: ToolCategory::Vcs,
            description: "Create a git commit".to_string(),
            input_schema: serde_json::json!({
                "type": "object",
                "properties": {
                    "message": {"type": "string"},
                    "files": {"type": "array", "items": {"type": "string"}},
                    "amend": {"type": "boolean"}
                },
                "required": ["message"]
            }),
            output_schema: serde_json::json!({
                "type": "object",
                "properties": {
                    "commit": {"type": "string"},
                    "message": {"type": "string"}
                }
            }),
            requires_approval: true,
            idempotent: false,
        },
        // Browser tools
        ToolDef {
            name: "browser_navigate".to_string(),
            category: ToolCategory::Browser,
            description: "Navigate browser to URL".to_string(),
            input_schema: serde_json::json!({
                "type": "object",
                "properties": {
                    "url": {"type": "string"},
                    "wait_until": {"type": "string"},
                    "timeout_ms": {"type": "integer"}
                },
                "required": ["url"]
            }),
            output_schema: serde_json::json!({"type": "boolean"}),
            requires_approval: false,
            idempotent: false,
        },
        ToolDef {
            name: "browser_click".to_string(),
            category: ToolCategory::Browser,
            description: "Click an element".to_string(),
            input_schema: serde_json::json!({
                "type": "object",
                "properties": {
                    "selector": {"type": "string"},
                    "button": {"type": "string"},
                    "click_count": {"type": "integer"}
                },
                "required": ["selector"]
            }),
            output_schema: serde_json::json!({"type": "boolean"}),
            requires_approval: false,
            idempotent: false,
        },
        ToolDef {
            name: "browser_fill".to_string(),
            category: ToolCategory::Browser,
            description: "Fill an input field".to_string(),
            input_schema: serde_json::json!({
                "type": "object",
                "properties": {
                    "selector": {"type": "string"},
                    "value": {"type": "string"},
                    "clear_first": {"type": "boolean"}
                },
                "required": ["selector", "value"]
            }),
            output_schema: serde_json::json!({"type": "boolean"}),
            requires_approval: false,
            idempotent: false,
        },
        ToolDef {
            name: "browser_screenshot".to_string(),
            category: ToolCategory::Browser,
            description: "Take a screenshot".to_string(),
            input_schema: serde_json::json!({
                "type": "object",
                "properties": {
                    "selector": {"type": "string"},
                    "format": {"type": "string"},
                    "full_page": {"type": "boolean"}
                }
            }),
            output_schema: serde_json::json!({"type": "string"}),
            requires_approval: false,
            idempotent: true,
        },
        ToolDef {
            name: "browser_a11y_tree".to_string(),
            category: ToolCategory::Browser,
            description: "Get accessibility tree".to_string(),
            input_schema: serde_json::json!({
                "type": "object",
                "properties": {
                    "selector": {"type": "string"}
                }
            }),
            output_schema: serde_json::json!({
                "type": "object",
                "properties": {
                    "role": {"type": "string"},
                    "name": {"type": "string"},
                    "children": {"type": "array"}
                }
            }),
            requires_approval: false,
            idempotent: true,
        },
        // LSP tools
        ToolDef {
            name: "lsp_definition".to_string(),
            category: ToolCategory::Lsp,
            description: "Go to definition".to_string(),
            input_schema: serde_json::json!({
                "type": "object",
                "properties": {
                    "uri": {"type": "string"},
                    "line": {"type": "integer"},
                    "character": {"type": "integer"}
                },
                "required": ["uri", "line", "character"]
            }),
            output_schema: serde_json::json!({
                "type": "array",
                "items": {
                    "type": "object",
                    "properties": {
                        "uri": {"type": "string"},
                        "range": {"type": "object"}
                    }
                }
            }),
            requires_approval: false,
            idempotent: true,
        },
        ToolDef {
            name: "lsp_references".to_string(),
            category: ToolCategory::Lsp,
            description: "Find all references".to_string(),
            input_schema: serde_json::json!({
                "type": "object",
                "properties": {
                    "uri": {"type": "string"},
                    "line": {"type": "integer"},
                    "character": {"type": "integer"},
                    "include_declaration": {"type": "boolean"}
                },
                "required": ["uri", "line", "character"]
            }),
            output_schema: serde_json::json!({
                "type": "array",
                "items": {
                    "type": "object",
                    "properties": {
                        "uri": {"type": "string"},
                        "range": {"type": "object"}
                    }
                }
            }),
            requires_approval: false,
            idempotent: true,
        },
        ToolDef {
            name: "lsp_rename".to_string(),
            category: ToolCategory::Lsp,
            description: "Rename symbol".to_string(),
            input_schema: serde_json::json!({
                "type": "object",
                "properties": {
                    "uri": {"type": "string"},
                    "line": {"type": "integer"},
                    "character": {"type": "integer"},
                    "new_name": {"type": "string"}
                },
                "required": ["uri", "line", "character", "new_name"]
            }),
            output_schema: serde_json::json!({
                "type": "object",
                "properties": {
                    "changes": {"type": "object"}
                }
            }),
            requires_approval: false,
            idempotent: false,
        },
        ToolDef {
            name: "lsp_symbols".to_string(),
            category: ToolCategory::Lsp,
            description: "Get document symbols".to_string(),
            input_schema: serde_json::json!({
                "type": "object",
                "properties": {
                    "uri": {"type": "string"}
                },
                "required": ["uri"]
            }),
            output_schema: serde_json::json!({
                "type": "array",
                "items": {
                    "type": "object",
                    "properties": {
                        "name": {"type": "string"},
                        "kind": {"type": "string"},
                        "range": {"type": "object"}
                    }
                }
            }),
            requires_approval: false,
            idempotent: true,
        },
        // Build/test tools
        ToolDef {
            name: "run_tests".to_string(),
            category: ToolCategory::Build,
            description: "Run tests".to_string(),
            input_schema: serde_json::json!({
                "type": "object",
                "properties": {
                    "filter": {"type": "string"},
                    "parallel": {"type": "boolean"},
                    "coverage": {"type": "boolean"},
                    "timeout_ms": {"type": "integer"}
                }
            }),
            output_schema: serde_json::json!({
                "type": "object",
                "properties": {
                    "success": {"type": "boolean"},
                    "total": {"type": "integer"},
                    "passed": {"type": "integer"},
                    "failed": {"type": "integer"},
                    "tests": {"type": "array"}
                }
            }),
            requires_approval: true,
            idempotent: true,
        },
        ToolDef {
            name: "lint".to_string(),
            category: ToolCategory::Build,
            description: "Run linter".to_string(),
            input_schema: serde_json::json!({
                "type": "object",
                "properties": {
                    "path": {"type": "string"},
                    "fix": {"type": "boolean"}
                }
            }),
            output_schema: serde_json::json!({
                "type": "object",
                "properties": {
                    "success": {"type": "boolean"},
                    "errors": {"type": "array"},
                    "warnings": {"type": "array"}
                }
            }),
            requires_approval: false,
            idempotent: true,
        },
        ToolDef {
            name: "typecheck".to_string(),
            category: ToolCategory::Build,
            description: "Run type checker".to_string(),
            input_schema: serde_json::json!({
                "type": "object",
                "properties": {
                    "path": {"type": "string"}
                }
            }),
            output_schema: serde_json::json!({
                "type": "object",
                "properties": {
                    "success": {"type": "boolean"},
                    "errors": {"type": "array"}
                }
            }),
            requires_approval: false,
            idempotent: true,
        },
        ToolDef {
            name: "build".to_string(),
            category: ToolCategory::Build,
            description: "Build project".to_string(),
            input_schema: serde_json::json!({
                "type": "object",
                "properties": {
                    "target": {"type": "string"},
                    "release": {"type": "boolean"},
                    "features": {"type": "array", "items": {"type": "string"}}
                }
            }),
            output_schema: serde_json::json!({
                "type": "object",
                "properties": {
                    "success": {"type": "boolean"},
                    "artifacts": {"type": "array"},
                    "errors": {"type": "array"}
                }
            }),
            requires_approval: true,
            idempotent: false,
        },
        // Planning tools
        ToolDef {
            name: "plan_intent".to_string(),
            category: ToolCategory::Plan,
            description: "Create intent from description".to_string(),
            input_schema: serde_json::json!({
                "type": "object",
                "properties": {
                    "description": {"type": "string"},
                    "constraints": {"type": "array", "items": {"type": "string"}},
                    "context": {"type": "object"}
                },
                "required": ["description"]
            }),
            output_schema: serde_json::json!({
                "type": "object",
                "properties": {
                    "id": {"type": "string"},
                    "goals": {"type": "array"}
                }
            }),
            requires_approval: false,
            idempotent: true,
        },
        ToolDef {
            name: "plan_route".to_string(),
            category: ToolCategory::Plan,
            description: "Create execution route from intent".to_string(),
            input_schema: serde_json::json!({
                "type": "object",
                "properties": {
                    "intent": {"type": "object"},
                    "available_tools": {"type": "array", "items": {"type": "string"}}
                },
                "required": ["intent"]
            }),
            output_schema: serde_json::json!({
                "type": "object",
                "properties": {
                    "id": {"type": "string"},
                    "steps": {"type": "array"}
                }
            }),
            requires_approval: false,
            idempotent: true,
        },
        ToolDef {
            name: "plan_compose".to_string(),
            category: ToolCategory::Plan,
            description: "Compose routes into DAG".to_string(),
            input_schema: serde_json::json!({
                "type": "object",
                "properties": {
                    "routes": {"type": "array"},
                    "parallel": {"type": "boolean"}
                },
                "required": ["routes"]
            }),
            output_schema: serde_json::json!({
                "type": "object",
                "properties": {
                    "id": {"type": "string"},
                    "nodes": {"type": "array"},
                    "edges": {"type": "array"}
                }
            }),
            requires_approval: false,
            idempotent: true,
        },
        // Policy check
        ToolDef {
            name: "policy_check".to_string(),
            category: ToolCategory::Security,
            description: "Check operation against policy".to_string(),
            input_schema: serde_json::json!({
                "type": "object",
                "properties": {
                    "policy": {"type": "string"},
                    "input": {"type": "object"}
                },
                "required": ["policy", "input"]
            }),
            output_schema: serde_json::json!({
                "type": "object",
                "properties": {
                    "allowed": {"type": "boolean"},
                    "reasons": {"type": "array"},
                    "violations": {"type": "array"}
                }
            }),
            requires_approval: false,
            idempotent: true,
        },
    ]
}
