//! The agent's `grep` tool: Dev's search engine ([`hanzo_search`]) as one native tool.
//!
//! The model gets a single function tool, `grep`, for file contents (regex), file
//! names (glob) and an identifier's definitions and references, answering in
//! `rg`'s `path:line:text` shape. The base instructions tell the model to use it
//! and to fall back to `rg` in the shell only when it returns an error.
//!
//! # Where it plugs in
//!
//! [`install`] contributes the tool through upstream's extension API
//! (`ToolContributor`) and observes tool starts (`ToolLifecycleContributor`). One
//! anchored edit in `patches/upstream.json` calls it from the app server's
//! `thread_extensions`, beside Kai, and one more per prompt rewrites its `rg`
//! line. That is the whole integration: no tool handler in core, no MCP server
//! (another process and protocol, and no view of Dev's writes), no wrapper around
//! the shell's `rg`.
//!
//! # Freshness
//!
//! Every tool start other than `grep`'s, and every command, calls
//! [`Search::since`]: the tool may write files it does not name, so files changed
//! from then on bypass a lagging index. See [`hanzo_search`] for the rest.
//!
//! The tool answers only for the local environment, whose files this process
//! reads; a remote environment gets an error that sends the model to its shell.

use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;
use std::time::SystemTime;

use codex_exec_server::LOCAL_ENVIRONMENT_ID;
use codex_extension_api::CommandStartInput;
use codex_extension_api::ExtensionData;
use codex_extension_api::ExtensionRegistryBuilder;
use codex_extension_api::FunctionCallError;
use codex_extension_api::ResponsesApiTool;
use codex_extension_api::ToolCall;
use codex_extension_api::ToolContributor;
use codex_extension_api::ToolExecutor;
use codex_extension_api::ToolExecutorFuture;
use codex_extension_api::ToolLifecycleContributor;
use codex_extension_api::ToolLifecycleFuture;
use codex_extension_api::ToolName;
use codex_extension_api::ToolOutput;
use codex_extension_api::ToolPayload;
use codex_extension_api::ToolSpec;
use codex_extension_api::ToolStartInput;
use codex_extension_api::parse_tool_input_schema;
use codex_protocol::models::FunctionCallOutputBody;
use codex_protocol::models::FunctionCallOutputPayload;
use codex_protocol::models::ResponseInputItem;
use hanzo_search::Format;
use hanzo_search::Kind;
use hanzo_search::Query;
use hanzo_search::Search;
use serde::Deserialize;
use serde_json::Value;
use serde_json::json;

/// The tool's model-visible name.
pub const NAME: &str = "grep";

/// Hits returned when the model names no limit.
const LIMIT: usize = 200;
/// The most hits the model may ask for.
const MOST: usize = 2_000;
/// The most context lines the model may ask for.
const CONTEXT: usize = 10;
/// The tool's own output bound, before the host's.
const BYTES: usize = 256 * 1024;

const DESCRIPTION: &str = "Search the workspace through Dev's index. \
kind=text (default): lines matching a regex (or a literal with fixed=true), as path:line:text. \
kind=name: files whose path matches a glob (\"*.rs\" matches by file name at any depth; \"src/**/*_test.py\" by path). \
kind=symbol: lines that spell an identifier on word boundaries, definitions first; textual, so it can miss semantic callers. \
Respects .gitignore and skips hidden and binary files. Always reflects your own edits. \
Results are ranked: files with the most hits first, then path, then line. \
Use this instead of rg, grep or find in the shell; use rg only if this tool returns an error.";

/// Registers the `grep` tool and the observer that keeps it current with Dev's writes.
pub fn install<C: Sync>(builder: &mut ExtensionRegistryBuilder<C>) {
    let search = Arc::new(Search::new());
    builder.tool_contributor(Arc::new(Tools(Arc::clone(&search))));
    builder.tool_lifecycle_contributor(Arc::new(Writes(search)));
}

struct Tools(Arc<Search>);

impl ToolContributor for Tools {
    fn tools(
        &self,
        _session_store: &ExtensionData,
        _thread_store: &ExtensionData,
    ) -> Vec<Arc<dyn for<'call> ToolExecutor<ToolCall<'call>>>> {
        vec![Arc::new(Grep(Arc::clone(&self.0)))]
    }
}

/// Marks the start of every tool and command that may write files.
struct Writes(Arc<Search>);

impl ToolLifecycleContributor for Writes {
    fn on_tool_start<'a>(&'a self, input: ToolStartInput<'a>) -> ToolLifecycleFuture<'a> {
        if !is_grep(input.tool_name) {
            self.0.since(SystemTime::now());
        }
        Box::pin(std::future::ready(()))
    }

    fn on_command_start<'a>(&'a self, _input: CommandStartInput<'a>) -> ToolLifecycleFuture<'a> {
        self.0.since(SystemTime::now());
        Box::pin(std::future::ready(()))
    }
}

fn is_grep(name: &ToolName) -> bool {
    name.is_default_namespace() && name.name == NAME
}

#[derive(Clone, Copy, Debug, Default, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
enum Output {
    #[default]
    Lines,
    Files,
    Count,
}

/// The tool's arguments, as the model sends them.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct Args {
    pattern: String,
    #[serde(default)]
    kind: Kind,
    #[serde(default)]
    paths: Vec<String>,
    #[serde(default)]
    glob: Vec<String>,
    #[serde(default)]
    fixed: bool,
    #[serde(default)]
    ignore_case: bool,
    #[serde(default)]
    context: usize,
    #[serde(default)]
    output: Output,
    limit: Option<usize>,
}

fn schema() -> Value {
    json!({
        "type": "object",
        "properties": {
            "pattern": {
                "type": "string",
                "description": "A regex for kind=text, a glob for kind=name, an identifier for kind=symbol."
            },
            "kind": {"type": "string", "enum": ["text", "name", "symbol"]},
            "paths": {
                "type": "array",
                "items": {"type": "string"},
                "description": "Files or directories to search, relative to the working directory. Default: the working directory."
            },
            "glob": {
                "type": "array",
                "items": {"type": "string"},
                "description": "For kind=text and kind=symbol: only files matching these globs; a leading ! excludes."
            },
            "fixed": {"type": "boolean", "description": "The pattern is a literal string."},
            "ignore_case": {"type": "boolean"},
            "context": {"type": "integer", "description": "Lines of context around each hit, at most 10."},
            "output": {
                "type": "string",
                "enum": ["lines", "files", "count"],
                "description": "lines: path:line:text. files: each matching file once. count: path:N."
            },
            "limit": {"type": "integer", "description": "The most hits, best first. Default 200, at most 2000."}
        },
        "required": ["pattern"],
        "additionalProperties": false
    })
}

struct Grep(Arc<Search>);

impl<'call> ToolExecutor<ToolCall<'call>> for Grep {
    fn tool_name(&self) -> ToolName {
        ToolName::plain(NAME)
    }

    fn spec(&self) -> ToolSpec {
        let parameters = match parse_tool_input_schema(&schema()) {
            Ok(parameters) => parameters,
            Err(error) => panic!("the grep schema is a constant and must parse: {error}"),
        };
        ToolSpec::Function(ResponsesApiTool {
            name: NAME.to_string(),
            description: DESCRIPTION.to_string(),
            strict: false,
            defer_loading: None,
            parameters,
            output_schema: None,
        })
    }

    fn supports_parallel_tool_calls(&self) -> bool {
        true
    }

    fn handle<'a>(&'a self, call: ToolCall<'call>) -> ToolExecutorFuture<'a>
    where
        'call: 'a,
    {
        Box::pin(async move {
            let ToolPayload::Function { arguments } = &call.payload else {
                return Err(FunctionCallError::Fatal(format!(
                    "{NAME} takes function arguments"
                )));
            };
            let args: Args = serde_json::from_str(arguments)
                .map_err(|error| FunctionCallError::RespondToModel(error.to_string()))?;
            let Some(environment) = call
                .environments
                .iter()
                .find(|environment| environment.environment_id == LOCAL_ENVIRONMENT_ID)
            else {
                return Err(FunctionCallError::RespondToModel(format!(
                    "{NAME} searches the local workspace; this environment is remote, so use rg in its shell"
                )));
            };
            let cwd = environment.cwd.to_path_buf();
            let budget = call.response_byte_budget(BYTES);
            let search = Arc::clone(&self.0);
            let body = tokio::task::spawn_blocking(move || answer(&search, args, &cwd, budget))
                .await
                .map_err(|error| FunctionCallError::Fatal(error.to_string()))?
                .map_err(FunctionCallError::RespondToModel)?;
            Ok(Box::new(Text(body)) as Box<dyn ToolOutput>)
        })
    }
}

fn query(args: Args, cwd: &Path) -> (Query, Format) {
    let format = match args.output {
        Output::Lines => Format::Lines,
        Output::Files => Format::Files,
        Output::Count => Format::Count,
    };
    let query = Query {
        kind: args.kind,
        pattern: args.pattern,
        root: cwd.to_path_buf(),
        paths: args.paths.into_iter().map(PathBuf::from).collect(),
        glob: args.glob,
        fixed: args.fixed,
        ignore_case: args.ignore_case,
        context: args.context.min(CONTEXT),
        limit: Some(args.limit.unwrap_or(LIMIT).min(MOST)),
        index: hanzo_search::Index::Auto,
    };
    (query, format)
}

/// The tool's text: the rendered hits within `budget` bytes, or why it failed.
fn answer(search: &Search, args: Args, cwd: &Path, budget: usize) -> Result<String, String> {
    let (query, format) = query(args, cwd);
    let hits = search
        .query(&query)
        .map_err(|error| format!("{NAME} failed: {error}. Use rg in the shell instead."))?;
    if hits.is_empty() {
        return Ok("no matches".to_string());
    }
    let text = hanzo_search::render(&hits, format);
    let limited = query.limit.is_some_and(|limit| hits.len() >= limit);
    if text.len() <= budget && !limited {
        return Ok(text);
    }
    let footer = format!(
        "[{} hits shown, best first; more may exist: narrow paths or glob, or raise limit]\n",
        hits.len()
    );
    if text.len() + footer.len() <= budget {
        return Ok(text + &footer);
    }
    let room = budget.saturating_sub(footer.len());
    let cut = text[..text.floor_char_boundary(room)]
        .rfind('\n')
        .map_or(0, |at| at + 1);
    let shown = text[..cut].lines().count();
    Ok(format!(
        "{}[output cut at {shown} lines of {}: narrow paths or glob, or lower limit]\n",
        &text[..cut],
        text.lines().count()
    ))
}

/// Plain text for the model.
struct Text(String);

impl ToolOutput for Text {
    fn log_output(&self) -> String {
        self.0.clone()
    }

    fn success_for_logging(&self) -> bool {
        true
    }

    fn to_response_item(&self, call_id: &str, _payload: &ToolPayload) -> ResponseInputItem {
        ResponseInputItem::FunctionCallOutput {
            call_id: call_id.to_string(),
            output: FunctionCallOutputPayload {
                body: FunctionCallOutputBody::Text(self.0.clone()),
                success: Some(true),
            },
        }
    }

    fn code_mode_result(&self, _payload: &ToolPayload) -> Value {
        Value::String(self.0.clone())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    fn args(value: Value) -> Args {
        serde_json::from_value(value).unwrap_or_else(|error| panic!("{error}"))
    }

    fn tree() -> tempfile::TempDir {
        let dir = tempfile::tempdir().unwrap_or_else(|error| panic!("{error}"));
        for (path, text) in [
            ("src/a.rs", "fn alpha() {}\nalpha();\n"),
            ("src/b.rs", "alpha();\n"),
            ("notes.md", "nothing\n"),
        ] {
            let path = dir.path().join(path);
            std::fs::create_dir_all(path.parent().unwrap_or(dir.path()))
                .unwrap_or_else(|error| panic!("{error}"));
            std::fs::write(path, text).unwrap_or_else(|error| panic!("{error}"));
        }
        dir
    }

    #[test]
    fn the_schema_parses_and_names_the_tool() {
        let tool = Grep(Arc::new(Search::new()));
        let ToolSpec::Function(spec) = tool.spec() else {
            panic!("grep is a function tool");
        };
        assert_eq!(spec.name, NAME);
        assert!(is_grep(&tool.tool_name()));
    }

    #[test]
    fn it_answers_text_names_and_symbols_as_rg_lines() {
        let dir = tree();
        let search = Search::new();
        let text = answer(
            &search,
            args(json!({"pattern": "alpha"})),
            dir.path(),
            BYTES,
        );
        assert_eq!(
            text,
            Ok("src/a.rs:1:fn alpha() {}\nsrc/a.rs:2:alpha();\nsrc/b.rs:1:alpha();\n".to_string())
        );
        let names = answer(
            &search,
            args(json!({"pattern": "*.rs", "kind": "name"})),
            dir.path(),
            BYTES,
        );
        assert_eq!(names, Ok("src/a.rs\nsrc/b.rs\n".to_string()));
        let symbol = answer(
            &search,
            args(json!({"pattern": "alpha", "kind": "symbol", "output": "count"})),
            dir.path(),
            BYTES,
        );
        assert_eq!(symbol, Ok("src/a.rs:2\nsrc/b.rs:1\n".to_string()));
        let none = answer(&search, args(json!({"pattern": "zzz"})), dir.path(), BYTES);
        assert_eq!(none, Ok("no matches".to_string()));
    }

    #[test]
    fn a_bad_pattern_sends_the_model_to_rg() {
        let dir = tree();
        let Err(error) = answer(
            &Search::new(),
            args(json!({"pattern": "("})),
            dir.path(),
            BYTES,
        ) else {
            panic!("a pattern that does not compile is an error");
        };
        assert!(error.contains("Use rg in the shell"), "{error}");
    }

    #[test]
    fn output_past_the_budget_is_cut_at_a_line_and_says_so() {
        let dir = tree();
        std::fs::write(dir.path().join("src/c.rs"), "alpha();\n".repeat(30))
            .unwrap_or_else(|error| panic!("{error}"));
        let text = answer(
            &Search::new(),
            args(json!({"pattern": "alpha"})),
            dir.path(),
            200,
        )
        .unwrap_or_else(|error| panic!("{error}"));
        assert!(text.len() <= 200, "{text}");
        let lines: Vec<&str> = text.lines().collect();
        assert_eq!(lines[0], "src/c.rs:1:alpha();");
        assert_eq!(
            lines.last().copied(),
            Some(
                format!(
                    "[output cut at {} lines of 33: narrow paths or glob, or lower limit]",
                    lines.len() - 1
                )
                .as_str()
            )
        );
        let limited = answer(
            &Search::new(),
            args(json!({"pattern": "alpha", "limit": 1})),
            dir.path(),
            BYTES,
        )
        .unwrap_or_else(|error| panic!("{error}"));
        assert_eq!(
            limited,
            "src/c.rs:1:alpha();\n[1 hits shown, best first; more may exist: narrow paths or glob, or raise limit]\n"
        );
    }

    #[test]
    fn unknown_arguments_are_refused() {
        let parsed: Result<Args, _> =
            serde_json::from_value(json!({"pattern": "x", "regex": true}));
        assert!(parsed.is_err());
    }

    #[test]
    fn only_grep_itself_is_not_a_write() {
        assert!(is_grep(&ToolName::plain(NAME)));
        assert!(!is_grep(&ToolName::plain("apply_patch")));
        assert!(!is_grep(&ToolName::namespaced("web", NAME)));
    }
}
