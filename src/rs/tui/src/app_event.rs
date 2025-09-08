use dev_core::config_types::ReasoningEffort;
use dev_core::config_types::TextVerbosity;
use dev_core::config_types::ThemeName;
use dev_core::protocol::Event;
use dev_file_search::FileMatch;
use crossterm::event::KeyEvent;
use crossterm::event::MouseEvent;
use ratatui::text::Line;
use crate::streaming::StreamKind;
use std::time::Duration;

use crate::app::ChatWidgetArgs;
use crate::bottom_pane::chrome_selection_view::ChromeLaunchOption;
use crate::slash_command::SlashCommand;
use dev_protocol::models::ResponseItem;
use std::fmt;
use std::path::PathBuf;

/// Wrapper to allow including non-Debug types in Debug enums without leaking internals.
pub(crate) struct Redacted<T>(pub T);

impl<T> fmt::Debug for Redacted<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("<redacted>")
    }
}

#[allow(clippy::large_enum_variant)]
#[derive(Debug)]
pub(crate) enum AppEvent {
    CodexEvent(Event),

    /// Request a redraw which will be debounced by the [`App`].
    RequestRedraw,

    /// Actually draw the next frame.
    Redraw,

    /// Schedule a one-shot animation frame roughly after the given duration.
    /// Multiple requests are coalesced by the central frame scheduler.
    ScheduleFrameIn(Duration),

    /// Internal: flush any pending out-of-order ExecEnd events that did not
    /// receive a matching ExecBegin within a short pairing window. This lets
    /// the TUI render a fallback "Ran call_<id>" cell so output is not lost.
    FlushPendingExecEnds,

    KeyEvent(KeyEvent),

    MouseEvent(MouseEvent),

    /// Text pasted from the terminal clipboard.
    Paste(String),

    /// Request to exit the application gracefully.
    ExitRequest,

    /// Forward an `Op` to the Agent. Using an `AppEvent` for this avoids
    /// bubbling channels through layers of widgets.
    CodexOp(dev_core::protocol::Op),

    /// Dispatch a recognized slash command from the UI (composer) to the app
    /// layer so it can be handled centrally. Includes the full command text.
    DispatchCommand(SlashCommand, String),

    /// Signal that agents are about to start (triggered when /plan, /solve, /code commands are entered)
    PrepareAgents,

    /// Update the reasoning effort level
    UpdateReasoningEffort(ReasoningEffort),

    /// Update the text verbosity level
    UpdateTextVerbosity(TextVerbosity),

    /// Update the theme (with history event)
    UpdateTheme(ThemeName),

    /// Preview theme (no history event)
    PreviewTheme(ThemeName),
    /// Bottom composer expanded (e.g., slash command popup opened)
    ComposerExpanded,

    /// Kick off an asynchronous file search for the given query (text after
    /// the `@`). Previous searches may be cancelled by the app layer so there
    /// is at most one in-flight search.
    StartFileSearch(String),

    /// Result of a completed asynchronous file search. The `query` echoes the
    /// original search term so the UI can decide whether the results are
    /// still relevant.
    FileSearchResult {
        query: String,
        matches: Vec<FileMatch>,
    },

    /// Result of computing a `/diff` command.
    #[allow(dead_code)]
    DiffResult(String),

    InsertHistory(Vec<Line<'static>>),
    InsertHistoryWithKind { id: Option<String>, kind: StreamKind, lines: Vec<Line<'static>> },
    /// Finalized assistant answer with raw markdown for re-rendering under theme changes.
    InsertFinalAnswer { id: Option<String>, lines: Vec<Line<'static>>, source: String },

    #[allow(dead_code)]
    StartCommitAnimation,
    #[allow(dead_code)]
    StopCommitAnimation,
    CommitTick,

    /// Onboarding: result of login_with_chatgpt.
    OnboardingAuthComplete(Result<(), String>),
    OnboardingComplete(ChatWidgetArgs),

    /// Show Chrome launch options dialog
    #[allow(dead_code)]
    ShowChromeOptions(Option<u16>),

    /// Chrome launch option selected by user
    ChromeLaunchOptionSelected(ChromeLaunchOption, Option<u16>),

    /// Start a new chat session by resuming from the given rollout file
    ResumeFrom(std::path::PathBuf),

    /// Begin jump-back to the Nth last user message (1 = latest).
    /// Trims visible history up to that point and pre-fills the composer.
    JumpBack { nth: usize, prefill: String },
    /// Result of an async jump-back fork operation performed off the UI thread.
    /// Carries the forked conversation, trimmed prefix to replay, and composer prefill.
    JumpBackForked {
        cfg: dev_core::config::Config,
        new_conv: Redacted<dev_core::NewConversation>,
        prefix_items: Vec<ResponseItem>,
        prefill: String,
    },

    /// Register an image placeholder inserted by the composer with its backing path
    /// so ChatWidget can resolve it to a LocalImage on submit.
    RegisterPastedImage { placeholder: String, path: PathBuf },
    
}
