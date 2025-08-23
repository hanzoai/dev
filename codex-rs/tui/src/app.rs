use crate::app_event::AppEvent;
use crate::app_event_sender::AppEventSender;
use crate::chatwidget::ChatWidget;
use crate::file_search::FileSearchManager;
use crate::get_git_diff::get_git_diff;
use crate::get_login_status;
use crate::onboarding::onboarding_screen::KeyboardHandler;
use crate::onboarding::onboarding_screen::OnboardingScreen;
use crate::onboarding::onboarding_screen::OnboardingScreenArgs;
use crate::slash_command::SlashCommand;
use crate::transcript_app::TranscriptApp;
use crate::tui;
use crate::tui::TerminalInfo;
use codex_core::ConversationManager;
use codex_login::{AuthManager, AuthMode};
use codex_core::config::Config;
use codex_core::protocol::Event;
use codex_core::protocol::Op;
use color_eyre::eyre::Result;
use crossterm::SynchronizedUpdate;
use crossterm::event::KeyCode;
use crossterm::event::KeyEvent;
use crossterm::event::KeyEventKind;
use crossterm::execute;
use crossterm::terminal::supports_keyboard_enhancement;
use std::path::PathBuf;
use ratatui::prelude::Rect;
use ratatui::text::Line;
use std::sync::Arc;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;
use std::sync::mpsc::Receiver;
use std::sync::mpsc::channel;
use std::thread;
use std::time::Duration;

/// Time window for debouncing redraw requests.
///
/// Raising this slightly helps coalesce bursts of updates during typing and
/// reduces render thrash, improving perceived input latency while staying
/// comfortably under a 60 FPS refresh budget.
const REDRAW_DEBOUNCE: Duration = Duration::from_millis(16);

/// Top-level application state: which full-screen view is currently active.
#[allow(clippy::large_enum_variant)]
enum AppState<'a> {
    Onboarding {
        screen: OnboardingScreen,
    },
    /// The main chat UI is visible.
    Chat {
        /// Boxed to avoid a large enum variant and reduce the overall size of
        /// `AppState`.
        widget: Box<ChatWidget<'a>>,
    },
}

pub(crate) struct App<'a> {
    _server: Arc<ConversationManager>,
    app_event_tx: AppEventSender,
    app_event_rx: Receiver<AppEvent>,
    app_state: AppState<'a>,

    /// Config is stored here so we can recreate ChatWidgets as needed.
    config: Config,

    file_search: FileSearchManager,

    /// True when a redraw has been scheduled but not yet executed.
    pending_redraw: Arc<AtomicBool>,

    // Transcript overlay state
    _transcript_overlay: Option<TranscriptApp>,
    _deferred_history_lines: Vec<Line<'static>>,
    _transcript_saved_viewport: Option<Rect>,

    enhanced_keys_supported: bool,

    /// Debug flag for logging LLM requests/responses
    _debug: bool,

    /// Controls the animation thread that sends CommitTick events.
    commit_anim_running: Arc<AtomicBool>,

    /// Terminal information queried at startup
    terminal_info: TerminalInfo,

    /// Perform a hard clear on the first frame to ensure the entire buffer
    /// starts with our theme background. This avoids terminals that may show
    /// profile defaults until all cells are explicitly painted.
    clear_on_first_frame: bool,
}

/// Aggregate parameters needed to create a `ChatWidget`, as creation may be
/// deferred until after the Git warning screen is dismissed.
#[derive(Clone, Debug)]
pub(crate) struct ChatWidgetArgs {
    pub(crate) config: Config,
    initial_prompt: Option<String>,
    initial_images: Vec<PathBuf>,
    enhanced_keys_supported: bool,
    terminal_info: TerminalInfo,
}

impl App<'_> {
    pub(crate) fn new(
        config: Config,
        initial_prompt: Option<String>,
        initial_images: Vec<std::path::PathBuf>,
        show_trust_screen: bool,
        debug: bool,
        terminal_info: TerminalInfo,
    ) -> Self {
        let conversation_manager = Arc::new(ConversationManager::new(AuthManager::shared(
            config.codex_home.clone(),
            AuthMode::ApiKey,
        )));

        let (app_event_tx, app_event_rx) = channel();
        let app_event_tx = AppEventSender::new(app_event_tx);
        let pending_redraw = Arc::new(AtomicBool::new(false));

        let enhanced_keys_supported = supports_keyboard_enhancement().unwrap_or(false);

        // Spawn a dedicated thread for reading the crossterm event loop and
        // re-publishing the events as AppEvents, as appropriate.
        {
            let app_event_tx = app_event_tx.clone();
            std::thread::spawn(move || {
                loop {
                    // This timeout is necessary to avoid holding the event lock
                    // that crossterm::event::read() acquires. In particular,
                    // reading the cursor position (crossterm::cursor::position())
                    // needs to acquire the event lock, and so will fail if it
                    // can't acquire it within 2 sec. Resizing the terminal
                    // crashes the app if the cursor position can't be read.
                    // Keep the timeout small to minimize input-to-echo latency.
                    if let Ok(true) = crossterm::event::poll(Duration::from_millis(5)) {
                        if let Ok(event) = crossterm::event::read() {
                            match event {
                                crossterm::event::Event::Key(key_event) => {
                                    app_event_tx.send(AppEvent::KeyEvent(key_event));
                                }
                                crossterm::event::Event::Resize(_, _) => {
                                    app_event_tx.send(AppEvent::RequestRedraw);
                                }
                                crossterm::event::Event::Paste(pasted) => {
                                    // Many terminals convert newlines to \r when pasting (e.g., iTerm2),
                                    // but tui-textarea expects \n. Normalize CR to LF.
                                    // [tui-textarea]: https://github.com/rhysd/tui-textarea/blob/4d18622eeac13b309e0ff6a55a46ac6706da68cf/src/textarea.rs#L782-L783
                                    // [iTerm2]: https://github.com/gnachman/iTerm2/blob/5d0c0d9f68523cbd0494dad5422998964a2ecd8d/sources/iTermPasteHelper.m#L206-L216
                                    let pasted = pasted.replace("\r", "\n");
                                    app_event_tx.send(AppEvent::Paste(pasted));
                                }
                                crossterm::event::Event::Mouse(mouse_event) => {
                                    app_event_tx.send(AppEvent::MouseEvent(mouse_event));
                                }
                                _ => {
                                    // Ignore any other events.
                                }
                            }
                        }
                    } else {
                        // Timeout expired, no `Event` is available; yield cooperatively
                        std::thread::yield_now();
                    }
                }
            });
        }

        let login_status = get_login_status(&config);
        let should_show_onboarding =
            should_show_onboarding(login_status, &config, show_trust_screen);
        let app_state = if should_show_onboarding {
            let show_login_screen = should_show_login_screen(login_status, &config);
            let chat_widget_args = ChatWidgetArgs {
                config: config.clone(),
                initial_prompt,
                initial_images,
                enhanced_keys_supported,
                terminal_info: terminal_info.clone(),
            };
            AppState::Onboarding {
                screen: OnboardingScreen::new(OnboardingScreenArgs {
                    event_tx: app_event_tx.clone(),
                    codex_home: config.codex_home.clone(),
                    cwd: config.cwd.clone(),
                    show_trust_screen,
                    show_login_screen,
                    chat_widget_args,
                    login_status,
                }),
            }
        } else {
            let mut chat_widget = ChatWidget::new(
                config.clone(),
                app_event_tx.clone(),
                initial_prompt,
                initial_images,
                enhanced_keys_supported,
                terminal_info.clone(),
            );
            // Check for initial animations after widget is created
            chat_widget.check_for_initial_animations();
            AppState::Chat {
                widget: Box::new(chat_widget),
            }
        };

        let file_search = FileSearchManager::new(config.cwd.clone(), app_event_tx.clone());
        Self {
            _server: conversation_manager,
            app_event_tx,
            app_event_rx,
            app_state,
            config,
            file_search,
            pending_redraw,
            _transcript_overlay: None,
            _deferred_history_lines: Vec::new(),
            _transcript_saved_viewport: None,
            enhanced_keys_supported,
            _debug: debug,
            commit_anim_running: Arc::new(AtomicBool::new(false)),
            terminal_info,
            clear_on_first_frame: true,
        }
    }


    /// Schedule a redraw if one is not already pending.
    #[allow(clippy::unwrap_used)]
    fn schedule_redraw(&self) {
        // Attempt to set the flag to `true`. If it was already `true`, another
        // redraw is already pending so we can return early.
        if self
            .pending_redraw
            .compare_exchange(false, true, Ordering::Acquire, Ordering::Relaxed)
            .is_err()
        {
            return;
        }

        let tx = self.app_event_tx.clone();
        let pending_redraw = self.pending_redraw.clone();
        thread::spawn(move || {
            thread::sleep(REDRAW_DEBOUNCE);
            tx.send(AppEvent::Redraw);
            pending_redraw.store(false, Ordering::Release);
        });
    }
    
    /// Schedule a redraw after the specified duration
    fn schedule_redraw_in(&self, duration: Duration) {
        let tx = self.app_event_tx.clone();
        thread::spawn(move || {
            thread::sleep(duration);
            tx.send(AppEvent::RequestRedraw);
        });
    }

    pub(crate) fn run(&mut self, terminal: &mut tui::Tui) -> Result<()> {
        // Insert an event to trigger the first render.
        let app_event_tx = self.app_event_tx.clone();
        app_event_tx.send(AppEvent::RequestRedraw);

        while let Ok(event) = self.app_event_rx.recv() {
            match event {
                AppEvent::InsertHistory(lines) => match &mut self.app_state {
                    AppState::Chat { widget } => widget.insert_history_lines(lines),
                    AppState::Onboarding { .. } => {}
                },
                AppEvent::InsertHistoryWithKind { kind, lines } => match &mut self.app_state {
                    AppState::Chat { widget } => widget.insert_history_lines_with_kind(kind, lines),
                    AppState::Onboarding { .. } => {}
                },
                AppEvent::RequestRedraw => {
                    self.schedule_redraw();
                }
                AppEvent::ImmediateRedraw => {
                    // Draw immediately; no debounce
                    std::io::stdout().sync_update(|_| self.draw_next_frame(terminal))??;
                }
                AppEvent::Redraw => {
                    std::io::stdout().sync_update(|_| self.draw_next_frame(terminal))??;
                }
                AppEvent::StartCommitAnimation => {
                    if self
                        .commit_anim_running
                        .compare_exchange(false, true, Ordering::Acquire, Ordering::Relaxed)
                        .is_ok()
                    {
                        let tx = self.app_event_tx.clone();
                        let running = self.commit_anim_running.clone();
                        thread::spawn(move || {
                            while running.load(Ordering::Relaxed) {
                                thread::sleep(Duration::from_millis(50));
                                tx.send(AppEvent::CommitTick);
                            }
                        });
                    }
                }
                AppEvent::StopCommitAnimation => {
                    self.commit_anim_running.store(false, Ordering::Release);
                }
                AppEvent::CommitTick => {
                    // Advance streaming animation: commit at most one queued line
                    if let AppState::Chat { widget } = &mut self.app_state {
                        widget.on_commit_tick();
                    }
                }
                AppEvent::KeyEvent(key_event) => {
                    match key_event {
                        KeyEvent {
                            code: KeyCode::Char('m'),
                            modifiers: crossterm::event::KeyModifiers::CONTROL,
                            kind: KeyEventKind::Press,
                            ..
                        } => {
                            // Toggle mouse capture to allow text selection
                            use crossterm::event::DisableMouseCapture;
                            use crossterm::event::EnableMouseCapture;
                            use crossterm::execute;
                            use std::io::stdout;

                            // Static variable to track mouse capture state
                            static mut MOUSE_CAPTURE_ENABLED: bool = true;

                            unsafe {
                                MOUSE_CAPTURE_ENABLED = !MOUSE_CAPTURE_ENABLED;
                                if MOUSE_CAPTURE_ENABLED {
                                    let _ = execute!(stdout(), EnableMouseCapture);
                                } else {
                                    let _ = execute!(stdout(), DisableMouseCapture);
                                }
                            }
                            self.app_event_tx.send(AppEvent::RequestRedraw);
                        }
                        KeyEvent {
                            code: KeyCode::Char('c'),
                            modifiers: crossterm::event::KeyModifiers::CONTROL,
                            kind: KeyEventKind::Press,
                            ..
                        } => match &mut self.app_state {
                            AppState::Chat { widget } => {
                                widget.on_ctrl_c();
                            }
                            AppState::Onboarding { .. } => {
                                self.app_event_tx.send(AppEvent::ExitRequest);
                            }
                        },
                        KeyEvent {
                            code: KeyCode::Char('z'),
                            modifiers: crossterm::event::KeyModifiers::CONTROL,
                            kind: KeyEventKind::Press,
                            ..
                        } => {
                            #[cfg(unix)]
                            {
                                self.suspend(terminal)?;
                            }
                            // No-op on non-Unix platforms.
                        }
                        KeyEvent {
                            code: KeyCode::Char('r') | KeyCode::Char('t'),
                            modifiers: crossterm::event::KeyModifiers::CONTROL,
                            kind: KeyEventKind::Press,
                            ..
                        }
                        | KeyEvent {
                            code: KeyCode::Char('r') | KeyCode::Char('t'),
                            modifiers: crossterm::event::KeyModifiers::CONTROL,
                            kind: KeyEventKind::Repeat,
                            ..
                        } => {
                            // Toggle reasoning/thinking visibility (Ctrl+R or Ctrl+T)
                            match &mut self.app_state {
                                AppState::Chat { widget } => {
                                    widget.toggle_reasoning_visibility();
                                }
                                AppState::Onboarding { .. } => {}
                            }
                        }
                        KeyEvent {
                            code: KeyCode::Char('d'),
                            modifiers: crossterm::event::KeyModifiers::CONTROL,
                            kind: KeyEventKind::Press,
                            ..
                        } => {
                            // Toggle diffs overlay
                            if let AppState::Chat { widget } = &mut self.app_state {
                                widget.toggle_diffs_popup();
                            }
                        }
                        KeyEvent {
                            kind: KeyEventKind::Press | KeyEventKind::Repeat,
                            ..
                        } => {
                            self.dispatch_key_event(key_event);
                        }
                        _ => {
                            // Ignore Release key events.
                        }
                    };
                }
                AppEvent::MouseEvent(mouse_event) => {
                    self.dispatch_mouse_event(mouse_event);
                }
                AppEvent::Paste(text) => {
                    self.dispatch_paste_event(text);
                }
                AppEvent::CodexEvent(event) => {
                    self.dispatch_codex_event(event);
                }
                AppEvent::ExitRequest => {
                    break;
                }
                AppEvent::CodexOp(op) => match &mut self.app_state {
                    AppState::Chat { widget } => widget.submit_op(op),
                    AppState::Onboarding { .. } => {}
                },
                AppEvent::DispatchCommand(command, command_text) => {
                    // Extract command arguments by removing the slash command from the beginning
                    // e.g., "/browser status" -> "status", "/chrome 9222" -> "9222"
                    let command_args = {
                        let cmd_with_slash = format!("/{}", command.command());
                        if command_text.starts_with(&cmd_with_slash) {
                            command_text[cmd_with_slash.len()..].trim().to_string()
                        } else {
                            // Fallback: if format doesn't match, use the full text
                            command_text.clone()
                        }
                    };

                    match command {
                        SlashCommand::New => {
                            // Clear the current conversation and start fresh
                            if let AppState::Chat { widget } = &mut self.app_state {
                                widget.new_conversation(self.enhanced_keys_supported);
                            } else {
                                // If we're not in chat state, create a new chat widget
                                let new_widget = Box::new(ChatWidget::new(
                                    self.config.clone(),
                                    self.app_event_tx.clone(),
                                    None,
                                    Vec::new(),
                                    self.enhanced_keys_supported,
                                    self.terminal_info.clone(),
                                ));
                                self.app_state = AppState::Chat { widget: new_widget };
                            }
                            self.app_event_tx.send(AppEvent::RequestRedraw);
                        }
                        SlashCommand::Init => {
                            // Guard: do not run if a task is active.
                            if let AppState::Chat { widget } = &mut self.app_state {
                                const INIT_PROMPT: &str =
                                    include_str!("../prompt_for_init_command.md");
                                widget.submit_text_message(INIT_PROMPT.to_string());
                            }
                        }
                        SlashCommand::Compact => {
                            if let AppState::Chat { widget } = &mut self.app_state {
                                widget.clear_token_usage();
                                self.app_event_tx.send(AppEvent::CodexOp(Op::Compact));
                            }
                        }
                        SlashCommand::Quit => {
                            break;
                        }
                        SlashCommand::Logout => {
                            if let Err(e) = codex_login::logout(&self.config.codex_home) {
                                tracing::error!("failed to logout: {e}");
                            }
                            break;
                        }
                        SlashCommand::Diff => {
                            let (is_git_repo, diff_text) = match tokio::runtime::Handle::current().block_on(get_git_diff()) {
                                Ok(v) => v,
                                Err(e) => {
                                    let msg = format!("Failed to compute diff: {e}");
                                    if let AppState::Chat { widget } = &mut self.app_state {
                                        widget.add_diff_output(msg);
                                    }
                                    continue;
                                }
                            };

                            if let AppState::Chat { widget } = &mut self.app_state {
                                let text = if is_git_repo {
                                    diff_text
                                } else {
                                    "`/diff` — _not inside a git repository_".to_string()
                                };
                                widget.add_diff_output(text);
                            }
                        }
                        SlashCommand::Mention => {
                            // The mention feature is handled differently in our fork
                            // For now, just add @ to the composer
                            if let AppState::Chat { widget } = &mut self.app_state {
                                widget.insert_str("@");
                            }
                        }
                        SlashCommand::Status => {
                            if let AppState::Chat { widget } = &mut self.app_state {
                                widget.add_status_output();
                            }
                        }
                        SlashCommand::Reasoning => {
                            if let AppState::Chat { widget } = &mut self.app_state {
                                widget.handle_reasoning_command(command_args);
                            }
                        }
                        SlashCommand::Verbosity => {
                            if let AppState::Chat { widget } = &mut self.app_state {
                                widget.handle_verbosity_command(command_args);
                            }
                        }
                        SlashCommand::Theme => {
                            // Theme selection is handled in submit_user_message
                            // This case is here for completeness
                            if let AppState::Chat { widget } = &mut self.app_state {
                                widget.show_theme_selection();
                            }
                        }
                        SlashCommand::Prompts => {
                            if let AppState::Chat { widget } = &mut self.app_state {
                                widget.add_prompts_output();
                            }
                        }
                        SlashCommand::Perf => {
                            if let AppState::Chat { widget } = &mut self.app_state {
                                widget.handle_perf_command(command_args);
                            }
                        }
                        // Prompt-expanding commands should have been handled in submit_user_message
                        // but add a fallback just in case
                        SlashCommand::Plan | SlashCommand::Solve | SlashCommand::Code => {
                            // These should have been expanded already, but handle them anyway
                            if let AppState::Chat { widget } = &mut self.app_state {
                                let expanded = command.expand_prompt(&command_text);
                                if let Some(prompt) = expanded {
                                    widget.submit_text_message(prompt);
                                }
                            }
                        }
                        SlashCommand::Browser => {
                            if let AppState::Chat { widget } = &mut self.app_state {
                                widget.handle_browser_command(command_args);
                            }
                        }
                        SlashCommand::Chrome => {
                            if let AppState::Chat { widget } = &mut self.app_state {
                                tracing::info!("[cdp] /chrome invoked, args='{}'", command_args);
                                widget.handle_chrome_command(command_args);
                            }
                        }
                        #[cfg(debug_assertions)]
                        SlashCommand::TestApproval => {
                            use codex_core::protocol::EventMsg;
                            use std::collections::HashMap;

                            use codex_core::protocol::ApplyPatchApprovalRequestEvent;
                            use codex_core::protocol::FileChange;

                            self.app_event_tx.send(AppEvent::CodexEvent(Event {
                                id: "1".to_string(),
                                // msg: EventMsg::ExecApprovalRequest(ExecApprovalRequestEvent {
                                //     call_id: "1".to_string(),
                                //     command: vec!["git".into(), "apply".into()],
                                //     cwd: self.config.cwd.clone(),
                                //     reason: Some("test".to_string()),
                                // }),
                                msg: EventMsg::ApplyPatchApprovalRequest(
                                    ApplyPatchApprovalRequestEvent {
                                        call_id: "1".to_string(),
                                        changes: HashMap::from([
                                            (
                                                PathBuf::from("/tmp/test.txt"),
                                                FileChange::Add {
                                                    content: "test".to_string(),
                                                },
                                            ),
                                            (
                                                PathBuf::from("/tmp/test2.txt"),
                                                FileChange::Update {
                                                    unified_diff: "+test\n-test2".to_string(),
                                                    move_path: None,
                                                },
                                            ),
                                        ]),
                                        reason: None,
                                        grant_root: Some(PathBuf::from("/tmp")),
                                    },
                                ),
                            }));
                        }
                    }
                }
                AppEvent::PrepareAgents => {
                    if let AppState::Chat { widget } = &mut self.app_state {
                        widget.prepare_agents();
                    }
                }
                AppEvent::UpdateReasoningEffort(new_effort) => {
                    if let AppState::Chat { widget } = &mut self.app_state {
                        widget.set_reasoning_effort(new_effort);
                    }
                }
                AppEvent::UpdateTextVerbosity(new_verbosity) => {
                    if let AppState::Chat { widget } = &mut self.app_state {
                        widget.set_text_verbosity(new_verbosity);
                    }
                }
                AppEvent::DiffResult(text) => {
                    if let AppState::Chat { widget } = &mut self.app_state {
                        widget.add_diff_output(text);
                    }
                }
                AppEvent::UpdateTheme(new_theme) => {
                    // Switch the theme immediately
                    crate::theme::switch_theme(new_theme);

                    // Clear terminal with new theme colors
                    let theme_bg = crate::colors::background();
                    let theme_fg = crate::colors::text();
                    let _ = crossterm::execute!(
                        std::io::stdout(),
                        crossterm::style::SetColors(crossterm::style::Colors::new(
                            theme_fg.into(),
                            theme_bg.into()
                        )),
                        crossterm::terminal::Clear(crossterm::terminal::ClearType::All),
                        crossterm::cursor::MoveTo(0, 0),
                        crossterm::terminal::SetTitle("Code"),
                        crossterm::terminal::EnableLineWrap
                    );

                    // Update config and save to file
                    if let AppState::Chat { widget } = &mut self.app_state {
                        widget.set_theme(new_theme);
                    }

                    // Request a redraw to apply the new theme
                    self.schedule_redraw();
                }
                AppEvent::PreviewTheme(new_theme) => {
                    // Switch the theme immediately for preview (no history event)
                    crate::theme::switch_theme(new_theme);

                    // Clear terminal with new theme colors
                    let theme_bg = crate::colors::background();
                    let theme_fg = crate::colors::text();
                    let _ = crossterm::execute!(
                        std::io::stdout(),
                        crossterm::style::SetColors(crossterm::style::Colors::new(
                            theme_fg.into(),
                            theme_bg.into()
                        )),
                        crossterm::terminal::Clear(crossterm::terminal::ClearType::All),
                        crossterm::cursor::MoveTo(0, 0),
                        crossterm::terminal::SetTitle("Code"),
                        crossterm::terminal::EnableLineWrap
                    );

                    // Retint pre-rendered history cells so the preview reflects immediately
                    if let AppState::Chat { widget } = &mut self.app_state {
                        widget.retint_history_for_preview();
                    }

                    // Don't update config or add to history for previews
                    // Request a redraw to apply the new theme
                    self.schedule_redraw();
                }
                AppEvent::ComposerExpanded => {
                    if let AppState::Chat { widget } = &mut self.app_state {
                        widget.on_composer_expanded();
                    }
                    self.schedule_redraw();
                }
                AppEvent::OnboardingAuthComplete(result) => {
                    if let AppState::Onboarding { screen } = &mut self.app_state {
                        screen.on_auth_complete(result);
                    }
                }
                AppEvent::OnboardingComplete(ChatWidgetArgs {
                    config,
                    enhanced_keys_supported,
                    initial_images,
                    initial_prompt,
                    terminal_info,
                }) => {
                    self.app_state = AppState::Chat {
                        widget: Box::new(ChatWidget::new(
                            config,
                            app_event_tx.clone(),
                            initial_prompt,
                            initial_images,
                            enhanced_keys_supported,
                            terminal_info,
                        )),
                    }
                }
                AppEvent::StartFileSearch(query) => {
                    if !query.is_empty() {
                        self.file_search.on_user_query(query);
                    }
                }
                AppEvent::FileSearchResult { query, matches } => {
                    if let AppState::Chat { widget } = &mut self.app_state {
                        widget.apply_file_search_result(query, matches);
                    }
                }
                AppEvent::ShowChromeOptions(port) => {
                    if let AppState::Chat { widget } = &mut self.app_state {
                        widget.show_chrome_options(port);
                    }
                }
                AppEvent::ChromeLaunchOptionSelected(option, port) => {
                    if let AppState::Chat { widget } = &mut self.app_state {
                        widget.handle_chrome_launch_option(option, port);
                    }
                }
                AppEvent::ScheduleFrameIn(duration) => {
                    // Schedule the next redraw with the requested duration
                    self.schedule_redraw_in(duration);
                }
            }
        }
        terminal.clear()?;

        Ok(())
    }

    #[cfg(unix)]
    fn suspend(&mut self, terminal: &mut tui::Tui) -> Result<()> {
        tui::restore()?;
        // SAFETY: Unix-only code path. We intentionally send SIGTSTP to the
        // current process group (pid 0) to trigger standard job-control
        // suspension semantics. This FFI does not involve any raw pointers,
        // is not called from a signal handler, and uses a constant signal.
        // Errors from kill are acceptable (e.g., if already stopped) — the
        // subsequent re-init path will still leave the terminal in a good state.
        // We considered `nix`, but didn't think it was worth pulling in for this one call.
        unsafe { libc::kill(0, libc::SIGTSTP) };
        let (new_terminal, new_terminal_info) = tui::init(&self.config)?;
        *terminal = new_terminal;
        self.terminal_info = new_terminal_info;
        terminal.clear()?;
        self.app_event_tx.send(AppEvent::RequestRedraw);
        Ok(())
    }

    pub(crate) fn token_usage(&self) -> codex_core::protocol::TokenUsage {
        match &self.app_state {
            AppState::Chat { widget } => widget.token_usage().clone(),
            AppState::Onboarding { .. } => codex_core::protocol::TokenUsage::default(),
        }
    }

    fn draw_next_frame(&mut self, terminal: &mut tui::Tui) -> Result<()> {
        if self.clear_on_first_frame || matches!(self.app_state, AppState::Onboarding { .. }) {
            terminal.clear()?;
            self.clear_on_first_frame = false;
        }

        // Terminal resize handling - simplified version since private fields aren't accessible
        // The terminal will handle resize events internally
        let _screen_size = terminal.size()?;

        terminal.draw(|frame| {
            match &mut self.app_state {
                AppState::Chat { widget } => {
                    if let Some((x, y)) = widget.cursor_pos(frame.area()) {
                        frame.set_cursor_position((x, y));
                    }
                    frame.render_widget_ref(&**widget, frame.area())
                }
                AppState::Onboarding { screen } => frame.render_widget_ref(&*screen, frame.area()),
            }
        })?;
        Ok(())
    }

    /// Dispatch a KeyEvent to the current view and let it decide what to do
    /// with it.
    fn dispatch_key_event(&mut self, key_event: KeyEvent) {
        match &mut self.app_state {
            AppState::Chat { widget } => {
                widget.handle_key_event(key_event);
            }
            AppState::Onboarding { screen } => match key_event.code {
                KeyCode::Char('q') => {
                    self.app_event_tx.send(AppEvent::ExitRequest);
                }
                _ => screen.handle_key_event(key_event),
            },
        }
    }

    fn dispatch_paste_event(&mut self, pasted: String) {
        match &mut self.app_state {
            AppState::Chat { widget } => widget.handle_paste(pasted),
            AppState::Onboarding { .. } => {}
        }
    }

    fn dispatch_mouse_event(&mut self, mouse_event: crossterm::event::MouseEvent) {
        match &mut self.app_state {
            AppState::Chat { widget } => {
                widget.handle_mouse_event(mouse_event);
            }
            AppState::Onboarding { .. } => {}
        }
    }

    fn dispatch_codex_event(&mut self, event: Event) {
        match &mut self.app_state {
            AppState::Chat { widget } => widget.handle_codex_event(event),
            AppState::Onboarding { .. } => {}
        }
    }
}

fn should_show_onboarding(
    _login_status: crate::LoginStatus,
    _config: &Config,
    show_trust_screen: bool,
) -> bool {
    if show_trust_screen {
        return true;
    }
    // Defer login screen visibility decision to onboarding screen logic.
    // Here we only gate on trust flow.
    false
}

fn should_show_login_screen(login_status: crate::LoginStatus, _config: &Config) -> bool {
    matches!(login_status, crate::LoginStatus::NotAuthenticated)
}

#[cfg(test)]
mod tests {
    use super::*;
    use codex_core::config::ConfigOverrides;
    use codex_core::config::ConfigToml;
    use codex_login::AuthMode;

    fn make_config(preferred: AuthMode) -> Config {
        let mut cfg = Config::load_from_base_config_with_overrides(
            ConfigToml::default(),
            ConfigOverrides::default(),
            std::env::temp_dir(),
        )
        .expect("load default config");
        cfg.preferred_auth_method = preferred;
        cfg
    }

    #[test]
    fn shows_login_when_not_authenticated() {
        let cfg = make_config(AuthMode::ChatGPT);
        assert!(should_show_login_screen(
            LoginStatus::NotAuthenticated,
            &cfg
        ));
    }

    #[test]
    fn shows_login_when_api_key_but_prefers_chatgpt() {
        let cfg = make_config(AuthMode::ChatGPT);
        assert!(should_show_login_screen(
            LoginStatus::AuthMode(AuthMode::ApiKey),
            &cfg
        ))
    }

    #[test]
    fn hides_login_when_api_key_and_prefers_api_key() {
        let cfg = make_config(AuthMode::ApiKey);
        assert!(!should_show_login_screen(
            LoginStatus::AuthMode(AuthMode::ApiKey),
            &cfg
        ))
    }

    #[test]
    fn hides_login_when_chatgpt_and_prefers_chatgpt() {
        let cfg = make_config(AuthMode::ChatGPT);
        assert!(!should_show_login_screen(
            LoginStatus::AuthMode(AuthMode::ChatGPT),
            &cfg
        ))
    }
}
