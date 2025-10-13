//! Bottom pane: shows the ChatComposer or a BottomPaneView, if one is active.

use crate::app_event::{AppEvent, AutoContinueMode};
use crate::app_event_sender::AppEventSender;
use crate::auto_drive_style::AutoDriveVariant;
use crate::chatwidget::BackgroundOrderTicket;
use crate::glitch_animation;
use crate::user_approval_widget::{ApprovalRequest, UserApprovalWidget};
use bottom_pane_view::BottomPaneView;
use crate::util::buffer::fill_rect;
use code_core::protocol::TokenUsage;
use code_file_search::FileMatch;
use crossterm::event::{KeyCode, KeyEvent, KeyEventKind};
use ratatui::buffer::Buffer;
use ratatui::layout::Rect;
use ratatui::style::{Modifier, Style};
use ratatui::widgets::WidgetRef;
use std::cell::{Cell, RefCell};
use std::time::{Duration, Instant};

mod approval_modal_view;
#[cfg(feature = "code-fork")]
mod approval_ui;
mod auto_coordinator_view;
mod auto_drive_settings_view;
mod bottom_pane_view;
mod chat_composer;
mod chat_composer_history;
pub mod chrome_selection_view;
mod diff_popup;
mod custom_prompt_view;
mod command_popup;
mod file_search_popup;
mod paste_burst;
mod popup_consts;
mod agent_editor_view;
mod agents_overview_view;
mod model_selection_view;
mod scroll_state;
mod selection_popup_common;
pub mod list_selection_view;
pub(crate) use list_selection_view::SelectionAction;
pub(crate) use custom_prompt_view::CustomPromptView;
mod cloud_tasks_view;
pub(crate) use cloud_tasks_view::CloudTasksView;
pub mod resume_selection_view;
pub mod agents_settings_view;
mod github_settings_view;
pub mod mcp_settings_view;
mod login_accounts_view;
// no direct use of list_selection_view or its items here
mod textarea;
pub mod form_text_field;
mod theme_selection_view;
mod verbosity_selection_view;
pub(crate) mod validation_settings_view;
mod update_settings_view;
mod undo_timeline_view;
mod notifications_settings_view;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum CancellationEvent {
    Ignored,
    Handled,
}

pub(crate) use chat_composer::ChatComposer;
pub(crate) use chat_composer::InputResult;
pub(crate) use auto_coordinator_view::{
    AutoActiveViewModel,
    AutoCoordinatorButton,
    AutoCoordinatorView,
    AutoCoordinatorViewModel,
    CountdownState,
};
pub(crate) use auto_drive_settings_view::AutoDriveSettingsView;
pub(crate) use login_accounts_view::{
    LoginAccountsState,
    LoginAccountsView,
    LoginAddAccountState,
    LoginAddAccountView,
};

pub(crate) use update_settings_view::{UpdateSettingsView, UpdateSharedState};
pub(crate) use notifications_settings_view::{NotificationsMode, NotificationsSettingsView};
use approval_modal_view::ApprovalModalView;
#[cfg(feature = "code-fork")]
use approval_ui::ApprovalUi;
use code_common::model_presets::ModelPreset;
use code_core::config_types::ReasoningEffort;
use code_core::config_types::TextVerbosity;
use code_core::config_types::ThemeName;
use model_selection_view::ModelSelectionView;
use theme_selection_view::ThemeSelectionView;
use verbosity_selection_view::VerbositySelectionView;
pub(crate) use undo_timeline_view::{UndoTimelineEntry, UndoTimelineEntryKind, UndoTimelineView};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ActiveViewKind {
    None,
    AutoCoordinator,
    AutoSettings,
    Other,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum AutoDriveTransitionPhase {
    Entering,
    #[allow(dead_code)]
    Exiting,
}

#[derive(Clone, Copy, Debug)]
struct TransitionGeometry {
    start: Rect,
    target: Rect,
}

#[derive(Clone, Copy, Debug)]
struct AutoDriveTransitionState {
    phase: AutoDriveTransitionPhase,
    started_at: Instant,
    geometry: Option<TransitionGeometry>,
    start_override: Option<Rect>,
    variant: AnimationVariant,
}

impl AutoDriveTransitionState {
    const ENTER_ANIMATION: Duration = Duration::from_millis(600);
    const ENTER_FADE: Duration = Duration::from_millis(250);
    const EXIT_BUILD: Duration = Duration::from_millis(320);
    const EXIT_FADE: Duration = Duration::from_millis(500);

    fn new(phase: AutoDriveTransitionPhase, start_override: Option<Rect>) -> Self {
        Self {
            phase,
            started_at: Instant::now(),
            geometry: None,
            start_override,
            variant: select_animation_variant(),
        }
    }

    fn frame(&self) -> AutoDriveTransitionFrame {
        match self.phase {
            AutoDriveTransitionPhase::Entering => {
                let elapsed = self.started_at.elapsed();
                if elapsed < Self::ENTER_ANIMATION {
                    let t = elapsed.as_secs_f32() / Self::ENTER_ANIMATION.as_secs_f32();
                    AutoDriveTransitionFrame {
                        t: t.clamp(0.0, 1.0),
                        alpha: 1.0,
                    }
                } else if elapsed
                    < Self::ENTER_ANIMATION.saturating_add(Self::ENTER_FADE)
                {
                    let fade_elapsed = elapsed - Self::ENTER_ANIMATION;
                    let fade_ratio = fade_elapsed.as_secs_f32() / Self::ENTER_FADE.as_secs_f32();
                    AutoDriveTransitionFrame {
                        t: 1.0,
                        alpha: (1.0 - fade_ratio).clamp(0.0, 1.0),
                    }
                } else {
                    AutoDriveTransitionFrame::done()
                }
            }
            AutoDriveTransitionPhase::Exiting => {
                let elapsed = self.started_at.elapsed();
                if elapsed < Self::EXIT_BUILD {
                    let t = elapsed.as_secs_f32() / Self::EXIT_BUILD.as_secs_f32();
                    AutoDriveTransitionFrame {
                        t: t.clamp(0.0, 1.0),
                        alpha: 1.0,
                    }
                } else if elapsed < Self::EXIT_BUILD.saturating_add(Self::EXIT_FADE) {
                    let fade_elapsed = elapsed - Self::EXIT_BUILD;
                    let fade_ratio = fade_elapsed.as_secs_f32() / Self::EXIT_FADE.as_secs_f32();
                    AutoDriveTransitionFrame {
                        t: 1.0,
                        alpha: (1.0 - fade_ratio).clamp(0.0, 1.0),
                    }
                } else {
                    AutoDriveTransitionFrame::done()
                }
            }
        }
    }

    fn ensure_geometry(&mut self, mut geometry: TransitionGeometry) {
        if self.geometry.is_none() {
            let start = if let Some(override_rect) = self.start_override {
                override_rect
            } else {
                geometry.start
            };

            let target = normalize_target_rect(geometry.target);

            geometry.start = start;
            geometry.target = target;
            self.geometry = Some(geometry);
        }
    }

    fn geometry(&self) -> Option<TransitionGeometry> {
        self.geometry
    }

    fn variant(&self) -> AnimationVariant { self.variant }
}

#[derive(Clone, Copy, Debug)]
struct AutoDriveTransitionFrame {
    t: f32,
    alpha: f32,
}

impl AutoDriveTransitionFrame {
    fn done() -> Self {
        Self {
            t: 1.0,
            alpha: 0.0,
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct AnimationFrameCtx {
    sweep: f32,
    fade: f32,
    elapsed: f32,
    start_rect: Rect,
    target_rect: Rect,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum AnimationVariant {
    Glide,
    Wavefront,
    Bloom,
    Slide,
    Fade,
}

impl AnimationVariant {
    fn all() -> [Self; 5] {
        [
            AnimationVariant::Glide,
            AnimationVariant::Wavefront,
            AnimationVariant::Bloom,
            AnimationVariant::Slide,
            AnimationVariant::Fade,
        ]
    }
}

fn select_animation_variant() -> AnimationVariant {
    let index = std::env::var("ANI_MODE")
        .ok()
        .and_then(|raw| raw.trim().parse::<usize>().ok())
        .unwrap_or(0)
        % AnimationVariant::all().len();
    AnimationVariant::all()[index]
}

fn normalize_target_rect(target: Rect) -> Rect {
    if target.width >= 3 && target.height >= 3 {
        return target;
    }

    Rect {
        width: target.width.max(3),
        height: target.height.max(3),
        ..target
    }
}

/// Pane displayed in the lower half of the chat UI.
pub(crate) struct BottomPane<'a> {
    /// Composer is retained even when a BottomPaneView is displayed so the
    /// input state is retained when the view is closed.
    composer: ChatComposer,

    /// If present, this is displayed instead of the `composer`.
    active_view: Option<Box<dyn BottomPaneView<'a> + 'a>>,
    active_view_kind: ActiveViewKind,

    app_event_tx: AppEventSender,
    has_input_focus: bool,
    is_task_running: bool,
    ctrl_c_quit_hint: bool,

    /// True if the active view is the StatusIndicatorView that replaces the
    /// composer during a running task.
    status_view_active: bool,

    /// Whether to reserve an empty spacer line above the input composer.
    /// Defaults to true for visual breathing room, but can be disabled when
    /// the chat history is scrolled up to allow history to reclaim that row.
    top_spacer_enabled: bool,

    pub(crate) using_chatgpt_auth: bool,

    auto_drive_variant: AutoDriveVariant,
    auto_drive_active: bool,

    auto_transition: RefCell<Option<AutoDriveTransitionState>>,
    last_composer_rect: Cell<Option<Rect>>,
}

pub(crate) struct BottomPaneParams {
    pub(crate) app_event_tx: AppEventSender,
    pub(crate) has_input_focus: bool,
    pub(crate) enhanced_keys_supported: bool,
    pub(crate) using_chatgpt_auth: bool,
    pub(crate) auto_drive_variant: AutoDriveVariant,
}

impl BottomPane<'_> {
    // Reduce bottom padding so footer sits one line lower
    const BOTTOM_PAD_LINES: u16 = 1;
    pub fn new(params: BottomPaneParams) -> Self {
        let enhanced_keys_supported = params.enhanced_keys_supported;
        let composer = ChatComposer::new(
            params.has_input_focus,
            params.app_event_tx.clone(),
            enhanced_keys_supported,
            params.using_chatgpt_auth,
        );

        Self {
            composer,
            active_view: None,
            active_view_kind: ActiveViewKind::None,
            app_event_tx: params.app_event_tx,
            has_input_focus: params.has_input_focus,
            is_task_running: false,
            ctrl_c_quit_hint: false,
            status_view_active: false,
            top_spacer_enabled: true,
            using_chatgpt_auth: params.using_chatgpt_auth,
            auto_drive_variant: params.auto_drive_variant,
            auto_drive_active: false,
            auto_transition: RefCell::new(None),
            last_composer_rect: Cell::new(None),
        }
    }

    fn auto_view_mut(&mut self) -> Option<&mut AutoCoordinatorView> {
        if self.active_view_kind != ActiveViewKind::AutoCoordinator {
            return None;
        }
        self.active_view
            .as_mut()
            .and_then(|view| view.as_any_mut())
            .and_then(|any| any.downcast_mut::<AutoCoordinatorView>())
    }

    fn apply_auto_drive_style(&mut self) {
        if !self.auto_drive_active {
            self.composer.set_auto_drive_style(None);
            return;
        }

        let style = self.auto_drive_variant.style();
        self.composer.set_auto_drive_active(true);
        self.composer
            .set_auto_drive_style(Some(style.composer.clone()));
        if let Some(view) = self.auto_view_mut() {
            view.set_style(style.clone());
        }

        self.request_redraw();
    }

    fn enable_auto_drive_style(&mut self) {
        if !self.auto_drive_active {
            self.auto_drive_active = true;
            self.composer.set_auto_drive_active(true);
        }
        self.apply_auto_drive_style();
    }

    fn disable_auto_drive_style(&mut self) {
        if !self.auto_drive_active {
            return;
        }
        self.auto_drive_active = false;
        self.composer.set_auto_drive_active(false);
        self.composer.set_auto_drive_style(None);
        let style = self.auto_drive_variant.style();
        if let Some(view) = self.auto_view_mut() {
            view.set_style(style);
        }
        self.request_redraw();
    }

    pub(crate) fn set_auto_drive_variant(&mut self, variant: AutoDriveVariant) {
        if self.auto_drive_variant == variant {
            return;
        }
        self.auto_drive_variant = variant;
        if self.auto_drive_active {
            self.apply_auto_drive_style();
        }
    }

    fn begin_auto_transition(&self, phase: AutoDriveTransitionPhase) {
        let start_rect = self.last_composer_rect.get();
        let state = AutoDriveTransitionState::new(phase, start_rect);
        self.auto_transition.replace(Some(state));
        self.request_redraw();
        self
            .app_event_tx
            .send(AppEvent::ScheduleFrameIn(Duration::from_millis(16)));
    }

    /// Show Agents overview (Agents + Commands sections)
    pub fn show_agents_overview(
        &mut self,
        agents: Vec<(String, bool, bool, String)>,
        commands: Vec<String>,
        selected_index: usize,
    ) {
        use agents_overview_view::AgentsOverviewView;
        let view = AgentsOverviewView::new(agents, commands, selected_index, self.app_event_tx.clone());
        self.active_view = Some(Box::new(view));
        self.active_view_kind = ActiveViewKind::Other;
        self.status_view_active = false;
        self.request_redraw();
    }

    pub fn show_update_settings(&mut self, view: update_settings_view::UpdateSettingsView) {
        if !crate::updates::upgrade_ui_enabled() {
            self.request_redraw();
            return;
        }

        self.active_view = Some(Box::new(view));
        self.active_view_kind = ActiveViewKind::Other;
        self.status_view_active = false;
        self.request_redraw();
    }

    pub fn show_notifications_settings(&mut self, view: NotificationsSettingsView) {
        self.active_view = Some(Box::new(view));
        self.active_view_kind = ActiveViewKind::Other;
        self.status_view_active = false;
        self.request_redraw();
    }

    pub(crate) fn show_auto_drive_settings(
        &mut self,
        review_enabled: bool,
        agents_enabled: bool,
        continue_mode: AutoContinueMode,
    ) {
        let view = AutoDriveSettingsView::new(
            self.app_event_tx.clone(),
            review_enabled,
            agents_enabled,
            continue_mode,
        );
        self.active_view = Some(Box::new(view));
        self.active_view_kind = ActiveViewKind::AutoSettings;
        self.status_view_active = false;
        self.composer.set_embedded_mode(false);
        self.request_redraw();
    }

    pub(crate) fn clear_auto_drive_settings(&mut self) {
        if matches!(self.active_view_kind, ActiveViewKind::AutoSettings) {
            self.active_view = None;
            self.active_view_kind = ActiveViewKind::None;
            self.status_view_active = false;
            self.request_redraw();
        }
    }

    /// Show per-agent editor
    pub fn show_agent_editor(
        &mut self,
        name: String,
        enabled: bool,
        args_read_only: Option<Vec<String>>,
        args_write: Option<Vec<String>>,
        instructions: Option<String>,
        command: String,
    ) {
        use agent_editor_view::AgentEditorView;
        let view = AgentEditorView::new(
            name,
            enabled,
            args_read_only,
            args_write,
            instructions,
            command,
            self.app_event_tx.clone(),
        );
        self.active_view = Some(Box::new(view));
        self.active_view_kind = ActiveViewKind::Other;
        self.status_view_active = false;
        self.request_redraw();
    }

    pub fn show_login_accounts(&mut self, view: LoginAccountsView) {
        self.active_view = Some(Box::new(view));
        self.active_view_kind = ActiveViewKind::Other;
        self.status_view_active = false;
        self.request_redraw();
    }

    pub fn show_login_add_account(&mut self, view: LoginAddAccountView) {
        self.active_view = Some(Box::new(view));
        self.active_view_kind = ActiveViewKind::Other;
        self.status_view_active = false;
        self.request_redraw();
    }

    pub fn set_using_chatgpt_auth(&mut self, using: bool) {
        if self.using_chatgpt_auth != using {
            self.using_chatgpt_auth = using;
            self.composer.set_using_chatgpt_auth(using);
            self.request_redraw();
        }
    }

    pub fn set_has_chat_history(&mut self, has_history: bool) {
        self.composer.set_has_chat_history(has_history);
    }

    pub fn desired_height(&self, width: u16) -> u16 {
        let (view_height, pad_lines) = if let Some(view) = self.active_view.as_ref() {
            let is_auto = matches!(self.active_view_kind, ActiveViewKind::AutoCoordinator);
            let top_spacer = if is_auto {
                0
            } else if self.top_spacer_enabled {
                1
            } else {
                0
            };
            let pad = if is_auto { 0 } else { BottomPane::BOTTOM_PAD_LINES };
            let base_height = if is_auto {
                view
                    .as_any()
                    .and_then(|any| any.downcast_ref::<AutoCoordinatorView>())
                    .map(|auto_view| auto_view.desired_height_with_composer(width, &self.composer))
                    .unwrap_or_else(|| view.desired_height(width))
            } else {
                view.desired_height(width)
            };

            (base_height.saturating_add(top_spacer), pad)
        } else {
            // Optionally add 1 for the empty line above the composer
            let spacer = if self.top_spacer_enabled { 1 } else { 0 };
            (spacer + self.composer.desired_height(width), Self::BOTTOM_PAD_LINES)
        };

        view_height.saturating_add(pad_lines)
    }

    pub fn cursor_pos(&self, area: Rect) -> Option<(u16, u16)> {
        // Hide the cursor whenever an overlay view is active (e.g. approval modal).
        // But keep cursor visible when only status overlay is shown.
        if self.active_view.is_some() {
            None
        } else {
            // Account for the optional empty line above the composer
            let y_offset = if self.top_spacer_enabled { 1u16 } else { 0u16 };

            // Adjust composer area to account for empty line and padding
            let horizontal_padding = 1u16; // Message input uses 1 char padding
            let composer_rect = Rect {
                x: area.x + horizontal_padding,
                y: area.y + y_offset,
                width: area.width.saturating_sub(horizontal_padding * 2),
                height: (area.height.saturating_sub(y_offset))
                    - BottomPane::BOTTOM_PAD_LINES
                        .min((area.height.saturating_sub(y_offset)).saturating_sub(1)),
            };
            self.composer.cursor_pos(composer_rect)
        }
    }

    /// Forward a key event to the active view or the composer.
    pub fn handle_key_event(&mut self, key_event: KeyEvent) -> InputResult {
        if let Some(mut view) = self.active_view.take() {
            let kind = self.active_view_kind;
            if matches!(kind, ActiveViewKind::AutoCoordinator) {
                let consumed = if let Some(auto_view) = view
                    .as_any_mut()
                    .and_then(|any| any.downcast_mut::<AutoCoordinatorView>())
                {
                    auto_view.handle_active_key_event(self, key_event)
                } else {
                    view.handle_key_event(self, key_event);
                    true
                };

                if !view.is_complete() {
                    self.active_view = Some(view);
                    self.active_view_kind = kind;
                } else {
                    self.active_view_kind = ActiveViewKind::None;
                }

                if consumed {
                    self.request_redraw();
                    if matches!(key_event.kind, KeyEventKind::Press | KeyEventKind::Repeat) {
                        match key_event.code {
                            KeyCode::Up => return InputResult::ScrollUp,
                            KeyCode::Down => return InputResult::ScrollDown,
                            _ => {}
                        }
                    }
                    return InputResult::None;
                }

                return self.handle_composer_key_event(key_event);
            }

            view.handle_key_event(self, key_event);
            if !view.is_complete() {
                self.active_view = Some(view);
                self.active_view_kind = kind;
            } else {
                self.active_view_kind = ActiveViewKind::None;
            }
            // Don't create a status view - keep composer visible
            // Debounce view navigation redraws to reduce render thrash
            self.request_redraw();

            InputResult::None
        } else {
            self.handle_composer_key_event(key_event)
        }
    }

    fn handle_composer_key_event(&mut self, key_event: KeyEvent) -> InputResult {
        let (input_result, needs_redraw) = self.composer.handle_key_event(key_event);
        if needs_redraw {
            // Route input updates through the app's debounced redraw path so typing
            // doesn't attempt a full-screen redraw per key.
            self.request_redraw();
        }
        if self.composer.is_in_paste_burst() {
            self.request_redraw_in(ChatComposer::recommended_paste_flush_delay());
        }
        input_result
    }

    /// Attempt to navigate history upwards from the composer. Returns true if consumed.
    pub(crate) fn try_history_up(&mut self) -> bool {
        let consumed = self.composer.try_history_up();
        if consumed { self.request_redraw(); }
        consumed
    }

    /// Attempt to navigate history downwards from the composer. Returns true if consumed.
    pub(crate) fn try_history_down(&mut self) -> bool {
        let consumed = self.composer.try_history_down();
        if consumed { self.request_redraw(); }
        consumed
    }

    /// Returns true if the composer is currently browsing history.
    pub(crate) fn history_is_browsing(&self) -> bool { self.composer.history_is_browsing() }

    /// After a chat scroll-up, make the next Down key scroll chat instead of moving within input.
    pub(crate) fn mark_next_down_scrolls_history(&mut self) { self.composer.mark_next_down_scrolls_history(); }

    /// Handle Ctrl-C in the bottom pane. If a modal view is active it gets a
    /// chance to consume the event (e.g. to dismiss itself).
    pub(crate) fn on_ctrl_c(&mut self) -> CancellationEvent {
        let kind = self.active_view_kind;
        let mut view = match self.active_view.take() {
            Some(view) => view,
            None => return CancellationEvent::Ignored,
        };

        let event = view.on_ctrl_c(self);
        match event {
            CancellationEvent::Handled => {
                if !view.is_complete() {
                    self.active_view = Some(view);
                    self.active_view_kind = kind;
                } else {
                    self.active_view_kind = ActiveViewKind::None;
                }
                // Don't create a status view - keep composer visible
                self.show_ctrl_c_quit_hint();
            }
            CancellationEvent::Ignored => {
                self.active_view = Some(view);
                self.active_view_kind = kind;
            }
        }
        event
    }

    pub fn handle_paste(&mut self, pasted: String) {
        if let Some(mut view) = self.active_view.take() {
            use crate::bottom_pane::bottom_pane_view::ConditionalUpdate;
            let kind = self.active_view_kind;
            let update = view.handle_paste_with_composer(&mut self.composer, pasted);
            if !view.is_complete() {
                self.active_view = Some(view);
                self.active_view_kind = kind;
            } else {
                self.active_view_kind = ActiveViewKind::None;
            }
            if matches!(update, ConditionalUpdate::NeedsRedraw) {
                self.request_redraw();
            }
            return;
        }
        let needs_redraw = self.composer.handle_paste(pasted);
        if needs_redraw {
            // Large pastes may arrive as bursts; coalesce paints
            self.request_redraw();
        }
    }

    pub(crate) fn insert_str(&mut self, text: &str) {
        self.composer.insert_str(text);
        self.request_redraw();
    }

    /// Clear the composer text and reset transient composer state.
    pub(crate) fn clear_composer(&mut self) {
        self.composer.clear_text();
        self.request_redraw();
    }

    /// Attempt to close the file-search popup if visible. Returns true if closed.
    pub(crate) fn close_file_popup_if_active(&mut self) -> bool {
        let closed = self.composer.close_file_popup_if_active();
        if closed { self.request_redraw(); }
        closed
    }

    pub(crate) fn file_popup_visible(&self) -> bool {
        self.composer.file_popup_visible()
    }

    /// True if a modal/overlay view is currently displayed (not the composer popup).
    pub(crate) fn has_active_modal_view(&self) -> bool {
        // Consider a modal inactive once it has completed to avoid blocking
        // Esc routing and other overlay checks after a decision is made.
        match self.active_view.as_ref() {
            Some(_) if matches!(self.active_view_kind, ActiveViewKind::AutoCoordinator) => false,
            Some(view) => !view.is_complete(),
            None => false,
        }
    }

    /// Enable or disable compact compose mode. When enabled, the spacer line
    /// above the input composer is removed so the history can scroll into that
    /// row. This is typically toggled when the user scrolls up.
    pub(crate) fn set_compact_compose(&mut self, compact: bool) {
        let new_enabled = !compact;
        if self.top_spacer_enabled != new_enabled {
            self.top_spacer_enabled = new_enabled;
            self.request_redraw();
        }
    }

    /// Update the status indicator text. Shows status as overlay above composer
    /// to allow continued input while processing.
    pub(crate) fn update_status_text(&mut self, text: String) {
        if let Some(view) = self.active_view.as_mut() {
            let _ = view.update_status_text(text.clone());
        }

        // Pass status message to composer for dynamic title display
        self.composer.update_status_message(text);
        self.request_redraw();
    }

    /// Show an ephemeral footer notice for a custom duration.
    pub(crate) fn flash_footer_notice_for(&mut self, text: String, dur: Duration) {
        self.composer.flash_footer_notice_for(text, dur);
        // Ask app to clear it slightly after expiry to avoid flicker on boundary
        self.app_event_tx
            .send(AppEvent::ScheduleFrameIn(dur + Duration::from_millis(100)));
        self.request_redraw();
    }

    pub(crate) fn set_standard_terminal_hint(&mut self, hint: Option<String>) {
        self.composer.set_standard_terminal_hint(hint);
        self.request_redraw();
    }

    pub(crate) fn show_ctrl_c_quit_hint(&mut self) {
        self.ctrl_c_quit_hint = true;
        self.composer
            .set_ctrl_c_quit_hint(true, self.has_input_focus);
        self.request_redraw();
    }

    pub(crate) fn clear_ctrl_c_quit_hint(&mut self) {
        if self.ctrl_c_quit_hint {
            self.ctrl_c_quit_hint = false;
            self.composer
                .set_ctrl_c_quit_hint(false, self.has_input_focus);
            self.request_redraw();
        }
    }

    pub(crate) fn ctrl_c_quit_hint_visible(&self) -> bool {
        self.ctrl_c_quit_hint
    }

    pub fn set_task_running(&mut self, running: bool) {
        self.is_task_running = running;
        self.composer.set_task_running(running);

        if running {
            // No longer need separate status widget - title shows in composer
            self.request_redraw();
        } else {
            // Status now shown in composer title
            // Drop the status view when a task completes, but keep other
            // modal views (e.g. approval dialogs).
            if let Some(mut view) = self.active_view.take() {
                let kind = self.active_view_kind;
                if !view.should_hide_when_task_is_done() {
                    self.active_view = Some(view);
                    self.active_view_kind = kind;
                } else {
                    self.active_view_kind = ActiveViewKind::None;
                }
                self.status_view_active = false;
            }
        }
    }

    #[allow(dead_code)]
    pub(crate) fn composer_is_empty(&self) -> bool {
        self.composer.is_empty()
    }

    pub(crate) fn composer_text(&self) -> String {
        self.composer.text().to_string()
    }

    pub(crate) fn is_task_running(&self) -> bool {
        self.is_task_running
    }

    // is_normal_backtrack_mode removed; App-level policy handles Esc behavior directly.

    /// Update the *context-window remaining* indicator in the composer. This
    /// is forwarded directly to the underlying `ChatComposer`.
    pub(crate) fn set_token_usage(
        &mut self,
        total_token_usage: TokenUsage,
        last_token_usage: TokenUsage,
        model_context_window: Option<u64>,
    ) {
        self.composer
            .set_token_usage(total_token_usage, last_token_usage, model_context_window);
        self.request_redraw();
    }

    /// Called when the agent requests user approval.
    pub fn push_approval_request(
        &mut self,
        request: ApprovalRequest,
        ticket: BackgroundOrderTicket,
    ) {
        let (request, ticket) = if let Some(view) = self.active_view.as_mut() {
            match view.try_consume_approval_request(request, ticket.clone()) {
                Some((request, ticket)) => (request, ticket),
                None => {
                    self.request_redraw();
                    return;
                }
            }
        } else {
            (request, ticket)
        };

        // Otherwise create a new approval modal overlay.
        let modal = ApprovalModalView::new(request, ticket, self.app_event_tx.clone());
        self.active_view = Some(Box::new(modal));
        self.active_view_kind = ActiveViewKind::Other;
        // Hide any overlay status while a modal is visible.
        // Status shown in composer title now
        self.status_view_active = false;
        self.request_redraw()
    }

    /// Show the model selection UI
    pub fn show_model_selection(
        &mut self,
        presets: Vec<ModelPreset>,
        current_model: String,
        current_effort: ReasoningEffort,
    ) {
        let view = ModelSelectionView::new(presets, current_model, current_effort, self.app_event_tx.clone());
        self.active_view = Some(Box::new(view));
        self.active_view_kind = ActiveViewKind::Other;
        // Status shown in composer title now
        self.status_view_active = false;
        self.request_redraw()
    }

    /// Show the theme selection UI
    pub fn show_theme_selection(
        &mut self,
        _current_theme: ThemeName,
        tail_ticket: BackgroundOrderTicket,
        before_ticket: BackgroundOrderTicket,
    ) {
        let view = ThemeSelectionView::new(
            crate::theme::current_theme_name(),
            self.app_event_tx.clone(),
            tail_ticket,
            before_ticket,
        );
        self.active_view = Some(Box::new(view));
        self.active_view_kind = ActiveViewKind::Other;
        // Status shown in composer title now
        self.status_view_active = false;
        self.request_redraw()
    }

    /// Show the Chrome launch options UI
    pub fn show_chrome_selection(&mut self, port: Option<u16>) {
        use chrome_selection_view::ChromeSelectionView;
        let view = ChromeSelectionView::new(self.app_event_tx.clone(), port);
        self.active_view = Some(Box::new(view));
        self.active_view_kind = ActiveViewKind::Other;
        // Status shown in composer title now
        self.status_view_active = false;
        self.request_redraw()
    }

    /// Show the diffs popup with tabs for each file.
    #[allow(dead_code)]
    pub fn show_diff_popup(&mut self, tabs: Vec<(String, Vec<ratatui::text::Line<'static>>)>) {
        let view = diff_popup::DiffPopupView::new(tabs);
        self.active_view = Some(Box::new(view));
        self.active_view_kind = ActiveViewKind::Other;
        self.status_view_active = false;
        self.request_redraw()
    }

    /// Show the verbosity selection UI
    pub fn show_verbosity_selection(&mut self, current_verbosity: TextVerbosity) {
        let view = VerbositySelectionView::new(current_verbosity, self.app_event_tx.clone());
        self.active_view = Some(Box::new(view));
        self.active_view_kind = ActiveViewKind::Other;
        // Status shown in composer title now
        self.status_view_active = false;
        self.request_redraw()
    }

    /// Show a multi-line prompt input view (used for custom review instructions)
    pub fn show_custom_prompt(&mut self, view: CustomPromptView) {
        self.active_view = Some(Box::new(view));
        self.active_view_kind = ActiveViewKind::Other;
        self.status_view_active = false;
        self.request_redraw();
    }

    /// Show a generic list selection popup with items and actions.
    pub fn show_list_selection(
        &mut self,
        _title: String,
        _subtitle: Option<String>,
        _footer_hint: Option<String>,
        items: crate::bottom_pane::list_selection_view::ListSelectionView,
    ) {
        self.active_view = Some(Box::new(items));
        self.active_view_kind = ActiveViewKind::Other;
        // Status shown in composer title now
        self.status_view_active = false;
        self.request_redraw();
    }

    pub fn show_cloud_tasks(&mut self, view: CloudTasksView) {
        self.active_view = Some(Box::new(view));
        self.active_view_kind = ActiveViewKind::Other;
        self.status_view_active = false;
        self.request_redraw();
    }

    /// Show the resume selection UI with structured rows
    pub fn show_resume_selection(
        &mut self,
        title: String,
        subtitle: Option<String>,
        rows: Vec<resume_selection_view::ResumeRow>,
    ) {
        use resume_selection_view::ResumeSelectionView;
        let view = ResumeSelectionView::new(title, subtitle.unwrap_or_default(), rows, self.app_event_tx.clone());
        self.active_view = Some(Box::new(view));
        self.active_view_kind = ActiveViewKind::Other;
        self.status_view_active = false;
        self.request_redraw()
    }

    /// Show GitHub settings (token status + watcher toggle)
    pub fn show_github_settings(&mut self, watcher_enabled: bool, token_status: String, ready: bool) {
        use github_settings_view::GithubSettingsView;
        let view = GithubSettingsView::new(watcher_enabled, token_status, ready, self.app_event_tx.clone());
        self.active_view = Some(Box::new(view));
        self.active_view_kind = ActiveViewKind::Other;
        self.status_view_active = false;
        self.request_redraw();
    }

    pub fn show_undo_timeline_view(&mut self, view: UndoTimelineView) {
        self.active_view = Some(Box::new(view));
        self.active_view_kind = ActiveViewKind::Other;
        self.status_view_active = false;
        self.request_redraw();
    }

    /// Show MCP servers status/toggle UI
    pub fn show_mcp_settings(&mut self, rows: crate::bottom_pane::mcp_settings_view::McpServerRows) {
        use mcp_settings_view::McpSettingsView;
        let view = McpSettingsView::new(rows, self.app_event_tx.clone());
        self.active_view = Some(Box::new(view));
        self.active_view_kind = ActiveViewKind::Other;
        self.status_view_active = false;
        self.request_redraw();
    }

    /// Show validation harness settings (master toggle + per-tool toggles).
    pub fn show_validation_settings(
        &mut self,
        groups: Vec<(validation_settings_view::GroupStatus, bool)>,
        tools: Vec<validation_settings_view::ToolRow>,
    ) {
        use validation_settings_view::ValidationSettingsView;
        let view = ValidationSettingsView::new(groups, tools, self.app_event_tx.clone());
        self.active_view = Some(Box::new(view));
        self.active_view_kind = ActiveViewKind::Other;
        self.status_view_active = false;
        self.request_redraw();
    }

    /// Show Subagent editor UI
    pub fn show_subagent_editor(
        &mut self,
        name: String,
        available_agents: Vec<String>,
        existing: Vec<code_core::config_types::SubagentCommandConfig>,
        is_new: bool,
    ) {
        use crate::bottom_pane::agents_settings_view::SubagentEditorView;
        let view = SubagentEditorView::new_with_data(name, available_agents, existing, is_new, self.app_event_tx.clone());
        self.active_view = Some(Box::new(view));
        self.active_view_kind = ActiveViewKind::Other;
        self.status_view_active = false;
        self.request_redraw();
    }

    pub(crate) fn show_auto_coordinator_view(&mut self, model: AutoCoordinatorViewModel) {
        if self.active_view_kind != ActiveViewKind::AutoCoordinator {
            self.begin_auto_transition(AutoDriveTransitionPhase::Entering);
        }
        if let Some(existing) = self.active_view.as_mut() {
            if self.active_view_kind == ActiveViewKind::AutoCoordinator {
                if let Some(existing_any) = existing.as_any_mut() {
                    if let Some(auto_view) = existing_any.downcast_mut::<AutoCoordinatorView>() {
                        auto_view.update_model(model);
                        auto_view.set_style(self.auto_drive_variant.style());
                        let status_text = self
                            .composer
                            .status_message()
                            .map_or_else(String::new, str::to_string);
                        let _ = auto_view.update_status_text(status_text);
                        self.status_view_active = false;
                        self.composer.set_embedded_mode(true);
                        self.enable_auto_drive_style();
                        self.request_redraw();
                        return;
                    }
                }
            }
        }

        if self.active_view.is_some() && self.active_view_kind != ActiveViewKind::AutoCoordinator {
            return;
        }

        let mut view = AutoCoordinatorView::new(
            model,
            self.app_event_tx.clone(),
            self.auto_drive_variant.style(),
        );
        let status_text = self
            .composer
            .status_message()
            .map_or_else(String::new, str::to_string);
        let _ = view.update_status_text(status_text);
        self.active_view = Some(Box::new(view));
        self.active_view_kind = ActiveViewKind::AutoCoordinator;
        self.status_view_active = false;
        self.composer.set_embedded_mode(true);
        self.enable_auto_drive_style();
        self.request_redraw();
    }

    pub(crate) fn clear_auto_coordinator_view(&mut self, disable_style: bool) {
        if self.active_view_kind == ActiveViewKind::AutoCoordinator {
            self.active_view = None;
            self.active_view_kind = ActiveViewKind::None;
            self.status_view_active = false;
            self.composer.set_embedded_mode(false);
            if disable_style {
                self.disable_auto_drive_style();
            } else if self.auto_drive_active {
                self.apply_auto_drive_style();
            }
            self.request_redraw();
        }
    }

    /// Height (terminal rows) required by the current bottom pane.
    pub(crate) fn request_redraw(&self) {
        self.app_event_tx.send(AppEvent::RequestRedraw)
    }

    // Immediate redraw path removed; all UI updates flow through the
    // debounced RequestRedraw/App::Redraw scheduler to reduce thrash.

    pub(crate) fn flash_footer_notice(&mut self, text: String) {
        self.composer.flash_footer_notice(text);
        // Ask app to schedule a redraw shortly to clear the notice automatically
        self.app_event_tx
            .send(AppEvent::ScheduleFrameIn(std::time::Duration::from_millis(2100)));
        self.request_redraw();
    }

    /// Control footer hint visibility: whether to show Ctrl+R (reasoning) and Ctrl+D (diffs)
    #[allow(dead_code)]
    pub(crate) fn set_footer_hints(&mut self, show_reasoning: bool, show_diffs: bool) {
        self.composer.set_show_reasoning_hint(show_reasoning);
        self.composer.set_show_diffs_hint(show_diffs);
        self.request_redraw();
    }

    /// Convenience setters for individual hints
    pub(crate) fn set_reasoning_hint(&mut self, show: bool) {
        self.composer.set_show_reasoning_hint(show);
        self.request_redraw();
    }

    pub(crate) fn set_reasoning_state(&mut self, shown: bool) {
        self.composer.set_reasoning_state(shown);
        self.request_redraw();
    }

    pub(crate) fn set_diffs_hint(&mut self, show: bool) {
        self.composer.set_show_diffs_hint(show);
        self.request_redraw();
    }

    pub(crate) fn request_redraw_in(&self, dur: Duration) {
        self.app_event_tx.send(AppEvent::ScheduleFrameIn(dur));
    }

    // --- History helpers ---

    pub(crate) fn set_history_metadata(&mut self, log_id: u64, entry_count: usize) {
        self.composer.set_history_metadata(log_id, entry_count);
    }

    #[allow(dead_code)]
    pub(crate) fn flush_paste_burst_if_due(&mut self) -> bool {
        self.composer.flush_paste_burst_if_due()
    }

    #[allow(dead_code)]
    pub(crate) fn is_in_paste_burst(&self) -> bool {
        self.composer.is_in_paste_burst()
    }

    pub(crate) fn set_input_focus(&mut self, has_focus: bool) {
        self.has_input_focus = has_focus;
        self.composer.set_has_focus(has_focus);
        self.composer
            .set_ctrl_c_quit_hint(self.ctrl_c_quit_hint, self.has_input_focus);
    }

    pub(crate) fn on_history_entry_response(
        &mut self,
        log_id: u64,
        offset: usize,
        entry: Option<String>,
    ) {
        let updated = self
            .composer
            .on_history_entry_response(log_id, offset, entry);

        if updated {
            self.request_redraw();
        }
    }

    pub(crate) fn on_file_search_result(&mut self, query: String, matches: Vec<FileMatch>) {
        self.composer.on_file_search_result(query, matches);
        self.request_redraw();
    }

    pub(crate) fn clear_live_ring(&mut self) {}
    
    // test helper removed

    /// Ensure input focus is maintained, especially after redraws or content updates
    pub(crate) fn ensure_input_focus(&mut self) {
        // Only ensure focus if there's no active modal view
        if self.active_view.is_none() {
            if !self.has_input_focus {
                self.set_input_focus(true);
            } else {
                self.composer
                    .set_ctrl_c_quit_hint(self.ctrl_c_quit_hint, self.has_input_focus);
            }
        }
    }

    pub(crate) fn set_access_mode_label(&mut self, label: Option<String>) {
        self.composer.set_access_mode_label(label);
        // Hide the "(Shift+Tab change)" suffix after a short time for persistent modes.
        // Avoid using a global frame scheduler which can be coalesced; instead spawn
        // a tiny timer to request a redraw slightly after expiry.
        let dur = Duration::from_secs(4);
        self.composer.set_access_mode_hint_for(dur);
        let tx = self.app_event_tx.clone();
        std::thread::spawn(move || {
            std::thread::sleep(dur + Duration::from_millis(120));
            tx.send(AppEvent::RequestRedraw);
        });
        self.request_redraw();
    }

    pub(crate) fn set_access_mode_label_ephemeral(&mut self, label: String, dur: Duration) {
        self.composer.set_access_mode_label_ephemeral(label, dur);
        // Schedule a redraw after expiry without blocking other scheduled frames.
        let tx = self.app_event_tx.clone();
        std::thread::spawn(move || {
            std::thread::sleep(dur + Duration::from_millis(120));
            tx.send(AppEvent::RequestRedraw);
        });
        self.request_redraw();
    }

    #[allow(dead_code)]
    fn render_auto_coordinator_footer(&self, _area: Rect, _buf: &mut Buffer) {}

    // Removed restart_live_status_with_text – no longer used by the current streaming UI.
}

#[cfg(feature = "code-fork")]
fn build_user_approval_widget<'a>(
    request: ApprovalRequest,
    ticket: BackgroundOrderTicket,
    app_event_tx: AppEventSender,
) -> UserApprovalWidget<'a> {
    <UserApprovalWidget<'a> as ApprovalUi>::build(request, ticket, app_event_tx)
}

#[cfg(not(feature = "code-fork"))]
fn build_user_approval_widget<'a>(
    request: ApprovalRequest,
    ticket: BackgroundOrderTicket,
    app_event_tx: AppEventSender,
) -> UserApprovalWidget<'a> {
    UserApprovalWidget::new(request, ticket, app_event_tx)
}

impl WidgetRef for &BottomPane<'_> {
    fn render_ref(&self, area: Rect, buf: &mut Buffer) {
        // Base clear: fill the entire bottom pane with the theme background so
        // newly exposed rows (e.g., when the composer grows on paste) do not
        // show stale pixels from history.
        let base_style = ratatui::style::Style::default()
            .bg(crate::colors::background())
            .fg(crate::colors::text());
        fill_rect(buf, area, Some(' '), base_style);

        let composer_rect = compute_composer_rect(area, self.top_spacer_enabled);
        let mut overlay_target = composer_rect;
        let mut rendered = false;

        if let Some(view) = &self.active_view {
            if !view.is_complete() {
                let is_auto = matches!(self.active_view_kind, ActiveViewKind::AutoCoordinator);
                let mut avail = area.height;
                if self.top_spacer_enabled && avail > 0 {
                    avail = avail.saturating_sub(1);
                }
                if avail > 0 {
                    let pad = if is_auto {
                        BottomPane::BOTTOM_PAD_LINES.min(avail.saturating_sub(1))
                    } else {
                        0
                    };
                    let view_height = avail.saturating_sub(pad);
                    if view_height > 0 {
                        let horizontal_padding = 1u16;
                        let y_base = if self.top_spacer_enabled {
                            area.y + 1
                        } else {
                            area.y
                        };
                        let view_rect = Rect {
                            x: area.x + horizontal_padding,
                            y: y_base,
                            width: area.width.saturating_sub(horizontal_padding * 2),
                            height: view_height,
                        };
                        let view_bg = ratatui::style::Style::default().bg(crate::colors::background());
                        fill_rect(buf, view_rect, None, view_bg);
                        view.render_with_composer(view_rect, buf, &self.composer);
                        if is_auto {
                            overlay_target = view_rect;
                        }
                        rendered = true;
                    }
                }
            }
        }

        if !rendered {
            let comp_bg = ratatui::style::Style::default().bg(crate::colors::background());
            fill_rect(buf, composer_rect, None, comp_bg);
            (&self.composer).render_ref(composer_rect, buf);
            self.last_composer_rect.set(Some(composer_rect));
        }

        let transition_phase = self
            .auto_transition
            .borrow()
            .as_ref()
            .map(|state| state.phase);

        if let Some(phase) = transition_phase {
            let target = match phase {
                AutoDriveTransitionPhase::Entering => overlay_target,
                AutoDriveTransitionPhase::Exiting => composer_rect,
            };
            self.render_auto_drive_transition_overlay(area, buf, composer_rect, target);
        }
    }
}

impl BottomPane<'_> {
    fn render_auto_drive_transition_overlay(
        &self,
        area: Rect,
        buf: &mut Buffer,
        composer_rect: Rect,
        target_rect: Rect,
    ) {
        let mut slot = self.auto_transition.borrow_mut();
        let Some(state) = slot.as_mut() else {
            return;
        };

        if area.width < 4 || area.height < 3 {
            slot.take();
            return;
        }

        state.ensure_geometry(TransitionGeometry {
            start: composer_rect,
            target: target_rect,
        });

        let frame = state.frame();
        let Some(geom) = state.geometry() else {
            slot.take();
            return;
        };

        let target = geom.target;
        let sweep = frame.t.clamp(0.0, 1.0);
        let fade = frame.alpha.clamp(0.0, 1.0);

        let ctx = AnimationFrameCtx {
            sweep,
            fade,
            elapsed: state.started_at.elapsed().as_secs_f32(),
            start_rect: geom.start,
            target_rect: target,
        };

        let variant = state.variant();
        render_transition_frame(buf, area, &ctx, variant, &self.composer);

        if sweep >= 1.0 && fade <= 0.0 {
            slot.take();
        } else {
            self.request_redraw();
            self
                .app_event_tx
                .send(AppEvent::ScheduleFrameIn(Duration::from_millis(16)));
        }
    }
}

fn compute_composer_rect(area: Rect, top_spacer_enabled: bool) -> Rect {
    let horizontal_padding = 1u16;
    let mut y_offset = 0u16;
    if top_spacer_enabled {
        y_offset = y_offset.saturating_add(1);
    }
    let height = (area.height - y_offset)
        - BottomPane::BOTTOM_PAD_LINES.min((area.height - y_offset).saturating_sub(1));
    Rect {
        x: area.x + horizontal_padding,
        y: area.y + y_offset,
        width: area.width.saturating_sub(horizontal_padding * 2),
        height,
    }
}

fn render_transition_frame(
    buf: &mut Buffer,
    overlay_area: Rect,
    ctx: &AnimationFrameCtx,
    variant: AnimationVariant,
    composer: &ChatComposer,
) {
    if ctx.target_rect.width < 3 || ctx.target_rect.height < 3 {
        return;
    }

    let mut progress = ctx.sweep.clamp(0.0, 1.0);
    if progress < 0.01 {
        progress = 0.0;
    }

    const BORDER_STAGE: f32 = 0.30;
    const FILL_STAGE: f32 = 0.80;

    let border_phase = (progress / BORDER_STAGE).clamp(0.0, 1.0);
    let fill_phase = ((progress - BORDER_STAGE) / (FILL_STAGE - BORDER_STAGE)).clamp(0.0, 1.0);
    let clear_phase = ((progress - FILL_STAGE) / (1.0 - FILL_STAGE)).clamp(0.0, 1.0);

    let growth_phase = if progress <= BORDER_STAGE {
        0.0
    } else {
        let raw = ((progress - BORDER_STAGE) / (1.0 - BORDER_STAGE)).clamp(0.0, 1.0);
        ease_in_out_cubic(raw)
    };

    let start_rect = normalize_target_rect(ctx.start_rect);
    let target_rect = normalize_target_rect(ctx.target_rect);

    let start_left = start_rect.x as f32;
    let start_top = start_rect.y as f32;
    let start_width = start_rect.width.max(3) as f32;
    let start_height = start_rect.height.max(3) as f32;

    let target_left = target_rect.x as f32;
    let target_top = target_rect.y as f32;
    let target_width = target_rect.width.max(3) as f32;
    let target_height = target_rect.height.max(3) as f32;

    let left_f = start_left + (target_left - start_left) * growth_phase;
    let top_f = start_top + (target_top - start_top) * growth_phase;
    let width_f = start_width + (target_width - start_width) * growth_phase;
    let height_f = start_height + (target_height - start_height) * growth_phase;

    let mut active_rect = Rect {
        x: left_f.floor().max(overlay_area.x as f32) as u16,
        y: top_f.floor().max(overlay_area.y as f32) as u16,
        width: width_f.ceil().max(3.0) as u16,
        height: height_f.ceil().max(3.0) as u16,
    };

    let overlay_right = overlay_area.x.saturating_add(overlay_area.width);
    let overlay_bottom = overlay_area.y.saturating_add(overlay_area.height);
    if active_rect.x + active_rect.width > overlay_right {
        active_rect.width = overlay_right.saturating_sub(active_rect.x);
    }
    if active_rect.y + active_rect.height > overlay_bottom {
        active_rect.height = overlay_bottom.saturating_sub(active_rect.y);
    }

    let start_right = start_rect.x.saturating_add(start_rect.width);
    let start_bottom = start_rect.y.saturating_add(start_rect.height);
    let target_right = target_rect.x.saturating_add(target_rect.width);
    let target_bottom = target_rect.y.saturating_add(target_rect.height);

    let union_left = start_rect.x.min(target_rect.x);
    let union_top = start_rect.y.min(target_rect.y);
    let union_right = start_right.max(target_right);
    let union_bottom = start_bottom.max(target_bottom);

    let cover_x0 = union_left.saturating_sub(1).max(overlay_area.x);
    let cover_y0 = union_top.saturating_sub(1).max(overlay_area.y);
    let cover_x1 = union_right.saturating_add(1).min(overlay_right);
    let cover_y1 = union_bottom.saturating_add(1).min(overlay_bottom);

    let width = cover_x1.saturating_sub(cover_x0);
    let height = cover_y1.saturating_sub(cover_y0);
    if width > 0 && height > 0 {
        let coverage = Rect {
            x: cover_x0,
            y: cover_y0,
            width,
            height,
        };

        fill_rect(buf, coverage, Some(' '), Style::default().bg(crate::colors::background()));
    }

    let front_rect = shrink_rect_bottom(start_rect, 1).unwrap_or(start_rect);
    composer.render_ref(front_rect, buf);

    if progress <= 0.0 {
        draw_box_outline(buf, start_rect);
        return;
    }

    if border_phase > 0.0 {
        render_start_border_trace(buf, target_rect, border_phase, ctx, variant);
    }

    if fill_phase > 0.0 {
        render_fill_stage(buf, active_rect, front_rect, variant, ctx, fill_phase);
    }

    if clear_phase > 0.0 {
        render_clear_stage(buf, target_rect, front_rect, clear_phase, ctx);
    }

    if clear_phase >= 1.0 || ctx.fade <= 0.01 {
        draw_box_outline_with_style(
            buf,
            target_rect,
            Style::default()
                .fg(crate::colors::border_dim())
                .bg(crate::colors::background())
                .add_modifier(Modifier::BOLD),
        );
    }

    composer.render_ref(front_rect, buf);
}

fn set_cell(buf: &mut Buffer, x: u16, y: u16, ch: char, style: Style) {
    let mut tmp = [0u8; 4];
    let cell = &mut buf[(x, y)];
    cell.set_symbol(ch.encode_utf8(&mut tmp));
    cell.set_style(style);
}

fn point_in_rect(x: u16, y: u16, rect: Rect) -> bool {
    x >= rect.x
        && x < rect.x.saturating_add(rect.width)
        && y >= rect.y
        && y < rect.y.saturating_add(rect.height)
}

fn shrink_rect_bottom(rect: Rect, rows: u16) -> Option<Rect> {
    if rect.height == 0 || rect.height <= rows {
        return None;
    }
    Some(Rect {
        x: rect.x,
        y: rect.y,
        width: rect.width,
        height: rect.height - rows,
    })
}

fn render_start_border_trace(
    buf: &mut Buffer,
    rect: Rect,
    phase: f32,
    ctx: &AnimationFrameCtx,
    variant: AnimationVariant,
) {
    if rect.width < 3 || rect.height < 3 {
        return;
    }

    let bottom = rect.y + rect.height - 1;
    let mut reveal_rows = (rect.height as f32 * phase).ceil() as u16;
    if reveal_rows == 0 {
        return;
    }
    reveal_rows = reveal_rows.min(rect.height);

    let start_y = bottom.saturating_sub(reveal_rows.saturating_sub(1));
    let profile = variant_profile(variant);
    let frame = (ctx.elapsed * 72.0) as u32;
    for y in start_y..=bottom {
        let progress = 1.0 - ((bottom.saturating_sub(y)) as f32 / rect.height.max(1) as f32);
        let hue = (profile.hue_shift + progress * 0.35 + ctx.elapsed * 0.22).fract();
        let intensity = (phase * 0.65 + progress * 0.45).clamp(0.15, 1.0);
        if y == bottom {
            for x in rect.x..rect.x + rect.width {
                if profile.outline_hole_stride > 0 {
                    let idx = ((x as u32 + frame + y as u32 * 3) % profile.outline_hole_stride as u32)
                        as u32;
                    if idx == profile.outline_hole_offset as u32 {
                        continue;
                    }
                }
                let noise = ((x as u32 * 13 + y as u32 * 7 + frame * 3) % 17) as f32 / 16.0;
                let ch = density_char(noise, &profile.outline_table);
                let style = vivid_style(hue + noise * 0.08, intensity.max(0.35), ctx);
                paint_outline_cell(buf, x, y, ch, style);
            }
        } else {
            for &(edge_x, sign) in &[(rect.x, 1i16), (rect.x + rect.width - 1, -1i16)] {
                if profile.outline_hole_stride > 0 {
                    let idx = ((edge_x as u32 * 11 + y as u32 * 5 + frame) % profile.outline_hole_stride as u32)
                        as u32;
                    if idx == profile.outline_hole_offset as u32 {
                        continue;
                    }
                }
                let noise = ((edge_x as u32 * 5 + y as u32 * 19 + frame * 5) % 23) as f32 / 22.0;
                let ch = density_char(noise, &profile.outline_table);
                let hue_mod = hue + (sign as f32) * 0.05 * noise;
                let style = vivid_style(hue_mod.fract(), (intensity * (0.8 + noise * 0.4)).clamp(0.2, 1.0), ctx);
                paint_outline_cell(buf, edge_x, y, ch, style);
            }
        }
    }

    if reveal_rows >= rect.height {
        let hue = (ctx.elapsed * 0.24).fract();
        let top = rect.y;
        for x in rect.x..rect.x + rect.width {
            if profile.outline_hole_stride > 0 {
                let idx = ((x as u32 + top as u32 * 17 + frame * 2) % profile.outline_hole_stride as u32) as u32;
                if idx == profile.outline_hole_offset as u32 {
                    continue;
                }
            }
            let noise = ((x as u32 * 7 + top as u32 * 13 + frame) % 19) as f32 / 18.0;
            let ch = density_char(noise, &profile.outline_table);
            let style = vivid_style((hue + noise * 0.07).fract(), (0.85 + noise * 0.3).clamp(0.2, 1.0), ctx);
            paint_outline_cell(buf, x, top, ch, style);
        }
    }
}

fn render_fill_stage(
    buf: &mut Buffer,
    rect: Rect,
    start_rect: Rect,
    variant: AnimationVariant,
    ctx: &AnimationFrameCtx,
    phase: f32,
) {
    if rect.width == 0 || rect.height == 0 {
        return;
    }

    let mut filled_rows = (rect.height as f32 * phase).ceil() as u16;
    if filled_rows == 0 {
        return;
    }
    filled_rows = filled_rows.min(rect.height);

    let profile = variant_profile(variant);
    let bottom = rect.y + rect.height - 1;
    let start_y = bottom.saturating_sub(filled_rows.saturating_sub(1));
    let frame = (ctx.elapsed * 60.0) as u32;

    for y in start_y..=bottom {
        let row_ratio = (bottom.saturating_sub(y)) as f32 / rect.height.max(1) as f32;
        for x in rect.x..rect.x + rect.width {
            if point_in_rect(x, y, start_rect) {
                continue;
            }
            if profile.hole_stride > 0 {
                let idx = ((x as u32 + y as u32 + frame) % profile.hole_stride as u32) as u32;
                if idx == profile.hole_offset as u32 {
                    continue;
                }
            }

            let col_ratio = (x.saturating_sub(rect.x)) as f32 / rect.width.max(1) as f32;
            let oscillation = ((ctx.elapsed * profile.time_speed)
                + col_ratio * profile.horizontal_wave
                + (1.0 - row_ratio) * profile.vertical_wave)
                .sin()
                * 0.5
                + 0.5;

            let intensity = clamp01(
                phase * 0.55 + (1.0 - row_ratio) * 0.45 + oscillation * profile.jitter_scale,
            );
            let hue = (profile.hue_shift + col_ratio * 0.4 + (1.0 - row_ratio) * 0.35
                + ctx.elapsed * profile.hue_time_speed)
                .fract();

            let ch = density_char(intensity, &profile.char_table);
            let style = vivid_style(hue, intensity.max(0.2), ctx);
            set_cell(buf, x, y, ch, style);
        }
    }
}

fn render_clear_stage(
    buf: &mut Buffer,
    rect: Rect,
    start_rect: Rect,
    phase: f32,
    ctx: &AnimationFrameCtx,
) {
    if rect.width < 2 || rect.height < 2 {
        return;
    }

    let mut cleared_rows = (rect.height as f32 * phase).ceil() as u16;
    if cleared_rows == 0 {
        return;
    }
    cleared_rows = cleared_rows.min(rect.height);

    let bottom = rect.y + rect.height - 1;
    let start_y = bottom.saturating_sub(cleared_rows.saturating_sub(1));
    let bg_style = Style::default().bg(crate::colors::background());

    let allow_inner_release = phase >= 0.85;
    for y in start_y..=bottom {
        for x in rect.x..rect.x + rect.width {
            if !allow_inner_release && point_in_rect(x, y, start_rect) {
                continue;
            }
            set_cell(buf, x, y, ' ', bg_style);
        }
    }

    draw_final_border_partial(buf, rect, phase, ctx);
}

fn draw_final_border_partial(
    buf: &mut Buffer,
    rect: Rect,
    phase: f32,
    ctx: &AnimationFrameCtx,
) {

    if rect.width < 2 || rect.height < 2 {
        return;
    }

    let bottom = rect.y + rect.height - 1;
    let mut reveal_rows = (rect.height as f32 * phase).ceil() as u16;
    if reveal_rows == 0 {
        return;
    }
    reveal_rows = reveal_rows.min(rect.height);

    let start_y = bottom.saturating_sub(reveal_rows.saturating_sub(1));
    let base_border = crate::colors::border_dim();

    for y in start_y..=bottom {
        let row_ratio = (bottom.saturating_sub(y)) as f32 / rect.height.max(1) as f32;
        let hue = ((1.0 - row_ratio) * 0.3 + ctx.elapsed * 0.15).fract();
        let accent = glitch_animation::gradient_multi(hue);
        let fg = glitch_animation::blend_to_background(
            glitch_animation::mix_rgb(base_border, accent, 0.2 + phase * 0.15),
            ctx.fade.max(0.02),
        );
        let style = Style::default().fg(fg).bg(crate::colors::background()).add_modifier(Modifier::BOLD);

        if y == bottom {
            for x in rect.x..rect.x + rect.width {
                let ch = if x == rect.x {
                    '└'
                } else if x + 1 == rect.x + rect.width {
                    '┘'
                } else {
                    '─'
                };
                paint_outline_cell(buf, x, y, ch, style);
            }
        } else if y == rect.y {
            for x in rect.x..rect.x + rect.width {
                let ch = if x == rect.x {
                    '┌'
                } else if x + 1 == rect.x + rect.width {
                    '┐'
                } else {
                    '─'
                };
                paint_outline_cell(buf, x, y, ch, style);
            }
        } else {
            paint_outline_cell(buf, rect.x, y, '│', style);
            paint_outline_cell(buf, rect.x + rect.width - 1, y, '│', style);
        }
    }
}

#[derive(Clone, Copy)]
struct VariantProfile {
    char_table: [char; 5],
    outline_table: [char; 4],
    hue_shift: f32,
    hue_time_speed: f32,
    time_speed: f32,
    horizontal_wave: f32,
    vertical_wave: f32,
    jitter_scale: f32,
    hole_stride: u16,
    hole_offset: u16,
    outline_hole_stride: u16,
    outline_hole_offset: u16,
}

fn variant_profile(variant: AnimationVariant) -> VariantProfile {
    match variant {
        AnimationVariant::Glide => VariantProfile {
            char_table: ['░', '▒', '▓', '▓', '█'],
            outline_table: ['░', '▒', '▓', '█'],
            hue_shift: 0.33,
            hue_time_speed: 0.24,
            time_speed: 2.6,
            horizontal_wave: 6.0,
            vertical_wave: 3.8,
            jitter_scale: 0.30,
            hole_stride: 0,
            hole_offset: 0,
            outline_hole_stride: 6,
            outline_hole_offset: 2,
        },
        AnimationVariant::Wavefront => VariantProfile {
            char_table: ['·', '░', '▒', '▓', '█'],
            outline_table: ['·', '░', '▒', '▓'],
            hue_shift: 0.35,
            hue_time_speed: 0.26,
            time_speed: 3.8,
            horizontal_wave: 9.5,
            vertical_wave: 6.0,
            jitter_scale: 0.36,
            hole_stride: 9,
            hole_offset: 3,
            outline_hole_stride: 5,
            outline_hole_offset: 1,
        },
        AnimationVariant::Bloom => VariantProfile {
            char_table: ['░', '▒', '▓', '█', '█'],
            outline_table: ['░', '▒', '▓', '█'],
            hue_shift: 0.34,
            hue_time_speed: 0.22,
            time_speed: 4.4,
            horizontal_wave: 5.6,
            vertical_wave: 7.2,
            jitter_scale: 0.34,
            hole_stride: 7,
            hole_offset: 2,
            outline_hole_stride: 4,
            outline_hole_offset: 1,
        },
        AnimationVariant::Slide => VariantProfile {
            char_table: ['·', '░', '▒', '▓', '█'],
            outline_table: ['░', '▒', '▓', '█'],
            hue_shift: 0.36,
            hue_time_speed: 0.25,
            time_speed: 3.6,
            horizontal_wave: 10.2,
            vertical_wave: 4.6,
            jitter_scale: 0.32,
            hole_stride: 5,
            hole_offset: 1,
            outline_hole_stride: 8,
            outline_hole_offset: 3,
        },
        AnimationVariant::Fade => VariantProfile {
            char_table: ['░', '▒', '▒', '▓', '█'],
            outline_table: ['░', '▒', '▓', '█'],
            hue_shift: 0.35,
            hue_time_speed: 0.26,
            time_speed: 4.6,
            horizontal_wave: 7.2,
            vertical_wave: 7.8,
            jitter_scale: 0.36,
            hole_stride: 10,
            hole_offset: 4,
            outline_hole_stride: 3,
            outline_hole_offset: 1,
        },
    }
}

fn vivid_style(hue: f32, intensity: f32, ctx: &AnimationFrameCtx) -> Style {
    use ratatui::style::Color;
    let base = glitch_animation::gradient_multi(hue);
    let bright = glitch_animation::mix_rgb(base, Color::Rgb(255, 255, 255), 0.25 + intensity * 0.4);
    let fg = glitch_animation::blend_to_background(bright, ctx.fade.max(0.05));
    Style::default()
        .fg(fg)
        .bg(crate::colors::background())
        .add_modifier(Modifier::BOLD)
}

fn density_char(intensity: f32, table: &[char]) -> char {
    let clamped = intensity.clamp(0.0, 0.9999);
    let len = table.len().max(1);
    let idx = (clamped * len as f32).floor() as usize;
    table[idx.min(len - 1)]
}

fn clamp01(v: f32) -> f32 {
    v.clamp(0.0, 1.0)
}

fn draw_box_outline(buf: &mut Buffer, rect: Rect) {
    let style = Style::default()
        .fg(crate::colors::border())
        .bg(crate::colors::background())
        .add_modifier(Modifier::BOLD);
    draw_box_outline_with_style(buf, rect, style);
}

fn draw_box_outline_with_style(buf: &mut Buffer, rect: Rect, style: Style) {
    if rect.width < 2 || rect.height < 2 {
        return;
    }

    let top_y = rect.y;
    let bottom_y = rect.y + rect.height - 1;
    for x in rect.x..rect.x + rect.width {
        let ch_top = if x == rect.x {
            '┌'
        } else if x + 1 == rect.x + rect.width {
            '┐'
        } else {
            '─'
        };
        let ch_bottom = if x == rect.x {
            '└'
        } else if x + 1 == rect.x + rect.width {
            '┘'
        } else {
            '─'
        };

        paint_outline_cell(buf, x, top_y, ch_top, style);
        paint_outline_cell(buf, x, bottom_y, ch_bottom, style);
    }

    for y in (rect.y + 1)..(rect.y + rect.height - 1) {
        paint_outline_cell(buf, rect.x, y, '│', style);
        paint_outline_cell(buf, rect.x + rect.width - 1, y, '│', style);
    }
}

fn paint_outline_cell(buf: &mut Buffer, x: u16, y: u16, ch: char, style: Style) {
    let mut tmp = [0u8; 4];
    let cell = &mut buf[(x, y)];
    cell.set_symbol(ch.encode_utf8(&mut tmp));
    cell.set_style(style);
}

fn ease_in_out_cubic(t: f32) -> f32 {
    let v = t.clamp(0.0, 1.0);
    if v < 0.5 {
        4.0 * v * v * v
    } else {
        1.0 - (-2.0 * v + 2.0).powf(3.0) / 2.0
    }
}
