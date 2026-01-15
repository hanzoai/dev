mod auto_compact;
mod auto_coordinator;
mod auto_drive_history;
mod controller;
mod coordinator_router;
mod coordinator_user_schema;
mod retry;
mod session_metrics;

#[cfg(feature = "dev-faults")]
mod faults;

pub use auto_coordinator::AutoCoordinatorCommand;
pub use auto_coordinator::AutoCoordinatorEvent;
pub use auto_coordinator::AutoCoordinatorEventSender;
pub use auto_coordinator::AutoCoordinatorHandle;
pub use auto_coordinator::AutoCoordinatorStatus;
pub use auto_coordinator::AutoTurnAgentsAction;
pub use auto_coordinator::AutoTurnAgentsTiming;
pub use auto_coordinator::AutoTurnCliAction;
pub use auto_coordinator::MODEL_SLUG;
pub use auto_coordinator::TurnComplexity;
pub use auto_coordinator::TurnConfig;
pub use auto_coordinator::TurnDescriptor;
pub use auto_coordinator::TurnMode;
pub use auto_coordinator::start_auto_coordinator;

pub use controller::AUTO_RESOLVE_MAX_REVIEW_ATTEMPTS;
pub use controller::AUTO_RESOLVE_REVIEW_FOLLOWUP;
pub use controller::AUTO_RESTART_BASE_DELAY;
pub use controller::AUTO_RESTART_MAX_DELAY;
pub use controller::AutoContinueMode;
pub use controller::AutoControllerEffect;
pub use controller::AutoDriveController;
pub use controller::AutoResolvePhase;
pub use controller::AutoResolveState;
pub use controller::AutoRestartState;
pub use controller::AutoRunPhase;
pub use controller::AutoRunSummary;
pub use controller::AutoTurnReviewState;
pub use controller::PhaseTransition;
pub use controller::TransitionEffects;

pub use auto_drive_history::AutoDriveHistory;
pub use coordinator_router::CoordinatorContext;
pub use coordinator_router::CoordinatorRouterResponse;
pub use coordinator_router::route_user_message;
pub use coordinator_user_schema::parse_user_turn_reply;
pub use coordinator_user_schema::user_turn_schema;
pub use session_metrics::SessionMetrics;
