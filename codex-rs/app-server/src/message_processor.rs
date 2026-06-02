use std::collections::HashSet;
use std::future::Future;
use std::sync::Arc;
use std::sync::OnceLock;
use std::sync::atomic::AtomicBool;

use crate::attestation::app_server_attestation_provider;
use crate::config_manager::ConfigManager;
use crate::connection_rpc_gate::ConnectionRpcGate;
use crate::error_code::invalid_request;
use crate::extensions::app_server_extension_event_sink;
use crate::extensions::guardian_agent_spawner;
use crate::extensions::thread_extensions;
use crate::fs_watch::FsWatchManager;
use crate::outgoing_message::ConnectionId;
use crate::outgoing_message::ConnectionRequestId;
use crate::outgoing_message::OutgoingMessageSender;
use crate::outgoing_message::RequestContext;
use crate::request_processors::AccountRequestProcessor;
use crate::request_processors::AppsRequestProcessor;
use crate::request_processors::CatalogRequestProcessor;
use crate::request_processors::CommandExecRequestProcessor;
use crate::request_processors::ConfigRequestProcessor;
use crate::request_processors::EnvironmentRequestProcessor;
use crate::request_processors::ExternalAgentConfigRequestProcessor;
use crate::request_processors::FeedbackRequestProcessor;
use crate::request_processors::FsRequestProcessor;
use crate::request_processors::GitRequestProcessor;
use crate::request_processors::InitializeRequestProcessor;
use crate::request_processors::MarketplaceRequestProcessor;
use crate::request_processors::McpRequestProcessor;
use crate::request_processors::PluginRequestProcessor;
use crate::request_processors::ProcessExecRequestProcessor;
use crate::request_processors::RemoteControlRequestProcessor;
use crate::request_processors::SearchRequestProcessor;
use crate::request_processors::ThreadGoalRequestProcessor;
use crate::request_processors::ThreadRequestProcessor;
use crate::request_processors::TurnRequestProcessor;
use crate::request_processors::WindowsSandboxRequestProcessor;
use crate::request_serialization::QueuedInitializedRequest;
use crate::request_serialization::RequestSerializationQueueKey;
use crate::request_serialization::RequestSerializationQueues;
use crate::skills_watcher::SkillsWatcher;
use crate::thread_state::ConnectionCapabilities;
use crate::thread_state::ThreadStateManager;
use crate::transport::AppServerTransport;
use crate::transport::RemoteControlHandle;
use async_trait::async_trait;
use codex_analytics::AnalyticsEventsClient;
use codex_analytics::AppServerRpcTransport;
use codex_app_server_protocol::AuthMode as LoginAuthMode;
use codex_app_server_protocol::ChatgptAuthTokensRefreshParams;
use codex_app_server_protocol::ChatgptAuthTokensRefreshReason;
use codex_app_server_protocol::ChatgptAuthTokensRefreshResponse;
use codex_app_server_protocol::ClientNotification;
use codex_app_server_protocol::ClientRequest;
use codex_app_server_protocol::ClientResponsePayload;
use codex_app_server_protocol::ConfigWarningNotification;
use codex_app_server_protocol::ExperimentalApi;
use codex_app_server_protocol::JSONRPCError;
use codex_app_server_protocol::JSONRPCErrorError;
use codex_app_server_protocol::JSONRPCNotification;
use codex_app_server_protocol::JSONRPCRequest;
use codex_app_server_protocol::JSONRPCResponse;
use codex_app_server_protocol::ServerRequestPayload;
use codex_app_server_protocol::experimental_required_message;
use codex_arg0::Arg0DispatchPaths;
use codex_chatgpt::connectors;
use codex_core::AnalyticsEventsClient;
use codex_core::AppServerRpcTransport;
use codex_core::AuthManager;
use codex_core::ThreadManager;
use codex_core::config::Config;
use codex_exec_server::EnvironmentManager;
use codex_feedback::CodexFeedback;
use codex_login::AuthMode as LoginAuthMode;
use codex_login::auth::ExternalAuth;
use codex_login::auth::ExternalAuthRefreshContext;
use codex_login::auth::ExternalAuthRefreshReason;
use codex_login::auth::ExternalAuthTokens;
use codex_protocol::ThreadId;
use codex_protocol::protocol::SessionSource;
use codex_protocol::protocol::W3cTraceContext;
use codex_rollout::StateDbHandle;
use codex_state::log_db::LogDbLayer;
use tokio::sync::Mutex;
use tokio::sync::Semaphore;
use tokio::sync::broadcast;
use tokio::sync::watch;
use tokio::time::Duration;
use tokio::time::timeout;
use tokio_util::sync::CancellationToken;
use tracing::Instrument;

const EXTERNAL_AUTH_REFRESH_TIMEOUT: Duration = Duration::from_secs(10);

#[derive(Clone)]
struct ExternalAuthRefreshBridge {
    outgoing: Arc<OutgoingMessageSender>,
}

impl ExternalAuthRefreshBridge {
    fn map_reason(reason: ExternalAuthRefreshReason) -> ChatgptAuthTokensRefreshReason {
        match reason {
            ExternalAuthRefreshReason::Unauthorized => ChatgptAuthTokensRefreshReason::Unauthorized,
        }
    }
}

#[async_trait]
impl ExternalAuth for ExternalAuthRefreshBridge {
    fn auth_mode(&self) -> LoginAuthMode {
        LoginAuthMode::Chatgpt
    }

    async fn refresh(
        &self,
        context: ExternalAuthRefreshContext,
    ) -> std::io::Result<ExternalAuthTokens> {
        let params = ChatgptAuthTokensRefreshParams {
            reason: Self::map_reason(context.reason),
            previous_account_id: context.previous_account_id,
        };

        let (request_id, rx) = self
            .outgoing
            .send_request(ServerRequestPayload::ChatgptAuthTokensRefresh(params))
            .await;

        let result = match timeout(EXTERNAL_AUTH_REFRESH_TIMEOUT, rx).await {
            Ok(result) => {
                // Two failure scenarios:
                // 1) `oneshot::Receiver` failed (sender dropped) => request canceled/channel closed.
                // 2) client answered with JSON-RPC error payload => propagate code/message.
                let result = result.map_err(|err| {
                    std::io::Error::other(format!("auth refresh request canceled: {err}"))
                })?;
                result.map_err(|err| {
                    std::io::Error::other(format!(
                        "auth refresh request failed: code={} message={}",
                        err.code, err.message
                    ))
                })?
            }
            Err(_) => {
                let _canceled = self.outgoing.cancel_request(&request_id).await;
                return Err(std::io::Error::other(format!(
                    "auth refresh request timed out after {}s",
                    EXTERNAL_AUTH_REFRESH_TIMEOUT.as_secs()
                )));
            }
        };

        let response: ChatgptAuthTokensRefreshResponse =
            serde_json::from_value(result).map_err(std::io::Error::other)?;

        Ok(ExternalAuthTokens::chatgpt(
            response.access_token,
            response.chatgpt_account_id,
            response.chatgpt_plan_type,
        ))
    }
}

pub(crate) struct MessageProcessor {
    outgoing: Arc<OutgoingMessageSender>,
    codex_message_processor: CodexMessageProcessor,
    config_api: ConfigApi,
    external_agent_config_api: ExternalAgentConfigApi,
    fs_api: FsApi,
    auth_manager: Arc<AuthManager>,
    analytics_events_client: AnalyticsEventsClient,
    fs_watch_manager: FsWatchManager,
    config: Arc<Config>,
    config_warnings: Arc<Vec<ConfigWarningNotification>>,
    rpc_transport: AppServerRpcTransport,
}

#[derive(Debug)]
pub(crate) struct ConnectionSessionState {
    pub(crate) rpc_gate: Arc<ConnectionRpcGate>,
    initialized: OnceLock<InitializedConnectionSessionState>,
}

#[derive(Debug)]
pub(crate) struct InitializedConnectionSessionState {
    pub(crate) experimental_api_enabled: bool,
    pub(crate) opted_out_notification_methods: HashSet<String>,
    pub(crate) app_server_client_name: String,
    pub(crate) client_version: String,
    pub(crate) request_attestation: bool,
}

impl Default for ConnectionSessionState {
    fn default() -> Self {
        Self::new()
    }
}

impl ConnectionSessionState {
    pub(crate) fn new() -> Self {
        Self {
            rpc_gate: Arc::new(ConnectionRpcGate::new()),
            initialized: OnceLock::new(),
        }
    }

    pub(crate) fn initialized(&self) -> bool {
        self.initialized.get().is_some()
    }

    pub(crate) fn experimental_api_enabled(&self) -> bool {
        self.initialized
            .get()
            .is_some_and(|session| session.experimental_api_enabled)
    }

    pub(crate) fn opted_out_notification_methods(&self) -> HashSet<String> {
        self.initialized
            .get()
            .map(|session| session.opted_out_notification_methods.clone())
            .unwrap_or_default()
    }

    pub(crate) fn app_server_client_name(&self) -> Option<&str> {
        self.initialized
            .get()
            .map(|session| session.app_server_client_name.as_str())
    }

    pub(crate) fn client_version(&self) -> Option<&str> {
        self.initialized
            .get()
            .map(|session| session.client_version.as_str())
    }

    pub(crate) fn request_attestation(&self) -> bool {
        self.initialized
            .get()
            .is_some_and(|session| session.request_attestation)
    }

    pub(crate) fn initialize(&self, session: InitializedConnectionSessionState) -> Result<(), ()> {
        self.initialized.set(session).map_err(|_| ())
    }
}

pub(crate) struct MessageProcessorArgs {
    pub(crate) outgoing: Arc<OutgoingMessageSender>,
    pub(crate) analytics_events_client: AnalyticsEventsClient,
    pub(crate) arg0_paths: Arg0DispatchPaths,
    pub(crate) config: Arc<Config>,
    pub(crate) config_manager: ConfigManager,
    pub(crate) environment_manager: Arc<EnvironmentManager>,
    pub(crate) feedback: CodexFeedback,
    pub(crate) log_db: Option<LogDbLayer>,
    pub(crate) state_db: Option<StateDbHandle>,
    pub(crate) config_warnings: Vec<ConfigWarningNotification>,
    pub(crate) session_source: SessionSource,
    pub(crate) enable_codex_api_key_env: bool,
    pub(crate) rpc_transport: AppServerRpcTransport,
}

impl MessageProcessor {
    /// Create a new `MessageProcessor`, retaining a handle to the outgoing
    /// `Sender` so handlers can enqueue messages to be written to stdout.
    pub(crate) fn new(args: MessageProcessorArgs) -> Self {
        let MessageProcessorArgs {
            outgoing,
            analytics_events_client,
            arg0_paths,
            config,
            config_manager,
            environment_manager,
            feedback,
            log_db,
            state_db,
            config_warnings,
            session_source,
            enable_codex_api_key_env,
            rpc_transport,
        } = args;
        let auth_manager = AuthManager::shared_with_external_auth(
            config.codex_home.clone(),
            enable_codex_api_key_env,
            config.cli_auth_credentials_store_mode,
            Arc::new(ExternalAuthRefreshBridge {
                outgoing: outgoing.clone(),
            }),
        );
        let thread_manager = Arc::new(ThreadManager::new(
            config.as_ref(),
            auth_manager.clone(),
            session_source,
            CollaborationModesConfig {
                default_mode_request_user_input: config
                    .features
                    .enabled(Feature::DefaultModeRequestUserInput),
            },
            environment_manager,
        ));
        auth_manager.set_forced_chatgpt_workspace_id(config.forced_chatgpt_workspace_id.clone());
        let analytics_events_client = AnalyticsEventsClient::new(
            Arc::clone(&auth_manager),
            config.chatgpt_base_url.trim_end_matches('/').to_string(),
            config.analytics_enabled,
        );
        thread_manager
            .plugins_manager()
            .set_analytics_events_client(analytics_events_client.clone());
        let skills_watcher = SkillsWatcher::new(thread_manager.skills_manager(), outgoing.clone());

        let cli_overrides = Arc::new(RwLock::new(cli_overrides));
        let runtime_feature_enablement = Arc::new(RwLock::new(BTreeMap::new()));
        let cloud_requirements = Arc::new(RwLock::new(cloud_requirements));
        let codex_message_processor = CodexMessageProcessor::new(CodexMessageProcessorArgs {
            auth_manager: auth_manager.clone(),
            thread_manager: Arc::clone(&thread_manager),
            outgoing: outgoing.clone(),
            analytics_events_client: analytics_events_client.clone(),
            arg0_paths,
            config: Arc::clone(&config),
            cli_overrides: cli_overrides.clone(),
            runtime_feature_enablement: runtime_feature_enablement.clone(),
            cloud_requirements: cloud_requirements.clone(),
            feedback,
            log_db,
        });
        // Keep plugin startup warmups aligned at app-server startup.
        // TODO(xl): Move into PluginManager once this no longer depends on config feature gating.
        thread_manager
            .plugins_manager()
            .maybe_start_plugin_startup_tasks_for_config(&config, auth_manager.clone());
        let config_api = ConfigApi::new(
            config.codex_home.clone(),
            cli_overrides,
            runtime_feature_enablement,
            loader_overrides,
            cloud_requirements,
            thread_manager,
            analytics_events_client.clone(),
        );
        let external_agent_config_processor = ExternalAgentConfigRequestProcessor::new(
            outgoing.clone(),
            Arc::clone(&thread_manager),
            config_manager.clone(),
            config_processor.clone(),
            arg0_paths,
            config.codex_home.to_path_buf(),
        );
        let environment_processor =
            EnvironmentRequestProcessor::new(thread_manager.environment_manager());
        let fs_processor = FsRequestProcessor::new(
            Arc::clone(&environment_manager_for_requests),
            FsWatchManager::new(outgoing.clone()),
        );
        let windows_sandbox_processor = WindowsSandboxRequestProcessor::new(
            outgoing.clone(),
            Arc::clone(&config),
            config_manager,
        );

        Self {
            outgoing,
            codex_message_processor,
            config_api,
            external_agent_config_api,
            fs_api,
            auth_manager,
            analytics_events_client,
            fs_watch_manager,
            config,
            config_warnings: Arc::new(config_warnings),
            rpc_transport,
        }
    }

    pub(crate) fn clear_runtime_references(&self) {
        self.auth_manager.clear_external_auth();
    }

    pub(crate) async fn process_request(
        self: &Arc<Self>,
        connection_id: ConnectionId,
        request: JSONRPCRequest,
        transport: &AppServerTransport,
        session: Arc<ConnectionSessionState>,
    ) {
        let request_method = request.method.as_str();
        tracing::trace!(
            ?connection_id,
            request_id = ?request.id,
            "app-server request: {request_method}"
        );
        let request_id = ConnectionRequestId {
            connection_id,
            request_id: request.id.clone(),
        };
        let request_span =
            crate::app_server_tracing::request_span(&request, transport, connection_id, &session);
        let request_trace = request.trace.as_ref().map(|trace| W3cTraceContext {
            traceparent: trace.traceparent.clone(),
            tracestate: trace.tracestate.clone(),
        });
        let request_context = RequestContext::new(request_id.clone(), request_span, request_trace);
        Self::run_request_with_context(
            Arc::clone(&self.outgoing),
            request_context.clone(),
            async {
                let codex_request = serde_json::to_value(&request)
                    .map_err(|err| invalid_request(format!("Invalid request: {err}")))
                    .and_then(|request_json| {
                        serde_json::from_value::<ClientRequest>(request_json)
                            .map_err(|err| invalid_request(format!("Invalid request: {err}")))
                    });
                let result = match codex_request {
                    Ok(codex_request) => {
                        // Websocket callers finalize outbound readiness in lib.rs after mirroring
                        // session state into outbound state and sending initialize notifications to
                        // this specific connection. Passing `None` avoids marking the connection
                        // ready too early from inside the shared request handler.
                        self.handle_client_request(
                            request_id.clone(),
                            codex_request,
                            Arc::clone(&session),
                            /*outbound_initialized*/ None,
                            request_context.clone(),
                        )
                        .await
                    }
                    Err(error) => Err(error),
                };
                if let Err(error) = result {
                    self.outgoing.send_error(request_id.clone(), error).await;
                }
            },
        )
        .await;
    }

    /// Handles a typed request path used by in-process embedders.
    ///
    /// This bypasses JSON request deserialization but keeps identical request
    /// semantics by delegating to `handle_client_request`.
    pub(crate) async fn process_client_request(
        self: &Arc<Self>,
        connection_id: ConnectionId,
        request: ClientRequest,
        session: Arc<ConnectionSessionState>,
        outbound_initialized: &AtomicBool,
    ) {
        let request_id = ConnectionRequestId {
            connection_id,
            request_id: request.id().clone(),
        };
        let request_span =
            crate::app_server_tracing::typed_request_span(&request, connection_id, &session);
        let request_context =
            RequestContext::new(request_id.clone(), request_span, /*parent_trace*/ None);
        tracing::trace!(
            ?connection_id,
            request_id = ?request_id.request_id,
            "app-server typed request"
        );
        Self::run_request_with_context(
            Arc::clone(&self.outgoing),
            request_context.clone(),
            async {
                // In-process clients do not have the websocket transport loop that performs
                // post-initialize bookkeeping, so they still finalize outbound readiness in
                // the shared request handler.
                let result = self
                    .handle_client_request(
                        request_id.clone(),
                        request,
                        Arc::clone(&session),
                        Some(outbound_initialized),
                        request_context.clone(),
                    )
                    .await;
                if let Err(error) = result {
                    self.outgoing.send_error(request_id.clone(), error).await;
                }
            },
        )
        .await;
    }

    pub(crate) async fn process_notification(&self, notification: JSONRPCNotification) {
        // Currently, we do not expect to receive any notifications from the
        // client, so we just log them.
        tracing::info!("<- notification: {:?}", notification);
    }

    /// Handles typed notifications from in-process clients.
    pub(crate) async fn process_client_notification(&self, notification: ClientNotification) {
        // Currently, we do not expect to receive any typed notifications from
        // in-process clients, so we just log them.
        tracing::info!("<- typed notification: {:?}", notification);
    }

    async fn run_request_with_context<F>(
        outgoing: Arc<OutgoingMessageSender>,
        request_context: RequestContext,
        request_fut: F,
    ) where
        F: Future<Output = ()>,
    {
        outgoing
            .register_request_context(request_context.clone())
            .await;
        request_fut.instrument(request_context.span()).await;
    }

    pub(crate) fn thread_created_receiver(&self) -> broadcast::Receiver<ThreadId> {
        self.thread_processor.thread_created_receiver()
    }

    pub(crate) async fn send_initialize_notifications_to_connection(
        &self,
        connection_id: ConnectionId,
    ) {
        self.initialize_processor
            .send_initialize_notifications_to_connection(connection_id)
            .await;
    }

    pub(crate) async fn connection_initialized(
        &self,
        connection_id: ConnectionId,
        request_attestation: bool,
    ) {
        self.thread_processor
            .connection_initialized(
                connection_id,
                ConnectionCapabilities {
                    request_attestation,
                },
            )
            .await;
    }

    pub(crate) async fn send_initialize_notifications(&self) {
        self.initialize_processor
            .send_initialize_notifications()
            .await;
    }

    pub(crate) async fn try_attach_thread_listener(
        &self,
        thread_id: ThreadId,
        connection_ids: Vec<ConnectionId>,
    ) {
        self.thread_processor
            .try_attach_thread_listener(thread_id, connection_ids)
            .await;
    }

    pub(crate) async fn drain_background_tasks(&self) {
        self.thread_processor.drain_background_tasks().await;
    }

    pub(crate) async fn cancel_active_login(&self) {
        self.account_processor.cancel_active_login().await;
    }

    pub(crate) async fn clear_all_thread_listeners(&self) {
        self.thread_processor.clear_all_thread_listeners().await;
    }

    pub(crate) async fn shutdown_threads(&self) {
        self.thread_processor.shutdown_threads().await;
    }

    pub(crate) async fn connection_closed(
        &self,
        connection_id: ConnectionId,
        session_state: &ConnectionSessionState,
    ) {
        session_state.rpc_gate.shutdown().await;
        self.outgoing.connection_closed(connection_id).await;
        self.fs_processor.connection_closed(connection_id).await;
        self.command_exec_processor
            .connection_closed(connection_id)
            .await;
        self.process_exec_processor
            .connection_closed(connection_id)
            .await;
        self.thread_processor.connection_closed(connection_id).await;
    }

    pub(crate) fn subscribe_running_assistant_turn_count(&self) -> watch::Receiver<usize> {
        self.thread_processor
            .subscribe_running_assistant_turn_count()
    }

    /// Handle a standalone JSON-RPC response originating from the peer.
    pub(crate) async fn process_response(&self, response: JSONRPCResponse) {
        tracing::info!("<- response: {:?}", response);
        let JSONRPCResponse { id, result, .. } = response;
        self.outgoing.notify_client_response(id, result).await
    }

    /// Handle an error object received from the peer.
    pub(crate) async fn process_error(&self, err: JSONRPCError) {
        tracing::error!("<- error: {:?}", err);
        self.outgoing.notify_client_error(err.id, err.error).await;
    }

    async fn handle_client_request(
        self: &Arc<Self>,
        connection_request_id: ConnectionRequestId,
        codex_request: ClientRequest,
        session: Arc<ConnectionSessionState>,
        // `Some(...)` means the caller wants initialize to immediately mark the
        // connection outbound-ready. Websocket JSON-RPC calls pass `None` so
        // lib.rs can deliver connection-scoped initialize notifications first.
        outbound_initialized: Option<&AtomicBool>,
        request_context: RequestContext,
    ) -> Result<(), JSONRPCErrorError> {
        let connection_id = connection_request_id.connection_id;
        if let ClientRequest::Initialize { request_id, params } = codex_request {
            let connection_initialized = self
                .initialize_processor
                .initialize(
                    connection_id,
                    request_id,
                };
                if session.initialized {
                    let error = JSONRPCErrorError {
                        code: INVALID_REQUEST_ERROR_CODE,
                        message: "Already initialized".to_string(),
                        data: None,
                    };
                    self.outgoing.send_error(connection_request_id, error).await;
                    return;
                }

                // TODO(maxj): Revisit capability scoping for `experimental_api_enabled`.
                // Current behavior is per-connection. Reviewer feedback notes this can
                // create odd cross-client behavior (for example dynamic tool calls on a
                // shared thread when another connected client did not opt into
                // experimental API). Proposed direction is instance-global first-write-wins
                // with initialize-time mismatch rejection.
                let analytics_initialize_params = params.clone();
                let (experimental_api_enabled, opt_out_notification_methods) =
                    match params.capabilities {
                        Some(capabilities) => (
                            capabilities.experimental_api,
                            capabilities
                                .opt_out_notification_methods
                                .unwrap_or_default(),
                        ),
                        None => (false, Vec::new()),
                    };
                session.experimental_api_enabled = experimental_api_enabled;
                session.opted_out_notification_methods =
                    opt_out_notification_methods.into_iter().collect();
                let ClientInfo {
                    name,
                    title: _title,
                    version,
                } = params.client_info;
                session.app_server_client_name = Some(name.clone());
                session.client_version = Some(version.clone());
                let originator = name.clone();
                if let Err(error) = set_default_originator(originator.clone()) {
                    match error {
                        SetOriginatorError::InvalidHeaderValue => {
                            let error = JSONRPCErrorError {
                                code: INVALID_REQUEST_ERROR_CODE,
                                message: format!(
                                    "Invalid clientInfo.name: '{name}'. Must be a valid HTTP header value."
                                ),
                                data: None,
                            };
                            self.outgoing
                                .send_error(connection_request_id.clone(), error)
                                .await;
                            return;
                        }
                        SetOriginatorError::AlreadyInitialized => {
                            // No-op. This is expected to happen if the originator is already set via env var.
                            // TODO(owen): Once we remove support for CODEX_INTERNAL_ORIGINATOR_OVERRIDE,
                            // this will be an unexpected state and we can return a JSON-RPC error indicating
                            // internal server error.
                        }
                    }
                }
                if self.config.features.enabled(Feature::GeneralAnalytics) {
                    self.analytics_events_client.track_initialize(
                        connection_id.0,
                        analytics_initialize_params,
                        originator,
                        self.rpc_transport,
                    );
                }
                set_default_client_residency_requirement(self.config.enforce_residency.value());
                let user_agent_suffix = format!("{name}; {version}");
                if let Ok(mut suffix) = USER_AGENT_SUFFIX.lock() {
                    *suffix = Some(user_agent_suffix);
                }

                let user_agent = get_codex_user_agent();
                let codex_home = match self.config.codex_home.clone().try_into() {
                    Ok(codex_home) => codex_home,
                    Err(err) => {
                        let error = JSONRPCErrorError {
                            code: INTERNAL_ERROR_CODE,
                            message: format!("Invalid CODEX_HOME: {err}"),
                            data: None,
                        };
                        self.outgoing.send_error(connection_request_id, error).await;
                        return;
                    }
                };
                let response = InitializeResponse {
                    user_agent,
                    codex_home,
                    platform_family: std::env::consts::FAMILY.to_string(),
                    platform_os: std::env::consts::OS.to_string(),
                };
                self.outgoing
                    .send_response(connection_request_id, response)
                    .await;
            }
            return Ok(());
        }

        self.dispatch_initialized_client_request(
            connection_request_id,
            codex_request,
            session,
            request_context,
        )
        .await
    }

    async fn dispatch_initialized_client_request(
        self: &Arc<Self>,
        connection_request_id: ConnectionRequestId,
        codex_request: ClientRequest,
        session: Arc<ConnectionSessionState>,
        request_context: RequestContext,
    ) -> Result<(), JSONRPCErrorError> {
        if !session.initialized() {
            return Err(invalid_request("Not initialized"));
        }

        if let Some(reason) = codex_request.experimental_reason()
            && !session.experimental_api_enabled()
        {
            return Err(invalid_request(experimental_required_message(reason)));
        }
        let connection_id = connection_request_id.connection_id;
        self.initialize_processor.track_initialized_request(
            connection_id,
            connection_request_id.request_id.clone(),
            &codex_request,
        );

        let serialization_scope = codex_request.serialization_scope();
        let app_server_client_name = session.app_server_client_name().map(str::to_string);
        let client_version = session.client_version().map(str::to_string);
        let error_request_id = connection_request_id.clone();
        let rpc_gate = Arc::clone(&session.rpc_gate);
        let processor = Arc::clone(self);
        let span = request_context.span();
        let request = QueuedInitializedRequest::new(
            rpc_gate,
            async move {
                let processor_for_request = Arc::clone(&processor);
                let result = processor_for_request
                    .handle_initialized_client_request(
                        connection_request_id,
                        codex_request,
                        request_context,
                        app_server_client_name,
                        client_version,
                    )
                    .await;
                if let Err(error) = result {
                    processor.outgoing.send_error(error_request_id, error).await;
                }
            }
            .instrument(span),
        );

        if let Some(scope) = serialization_scope {
            let (key, access) = RequestSerializationQueueKey::from_scope(connection_id, scope);
            self.request_serialization_queues
                .enqueue(key, access, request)
                .await;
        } else {
            tokio::spawn(async move {
                request.run().await;
            });
        }
        Ok(())
    }

    async fn handle_initialized_client_request(
        self: Arc<Self>,
        connection_request_id: ConnectionRequestId,
        codex_request: ClientRequest,
        request_context: RequestContext,
        app_server_client_name: Option<String>,
        client_version: Option<String>,
    ) -> Result<(), JSONRPCErrorError> {
        let connection_id = connection_request_id.connection_id;
        let request_id = ConnectionRequestId {
            connection_id,
            request_id: codex_request.id().clone(),
        };

        let result: Result<Option<ClientResponsePayload>, JSONRPCErrorError> = match codex_request {
            ClientRequest::Initialize { .. } => {
                panic!("Initialize should be handled before initialized request dispatch");
            }
            ClientRequest::ConfigRead { params, .. } => self
                .config_processor
                .read(params)
                .await
                .map(|response| Some(response.into())),
            ClientRequest::WindowsSandboxReadiness { .. } => self
                .windows_sandbox_processor
                .windows_sandbox_readiness()
                .await
                .map(|response| Some(response.into())),
            ClientRequest::ExternalAgentConfigDetect { params, .. } => self
                .external_agent_config_processor
                .detect(params)
                .await
                .map(|response| Some(response.into())),
            ClientRequest::ExternalAgentConfigImport { params, .. } => self
                .external_agent_config_processor
                .import(request_id.clone(), params)
                .await
                .map(|()| None),
            ClientRequest::ConfigValueWrite { params, .. } => {
                self.config_processor.value_write(params).await.map(Some)
            }
            ClientRequest::ConfigBatchWrite { params, .. } => {
                self.config_processor.batch_write(params).await.map(Some)
            }
            ClientRequest::ExperimentalFeatureEnablementSet { params, .. } => {
                self.config_processor
                    .experimental_feature_enablement_set(request_id.clone(), params)
                    .await
            }
            ClientRequest::RemoteControlEnable { .. } => self
                .remote_control_processor
                .enable()
                .map(|response| Some(response.into())),
            ClientRequest::RemoteControlDisable { .. } => self
                .remote_control_processor
                .disable()
                .map(|response| Some(response.into())),
            ClientRequest::RemoteControlStatusRead { .. } => self
                .remote_control_processor
                .status_read()
                .map(|response| Some(response.into())),
            ClientRequest::RemoteControlPairingStart { params, .. } => self
                .remote_control_processor
                .pairing_start(params)
                .await
                .map(|response| Some(response.into())),
            ClientRequest::ConfigRequirementsRead { params: _, .. } => self
                .config_processor
                .config_requirements_read()
                .await
                .map(|response| Some(response.into())),
            ClientRequest::EnvironmentAdd { params, .. } => {
                self.environment_processor.environment_add(params).await
            }
            ClientRequest::FsReadFile { params, .. } => self
                .fs_processor
                .read_file(params)
                .await
                .map(|response| Some(response.into())),
            ClientRequest::FsWriteFile { params, .. } => self
                .fs_processor
                .write_file(params)
                .await
                .map(|response| Some(response.into())),
            ClientRequest::FsCreateDirectory { params, .. } => self
                .fs_processor
                .create_directory(params)
                .await
                .map(|response| Some(response.into())),
            ClientRequest::FsGetMetadata { params, .. } => self
                .fs_processor
                .get_metadata(params)
                .await
                .map(|response| Some(response.into())),
            ClientRequest::FsReadDirectory { params, .. } => self
                .fs_processor
                .read_directory(params)
                .await
                .map(|response| Some(response.into())),
            ClientRequest::FsRemove { params, .. } => self
                .fs_processor
                .remove(params)
                .await
                .map(|response| Some(response.into())),
            ClientRequest::FsCopy { params, .. } => self
                .fs_processor
                .copy(params)
                .await
                .map(|response| Some(response.into())),
            ClientRequest::FsWatch { params, .. } => self
                .fs_processor
                .watch(connection_id, params)
                .await
                .map(|response| Some(response.into())),
            ClientRequest::FsUnwatch { params, .. } => self
                .fs_processor
                .unwatch(connection_id, params)
                .await
                .map(|response| Some(response.into())),
            ClientRequest::ModelProviderCapabilitiesRead { params: _, .. } => self
                .config_processor
                .model_provider_capabilities_read()
                .await
                .map(|response| Some(response.into())),
            ClientRequest::ThreadStart { params, .. } => {
                self.thread_processor
                    .thread_start(
                        request_id.clone(),
                        params,
                        app_server_client_name.clone(),
                        client_version.clone(),
                        request_context,
                    )
                    .await
            }
            ClientRequest::ThreadUnsubscribe { params, .. } => {
                self.thread_processor
                    .thread_unsubscribe(&request_id, params)
                    .await
            }
            ClientRequest::ThreadResume { params, .. } => {
                self.thread_processor
                    .thread_resume(
                        request_id.clone(),
                        params,
                        app_server_client_name.clone(),
                        client_version.clone(),
                    )
                    .await
            }
            ClientRequest::ThreadFork { params, .. } => {
                self.thread_processor
                    .thread_fork(
                        request_id.clone(),
                        params,
                        app_server_client_name.clone(),
                        client_version.clone(),
                    )
                    .await
            }
            ClientRequest::ThreadArchive { params, .. } => {
                self.thread_processor
                    .thread_archive(request_id.clone(), params)
                    .await
            }
            ClientRequest::ThreadIncrementElicitation { params, .. } => {
                self.thread_processor
                    .thread_increment_elicitation(params)
                    .await
            }
            ClientRequest::ThreadDecrementElicitation { params, .. } => {
                self.thread_processor
                    .thread_decrement_elicitation(params)
                    .await
            }
            ClientRequest::ThreadSetName { params, .. } => {
                self.thread_processor
                    .thread_set_name(request_id.clone(), params)
                    .await
            }
            ClientRequest::ThreadGoalSet { params, .. } => {
                self.thread_goal_processor
                    .thread_goal_set(request_id.clone(), params)
                    .await
            }
            ClientRequest::ThreadGoalGet { params, .. } => {
                self.thread_goal_processor.thread_goal_get(params).await
            }
            ClientRequest::ThreadGoalClear { params, .. } => {
                self.thread_goal_processor
                    .thread_goal_clear(request_id.clone(), params)
                    .await
            }
            ClientRequest::ThreadMetadataUpdate { params, .. } => {
                self.thread_processor.thread_metadata_update(params).await
            }
            ClientRequest::ThreadSettingsUpdate { params, .. } => {
                self.turn_processor
                    .thread_settings_update(&request_id, params)
                    .await
            }
            ClientRequest::ThreadMemoryModeSet { params, .. } => {
                self.thread_processor.thread_memory_mode_set(params).await
            }
            ClientRequest::MemoryReset { .. } => self.thread_processor.memory_reset().await,
            ClientRequest::ThreadUnarchive { params, .. } => {
                self.thread_processor
                    .thread_unarchive(request_id.clone(), params)
                    .await
            }
            ClientRequest::ThreadCompactStart { params, .. } => {
                self.thread_processor
                    .thread_compact_start(&request_id, params)
                    .await
            }
            ClientRequest::ThreadBackgroundTerminalsClean { params, .. } => {
                self.thread_processor
                    .thread_background_terminals_clean(&request_id, params)
                    .await
            }
            ClientRequest::ThreadRollback { params, .. } => {
                self.thread_processor
                    .thread_rollback(&request_id, params)
                    .await
            }
            ClientRequest::ThreadList { params, .. } => {
                self.thread_processor.thread_list(params).await
            }
            ClientRequest::ThreadSearch { params, .. } => {
                self.thread_processor.thread_search(params).await
            }
            ClientRequest::ThreadLoadedList { params, .. } => {
                self.thread_processor.thread_loaded_list(params).await
            }
            ClientRequest::ThreadRead { params, .. } => {
                self.thread_processor.thread_read(params).await
            }
            ClientRequest::ThreadTurnsList { params, .. } => {
                self.thread_processor.thread_turns_list(params).await
            }
            ClientRequest::ThreadTurnsItemsList { params, .. } => {
                self.thread_processor.thread_turns_items_list(params).await
            }
            ClientRequest::ThreadShellCommand { params, .. } => {
                self.thread_processor
                    .thread_shell_command(&request_id, params)
                    .await
            }
            ClientRequest::ThreadApproveGuardianDeniedAction { params, .. } => {
                self.thread_processor
                    .thread_approve_guardian_denied_action(&request_id, params)
                    .await
            }
            ClientRequest::GetConversationSummary { params, .. } => {
                self.thread_processor.conversation_summary(params).await
            }
            ClientRequest::SkillsList { params, .. } => {
                self.catalog_processor.skills_list(params).await
            }
            ClientRequest::SkillsExtraRootsSet { params, .. } => {
                self.catalog_processor.skills_extra_roots_set(params).await
            }
            ClientRequest::HooksList { params, .. } => {
                self.catalog_processor.hooks_list(params).await
            }
            ClientRequest::MarketplaceAdd { params, .. } => {
                self.marketplace_processor.marketplace_add(params).await
            }
            ClientRequest::MarketplaceRemove { params, .. } => {
                self.marketplace_processor.marketplace_remove(params).await
            }
            ClientRequest::MarketplaceUpgrade { params, .. } => {
                self.marketplace_processor.marketplace_upgrade(params).await
            }
            ClientRequest::PluginList { params, .. } => {
                self.plugin_processor.plugin_list(params).await
            }
            ClientRequest::PluginInstalled { params, .. } => {
                self.plugin_processor.plugin_installed(params).await
            }
            ClientRequest::PluginRead { params, .. } => {
                self.plugin_processor.plugin_read(params).await
            }
            ClientRequest::PluginSkillRead { params, .. } => {
                self.plugin_processor.plugin_skill_read(params).await
            }
            ClientRequest::PluginShareSave { params, .. } => {
                self.plugin_processor.plugin_share_save(params).await
            }
            ClientRequest::PluginShareUpdateTargets { params, .. } => {
                self.plugin_processor
                    .plugin_share_update_targets(params)
                    .await
            }
            ClientRequest::PluginShareList { params, .. } => {
                self.plugin_processor.plugin_share_list(params).await
            }
            ClientRequest::PluginShareCheckout { params, .. } => {
                self.plugin_processor.plugin_share_checkout(params).await
            }
            ClientRequest::PluginShareDelete { params, .. } => {
                self.plugin_processor.plugin_share_delete(params).await
            }
            ClientRequest::AppsList { params, .. } => {
                self.apps_processor.apps_list(&request_id, params).await
            }
            ClientRequest::SkillsConfigWrite { params, .. } => {
                self.catalog_processor.skills_config_write(params).await
            }
            ClientRequest::PluginInstall { params, .. } => {
                self.plugin_processor.plugin_install(params).await
            }
            ClientRequest::PluginUninstall { params, .. } => {
                self.plugin_processor.plugin_uninstall(params).await
            }
            ClientRequest::ModelList { params, .. } => {
                self.catalog_processor.model_list(params).await
            }
            ClientRequest::ExperimentalFeatureList { params, .. } => {
                self.catalog_processor
                    .experimental_feature_list(params)
                    .await
            }
            ClientRequest::PermissionProfileList { params, .. } => {
                self.catalog_processor.permission_profile_list(params).await
            }
            ClientRequest::CollaborationModeList { params, .. } => {
                self.catalog_processor.collaboration_mode_list(params).await
            }
            ClientRequest::MockExperimentalMethod { params, .. } => {
                self.catalog_processor
                    .mock_experimental_method(params)
                    .await
            }
            ClientRequest::TurnStart { params, .. } => {
                self.turn_processor
                    .turn_start(
                        request_id.clone(),
                        params,
                        app_server_client_name.clone(),
                        client_version.clone(),
                    )
                    .await
            }
            ClientRequest::ThreadInjectItems { params, .. } => {
                self.turn_processor.thread_inject_items(params).await
            }
            ClientRequest::TurnSteer { params, .. } => {
                self.turn_processor.turn_steer(&request_id, params).await
            }
            ClientRequest::TurnInterrupt { params, .. } => {
                self.turn_processor
                    .turn_interrupt(&request_id, params)
                    .await
            }
            ClientRequest::ThreadRealtimeStart { params, .. } => {
                self.turn_processor
                    .thread_realtime_start(&request_id, params)
                    .await
            }
            ClientRequest::ThreadRealtimeAppendAudio { params, .. } => {
                self.turn_processor
                    .thread_realtime_append_audio(&request_id, params)
                    .await
            }
            ClientRequest::ThreadRealtimeAppendText { params, .. } => {
                self.turn_processor
                    .thread_realtime_append_text(&request_id, params)
                    .await
            }
            ClientRequest::ThreadRealtimeStop { params, .. } => {
                self.turn_processor
                    .thread_realtime_stop(&request_id, params)
                    .await
            }
            ClientRequest::ThreadRealtimeListVoices { params: _, .. } => {
                self.turn_processor.thread_realtime_list_voices().await
            }
            ClientRequest::ReviewStart { params, .. } => {
                self.turn_processor.review_start(&request_id, params).await
            }
            ClientRequest::McpServerOauthLogin { params, .. } => {
                self.mcp_processor.mcp_server_oauth_login(params).await
            }
            ClientRequest::McpServerRefresh { params, .. } => {
                self.mcp_processor.mcp_server_refresh(params).await
            }
            ClientRequest::McpServerStatusList { params, .. } => {
                self.mcp_processor
                    .mcp_server_status_list(&request_id, params)
                    .await
            }
            ClientRequest::McpResourceRead { params, .. } => {
                self.mcp_processor
                    .mcp_resource_read(&request_id, params)
                    .await
            }
            ClientRequest::McpServerToolCall { params, .. } => {
                self.mcp_processor
                    .mcp_server_tool_call(&request_id, params)
                    .await
            }
            ClientRequest::WindowsSandboxSetupStart { params, .. } => {
                self.windows_sandbox_processor
                    .windows_sandbox_setup_start(&request_id, params)
                    .await
            }
            ClientRequest::LoginAccount { params, .. } => {
                self.account_processor
                    .login_account(request_id.clone(), params)
                    .await
            }
            ClientRequest::LogoutAccount { .. } => {
                self.account_processor
                    .logout_account(request_id.clone())
                    .await
            }
            ClientRequest::CancelLoginAccount { params, .. } => {
                self.account_processor.cancel_login_account(params).await
            }
            ClientRequest::GetAccount { params, .. } => {
                self.account_processor.get_account(params).await
            }
            ClientRequest::GetAuthStatus { params, .. } => {
                self.account_processor.get_auth_status(params).await
            }
            ClientRequest::GetAccountRateLimits { .. } => {
                self.account_processor.get_account_rate_limits().await
            }
            ClientRequest::SendAddCreditsNudgeEmail { params, .. } => {
                self.account_processor
                    .send_add_credits_nudge_email(params)
                    .await
            }
            ClientRequest::GitDiffToRemote { params, .. } => {
                self.git_processor.git_diff_to_remote(params).await
            }
            ClientRequest::FuzzyFileSearch { params, .. } => self
                .search_processor
                .fuzzy_file_search(params)
                .await
                .map(|response| Some(response.into())),
            ClientRequest::FuzzyFileSearchSessionStart { params, .. } => self
                .search_processor
                .fuzzy_file_search_session_start_response(params)
                .await
                .map(|response| Some(response.into())),
            ClientRequest::FuzzyFileSearchSessionUpdate { params, .. } => self
                .search_processor
                .fuzzy_file_search_session_update_response(params)
                .await
                .map(|response| Some(response.into())),
            ClientRequest::FuzzyFileSearchSessionStop { params, .. } => self
                .search_processor
                .fuzzy_file_search_session_stop(params)
                .await
                .map(|response| Some(response.into())),
            ClientRequest::OneOffCommandExec { params, .. } => {
                self.command_exec_processor
                    .one_off_command_exec(&request_id, params)
                    .await
            }
            ClientRequest::CommandExecWrite { params, .. } => {
                self.command_exec_processor
                    .command_exec_write(request_id.clone(), params)
                    .await
            }
            ClientRequest::CommandExecResize { params, .. } => {
                self.command_exec_processor
                    .command_exec_resize(request_id.clone(), params)
                    .await
            }
            ClientRequest::CommandExecTerminate { params, .. } => {
                self.command_exec_processor
                    .command_exec_terminate(request_id.clone(), params)
                    .await
            }
            ClientRequest::ProcessSpawn { params, .. } => self
                .process_exec_processor
                .process_spawn(request_id.clone(), params)
                .await
                .map(|()| None),
            ClientRequest::ProcessWriteStdin { params, .. } => {
                self.process_exec_processor
                    .process_write_stdin(request_id.clone(), params)
                    .await
            }
            ClientRequest::ProcessKill { params, .. } => {
                self.process_exec_processor
                    .process_kill(request_id.clone(), params)
                    .await
            }
            ClientRequest::ProcessResizePty { params, .. } => {
                self.process_exec_processor
                    .process_resize_pty(request_id.clone(), params)
                    .await
            }
            ClientRequest::FeedbackUpload { params, .. } => {
                self.feedback_processor.feedback_upload(params).await
            }
        };

        match result {
            Ok(Some(response)) => {
                self.outgoing
                    .send_response_as(request_id.clone(), response)
                    .await;
            }
            Ok(None) => {}
            Err(error) => {
                self.outgoing.send_error(request_id.clone(), error).await;
            }
        }
        Ok(())
    }
}

#[cfg(test)]
#[path = "message_processor_tracing_tests.rs"]
mod message_processor_tracing_tests;
