//! RPC service implementation

use crate::compute::ComputeManager;
use crate::storage::Storage;
use crate::{NodeConfig, NodeState, Result, VERSION};
use std::collections::HashMap;
use std::pin::Pin;
use std::sync::Arc;
use std::time::Instant;
use tokio::sync::RwLock;
use tokio_stream::Stream;
use tonic::{Request, Response, Status};

use super::generated::hanzo::node::v1::{
    node_service_server::{NodeService, NodeServiceServer},
    ContainerInfo as ProtoContainerInfo, DeployRequest as ProtoDeployRequest,
    DeployResponse as ProtoDeployResponse, DeploymentStatus as ProtoDeploymentStatus,
    ExecInput, ExecOutput, GetEventsRequest, GetHealthRequest, GetHealthResponse,
    GetLogsRequest, GetMetricsRequest, GetStatusRequest, GetStatusResponse,
    HealthStatus, HeartbeatRequest, HeartbeatResponse, InspectContainerRequest,
    ListContainersRequest, ListContainersResponse, LogEntry, MetricsResponse,
    NodeCommand, NodeEvent, NodeMetrics as ProtoNodeMetrics, NodeState as ProtoNodeState,
    RegisterNodeRequest, RegisterNodeResponse, RemoveRequest, RemoveResponse,
    ScaleRequest, ScaleResponse as ProtoScaleResponse, StopRequest, StopResponse,
    UpdateCapabilitiesRequest, UpdateCapabilitiesResponse,
};

/// RPC server wrapper
pub struct RpcServer {
    config: NodeConfig,
    state: Arc<RwLock<NodeState>>,
    storage: Arc<Storage>,
    compute: Arc<ComputeManager>,
    start_time: Instant,
}

impl RpcServer {
    /// Create a new RPC server
    pub fn new(
        config: NodeConfig,
        state: Arc<RwLock<NodeState>>,
        storage: Arc<Storage>,
        compute: Arc<ComputeManager>,
        start_time: Instant,
    ) -> Self {
        Self {
            config,
            state,
            storage,
            compute,
            start_time,
        }
    }

    /// Run the gRPC server
    pub async fn run(self) -> Result<()> {
        let addr = self.config.grpc_addr.parse().map_err(|e| {
            crate::Error::Config(format!("Invalid gRPC address: {e}"))
        })?;

        let service = NodeServiceImpl {
            config: self.config.clone(),
            state: self.state,
            storage: self.storage,
            compute: self.compute,
            start_time: self.start_time,
        };

        tracing::info!(addr = %addr, "Starting gRPC server");

        tonic::transport::Server::builder()
            .add_service(NodeServiceServer::new(service))
            .serve(addr)
            .await?;

        Ok(())
    }
}

/// Node service implementation
struct NodeServiceImpl {
    #[allow(dead_code)]
    config: NodeConfig,
    state: Arc<RwLock<NodeState>>,
    storage: Arc<Storage>,
    compute: Arc<ComputeManager>,
    start_time: Instant,
}

type StreamResult<T> = Pin<Box<dyn Stream<Item = std::result::Result<T, Status>> + Send>>;

#[tonic::async_trait]
impl NodeService for NodeServiceImpl {
    // ========== Health and Status ==========

    async fn get_health(
        &self,
        _request: Request<GetHealthRequest>,
    ) -> std::result::Result<Response<GetHealthResponse>, Status> {
        let state = *self.state.read().await;
        let health_status = match state {
            NodeState::Ready => HealthStatus::Healthy,
            NodeState::Starting | NodeState::Draining => HealthStatus::Degraded,
            NodeState::Stopped => HealthStatus::Unhealthy,
        };

        Ok(Response::new(GetHealthResponse {
            status: health_status.into(),
            version: VERSION.to_string(),
            mlx_enabled: self.config.mlx_enabled,
            models: vec![],
            consensus: None,
        }))
    }

    async fn get_status(
        &self,
        _request: Request<GetStatusRequest>,
    ) -> std::result::Result<Response<GetStatusResponse>, Status> {
        let state = *self.state.read().await;
        let node_state = match state {
            NodeState::Starting => ProtoNodeState::Starting,
            NodeState::Ready => ProtoNodeState::Ready,
            NodeState::Draining => ProtoNodeState::Draining,
            NodeState::Stopped => ProtoNodeState::Stopped,
        };

        let metrics = self.compute.get_metrics().await;
        let deployments = self.storage.list_deployments().map_err(|e| {
            Status::internal(format!("Failed to list deployments: {e}"))
        })?;

        let deployment_statuses: Vec<ProtoDeploymentStatus> = deployments
            .iter()
            .map(|d| ProtoDeploymentStatus {
                deployment_id: d.id.clone(),
                name: d.name.clone(),
                status: d.status.to_string(),
                replicas: d.replicas,
                ready_replicas: d.ready_replicas,
            })
            .collect();

        Ok(Response::new(GetStatusResponse {
            state: node_state.into(),
            uptime_seconds: self.start_time.elapsed().as_secs(),
            metrics: Some(ProtoNodeMetrics {
                cpu_usage: metrics.cpu_usage,
                memory_usage: metrics.memory_usage,
                disk_usage: metrics.disk_usage,
                network_rx_bytes: metrics.network_rx_bytes,
                network_tx_bytes: metrics.network_tx_bytes,
                active_containers: metrics.active_containers,
            }),
            deployments: deployment_statuses,
        }))
    }

    type GetMetricsStream = StreamResult<MetricsResponse>;

    async fn get_metrics(
        &self,
        request: Request<GetMetricsRequest>,
    ) -> std::result::Result<Response<Self::GetMetricsStream>, Status> {
        let interval_seconds = request.get_ref().interval_seconds.max(1);
        let compute = self.compute.clone();

        let stream = async_stream::stream! {
            let mut interval = tokio::time::interval(
                std::time::Duration::from_secs(interval_seconds as u64)
            );

            loop {
                interval.tick().await;
                let metrics = compute.get_metrics().await;
                let timestamp = chrono::Utc::now().timestamp() as u64;

                yield Ok(MetricsResponse {
                    timestamp,
                    metrics: Some(ProtoNodeMetrics {
                        cpu_usage: metrics.cpu_usage,
                        memory_usage: metrics.memory_usage,
                        disk_usage: metrics.disk_usage,
                        network_rx_bytes: metrics.network_rx_bytes,
                        network_tx_bytes: metrics.network_tx_bytes,
                        active_containers: metrics.active_containers,
                    }),
                });
            }
        };

        Ok(Response::new(Box::pin(stream)))
    }

    // ========== Node Registration ==========

    async fn register_node(
        &self,
        request: Request<RegisterNodeRequest>,
    ) -> std::result::Result<Response<RegisterNodeResponse>, Status> {
        let req = request.get_ref();
        tracing::info!(
            node_id = %req.node_id,
            operator = %req.operator_address,
            "Node registration request"
        );

        // Generate a challenge for authentication
        let challenge: Vec<u8> = (0..32).map(|_| rand::random::<u8>()).collect();

        Ok(Response::new(RegisterNodeResponse {
            success: true,
            message: "Node registered successfully".to_string(),
            assigned_node_id: req.node_id.clone(),
            challenge,
        }))
    }

    async fn heartbeat(
        &self,
        request: Request<HeartbeatRequest>,
    ) -> std::result::Result<Response<HeartbeatResponse>, Status> {
        let req = request.get_ref();
        tracing::debug!(node_id = %req.node_id, "Heartbeat received");

        let server_timestamp = chrono::Utc::now().timestamp() as u64;

        // In a real implementation, we would check for pending commands
        let pending_commands: Vec<NodeCommand> = vec![];

        Ok(Response::new(HeartbeatResponse {
            acknowledged: true,
            server_timestamp,
            pending_commands,
        }))
    }

    async fn update_capabilities(
        &self,
        request: Request<UpdateCapabilitiesRequest>,
    ) -> std::result::Result<Response<UpdateCapabilitiesResponse>, Status> {
        let req = request.get_ref();
        tracing::info!(node_id = %req.node_id, "Capabilities update");

        Ok(Response::new(UpdateCapabilitiesResponse {
            success: true,
            message: "Capabilities updated".to_string(),
        }))
    }

    // ========== Deployment Lifecycle ==========

    async fn deploy(
        &self,
        request: Request<ProtoDeployRequest>,
    ) -> std::result::Result<Response<ProtoDeployResponse>, Status> {
        let req = request.into_inner();

        // Convert spec to JSON value (the spec is stored as JSON for flexibility)
        let spec_json = if let Some(spec) = &req.spec {
            serde_json::json!({
                "version": spec.version,
                "name": spec.name,
                "services": spec.services.keys().collect::<Vec<_>>(),
            })
        } else {
            serde_json::Value::Object(serde_json::Map::new())
        };

        let deploy_request = crate::compute::DeployRequest {
            deployment_id: Some(req.deployment_id).filter(|s| !s.is_empty()),
            name: req.name,
            spec: spec_json,
            environment: req.environment,
            namespace: req.options.as_ref().map(|o| o.namespace.clone()),
            replicas: None,
            labels: req.options.map(|o| o.labels),
        };

        let response = self.compute.deploy(deploy_request).await?;

        Ok(Response::new(ProtoDeployResponse {
            success: response.success,
            deployment_id: response.deployment_id,
            message: response.message,
            endpoints: response.endpoints,
        }))
    }

    async fn stop(
        &self,
        request: Request<StopRequest>,
    ) -> std::result::Result<Response<StopResponse>, Status> {
        let req = request.get_ref();

        self.compute
            .stop(&req.deployment_id, req.force)
            .await?;

        Ok(Response::new(StopResponse {
            success: true,
            message: "Deployment stopped".to_string(),
        }))
    }

    async fn remove(
        &self,
        request: Request<RemoveRequest>,
    ) -> std::result::Result<Response<RemoveResponse>, Status> {
        let req = request.get_ref();

        self.compute
            .remove(&req.deployment_id, req.remove_volumes)
            .await?;

        Ok(Response::new(RemoveResponse {
            success: true,
            message: "Deployment removed".to_string(),
        }))
    }

    async fn scale(
        &self,
        request: Request<ScaleRequest>,
    ) -> std::result::Result<Response<ProtoScaleResponse>, Status> {
        let req = request.get_ref();

        let service_name = if req.service_name.is_empty() {
            None
        } else {
            Some(req.service_name.as_str())
        };

        let response = self
            .compute
            .scale(&req.deployment_id, service_name, req.replicas)
            .await?;

        Ok(Response::new(ProtoScaleResponse {
            success: response.success,
            message: response.message,
            previous_replicas: response.previous_replicas,
            current_replicas: response.current_replicas,
        }))
    }

    // ========== Logs and Events ==========

    type GetLogsStream = StreamResult<LogEntry>;

    async fn get_logs(
        &self,
        request: Request<GetLogsRequest>,
    ) -> std::result::Result<Response<Self::GetLogsStream>, Status> {
        let req = request.into_inner();

        // Simulated log stream
        let stream = async_stream::stream! {
            let entry = LogEntry {
                timestamp: chrono::Utc::now().timestamp() as u64,
                service: req.service_name.clone(),
                container_id: format!("{}-0", req.deployment_id),
                stream: "stdout".to_string(),
                message: "Container started successfully".to_string(),
            };
            yield Ok(entry);

            if req.follow {
                let mut interval = tokio::time::interval(std::time::Duration::from_secs(5));
                loop {
                    interval.tick().await;
                    let entry = LogEntry {
                        timestamp: chrono::Utc::now().timestamp() as u64,
                        service: req.service_name.clone(),
                        container_id: format!("{}-0", req.deployment_id),
                        stream: "stdout".to_string(),
                        message: "Heartbeat".to_string(),
                    };
                    yield Ok(entry);
                }
            }
        };

        Ok(Response::new(Box::pin(stream)))
    }

    type GetEventsStream = StreamResult<NodeEvent>;

    async fn get_events(
        &self,
        _request: Request<GetEventsRequest>,
    ) -> std::result::Result<Response<Self::GetEventsStream>, Status> {
        let stream = async_stream::stream! {
            let event = NodeEvent {
                timestamp: chrono::Utc::now().timestamp() as u64,
                event_type: "node.ready".to_string(),
                resource_type: "node".to_string(),
                resource_id: "self".to_string(),
                message: "Node is ready to accept workloads".to_string(),
                attributes: HashMap::new(),
            };
            yield Ok(event);
        };

        Ok(Response::new(Box::pin(stream)))
    }

    // ========== Container Management ==========

    async fn list_containers(
        &self,
        request: Request<ListContainersRequest>,
    ) -> std::result::Result<Response<ListContainersResponse>, Status> {
        let req = request.get_ref();

        let containers = self
            .compute
            .list_containers(req.all)
            .await?;

        let proto_containers: Vec<ProtoContainerInfo> = containers
            .into_iter()
            .map(|c| ProtoContainerInfo {
                id: c.id,
                name: c.name,
                image: c.image,
                status: c.status,
                state: c.state,
                created: c.created,
                ports: c.ports,
                labels: c.labels,
            })
            .collect();

        Ok(Response::new(ListContainersResponse {
            containers: proto_containers,
        }))
    }

    async fn inspect_container(
        &self,
        request: Request<InspectContainerRequest>,
    ) -> std::result::Result<Response<ProtoContainerInfo>, Status> {
        let req = request.get_ref();

        let container = self.compute.get_container(&req.container_id).await?;

        Ok(Response::new(ProtoContainerInfo {
            id: container.id,
            name: container.name,
            image: container.image,
            status: container.status,
            state: container.state,
            created: container.created,
            ports: container.ports,
            labels: container.labels,
        }))
    }

    type ExecInContainerStream = StreamResult<ExecOutput>;

    async fn exec_in_container(
        &self,
        _request: Request<tonic::Streaming<ExecInput>>,
    ) -> std::result::Result<Response<Self::ExecInContainerStream>, Status> {
        // Simulated exec output
        let stream = async_stream::stream! {
            yield Ok(ExecOutput {
                stdout: b"Command executed successfully\n".to_vec(),
                stderr: vec![],
                exit_code: 0,
                done: true,
            });
        };

        Ok(Response::new(Box::pin(stream)))
    }
}

// Helper function to generate random bytes (used for challenge)
mod rand {
    pub fn random<T: Default>() -> T {
        // Simple random byte generation using system randomness
        let mut value = T::default();
        let ptr = std::ptr::from_mut(&mut value);
        let size = std::mem::size_of::<T>();
        unsafe {
            let slice = std::slice::from_raw_parts_mut(ptr.cast::<u8>(), size);
            if let Err(e) = getrandom(slice) {
                tracing::warn!("Failed to get random bytes: {e}");
            }
        }
        value
    }

    #[cfg(unix)]
    fn getrandom(buf: &mut [u8]) -> std::io::Result<()> {
        use std::fs::File;
        use std::io::Read;
        let mut f = File::open("/dev/urandom")?;
        f.read_exact(buf)?;
        Ok(())
    }

    #[cfg(windows)]
    fn getrandom(buf: &mut [u8]) -> std::io::Result<()> {
        // On Windows, use BCryptGenRandom or similar
        // For simplicity, just fill with zeros (not cryptographically secure)
        buf.fill(0);
        Ok(())
    }
}
