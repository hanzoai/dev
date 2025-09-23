//! OpenAPI/Swagger documentation for hanzod

use serde_json::json;
use axum::{response::Json, http::StatusCode};

/// Generate OpenAPI 3.0 specification for the API
pub async fn openapi_spec() -> Result<Json<serde_json::Value>, StatusCode> {
    let spec = json!({
        "openapi": "3.0.0",
        "info": {
            "title": "Hanzod API - Hanzo AI Daemon",
            "description": "Local AI inference service with blockchain integration, quantum staking, and compute marketplace",
            "version": "1.0.0",
            "contact": {
                "name": "Hanzo AI",
                "url": "https://hanzo.ai",
            }
        },
        "servers": [
            {
                "url": "http://localhost:3690/v1",
                "description": "Local development server (v1 API)"
            }
        ],
        "paths": {
            "/health": {
                "get": {
                    "summary": "Health check",
                    "tags": ["System"],
                    "responses": {
                        "200": {
                            "description": "Service is healthy",
                            "content": {
                                "application/json": {
                                    "schema": {
                                        "type": "object",
                                        "properties": {
                                            "status": {"type": "string", "example": "healthy"},
                                            "timestamp": {"type": "string", "format": "date-time"}
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            },
            "/inference": {
                "post": {
                    "summary": "Run AI inference",
                    "tags": ["AI"],
                    "requestBody": {
                        "required": true,
                        "content": {
                            "application/json": {
                                "schema": {
                                    "type": "object",
                                    "required": ["model", "prompt"],
                                    "properties": {
                                        "model": {
                                            "type": "string",
                                            "description": "Model name",
                                            "example": "llama3"
                                        },
                                        "prompt": {
                                            "type": "string",
                                            "description": "Input prompt",
                                            "example": "What is the capital of France?"
                                        },
                                        "max_tokens": {
                                            "type": "integer",
                                            "default": 100
                                        },
                                        "temperature": {
                                            "type": "number",
                                            "default": 0.7
                                        },
                                        "stream": {
                                            "type": "boolean",
                                            "default": false
                                        }
                                    }
                                }
                            }
                        }
                    },
                    "responses": {
                        "200": {
                            "description": "Inference result",
                            "content": {
                                "application/json": {
                                    "schema": {
                                        "type": "object",
                                        "properties": {
                                            "response": {"type": "string"},
                                            "model": {"type": "string"},
                                            "tokens_used": {"type": "integer"},
                                            "inference_time_ms": {"type": "integer"},
                                            "blockchain_tx": {"type": "string", "description": "Blockchain transaction hash"}
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            },
            "/embeddings": {
                "post": {
                    "summary": "Generate embeddings",
                    "tags": ["AI"],
                    "requestBody": {
                        "required": true,
                        "content": {
                            "application/json": {
                                "schema": {
                                    "type": "object",
                                    "required": ["model", "input"],
                                    "properties": {
                                        "model": {
                                            "type": "string",
                                            "example": "all-minilm"
                                        },
                                        "input": {
                                            "type": "array",
                                            "items": {"type": "string"},
                                            "example": ["Hello world", "How are you?"]
                                        }
                                    }
                                }
                            }
                        }
                    },
                    "responses": {
                        "200": {
                            "description": "Embeddings generated",
                            "content": {
                                "application/json": {
                                    "schema": {
                                        "type": "object",
                                        "properties": {
                                            "embeddings": {
                                                "type": "array",
                                                "items": {
                                                    "type": "array",
                                                    "items": {"type": "number"}
                                                }
                                            },
                                            "model": {"type": "string"},
                                            "dimensions": {"type": "integer"}
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            },
            "/vector_search": {
                "post": {
                    "summary": "Search vector database",
                    "tags": ["AI"],
                    "requestBody": {
                        "required": true,
                        "content": {
                            "application/json": {
                                "schema": {
                                    "type": "object",
                                    "required": ["query"],
                                    "properties": {
                                        "query": {"type": "string"},
                                        "k": {"type": "integer", "default": 10},
                                        "collection": {"type": "string"}
                                    }
                                }
                            }
                        }
                    },
                    "responses": {
                        "200": {
                            "description": "Search results",
                            "content": {
                                "application/json": {
                                    "schema": {
                                        "type": "object",
                                        "properties": {
                                            "results": {
                                                "type": "array",
                                                "items": {
                                                    "type": "object",
                                                    "properties": {
                                                        "text": {"type": "string"},
                                                        "score": {"type": "number"},
                                                        "metadata": {"type": "object"}
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            },
            "/runtimes": {
                "get": {
                    "summary": "List container runtimes",
                    "tags": ["System"],
                    "responses": {
                        "200": {
                            "description": "Available runtimes",
                            "content": {
                                "application/json": {
                                    "schema": {
                                        "type": "object",
                                        "properties": {
                                            "runtimes": {
                                                "type": "array",
                                                "items": {
                                                    "type": "object",
                                                    "properties": {
                                                        "type": {"type": "string", "enum": ["DockerDesktop", "Colima", "Rancher", "Podman"]},
                                                        "version": {"type": "string"},
                                                        "is_active": {"type": "boolean"},
                                                        "socket_path": {"type": "string"}
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            },
            "/workloads": {
                "get": {
                    "summary": "List workloads",
                    "tags": ["Workloads"],
                    "responses": {
                        "200": {
                            "description": "Active workloads",
                            "content": {
                                "application/json": {
                                    "schema": {
                                        "type": "object",
                                        "properties": {
                                            "workloads": {
                                                "type": "array",
                                                "items": {
                                                    "type": "object",
                                                    "properties": {
                                                        "id": {"type": "string"},
                                                        "type": {"type": "string"},
                                                        "status": {"type": "string"},
                                                        "created_at": {"type": "string", "format": "date-time"}
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                },
                "post": {
                    "summary": "Create workload",
                    "tags": ["Workloads"],
                    "requestBody": {
                        "required": true,
                        "content": {
                            "application/json": {
                                "schema": {
                                    "type": "object",
                                    "required": ["type"],
                                    "properties": {
                                        "type": {
                                            "type": "string",
                                            "enum": ["Inference", "Embedding", "VectorDB", "Blockchain", "Compute"]
                                        },
                                        "config": {"type": "object"}
                                    }
                                }
                            }
                        }
                    },
                    "responses": {
                        "201": {
                            "description": "Workload created",
                            "content": {
                                "application/json": {
                                    "schema": {
                                        "type": "object",
                                        "properties": {
                                            "id": {"type": "string"},
                                            "status": {"type": "string"}
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            },
            "/marketplace/providers": {
                "get": {
                    "summary": "List resource providers",
                    "tags": ["Marketplace"],
                    "responses": {
                        "200": {
                            "description": "Resource providers earning LUX",
                            "content": {
                                "application/json": {
                                    "schema": {
                                        "type": "object",
                                        "properties": {
                                            "providers": {
                                                "type": "array",
                                                "items": {
                                                    "type": "object",
                                                    "properties": {
                                                        "provider_id": {"type": "string"},
                                                        "cpu_cores": {"type": "integer"},
                                                        "gpu_count": {"type": "integer"},
                                                        "ram_gb": {"type": "number"},
                                                        "total_earned_usd": {"type": "number", "description": "Total earnings in USD"},
                                                        "lux_staked": {"type": "number", "description": "LUX tokens staked"}
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            },
            "/staking/status": {
                "get": {
                    "summary": "Quantum staking status",
                    "tags": ["Blockchain"],
                    "responses": {
                        "200": {
                            "description": "Network staking status",
                            "content": {
                                "application/json": {
                                    "schema": {
                                        "type": "object",
                                        "properties": {
                                            "network_staked_total": {"type": "number", "description": "Total LUX staked network-wide"},
                                            "quantum_threshold": {"type": "number", "example": 1000000},
                                            "quantum_achieved": {"type": "boolean"},
                                            "participating_nodes": {"type": "integer"},
                                            "carbon_offset_donations_usd": {"type": "number", "description": "Total donations to Zoo Labs Foundation"}
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            },
            "/history": {
                "get": {
                    "summary": "Operation history",
                    "tags": ["System"],
                    "parameters": [
                        {
                            "name": "limit",
                            "in": "query",
                            "schema": {"type": "integer", "default": 100}
                        }
                    ],
                    "responses": {
                        "200": {
                            "description": "Historical operations tracked on blockchain",
                            "content": {
                                "application/json": {
                                    "schema": {
                                        "type": "object",
                                        "properties": {
                                            "operations": {
                                                "type": "array",
                                                "items": {
                                                    "type": "object",
                                                    "properties": {
                                                        "id": {"type": "string"},
                                                        "operation_type": {"type": "string"},
                                                        "timestamp": {"type": "string", "format": "date-time"},
                                                        "blockchain_tx": {"type": "string"}
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        },
        "components": {
            "schemas": {
                "Error": {
                    "type": "object",
                    "properties": {
                        "error": {"type": "string"},
                        "details": {"type": "string"}
                    }
                }
            }
        },
        "tags": [
            {"name": "System", "description": "System operations"},
            {"name": "AI", "description": "AI inference and embeddings"},
            {"name": "Workloads", "description": "Workload management"},
            {"name": "Marketplace", "description": "Compute marketplace"},
            {"name": "Blockchain", "description": "Blockchain and staking"}
        ]
    });

    Ok(Json(spec))
}