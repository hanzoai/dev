# Hanzo Platform - Terraform Variables
# Override these in terraform.tfvars or via environment variables

# =============================================================================
# Required Variables
# =============================================================================

variable "do_token" {
  description = "Digital Ocean API token (TF_VAR_do_token or terraform.tfvars)"
  type        = string
  sensitive   = true
}

variable "spaces_access_id" {
  description = "DO Spaces access key ID"
  type        = string
  sensitive   = true
}

variable "spaces_secret_key" {
  description = "DO Spaces secret access key"
  type        = string
  sensitive   = true
}

# =============================================================================
# Environment Configuration
# =============================================================================

variable "environment" {
  description = "Deployment environment"
  type        = string
  default     = "production"

  validation {
    condition     = contains(["production", "staging", "development"], var.environment)
    error_message = "Environment must be one of: production, staging, development."
  }
}

variable "region" {
  description = "Digital Ocean region"
  type        = string
  default     = "nyc3"

  validation {
    condition     = contains(["nyc1", "nyc3", "sfo2", "sfo3", "ams3", "sgp1", "lon1", "fra1", "tor1", "blr1", "syd1"], var.region)
    error_message = "Invalid Digital Ocean region."
  }
}

variable "domain" {
  description = "Primary domain name"
  type        = string
  default     = "hanzo.network"
}

# =============================================================================
# Compute Configuration
# =============================================================================

variable "node_count" {
  description = "Number of compute nodes"
  type        = number
  default     = 3

  validation {
    condition     = var.node_count >= 1 && var.node_count <= 20
    error_message = "Node count must be between 1 and 20."
  }
}

variable "node_size" {
  description = "Droplet size for compute nodes"
  type        = string
  default     = "s-4vcpu-8gb"
}

variable "node_image" {
  description = "Droplet image (OS)"
  type        = string
  default     = "docker-20-04"
}

# =============================================================================
# Database Configuration
# =============================================================================

variable "db_size" {
  description = "PostgreSQL cluster size"
  type        = string
  default     = "db-s-2vcpu-4gb"
}

variable "db_node_count" {
  description = "PostgreSQL node count (standby replicas)"
  type        = number
  default     = 1

  validation {
    condition     = contains([1, 2, 3], var.db_node_count)
    error_message = "Database node count must be 1, 2, or 3."
  }
}

variable "redis_size" {
  description = "Redis cluster size"
  type        = string
  default     = "db-s-1vcpu-2gb"
}

variable "redis_node_count" {
  description = "Redis node count (standby replicas)"
  type        = number
  default     = 1

  validation {
    condition     = contains([1, 2, 3], var.redis_node_count)
    error_message = "Redis node count must be 1, 2, or 3."
  }
}

# =============================================================================
# Access Configuration
# =============================================================================

variable "ssh_keys" {
  description = "List of SSH key names for droplet access"
  type        = list(string)
  default     = []
}

variable "allowed_ssh_ips" {
  description = "CIDR blocks allowed for SSH access"
  type        = list(string)
  default     = ["0.0.0.0/0"]  # Restrict this in production!
}

# =============================================================================
# Network Configuration
# =============================================================================

variable "vpc_ip_range" {
  description = "VPC IP address range"
  type        = string
  default     = "10.10.0.0/16"
}

variable "hanzo_network_id" {
  description = "Hanzo network ID (43114=mainnet, 43113=testnet, 1337=local)"
  type        = string
  default     = "43114"
}

variable "bootstrap_peers" {
  description = "Comma-separated list of bootstrap peer multiaddresses"
  type        = string
  default     = ""
  sensitive   = true
}

# =============================================================================
# Monitoring Configuration
# =============================================================================

variable "alert_email" {
  description = "Email address for monitoring alerts"
  type        = string
  default     = "ops@hanzo.ai"
}

variable "enable_monitoring" {
  description = "Enable DO monitoring and alerts"
  type        = bool
  default     = true
}

# =============================================================================
# Feature Flags
# =============================================================================

variable "enable_backups" {
  description = "Enable automated droplet backups"
  type        = bool
  default     = true
}

variable "enable_spaces_versioning" {
  description = "Enable Spaces bucket versioning"
  type        = bool
  default     = true
}

variable "enable_ha_database" {
  description = "Enable high-availability for databases"
  type        = bool
  default     = true
}
