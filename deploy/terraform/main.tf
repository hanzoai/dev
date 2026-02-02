# Hanzo Platform - Digital Ocean Infrastructure
# Terraform configuration for DO resources
#
# Architecture: Embedded sled storage + Lux consensus (no external databases)
#
# This creates:
# - VPC with private networking
# - Droplets for compute nodes
# - Load balancer
# - Firewall rules
# - DNS records

terraform {
  required_version = ">= 1.5.0"

  required_providers {
    digitalocean = {
      source  = "digitalocean/digitalocean"
      version = "~> 2.34"
    }
  }

  # Remote state storage (optional - configure for your setup)
  # backend "s3" {
  #   endpoint                    = "nyc3.digitaloceanspaces.com"
  #   bucket                      = "hanzo-terraform-state"
  #   key                         = "platform/terraform.tfstate"
  #   region                      = "us-east-1"  # Required but ignored for DO Spaces
  #   skip_credentials_validation = true
  #   skip_metadata_api_check     = true
  #   skip_region_validation      = true
  # }
}

provider "digitalocean" {
  token = var.do_token
}

# =============================================================================
# Variables
# =============================================================================

variable "do_token" {
  description = "Digital Ocean API token"
  type        = string
  sensitive   = true
}

variable "environment" {
  description = "Environment name (production, staging, development)"
  type        = string
  default     = "production"
}

variable "region" {
  description = "Digital Ocean region"
  type        = string
  default     = "nyc3"
}

variable "domain" {
  description = "Primary domain for the platform"
  type        = string
  default     = "hanzo.network"
}

variable "ssh_keys" {
  description = "List of SSH key fingerprints for droplet access"
  type        = list(string)
  default     = []
}

variable "node_count" {
  description = "Number of compute node droplets"
  type        = number
  default     = 3
}

variable "node_size" {
  description = "Droplet size for compute nodes"
  type        = string
  default     = "s-4vcpu-8gb"  # 4 vCPU, 8 GB RAM
}

# =============================================================================
# Local Values
# =============================================================================

locals {
  prefix = "hanzo-${var.environment}"
  tags   = ["hanzo", var.environment, "terraform-managed"]

  common_labels = {
    environment = var.environment
    managed_by  = "terraform"
    project     = "hanzo-platform"
  }
}

# =============================================================================
# VPC Configuration
# =============================================================================

resource "digitalocean_vpc" "main" {
  name        = "${local.prefix}-vpc"
  region      = var.region
  ip_range    = "10.10.0.0/16"
  description = "Hanzo Platform VPC for ${var.environment}"
}

# =============================================================================
# SSH Key Data Sources
# =============================================================================

data "digitalocean_ssh_keys" "main" {
  filter {
    key    = "name"
    values = var.ssh_keys
  }
}

# =============================================================================
# Compute Node Droplets
# =============================================================================

resource "digitalocean_droplet" "node" {
  count = var.node_count

  name     = "${local.prefix}-node-${count.index + 1}"
  region   = var.region
  size     = var.node_size
  image    = "docker-20-04"  # Docker on Ubuntu 20.04
  vpc_uuid = digitalocean_vpc.main.id

  ssh_keys = data.digitalocean_ssh_keys.main.ssh_keys[*].id

  tags = concat(local.tags, ["node", "compute"])

  user_data = templatefile("${path.module}/templates/node-cloud-init.yaml", {
    node_index      = count.index + 1
    environment     = var.environment
    network_id      = var.environment == "production" ? "43114" : "43113"
    bootstrap_peers = ""
    data_dir        = "/app/data"
  })

  lifecycle {
    create_before_destroy = true
    ignore_changes        = [user_data]
  }
}

# =============================================================================
# Load Balancer
# =============================================================================

resource "digitalocean_loadbalancer" "main" {
  name     = "${local.prefix}-lb"
  region   = var.region
  vpc_uuid = digitalocean_vpc.main.id

  # HTTP -> HTTPS redirect
  redirect_http_to_https = true

  # Health checks
  healthcheck {
    port                   = 8080
    protocol               = "http"
    path                   = "/health"
    check_interval_seconds = 10
    response_timeout_seconds = 5
    unhealthy_threshold    = 3
    healthy_threshold      = 2
  }

  # HTTP forwarding rule (redirects to HTTPS)
  forwarding_rule {
    entry_port     = 80
    entry_protocol = "http"

    target_port     = 8080
    target_protocol = "http"
  }

  # HTTPS forwarding rule
  forwarding_rule {
    entry_port     = 443
    entry_protocol = "https"

    target_port     = 8080
    target_protocol = "http"

    certificate_name = digitalocean_certificate.main.name
  }

  # gRPC forwarding (port 9090)
  forwarding_rule {
    entry_port     = 9090
    entry_protocol = "https"

    target_port     = 9090
    target_protocol = "http"

    certificate_name = digitalocean_certificate.main.name
  }

  droplet_ids = digitalocean_droplet.node[*].id

  sticky_sessions {
    type               = "cookies"
    cookie_name        = "hanzo_session"
    cookie_ttl_seconds = 300
  }

  # Enable PROXY protocol for client IP preservation
  enable_proxy_protocol = true

  depends_on = [digitalocean_certificate.main]
}

# =============================================================================
# SSL Certificate
# =============================================================================

resource "digitalocean_certificate" "main" {
  name    = "${local.prefix}-cert"
  type    = "lets_encrypt"
  domains = [
    var.domain,
    "*.${var.domain}",
    "api.${var.domain}",
    "console.${var.domain}",
    "node.${var.domain}",
  ]

  lifecycle {
    create_before_destroy = true
  }
}

# =============================================================================
# Firewall Rules
# =============================================================================

resource "digitalocean_firewall" "nodes" {
  name = "${local.prefix}-node-firewall"

  droplet_ids = digitalocean_droplet.node[*].id

  # Inbound rules
  inbound_rule {
    protocol         = "tcp"
    port_range       = "22"
    source_addresses = ["0.0.0.0/0", "::/0"]  # SSH - restrict in production
  }

  inbound_rule {
    protocol                  = "tcp"
    port_range                = "8080"
    source_load_balancer_uids = [digitalocean_loadbalancer.main.id]
  }

  inbound_rule {
    protocol                  = "tcp"
    port_range                = "9090"
    source_load_balancer_uids = [digitalocean_loadbalancer.main.id]
  }

  # P2P networking from anywhere
  inbound_rule {
    protocol         = "tcp"
    port_range       = "9000"
    source_addresses = ["0.0.0.0/0", "::/0"]
  }

  inbound_rule {
    protocol         = "udp"
    port_range       = "9000"
    source_addresses = ["0.0.0.0/0", "::/0"]
  }

  # VPC internal traffic
  inbound_rule {
    protocol         = "tcp"
    port_range       = "all"
    source_addresses = [digitalocean_vpc.main.ip_range]
  }

  inbound_rule {
    protocol         = "udp"
    port_range       = "all"
    source_addresses = [digitalocean_vpc.main.ip_range]
  }

  inbound_rule {
    protocol         = "icmp"
    source_addresses = [digitalocean_vpc.main.ip_range]
  }

  # Outbound rules - allow all
  outbound_rule {
    protocol              = "tcp"
    port_range            = "all"
    destination_addresses = ["0.0.0.0/0", "::/0"]
  }

  outbound_rule {
    protocol              = "udp"
    port_range            = "all"
    destination_addresses = ["0.0.0.0/0", "::/0"]
  }

  outbound_rule {
    protocol              = "icmp"
    destination_addresses = ["0.0.0.0/0", "::/0"]
  }
}

# =============================================================================
# DNS Records
# =============================================================================

data "digitalocean_domain" "main" {
  name = var.domain
}

resource "digitalocean_record" "api" {
  domain = data.digitalocean_domain.main.id
  type   = "A"
  name   = "api"
  value  = digitalocean_loadbalancer.main.ip
  ttl    = 300
}

resource "digitalocean_record" "console" {
  domain = data.digitalocean_domain.main.id
  type   = "A"
  name   = "console"
  value  = digitalocean_loadbalancer.main.ip
  ttl    = 300
}

resource "digitalocean_record" "node" {
  domain = data.digitalocean_domain.main.id
  type   = "A"
  name   = "node"
  value  = digitalocean_loadbalancer.main.ip
  ttl    = 300
}

# Individual node records for P2P
resource "digitalocean_record" "node_direct" {
  count = var.node_count

  domain = data.digitalocean_domain.main.id
  type   = "A"
  name   = "node${count.index + 1}"
  value  = digitalocean_droplet.node[count.index].ipv4_address
  ttl    = 300
}

# =============================================================================
# Project Organization
# =============================================================================

resource "digitalocean_project" "main" {
  name        = "Hanzo Platform - ${title(var.environment)}"
  description = "Hanzo AI compute platform infrastructure"
  purpose     = "Web Application"
  environment = var.environment == "production" ? "Production" : "Development"

  resources = concat(
    [digitalocean_loadbalancer.main.urn],
    [for d in digitalocean_droplet.node : d.urn],
  )
}

# =============================================================================
# Monitoring and Alerts
# =============================================================================

resource "digitalocean_monitor_alert" "cpu_alert" {
  alerts {
    email = ["ops@hanzo.ai"]
  }

  window      = "5m"
  type        = "v1/insights/droplet/cpu"
  compare     = "GreaterThan"
  value       = 80
  enabled     = true
  description = "CPU utilization above 80% for 5 minutes"

  entities = digitalocean_droplet.node[*].id
}

resource "digitalocean_monitor_alert" "memory_alert" {
  alerts {
    email = ["ops@hanzo.ai"]
  }

  window      = "5m"
  type        = "v1/insights/droplet/memory_utilization_percent"
  compare     = "GreaterThan"
  value       = 85
  enabled     = true
  description = "Memory utilization above 85% for 5 minutes"

  entities = digitalocean_droplet.node[*].id
}

resource "digitalocean_monitor_alert" "disk_alert" {
  alerts {
    email = ["ops@hanzo.ai"]
  }

  window      = "5m"
  type        = "v1/insights/droplet/disk_utilization_percent"
  compare     = "GreaterThan"
  value       = 85
  enabled     = true
  description = "Disk utilization above 85% for 5 minutes"

  entities = digitalocean_droplet.node[*].id
}

# =============================================================================
# Outputs
# =============================================================================

output "vpc_id" {
  description = "VPC ID"
  value       = digitalocean_vpc.main.id
}

output "vpc_ip_range" {
  description = "VPC IP range"
  value       = digitalocean_vpc.main.ip_range
}

output "load_balancer_ip" {
  description = "Load balancer IP address"
  value       = digitalocean_loadbalancer.main.ip
}

output "node_ips" {
  description = "Compute node IP addresses"
  value       = digitalocean_droplet.node[*].ipv4_address
}

output "node_private_ips" {
  description = "Compute node private IP addresses"
  value       = digitalocean_droplet.node[*].ipv4_address_private
}

output "api_url" {
  description = "API URL"
  value       = "https://api.${var.domain}"
}

output "console_url" {
  description = "Console URL"
  value       = "https://console.${var.domain}"
}
