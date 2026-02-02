# Hanzo Platform - Deployment Guide

This directory contains deployment configurations for the Hanzo Platform on Digital Ocean.

## Architecture

Hanzo Node uses **embedded storage and consensus**:

- **sled**: High-performance embedded key-value store (Rust)
- **Lux consensus**: Decentralized consensus via libp2p networking

No external databases (PostgreSQL, Redis, MinIO) are required. All state is persisted locally in `/app/data` and replicated across nodes via Lux consensus.

## Directory Structure

```
deploy/
├── do/                     # Digital Ocean App Platform
│   └── app.yaml           # App Platform specification
├── terraform/              # Infrastructure as Code
│   ├── main.tf            # Main Terraform configuration
│   └── templates/         # Cloud-init templates
│       └── node-cloud-init.yaml
├── scripts/               # Deployment scripts
│   └── deploy.sh         # Main deployment script
├── compose.yml            # Local development stack
├── Makefile              # Make targets for deployment
└── README.md             # This file
```

## Prerequisites

1. **Digital Ocean Account** with:
   - API token (`doctl auth init`)
   - Container registry created

2. **Tools Required**:
   - `terraform` >= 1.5.0
   - `doctl` (DO CLI)
   - `docker` and `docker compose`
   - `make`

3. **GitHub Secrets** (for CI/CD):
   - `DIGITALOCEAN_ACCESS_TOKEN`

## Quick Start

### Local Development

```bash
# Start the full local stack
make dev

# Or with monitoring (Prometheus + Grafana)
make dev-full

# View logs
make logs

# Check health
make health

# Stop everything
make down

# Clean up (removes volumes)
make clean
```

**Service URLs:**
- Console: http://localhost:3001
- Node API: http://localhost:8080
- Platform API: http://localhost:8000
- Grafana: http://localhost:3000 (admin/hanzo)

### Deploy to Digital Ocean

#### 1. Configure Terraform

```bash
# Copy example vars
cp terraform/terraform.tfvars.example terraform/terraform.tfvars

# Edit with your values
vim terraform/terraform.tfvars
```

#### 2. Plan and Apply Infrastructure

```bash
# Initialize Terraform
make tf-init

# Plan changes
make plan ENV=staging

# Apply changes
make apply ENV=staging
```

#### 3. Deploy App Platform

```bash
# Create the app
make app-create

# Or update existing
make app-update

# Trigger deployment
make app-deploy

# View logs
make app-logs
```

#### 4. Full Deployment

```bash
# Build, push, and deploy everything
make deploy
```

## Architecture Details

### Services

| Service | Port | Description |
|---------|------|-------------|
| console | 3001 | Next.js admin dashboard |
| hanzo-node | 8080, 9090, 9000 | Core compute node (sled + Lux) |
| platform-api | 8000 | Compute pools API |

### Data Layer

**Embedded sled storage:**
- ACID-compliant key-value store
- Zero-copy reads for high performance
- Automatic compaction and recovery
- Data persisted to `/app/data`

**Lux consensus:**
- libp2p-based P2P networking
- State replication across nodes
- Byzantine fault tolerant
- No central coordination required

### Infrastructure (Terraform)

- **VPC**: Private network (10.10.0.0/16)
- **Droplets**: Compute nodes with Docker
- **Load Balancer**: HTTPS termination
- **Firewall**: Network security rules
- **DNS**: Subdomain records

### App Platform

DO App Platform provides:
- Automatic deployments on git push
- Managed SSL certificates
- Auto-scaling
- Built-in monitoring

## Configuration

### Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `HANZO_NODE_URL` | Node API URL | http://localhost:8080 |
| `HANZO_DATA_DIR` | Data directory | /app/data |
| `HANZO_NETWORK_ID` | Network ID | 1337 (local) |

### Terraform Variables

See `terraform/terraform.tfvars.example` for all available options.

Key variables:
- `do_token`: DO API token
- `environment`: production/staging/development
- `region`: DO region (nyc3, sfo3, etc.)
- `node_count`: Number of compute nodes

## CI/CD

GitHub Actions workflow (`.github/workflows/do-deploy.yml`):

1. **On PR**: Terraform plan, validate app spec
2. **On push to main**: Build images, apply Terraform, deploy app
3. **Manual dispatch**: Choose environment and action

### Required Secrets

- `DIGITALOCEAN_ACCESS_TOKEN`: DO API token

## Monitoring

### Local Development

Start with monitoring profile:
```bash
make dev-full
```

Access:
- Prometheus: http://localhost:9091
- Grafana: http://localhost:3000

### Production

DO provides built-in monitoring. Additional:
- Node metrics exposed at `/v1/metrics`
- Prometheus scrape config in `hanzo-node/config/prometheus.yml`
- Alerts configured in Terraform

## Data Management

### Backup

```bash
# Local: backup sled data
docker compose exec hanzo-node tar -czvf /tmp/backup.tar.gz /app/data
docker compose cp hanzo-node:/tmp/backup.tar.gz ./backup.tar.gz

# Production: snapshot droplet volumes
doctl compute droplet-action snapshot <droplet-id> --snapshot-name "hanzo-backup-$(date +%Y%m%d)"
```

### Restore

```bash
# Local: restore sled data
docker compose cp ./backup.tar.gz hanzo-node:/tmp/backup.tar.gz
docker compose exec hanzo-node tar -xzvf /tmp/backup.tar.gz -C /
docker compose restart hanzo-node
```

## Troubleshooting

### Common Issues

1. **Port conflicts**: Check if ports 3001, 8080 are free
2. **Docker memory**: Ensure Docker has >= 4GB RAM
3. **Terraform state**: Use remote backend for team deployments
4. **Data corruption**: Check sled logs; restore from backup if needed

### Health Checks

```bash
# Local
make health

# Production
make health-prod
```

### Logs

```bash
# All services
make logs

# Specific service
make logs-node
make logs-console

# Production
make app-logs
```

### Node Data

```bash
# Check sled data directory
docker compose exec hanzo-node ls -la /app/data

# View sled metrics
curl http://localhost:8080/v1/metrics | grep sled
```

## Security Notes

1. **Never commit**:
   - `terraform.tfvars`
   - `.env` files with secrets
   - Private keys

2. **Production hardening**:
   - Restrict SSH access IPs
   - Use private networking
   - Enable backups
   - Regular security updates

3. **Secrets management**:
   - Use DO secrets for sensitive env vars
   - Rotate credentials regularly
   - Use least-privilege access

## Support

- Documentation: https://docs.hanzo.ai
- Issues: https://github.com/hanzoai/dev/issues
- Email: ops@hanzo.ai
