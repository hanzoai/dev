#!/usr/bin/env bash
# Hanzo Platform - Deployment Script
# Usage: ./deploy.sh [environment] [action]
#
# Environments: production, staging, development
# Actions: plan, apply, destroy, app-deploy, app-update

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(dirname "$SCRIPT_DIR")"
TERRAFORM_DIR="$ROOT_DIR/terraform"
DO_DIR="$ROOT_DIR/do"

# Default values
ENVIRONMENT="${1:-staging}"
ACTION="${2:-plan}"

# Logging
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
    exit 1
}

# Check dependencies
check_dependencies() {
    log_info "Checking dependencies..."

    local deps=("terraform" "doctl" "docker" "jq")
    for dep in "${deps[@]}"; do
        if ! command -v "$dep" &> /dev/null; then
            log_error "$dep is not installed. Please install it first."
        fi
    done

    log_success "All dependencies available"
}

# Validate environment
validate_environment() {
    case "$ENVIRONMENT" in
        production|staging|development)
            log_info "Environment: $ENVIRONMENT"
            ;;
        *)
            log_error "Invalid environment: $ENVIRONMENT. Must be: production, staging, development"
            ;;
    esac
}

# Load environment variables
load_env() {
    local env_file="$ROOT_DIR/.env.$ENVIRONMENT"
    if [[ -f "$env_file" ]]; then
        log_info "Loading environment from $env_file"
        set -a
        source "$env_file"
        set +a
    else
        log_warn "Environment file not found: $env_file"
    fi
}

# Terraform commands
terraform_init() {
    log_info "Initializing Terraform..."
    cd "$TERRAFORM_DIR"
    terraform init -upgrade
}

terraform_plan() {
    log_info "Running Terraform plan for $ENVIRONMENT..."
    cd "$TERRAFORM_DIR"

    terraform plan \
        -var-file="environments/$ENVIRONMENT.tfvars" \
        -out="tfplan.$ENVIRONMENT" \
        -detailed-exitcode || true

    log_success "Terraform plan complete. Review tfplan.$ENVIRONMENT"
}

terraform_apply() {
    log_info "Applying Terraform changes for $ENVIRONMENT..."
    cd "$TERRAFORM_DIR"

    if [[ ! -f "tfplan.$ENVIRONMENT" ]]; then
        log_error "No plan file found. Run 'plan' first."
    fi

    terraform apply "tfplan.$ENVIRONMENT"
    log_success "Terraform apply complete"
}

terraform_destroy() {
    log_warn "This will destroy all infrastructure for $ENVIRONMENT!"
    read -p "Are you sure? Type 'destroy' to confirm: " confirm

    if [[ "$confirm" != "destroy" ]]; then
        log_info "Aborted"
        exit 0
    fi

    cd "$TERRAFORM_DIR"
    terraform destroy \
        -var-file="environments/$ENVIRONMENT.tfvars" \
        -auto-approve

    log_success "Infrastructure destroyed"
}

# DO App Platform commands
app_create() {
    log_info "Creating DO App Platform app..."

    # Validate app spec
    doctl apps spec validate "$DO_DIR/app.yaml"

    # Create app
    doctl apps create --spec "$DO_DIR/app.yaml" --format json | jq .

    log_success "App created"
}

app_update() {
    log_info "Updating DO App Platform app..."

    # Get app ID
    local app_id
    app_id=$(doctl apps list --format ID,Spec.Name --no-header | grep "hanzo-platform" | awk '{print $1}')

    if [[ -z "$app_id" ]]; then
        log_error "App not found. Run 'app-create' first."
    fi

    # Update app
    doctl apps update "$app_id" --spec "$DO_DIR/app.yaml" --format json | jq .

    log_success "App updated"
}

app_deploy() {
    log_info "Triggering deployment for DO App Platform..."

    local app_id
    app_id=$(doctl apps list --format ID,Spec.Name --no-header | grep "hanzo-platform" | awk '{print $1}')

    if [[ -z "$app_id" ]]; then
        log_error "App not found."
    fi

    doctl apps create-deployment "$app_id" --format json | jq .

    log_success "Deployment triggered"
}

app_logs() {
    log_info "Fetching app logs..."

    local app_id
    app_id=$(doctl apps list --format ID,Spec.Name --no-header | grep "hanzo-platform" | awk '{print $1}')

    if [[ -z "$app_id" ]]; then
        log_error "App not found."
    fi

    doctl apps logs "$app_id" --follow --type=RUN
}

# Docker build and push
docker_build() {
    log_info "Building Docker images..."

    local tag="${1:-latest}"
    local registry="registry.digitalocean.com/hanzo"

    # Build hanzo-node
    log_info "Building hanzo-node..."
    docker build -t "$registry/hanzo-node:$tag" -f "$ROOT_DIR/../hanzo-node/Dockerfile" "$ROOT_DIR/../hanzo-node"

    # Build console
    log_info "Building console..."
    docker build -t "$registry/console:$tag" -f "$ROOT_DIR/../console/Dockerfile" "$ROOT_DIR/../console"

    log_success "Docker images built"
}

docker_push() {
    log_info "Pushing Docker images to DO registry..."

    local tag="${1:-latest}"
    local registry="registry.digitalocean.com/hanzo"

    # Login to DO registry
    doctl registry login

    # Push images
    docker push "$registry/hanzo-node:$tag"
    docker push "$registry/console:$tag"

    log_success "Docker images pushed"
}

# Database migrations
run_migrations() {
    log_info "Running database migrations..."

    local db_url
    db_url=$(terraform -chdir="$TERRAFORM_DIR" output -raw postgres_connection_pool_uri)

    DATABASE_URL="$db_url" "$ROOT_DIR/../platform/db/migrate.sh"

    log_success "Migrations complete"
}

# Health check
health_check() {
    log_info "Running health checks..."

    local console_url
    local node_url

    console_url=$(terraform -chdir="$TERRAFORM_DIR" output -raw console_url)
    node_url=$(terraform -chdir="$TERRAFORM_DIR" output -raw api_url)

    # Check console
    if curl -sf "$console_url/api/health" > /dev/null; then
        log_success "Console is healthy"
    else
        log_warn "Console health check failed"
    fi

    # Check node
    if curl -sf "$node_url/health" > /dev/null; then
        log_success "Hanzo Node is healthy"
    else
        log_warn "Hanzo Node health check failed"
    fi
}

# Show outputs
show_outputs() {
    log_info "Infrastructure outputs:"
    cd "$TERRAFORM_DIR"
    terraform output
}

# Main
main() {
    echo ""
    echo "=================================="
    echo "  Hanzo Platform Deployment"
    echo "  Environment: $ENVIRONMENT"
    echo "  Action: $ACTION"
    echo "=================================="
    echo ""

    check_dependencies
    validate_environment
    load_env

    case "$ACTION" in
        init)
            terraform_init
            ;;
        plan)
            terraform_init
            terraform_plan
            ;;
        apply)
            terraform_apply
            ;;
        destroy)
            terraform_destroy
            ;;
        app-create)
            app_create
            ;;
        app-update)
            app_update
            ;;
        app-deploy)
            app_deploy
            ;;
        app-logs)
            app_logs
            ;;
        docker-build)
            docker_build "${3:-latest}"
            ;;
        docker-push)
            docker_push "${3:-latest}"
            ;;
        migrate)
            run_migrations
            ;;
        health)
            health_check
            ;;
        outputs)
            show_outputs
            ;;
        *)
            echo "Usage: $0 [environment] [action]"
            echo ""
            echo "Environments:"
            echo "  production, staging, development"
            echo ""
            echo "Actions:"
            echo "  init         - Initialize Terraform"
            echo "  plan         - Plan infrastructure changes"
            echo "  apply        - Apply infrastructure changes"
            echo "  destroy      - Destroy infrastructure"
            echo "  app-create   - Create DO App Platform app"
            echo "  app-update   - Update DO App Platform app"
            echo "  app-deploy   - Trigger new deployment"
            echo "  app-logs     - View app logs"
            echo "  docker-build - Build Docker images"
            echo "  docker-push  - Push images to registry"
            echo "  migrate      - Run database migrations"
            echo "  health       - Run health checks"
            echo "  outputs      - Show Terraform outputs"
            exit 1
            ;;
    esac
}

main "$@"
