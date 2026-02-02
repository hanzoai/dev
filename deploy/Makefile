# Hanzo Platform - Deployment Makefile
# Commands for local development and DO deployment

.PHONY: help dev up down logs build push deploy plan apply destroy clean

# Default environment
ENV ?= staging
REGISTRY ?= registry.digitalocean.com/hanzo
TAG ?= latest

# Colors
BLUE := \033[0;34m
GREEN := \033[0;32m
YELLOW := \033[1;33m
NC := \033[0m

help: ## Show this help
	@echo "$(BLUE)Hanzo Platform - Deployment Commands$(NC)"
	@echo ""
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "  $(GREEN)%-15s$(NC) %s\n", $$1, $$2}'
	@echo ""
	@echo "Variables:"
	@echo "  ENV=$(ENV)       Environment (staging, production)"
	@echo "  TAG=$(TAG)     Docker image tag"

# ==========================================================================
# Local Development
# ==========================================================================

dev: ## Start local development stack
	@echo "$(BLUE)Starting local development stack...$(NC)"
	docker compose -f compose.yml up -d
	@echo ""
	@echo "$(GREEN)Services started:$(NC)"
	@echo "  Console:      http://localhost:3001"
	@echo "  Node API:     http://localhost:8080"
	@echo "  Platform API: http://localhost:8000"
	@echo "  PostgreSQL:   localhost:5432"
	@echo "  Redis:        localhost:6379"
	@echo "  MinIO:        http://localhost:9002"

dev-full: ## Start with monitoring and dev tools
	@echo "$(BLUE)Starting full development stack...$(NC)"
	docker compose -f compose.yml --profile monitoring --profile dev-tools up -d
	@echo ""
	@echo "$(GREEN)Additional services:$(NC)"
	@echo "  Prometheus:   http://localhost:9091"
	@echo "  Grafana:      http://localhost:3000 (admin/hanzo)"
	@echo "  pgAdmin:      http://localhost:5050"
	@echo "  Redis Cmd:    http://localhost:8081"

up: dev ## Alias for dev

down: ## Stop local development stack
	@echo "$(BLUE)Stopping development stack...$(NC)"
	docker compose -f compose.yml --profile monitoring --profile dev-tools down

logs: ## Follow logs for all services
	docker compose -f compose.yml logs -f

logs-node: ## Follow hanzo-node logs
	docker compose -f compose.yml logs -f hanzo-node

logs-console: ## Follow console logs
	docker compose -f compose.yml logs -f console

status: ## Show service status
	docker compose -f compose.yml ps

clean: ## Stop and remove all volumes
	@echo "$(YELLOW)WARNING: This will delete all data!$(NC)"
	@read -p "Are you sure? [y/N] " confirm && [ "$$confirm" = "y" ] || exit 1
	docker compose -f compose.yml --profile monitoring --profile dev-tools down -v
	@echo "$(GREEN)Cleanup complete$(NC)"

# ==========================================================================
# Docker Build
# ==========================================================================

build: build-node build-console build-platform ## Build all Docker images

build-node: ## Build hanzo-node image
	@echo "$(BLUE)Building hanzo-node...$(NC)"
	docker build -t $(REGISTRY)/hanzo-node:$(TAG) -f ../hanzo-node/Dockerfile ../hanzo-node

build-console: ## Build console image
	@echo "$(BLUE)Building console...$(NC)"
	docker build -t $(REGISTRY)/console:$(TAG) -f ../console/Dockerfile ../console

build-platform: ## Build platform-api image
	@echo "$(BLUE)Building platform-api...$(NC)"
	docker build -t $(REGISTRY)/platform-api:$(TAG) -f ../platform/Dockerfile ../platform

push: ## Push images to DO registry
	@echo "$(BLUE)Pushing images to registry...$(NC)"
	doctl registry login
	docker push $(REGISTRY)/hanzo-node:$(TAG)
	docker push $(REGISTRY)/console:$(TAG)
	docker push $(REGISTRY)/platform-api:$(TAG)
	@echo "$(GREEN)Images pushed$(NC)"

# ==========================================================================
# Terraform Infrastructure
# ==========================================================================

tf-init: ## Initialize Terraform
	@echo "$(BLUE)Initializing Terraform...$(NC)"
	cd terraform && terraform init -upgrade

plan: tf-init ## Plan infrastructure changes
	@echo "$(BLUE)Planning infrastructure for $(ENV)...$(NC)"
	cd terraform && terraform plan \
		-var="environment=$(ENV)" \
		-out=tfplan.$(ENV)
	@echo "$(GREEN)Plan saved to terraform/tfplan.$(ENV)$(NC)"

apply: ## Apply infrastructure changes
	@echo "$(BLUE)Applying infrastructure for $(ENV)...$(NC)"
	cd terraform && terraform apply tfplan.$(ENV)
	@echo "$(GREEN)Infrastructure applied$(NC)"

destroy: ## Destroy infrastructure (DANGEROUS)
	@echo "$(YELLOW)WARNING: This will destroy all infrastructure for $(ENV)!$(NC)"
	@read -p "Type 'destroy' to confirm: " confirm && [ "$$confirm" = "destroy" ] || exit 1
	cd terraform && terraform destroy \
		-var="environment=$(ENV)" \
		-auto-approve

outputs: ## Show Terraform outputs
	cd terraform && terraform output

# ==========================================================================
# DO App Platform
# ==========================================================================

app-validate: ## Validate DO app spec
	doctl apps spec validate do/app.yaml

app-create: app-validate ## Create DO App Platform app
	@echo "$(BLUE)Creating DO App Platform app...$(NC)"
	doctl apps create --spec do/app.yaml

app-update: app-validate ## Update DO App Platform app
	@echo "$(BLUE)Updating DO App Platform app...$(NC)"
	$(eval APP_ID := $(shell doctl apps list --format ID,Spec.Name --no-header | grep hanzo-platform | awk '{print $$1}'))
	doctl apps update $(APP_ID) --spec do/app.yaml

app-deploy: ## Trigger new deployment
	@echo "$(BLUE)Triggering deployment...$(NC)"
	$(eval APP_ID := $(shell doctl apps list --format ID,Spec.Name --no-header | grep hanzo-platform | awk '{print $$1}'))
	doctl apps create-deployment $(APP_ID)

app-logs: ## View app logs
	$(eval APP_ID := $(shell doctl apps list --format ID,Spec.Name --no-header | grep hanzo-platform | awk '{print $$1}'))
	doctl apps logs $(APP_ID) --follow --type=RUN

app-status: ## View app status
	$(eval APP_ID := $(shell doctl apps list --format ID,Spec.Name --no-header | grep hanzo-platform | awk '{print $$1}'))
	doctl apps get $(APP_ID)

# ==========================================================================
# Database
# ==========================================================================

migrate: ## Run database migrations
	@echo "$(BLUE)Running database migrations...$(NC)"
	cd ../platform/db && ./migrate.sh

migrate-local: ## Run migrations against local DB
	DATABASE_URL=postgresql://hanzo:hanzo@localhost:5432/hanzo \
		cd ../platform/db && ./migrate.sh

psql: ## Connect to local PostgreSQL
	docker exec -it hanzo-postgres psql -U hanzo -d hanzo

redis-cli: ## Connect to local Redis
	docker exec -it hanzo-redis redis-cli

# ==========================================================================
# Health Checks
# ==========================================================================

health: ## Run health checks
	@echo "$(BLUE)Running health checks...$(NC)"
	@echo -n "Console: " && curl -sf http://localhost:3001/api/health && echo " OK" || echo " FAILED"
	@echo -n "Node:    " && curl -sf http://localhost:8080/health && echo " OK" || echo " FAILED"
	@echo -n "Platform:" && curl -sf http://localhost:8000/health && echo " OK" || echo " FAILED"

health-prod: ## Check production health
	@echo "$(BLUE)Checking production health...$(NC)"
	@echo -n "Console: " && curl -sf https://console.hanzo.network/api/health && echo " OK" || echo " FAILED"
	@echo -n "Node:    " && curl -sf https://api.hanzo.network/health && echo " OK" || echo " FAILED"

# ==========================================================================
# Full Deployment
# ==========================================================================

deploy: build push app-deploy ## Build, push, and deploy
	@echo "$(GREEN)Deployment complete!$(NC)"

deploy-infra: plan apply app-update app-deploy ## Deploy infrastructure and app
	@echo "$(GREEN)Full deployment complete!$(NC)"
