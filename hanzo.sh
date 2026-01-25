#!/usr/bin/env bash
# hanzo.sh - Full-stack Hanzo installation script
# Supports macOS (arm64/x64) and Linux (x64/arm64)
#
# Usage:
#   curl -fsSL https://get.hanzo.ai | bash
#   curl -fsSL https://get.hanzo.ai | bash -s -- --components all
#   ./hanzo.sh [options]
#
# Options:
#   --dev          Install development dependencies and build from source
#   --prod         Production mode (use pre-built binaries) [default]
#   --components   Comma-separated list: dev,llm,chat,mcp,hanzod (default: dev,mcp)
#   --no-docker    Skip Docker-based services
#   --uninstall    Remove Hanzo installation
#   --version      Show version information
#   --quiet        Minimal output (for scripted installs)
#   --force        Skip confirmation prompts
#   --help         Show this help message
#
# Environment:
#   HANZO_HOME     Base directory (default: ~/.hanzo)
#   HANZO_VERSION  Version to install (default: latest)

set -euo pipefail

# Determine script directory (works even when piped from curl)
SCRIPT_DIR="${BASH_SOURCE[0]:-}"
if [[ -n "$SCRIPT_DIR" ]] && [[ -f "$SCRIPT_DIR" ]]; then
    SCRIPT_DIR="$(cd "$(dirname "$SCRIPT_DIR")" && pwd)"
else
    SCRIPT_DIR=""
fi

# Colors for output (disabled if not a terminal or in quiet mode)
if [[ -t 1 ]] && [[ "${QUIET:-false}" != "true" ]]; then
    RED='\033[0;31m'
    GREEN='\033[0;32m'
    YELLOW='\033[0;33m'
    BLUE='\033[0;34m'
    MAGENTA='\033[0;35m'
    CYAN='\033[0;36m'
    NC='\033[0m' # No Color
    BOLD='\033[1m'
else
    RED='' GREEN='' YELLOW='' BLUE='' MAGENTA='' CYAN='' NC='' BOLD=''
fi

# Configuration
HANZO_HOME="${HANZO_HOME:-$HOME/.hanzo}"
HANZO_BIN="${HANZO_BIN:-$HANZO_HOME/bin}"
HANZO_CONFIG="${HANZO_CONFIG:-$HANZO_HOME/config}"
HANZO_DATA="${HANZO_DATA:-$HANZO_HOME/data}"
HANZO_LOGS="${HANZO_LOGS:-$HANZO_HOME/logs}"

# Default options
MODE="prod"
COMPONENTS="dev,mcp"
SKIP_DOCKER=false
VERBOSE=false
QUIET=false
FORCE=false
UNINSTALL=false

# Version info
HANZO_VERSION="${HANZO_VERSION:-latest}"
SCRIPT_VERSION="1.1.0"
NODE_VERSION="20.18.1"
# shellcheck disable=SC2034
PYTHON_VERSION="3.11"
RUST_VERSION="stable"

# Cleanup trap for partial installations
CLEANUP_NEEDED=false
cleanup_on_error() {
    if [[ "$CLEANUP_NEEDED" == "true" ]]; then
        log_warn "Installation interrupted. Partial installation may remain at $HANZO_HOME"
        log_info "Run './hanzo.sh --uninstall' to clean up, then retry."
    fi
}
trap cleanup_on_error EXIT

# Logging functions
log_info() { [[ "$QUIET" == "true" ]] || echo -e "${BLUE}[INFO]${NC} $*"; }
log_success() { [[ "$QUIET" == "true" ]] || echo -e "${GREEN}[OK]${NC} $*"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $*"; }
log_error() { echo -e "${RED}[ERROR]${NC} $*" >&2; }
log_step() { [[ "$QUIET" == "true" ]] || echo -e "${MAGENTA}[STEP]${NC} ${BOLD}$*${NC}"; }
log_debug() { [[ "$VERBOSE" == "true" ]] && echo -e "${CYAN}[DEBUG]${NC} $*" || true; }

# Print banner
print_banner() {
    [[ "$QUIET" == "true" ]] && return 0
    echo -e "${CYAN}"
    cat << 'EOF'
    __  __
   / / / /___ _____  ____  ____
  / /_/ / __ `/ __ \/_  / / __ \
 / __  / /_/ / / / / / /_/ /_/ /
/_/ /_/\__,_/_/ /_/ /___/\____/

    AI Infrastructure Platform
EOF
    echo -e "${NC}"
    echo -e "${BOLD}Hanzo Full-Stack Installer v${SCRIPT_VERSION}${NC}"
    echo ""
}

# Show version
show_version() {
    echo "hanzo.sh version $SCRIPT_VERSION"
    echo "Hanzo installer for version: $HANZO_VERSION"
}

# Show help
show_help() {
    cat << 'EOF'
Hanzo Full-Stack Installation Script

USAGE:
    hanzo.sh [OPTIONS]
    curl -fsSL https://get.hanzo.ai | bash
    curl -fsSL https://get.hanzo.ai | bash -s -- [OPTIONS]

OPTIONS:
    --dev               Development mode (build from source)
    --prod              Production mode (pre-built binaries) [default]
    --components LIST   Components to install (comma-separated)
                        Available: dev,llm,chat,mcp,hanzod,all
                        Default: dev,mcp
    --no-docker         Skip Docker-based services (llm, chat)
    --uninstall         Remove Hanzo installation
    --verbose           Enable verbose output
    --quiet             Minimal output (for scripted installs)
    --force             Skip confirmation prompts
    --version           Show version information
    --help              Show this help message

COMPONENTS:
    dev                 Hanzo Dev CLI - AI coding assistant
    llm                 LLM Gateway - Unified proxy for 100+ LLM providers
    chat                Hanzo Chat - LibreChat fork with MCP integration
    mcp                 MCP Tools - Model Context Protocol server
    hanzod              Hanzod - AI blockchain daemon

ENVIRONMENT VARIABLES:
    HANZO_HOME          Base directory (default: ~/.hanzo)
    HANZO_VERSION       Version to install (default: latest)
    OPENAI_API_KEY      OpenAI API key
    ANTHROPIC_API_KEY   Anthropic API key

EXAMPLES:
    # Quick install (dev CLI + MCP)
    ./hanzo.sh

    # Full stack with all components
    ./hanzo.sh --components all

    # Development setup (build from source)
    ./hanzo.sh --dev --components dev,mcp,llm

    # Production with specific components
    ./hanzo.sh --prod --components dev,llm,chat

    # Scripted install (quiet mode)
    curl -fsSL https://get.hanzo.ai | bash -s -- --quiet --force

    # Uninstall
    ./hanzo.sh --uninstall

For more information: https://hanzo.ai/docs
EOF
}

# Detect platform
detect_platform() {
    local os arch

    os="$(uname -s)"
    arch="$(uname -m)"

    case "$os" in
        Darwin) PLATFORM="darwin" ;;
        Linux)  PLATFORM="linux" ;;
        MINGW*|MSYS*|CYGWIN*)
            log_error "Windows is not directly supported. Use WSL2."
            log_info "Install WSL2: https://docs.microsoft.com/en-us/windows/wsl/install"
            exit 1
            ;;
        *)
            log_error "Unsupported operating system: $os"
            exit 1
            ;;
    esac

    case "$arch" in
        x86_64|amd64) ARCH="x64" ;;
        aarch64|arm64) ARCH="arm64" ;;
        *)
            log_error "Unsupported architecture: $arch"
            exit 1
            ;;
    esac

    log_info "Detected platform: $PLATFORM-$ARCH"
}

# Uninstall Hanzo
uninstall_hanzo() {
    log_step "Uninstalling Hanzo..."

    # Confirm uninstall
    if [[ "$FORCE" != "true" ]]; then
        echo -e "${YELLOW}This will remove:${NC}"
        echo "  - $HANZO_HOME (all Hanzo data)"
        echo "  - Global npm packages (@hanzo/dev, @hanzo/mcp)"
        echo ""
        read -rp "Are you sure? [y/N] " confirm
        if [[ "$confirm" != "y" && "$confirm" != "Y" ]]; then
            log_info "Uninstall cancelled"
            exit 0
        fi
    fi

    # Remove npm packages
    if check_command npm; then
        log_info "Removing npm packages..."
        npm uninstall -g @hanzo/dev 2>/dev/null || true
        npm uninstall -g @hanzo/mcp 2>/dev/null || true
    fi

    # Stop Docker services if running
    if check_command docker; then
        for dir in "$HANZO_DATA/llm" "$HANZO_DATA/chat"; do
            if [[ -f "$dir/compose.yml" ]]; then
                log_info "Stopping services in $dir..."
                (cd "$dir" && docker compose down 2>/dev/null) || true
            fi
        done
    fi

    # Remove Hanzo home directory
    if [[ -d "$HANZO_HOME" ]]; then
        log_info "Removing $HANZO_HOME..."
        rm -rf "$HANZO_HOME"
    fi

    # Clean up shell config (careful not to break user's config)
    for rc_file in "$HOME/.zshrc" "$HOME/.bashrc" "$HOME/.profile"; do
        if [[ -f "$rc_file" ]] && grep -q "# Hanzo AI - Added by hanzo.sh" "$rc_file"; then
            log_info "Cleaning shell config in $rc_file..."
            # Remove the Hanzo block (from comment to empty line or EOF)
            sed -i.bak '/# Hanzo AI - Added by hanzo.sh/,/^$/d' "$rc_file" 2>/dev/null || \
            sed -i '' '/# Hanzo AI - Added by hanzo.sh/,/^$/d' "$rc_file" 2>/dev/null || true
            rm -f "${rc_file}.bak"
        fi
    done

    log_success "Hanzo has been uninstalled"
    log_info "Please restart your shell or run: exec \$SHELL"
}

# Check for required commands
check_command() {
    command -v "$1" >/dev/null 2>&1
}

# Install Homebrew on macOS
install_homebrew() {
    if check_command brew; then
        log_success "Homebrew already installed"
        return 0
    fi

    log_step "Installing Homebrew..."
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

    # Add to PATH for current session
    if [[ "$ARCH" == "arm64" ]]; then
        eval "$(/opt/homebrew/bin/brew shellenv)"
    else
        eval "$(/usr/local/bin/brew shellenv)"
    fi

    log_success "Homebrew installed"
}

# Install system dependencies
install_system_deps() {
    log_step "Installing system dependencies..."

    if [[ "$PLATFORM" == "darwin" ]]; then
        install_homebrew

        # Core dependencies
        local deps=(git curl wget jq openssl)

        # Development dependencies
        if [[ "$MODE" == "dev" ]]; then
            deps+=(cmake pkg-config protobuf)
        fi

        for dep in "${deps[@]}"; do
            if ! check_command "$dep"; then
                log_info "Installing $dep..."
                brew install "$dep" 2>/dev/null || true
            fi
        done

        log_success "System dependencies installed"

    elif [[ "$PLATFORM" == "linux" ]]; then
        # Detect package manager
        if check_command apt-get; then
            PKG_MANAGER="apt"
            sudo apt-get update -qq
            sudo apt-get install -y -qq git curl wget jq build-essential libssl-dev pkg-config
            if [[ "$MODE" == "dev" ]]; then
                sudo apt-get install -y -qq cmake protobuf-compiler
            fi
        elif check_command yum; then
            PKG_MANAGER="yum"
            sudo yum install -y git curl wget jq gcc gcc-c++ openssl-devel pkgconfig
            if [[ "$MODE" == "dev" ]]; then
                sudo yum install -y cmake protobuf-compiler
            fi
        elif check_command dnf; then
            PKG_MANAGER="dnf"
            sudo dnf install -y git curl wget jq gcc gcc-c++ openssl-devel pkgconfig
            if [[ "$MODE" == "dev" ]]; then
                sudo dnf install -y cmake protobuf-compiler
            fi
        elif check_command pacman; then
            PKG_MANAGER="pacman"
            sudo pacman -Sy --noconfirm git curl wget jq base-devel openssl pkgconf
            if [[ "$MODE" == "dev" ]]; then
                sudo pacman -S --noconfirm cmake protobuf
            fi
        else
            log_warn "Unknown package manager. Please install dependencies manually."
        fi

        log_success "System dependencies installed"
    fi
}

# Install Node.js
install_nodejs() {
    log_step "Setting up Node.js..."

    if check_command node; then
        local current_version
        current_version=$(node --version | sed 's/v//' | cut -d. -f1)
        if [[ "$current_version" -ge 20 ]]; then
            log_success "Node.js $(node --version) already installed"
            return 0
        fi
    fi

    if [[ "$PLATFORM" == "darwin" ]]; then
        # Use Homebrew for Node.js
        brew install node@20 2>/dev/null || true
        brew link node@20 --force --overwrite 2>/dev/null || true
    else
        # Use n for version management
        if ! check_command n; then
            curl -fsSL https://raw.githubusercontent.com/tj/n/master/bin/n | sudo bash -s lts
        fi
        sudo n "$NODE_VERSION"
    fi

    # Install pnpm
    if ! check_command pnpm; then
        log_info "Installing pnpm..."
        npm install -g pnpm 2>/dev/null || true
    fi

    log_success "Node.js $(node --version) ready"
}

# Install Python
install_python() {
    log_step "Setting up Python..."

    if check_command python3; then
        local current_version
        current_version=$(python3 --version | awk '{print $2}' | cut -d. -f1,2)
        if [[ "$current_version" == "3.11" ]] || [[ "$current_version" == "3.12" ]]; then
            log_success "Python $current_version already installed"
            install_uv
            return 0
        fi
    fi

    if [[ "$PLATFORM" == "darwin" ]]; then
        brew install python@3.11 2>/dev/null || true
        brew link python@3.11 --force --overwrite 2>/dev/null || true
    else
        if check_command apt-get; then
            sudo apt-get install -y -qq python3.11 python3.11-venv python3.11-dev
        elif check_command yum || check_command dnf; then
            sudo "${PKG_MANAGER:-dnf}" install -y python3.11 python3.11-devel
        fi
    fi

    install_uv
    log_success "Python ready"
}

# Install uv (fast Python package manager)
install_uv() {
    if check_command uv; then
        log_success "uv already installed"
        return 0
    fi

    log_info "Installing uv..."
    curl -LsSf https://astral.sh/uv/install.sh | sh
    export PATH="$HOME/.cargo/bin:$PATH"
    log_success "uv installed"
}

# Install Rust
install_rust() {
    log_step "Setting up Rust..."

    if check_command rustc; then
        log_success "Rust $(rustc --version | awk '{print $2}') already installed"
        return 0
    fi

    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y --default-toolchain "$RUST_VERSION"
    # shellcheck source=/dev/null
    source "$HOME/.cargo/env"

    log_success "Rust $(rustc --version | awk '{print $2}') installed"
}

# Install Docker
install_docker() {
    if [[ "$SKIP_DOCKER" == true ]]; then
        log_info "Skipping Docker installation (--no-docker flag)"
        return 0
    fi

    log_step "Setting up Docker..."

    if check_command docker; then
        if docker info >/dev/null 2>&1; then
            log_success "Docker already installed and running"
            return 0
        else
            log_warn "Docker installed but not running. Please start Docker."
        fi
        return 0
    fi

    if [[ "$PLATFORM" == "darwin" ]]; then
        log_info "Installing Docker Desktop..."
        brew install --cask docker 2>/dev/null || true
        log_warn "Please open Docker Desktop to complete setup"
    else
        log_info "Installing Docker Engine..."
        curl -fsSL https://get.docker.com | sh
        sudo usermod -aG docker "$USER" 2>/dev/null || true
        sudo systemctl enable docker 2>/dev/null || true
        sudo systemctl start docker 2>/dev/null || true
        log_warn "You may need to log out and back in for Docker permissions"
    fi

    # Install docker-compose if needed
    if ! check_command docker-compose && ! docker compose version >/dev/null 2>&1; then
        if [[ "$PLATFORM" == "linux" ]]; then
            sudo curl -L "https://github.com/docker/compose/releases/latest/download/docker-compose-$(uname -s)-$(uname -m)" \
                -o /usr/local/bin/docker-compose
            sudo chmod +x /usr/local/bin/docker-compose
        fi
    fi

    log_success "Docker setup complete"
}

# Create directory structure
create_directories() {
    log_step "Creating Hanzo directories..."

    mkdir -p "$HANZO_HOME"
    mkdir -p "$HANZO_BIN"
    mkdir -p "$HANZO_CONFIG"
    mkdir -p "$HANZO_DATA"
    mkdir -p "$HANZO_LOGS"

    log_success "Directories created at $HANZO_HOME"
}

# Install Hanzo Dev CLI
install_dev() {
    log_step "Installing Hanzo Dev CLI..."

    if [[ "$MODE" == "dev" ]]; then
        # Build from source - try multiple locations
        local source_dir=""

        # Priority 1: Script directory (if running from repo)
        if [[ -n "$SCRIPT_DIR" ]] && [[ -f "$SCRIPT_DIR/build-fast.sh" ]]; then
            source_dir="$SCRIPT_DIR"
        # Priority 2: Current directory
        elif [[ -f "./build-fast.sh" ]]; then
            source_dir="$(pwd)"
        # Priority 3: Common development path
        elif [[ -d "$HOME/work/hanzo/dev" ]] && [[ -f "$HOME/work/hanzo/dev/build-fast.sh" ]]; then
            source_dir="$HOME/work/hanzo/dev"
        fi

        if [[ -n "$source_dir" ]]; then
            log_info "Building from source at $source_dir..."
            local original_dir
            original_dir="$(pwd)"
            cd "$source_dir"

            if ./build-fast.sh; then
                local bin_path="./code-rs/target/dev-fast/code"
                if [[ -f "$bin_path" ]]; then
                    cp "$bin_path" "$HANZO_BIN/dev"
                    chmod +x "$HANZO_BIN/dev"
                    log_success "Built and installed dev CLI from source"
                    cd "$original_dir"
                    return 0
                else
                    log_warn "Build completed but binary not found at $bin_path"
                fi
            else
                log_warn "Build failed"
            fi
            cd "$original_dir"
        fi

        log_warn "Source build not available, falling back to binary install"
    fi

    # Production mode: Install from npm (pre-built binary)
    log_info "Installing from npm..."
    if npm install -g @hanzo/dev 2>/dev/null; then
        log_success "Hanzo Dev CLI installed via npm"
        return 0
    fi

    # Fallback: try with pnpm
    if check_command pnpm && pnpm add -g @hanzo/dev 2>/dev/null; then
        log_success "Hanzo Dev CLI installed via pnpm"
        return 0
    fi

    # Final fallback: direct binary download
    log_info "Package managers failed, downloading binary directly..."
    if download_dev_binary; then
        log_success "Hanzo Dev CLI installed via direct download"
        return 0
    fi

    log_error "Failed to install Hanzo Dev CLI"
    return 1
}

# Download pre-built dev binary
download_dev_binary() {
    local triple
    case "$PLATFORM-$ARCH" in
        darwin-arm64) triple="aarch64-apple-darwin" ;;
        darwin-x64)   triple="x86_64-apple-darwin" ;;
        linux-x64)    triple="x86_64-unknown-linux-musl" ;;
        linux-arm64)  triple="aarch64-unknown-linux-musl" ;;
        *)
            log_error "Unsupported platform for binary download: $PLATFORM-$ARCH"
            return 1
            ;;
    esac

    local base_url="https://github.com/hanzoai/dev/releases"
    local version_path="latest/download"

    # If specific version requested, use that
    if [[ "$HANZO_VERSION" != "latest" ]]; then
        version_path="download/v${HANZO_VERSION}"
    fi

    local url="${base_url}/${version_path}/code-${triple}.tar.gz"
    local tmp_dir
    tmp_dir="$(mktemp -d)"
    local tmp_file="${tmp_dir}/hanzo-dev.tar.gz"

    log_info "Downloading from $url..."
    log_debug "Temp directory: $tmp_dir"

    # Download with retry logic
    local max_retries=3
    local retry=0
    while [[ $retry -lt $max_retries ]]; do
        if curl -fsSL --connect-timeout 30 --max-time 300 "$url" -o "$tmp_file" 2>/dev/null; then
            break
        fi
        retry=$((retry + 1))
        if [[ $retry -lt $max_retries ]]; then
            log_warn "Download failed, retrying ($retry/$max_retries)..."
            sleep 2
        fi
    done

    if [[ ! -f "$tmp_file" ]]; then
        log_error "Failed to download binary after $max_retries attempts"
        rm -rf "$tmp_dir"
        return 1
    fi

    # Verify it's a valid gzip file
    if ! file "$tmp_file" | grep -q "gzip"; then
        log_error "Downloaded file is not a valid gzip archive"
        log_debug "File type: $(file "$tmp_file")"
        rm -rf "$tmp_dir"
        return 1
    fi

    # Extract to temp directory first
    log_info "Extracting binary..."
    if ! tar -xzf "$tmp_file" -C "$tmp_dir" 2>/dev/null; then
        log_error "Failed to extract archive"
        rm -rf "$tmp_dir"
        return 1
    fi

    # Find the binary (handle various naming conventions)
    local binary_path=""
    for candidate in "$tmp_dir/code" "$tmp_dir/code-${triple}" "$tmp_dir/dev"; do
        if [[ -f "$candidate" ]]; then
            binary_path="$candidate"
            break
        fi
    done

    # Also check subdirectories
    if [[ -z "$binary_path" ]]; then
        binary_path="$(find "$tmp_dir" -type f -name 'code' -o -name 'dev' 2>/dev/null | head -1)"
    fi

    if [[ -z "$binary_path" ]] || [[ ! -f "$binary_path" ]]; then
        log_error "Binary not found in archive"
        log_debug "Archive contents: $(ls -la "$tmp_dir")"
        rm -rf "$tmp_dir"
        return 1
    fi

    # Install to HANZO_BIN
    cp "$binary_path" "$HANZO_BIN/dev"
    chmod +x "$HANZO_BIN/dev"

    # Verify the binary works
    if ! "$HANZO_BIN/dev" --version >/dev/null 2>&1; then
        log_warn "Binary installed but version check failed (this may be ok)"
    fi

    # Cleanup
    rm -rf "$tmp_dir"

    log_debug "Binary installed to $HANZO_BIN/dev"
    return 0
}

# Install MCP Tools
install_mcp() {
    log_step "Installing MCP Tools..."

    npm install -g @hanzo/mcp 2>/dev/null || pnpm add -g @hanzo/mcp 2>/dev/null || {
        log_warn "Failed to install MCP from npm"
    }

    log_success "MCP Tools installed"
}

# Install LLM Gateway
install_llm() {
    if [[ "$SKIP_DOCKER" == true ]]; then
        log_warn "Skipping LLM Gateway (requires Docker)"
        return 0
    fi

    log_step "Installing LLM Gateway..."

    local llm_dir="$HANZO_DATA/llm"
    mkdir -p "$llm_dir"

    # Create docker-compose.yml for LLM Gateway
    cat > "$llm_dir/compose.yml" << 'YAML'
services:
  llm:
    image: ghcr.io/hanzoai/llm:latest
    container_name: hanzo-llm
    ports:
      - "4000:4000"
    environment:
      - DATABASE_URL=postgresql://hanzo:hanzo@db:5432/llm
      - REDIS_HOST=redis
      - MASTER_KEY=${HANZO_LLM_MASTER_KEY:-}
      - OPENAI_API_KEY=${OPENAI_API_KEY:-}
      - ANTHROPIC_API_KEY=${ANTHROPIC_API_KEY:-}
    depends_on:
      - db
      - redis
    restart: unless-stopped

  db:
    image: postgres:16-alpine
    container_name: hanzo-llm-db
    environment:
      - POSTGRES_USER=hanzo
      - POSTGRES_PASSWORD=hanzo
      - POSTGRES_DB=llm
    volumes:
      - llm_pgdata:/var/lib/postgresql/data
    restart: unless-stopped

  redis:
    image: redis:7-alpine
    container_name: hanzo-llm-redis
    restart: unless-stopped

volumes:
  llm_pgdata:
YAML

    # Create .env file
    cat > "$llm_dir/.env" << ENV
HANZO_LLM_MASTER_KEY=${HANZO_LLM_MASTER_KEY:-$(openssl rand -hex 16)}
OPENAI_API_KEY=${OPENAI_API_KEY:-}
ANTHROPIC_API_KEY=${ANTHROPIC_API_KEY:-}
ENV

    log_info "LLM Gateway configuration created at $llm_dir"
    log_info "Start with: cd $llm_dir && docker compose up -d"
    log_success "LLM Gateway installed"
}

# Install Hanzo Chat
install_chat() {
    if [[ "$SKIP_DOCKER" == true ]]; then
        log_warn "Skipping Hanzo Chat (requires Docker)"
        return 0
    fi

    log_step "Installing Hanzo Chat..."

    local chat_dir="$HANZO_DATA/chat"
    mkdir -p "$chat_dir"

    # Create docker-compose.yml for Chat
    cat > "$chat_dir/compose.yml" << 'YAML'
services:
  api:
    image: ghcr.io/danny-avila/librechat-dev:latest
    container_name: hanzo-chat
    ports:
      - "3081:3081"
    environment:
      - HOST=0.0.0.0
      - PORT=3081
      - MONGO_URI=mongodb://mongodb:27017/HanzoChat
      - MEILI_HOST=http://meilisearch:7700
      - RAG_PORT=8000
      - RAG_API_URL=http://rag_api:8000
    volumes:
      - ./images:/app/client/public/images
      - ./uploads:/app/uploads
      - ./logs:/app/api/logs
      - ./.env:/app/.env
    depends_on:
      - mongodb
      - meilisearch
    restart: unless-stopped

  mongodb:
    image: mongo:7
    container_name: hanzo-chat-mongodb
    volumes:
      - chat_mongo:/data/db
    restart: unless-stopped

  meilisearch:
    image: getmeili/meilisearch:v1.12.3
    container_name: hanzo-chat-meilisearch
    environment:
      - MEILI_NO_ANALYTICS=true
    volumes:
      - chat_meili:/meili_data
    restart: unless-stopped

  vectordb:
    image: ankane/pgvector:latest
    container_name: hanzo-chat-vectordb
    environment:
      - POSTGRES_DB=hanzo_chat
      - POSTGRES_USER=hanzo
      - POSTGRES_PASSWORD=hanzo
    volumes:
      - chat_pgvector:/var/lib/postgresql/data
    restart: unless-stopped

  rag_api:
    image: ghcr.io/danny-avila/librechat-rag-api-dev-lite:latest
    container_name: hanzo-chat-rag
    environment:
      - DB_HOST=vectordb
      - RAG_PORT=8000
    depends_on:
      - vectordb
    restart: unless-stopped

volumes:
  chat_mongo:
  chat_meili:
  chat_pgvector:
YAML

    # Create .env file
    cat > "$chat_dir/.env" << ENV
OPENAI_API_KEY=${OPENAI_API_KEY:-}
ANTHROPIC_API_KEY=${ANTHROPIC_API_KEY:-}
ENV

    log_info "Hanzo Chat configuration created at $chat_dir"
    log_info "Start with: cd $chat_dir && docker compose up -d"
    log_success "Hanzo Chat installed"
}

# Install Hanzod
install_hanzod() {
    log_step "Installing Hanzod..."

    if [[ "$MODE" == "dev" ]]; then
        # Build from source requires the hanzo-dev directory
        if [[ -d "/Users/z/work/hanzo/dev/hanzo-dev" ]]; then
            log_info "Building Hanzod from source..."
            cd /Users/z/work/hanzo/dev/hanzo-dev
            cargo build --release
            cp target/release/hanzod "$HANZO_BIN/" 2>/dev/null || true
        else
            log_warn "Hanzod source not found"
        fi
    else
        log_info "Hanzod binary distribution not yet available"
        log_info "Build from source with: ./hanzo.sh --dev --components hanzod"
    fi

    log_success "Hanzod setup complete"
}

# Setup shell integration
setup_shell() {
    log_step "Setting up shell integration..."

    local shell_config
    case "$SHELL" in
        */zsh)  shell_config="$HOME/.zshrc" ;;
        */bash) shell_config="$HOME/.bashrc" ;;
        *)      shell_config="$HOME/.profile" ;;
    esac

    # Check if already configured (look for the comment marker)
    if grep -q "# Hanzo AI - Added by hanzo.sh" "$shell_config" 2>/dev/null; then
        log_info "Shell already configured in $shell_config"
        return 0
    fi

    # Also check if HANZO_HOME is set via any means
    if grep -q "export HANZO_HOME=" "$shell_config" 2>/dev/null; then
        log_info "HANZO_HOME already set in $shell_config"
        return 0
    fi

    # Add to shell config
    cat >> "$shell_config" << EOF

# Hanzo AI - Added by hanzo.sh installer
export HANZO_HOME="$HANZO_HOME"
export PATH="\$HANZO_HOME/bin:\$PATH"
EOF

    log_success "Shell integration added to $shell_config"
    log_info "Run: source $shell_config"
}

# Generate configuration
generate_config() {
    log_step "Generating configuration..."

    # Main Hanzo config
    cat > "$HANZO_CONFIG/config.toml" << TOML
# Hanzo AI Configuration
# Generated by hanzo.sh installer

[general]
home = "$HANZO_HOME"
log_level = "info"

[dev]
# Dev CLI settings
model = "gpt-4.1"
model_provider = "openai"
approval_policy = "on-request"
model_reasoning_effort = "medium"

[llm]
# LLM Gateway settings
port = 4000
database_url = "postgresql://hanzo:hanzo@localhost:5432/llm"

[chat]
# Hanzo Chat settings
port = 3081

[mcp]
# MCP server settings
enabled = true
TOML

    log_success "Configuration generated at $HANZO_CONFIG/config.toml"
}

# Print completion message
print_completion() {
    # In quiet mode, just output success indicator
    if [[ "$QUIET" == "true" ]]; then
        echo "OK"
        return 0
    fi

    echo ""
    echo -e "${GREEN}${BOLD}Installation Complete!${NC}"
    echo ""
    echo "Installed components:"
    for comp in ${COMPONENTS//,/ }; do
        echo -e "  ${GREEN}*${NC} $comp"
    done
    echo ""
    echo -e "${BOLD}Quick Start:${NC}"
    echo ""
    echo "  1. Reload your shell:"

    # Detect shell for accurate instructions
    case "$SHELL" in
        */zsh)  echo "     source ~/.zshrc" ;;
        */bash) echo "     source ~/.bashrc" ;;
        *)      echo "     source ~/.profile  # or restart your terminal" ;;
    esac

    echo ""
    echo "  2. Set API keys (recommended):"
    echo "     export OPENAI_API_KEY=sk-..."
    echo "     export ANTHROPIC_API_KEY=sk-ant-..."
    echo ""
    echo "  3. Start coding:"
    echo "     dev"
    echo ""

    local step=4
    if [[ "$COMPONENTS" == *"llm"* ]] && [[ "$SKIP_DOCKER" == false ]]; then
        echo "  $step. Start LLM Gateway:"
        echo "     cd $HANZO_DATA/llm && docker compose up -d"
        echo "     Access at http://localhost:4000"
        echo ""
        step=$((step + 1))
    fi

    if [[ "$COMPONENTS" == *"chat"* ]] && [[ "$SKIP_DOCKER" == false ]]; then
        echo "  $step. Start Hanzo Chat:"
        echo "     cd $HANZO_DATA/chat && docker compose up -d"
        echo "     Open http://localhost:3081"
        echo ""
    fi

    echo -e "${BOLD}Documentation:${NC} https://hanzo.ai/docs"
    echo -e "${BOLD}Support:${NC} https://github.com/hanzoai/dev/issues"
    echo ""
}

# Parse command line arguments
parse_args() {
    while [[ $# -gt 0 ]]; do
        case "$1" in
            --dev)
                MODE="dev"
                shift
                ;;
            --prod)
                MODE="prod"
                shift
                ;;
            --components)
                if [[ -z "${2:-}" ]]; then
                    log_error "--components requires a value"
                    exit 1
                fi
                COMPONENTS="$2"
                shift 2
                ;;
            --no-docker)
                SKIP_DOCKER=true
                shift
                ;;
            --verbose)
                VERBOSE=true
                export VERBOSE
                set -x
                shift
                ;;
            --quiet|-q)
                QUIET=true
                export QUIET
                shift
                ;;
            --force|-f)
                FORCE=true
                export FORCE
                shift
                ;;
            --uninstall)
                UNINSTALL=true
                shift
                ;;
            --version|-V)
                show_version
                exit 0
                ;;
            --help|-h)
                show_help
                exit 0
                ;;
            -*)
                log_error "Unknown option: $1"
                log_info "Run './hanzo.sh --help' for usage information"
                exit 1
                ;;
            *)
                # Allow positional arguments for curl | bash -s -- usage
                log_warn "Ignoring unknown argument: $1"
                shift
                ;;
        esac
    done

    # Handle 'all' components
    if [[ "$COMPONENTS" == "all" ]]; then
        COMPONENTS="dev,llm,chat,mcp,hanzod"
    fi

    # Validate components
    local valid_components="dev llm chat mcp hanzod"
    for component in ${COMPONENTS//,/ }; do
        # shellcheck disable=SC2076
        if [[ ! " $valid_components " =~ " $component " ]]; then
            log_error "Unknown component: $component"
            log_info "Valid components: $valid_components"
            exit 1
        fi
    done
}

# Verify installation
verify_installation() {
    log_step "Verifying installation..."

    local failed=false

    # Check HANZO_BIN is in PATH or add it temporarily
    export PATH="$HANZO_BIN:$PATH"

    # Check dev CLI if installed
    if [[ "$COMPONENTS" == *"dev"* ]]; then
        if [[ -x "$HANZO_BIN/dev" ]]; then
            log_success "dev CLI binary found at $HANZO_BIN/dev"
        elif check_command dev; then
            log_success "dev CLI found in PATH"
        else
            log_warn "dev CLI not found - may need shell restart"
            failed=true
        fi
    fi

    # Check MCP if installed
    if [[ "$COMPONENTS" == *"mcp"* ]]; then
        if check_command hanzo-mcp 2>/dev/null || npm list -g @hanzo/mcp >/dev/null 2>&1; then
            log_success "MCP tools installed"
        else
            log_warn "MCP tools may not be fully installed"
        fi
    fi

    # Check Docker services
    if [[ "$SKIP_DOCKER" == false ]]; then
        if [[ "$COMPONENTS" == *"llm"* ]] && [[ -f "$HANZO_DATA/llm/compose.yml" ]]; then
            log_success "LLM Gateway config ready at $HANZO_DATA/llm"
        fi
        if [[ "$COMPONENTS" == *"chat"* ]] && [[ -f "$HANZO_DATA/chat/compose.yml" ]]; then
            log_success "Hanzo Chat config ready at $HANZO_DATA/chat"
        fi
    fi

    # Check config
    if [[ -f "$HANZO_CONFIG/config.toml" ]]; then
        log_success "Configuration file created"
    fi

    if [[ "$failed" == "true" ]]; then
        log_warn "Some verifications failed - installation may be incomplete"
        return 1
    fi

    return 0
}

# Main installation function
main() {
    # Parse args first (before banner, to handle --quiet, --version, --help early)
    parse_args "$@"

    # Handle uninstall
    if [[ "$UNINSTALL" == "true" ]]; then
        uninstall_hanzo
        exit 0
    fi

    print_banner

    log_info "Installation mode: $MODE"
    log_info "Components: $COMPONENTS"
    log_info "Skip Docker: $SKIP_DOCKER"
    [[ "$QUIET" != "true" ]] && echo ""

    # Mark that cleanup may be needed
    CLEANUP_NEEDED=true

    # Detect platform
    detect_platform

    # Create directories
    create_directories

    # Install base dependencies
    install_system_deps
    install_nodejs

    # Install optional dependencies based on mode
    if [[ "$MODE" == "dev" ]]; then
        install_rust
    fi

    # Install Docker if needed
    if [[ "$COMPONENTS" == *"llm"* ]] || [[ "$COMPONENTS" == *"chat"* ]]; then
        install_docker
    fi

    # Install components
    local install_failed=false
    for component in ${COMPONENTS//,/ }; do
        case "$component" in
            dev)
                install_dev || install_failed=true
                ;;
            llm)
                install_llm || install_failed=true
                ;;
            chat)
                install_chat || install_failed=true
                ;;
            mcp)
                install_mcp || install_failed=true
                ;;
            hanzod)
                install_hanzod || install_failed=true
                ;;
        esac
    done

    if [[ "$install_failed" == "true" ]]; then
        log_warn "Some components failed to install"
    fi

    # Setup shell and config
    setup_shell
    generate_config

    # Verify installation
    verify_installation || true

    # Mark cleanup not needed (successful install)
    CLEANUP_NEEDED=false

    # Done
    print_completion
}

# Run main function
main "$@"
