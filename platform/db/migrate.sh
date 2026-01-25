#!/usr/bin/env bash
#
# Platform Database Migration Script
# Runs PostgreSQL migrations for the Hanzo compute pools platform
#
# Usage:
#   ./migrate.sh up              # Run all pending migrations
#   ./migrate.sh down            # Rollback (drops all tables - use with caution!)
#   ./migrate.sh reset           # Drop and recreate all tables
#   ./migrate.sh status          # Show migration status
#   ./migrate.sh seed            # Run seed data only
#

set -euo pipefail

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MIGRATIONS_DIR="${SCRIPT_DIR}/migrations"
DATABASE_URL="${DATABASE_URL:-postgresql://hanzo:hanzo@localhost:5432/platform}"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Run a migration file
run_migration() {
    local file="$1"
    local filename=$(basename "$file")

    log_info "Running migration: ${filename}"

    if psql "${DATABASE_URL}" -f "$file" > /dev/null 2>&1; then
        log_success "Completed: ${filename}"
        return 0
    else
        log_error "Failed: ${filename}"
        psql "${DATABASE_URL}" -f "$file"  # Run again to show errors
        return 1
    fi
}

# Run all migrations
migrate_up() {
    log_info "Running all migrations..."

    for file in "${MIGRATIONS_DIR}"/[0-9]*.sql; do
        if [[ -f "$file" ]]; then
            run_migration "$file" || exit 1
        fi
    done

    log_success "All migrations completed successfully!"
}

# Drop all tables (dangerous!)
migrate_down() {
    log_warning "This will DROP ALL TABLES in the database!"
    read -p "Are you sure? Type 'yes' to confirm: " confirm

    if [[ "$confirm" != "yes" ]]; then
        log_info "Aborted."
        exit 0
    fi

    log_info "Dropping all tables..."

    psql "${DATABASE_URL}" <<EOF
-- Drop all views
DROP VIEW IF EXISTS v_pool_leaderboard CASCADE;
DROP VIEW IF EXISTS v_node_performance CASCADE;
DROP VIEW IF EXISTS v_user_portfolio CASCADE;
DROP VIEW IF EXISTS v_active_pools CASCADE;

-- Drop all tables
DROP TABLE IF EXISTS rewards CASCADE;
DROP TABLE IF EXISTS challenges CASCADE;
DROP TABLE IF EXISTS pool_memberships CASCADE;
DROP TABLE IF EXISTS resource_allocations CASCADE;
DROP TABLE IF EXISTS liquidity_events CASCADE;
DROP TABLE IF EXISTS price_snapshots_daily CASCADE;
DROP TABLE IF EXISTS price_snapshots_hourly CASCADE;
DROP TABLE IF EXISTS price_snapshots CASCADE;
DROP TABLE IF EXISTS trades CASCADE;
DROP TABLE IF EXISTS compute_offers CASCADE;
DROP TABLE IF EXISTS compute_nodes CASCADE;
DROP TABLE IF EXISTS compute_pools CASCADE;

-- Drop all types
DROP TYPE IF EXISTS membership_role CASCADE;
DROP TYPE IF EXISTS challenge_status CASCADE;
DROP TYPE IF EXISTS offer_status CASCADE;
DROP TYPE IF EXISTS node_status CASCADE;
DROP TYPE IF EXISTS pool_status CASCADE;
DROP TYPE IF EXISTS resource_type CASCADE;
DROP TYPE IF EXISTS trade_direction CASCADE;
DROP TYPE IF EXISTS trade_route CASCADE;

-- Drop functions
DROP FUNCTION IF EXISTS update_updated_at_column CASCADE;
DROP FUNCTION IF EXISTS calculate_swap_output CASCADE;
DROP FUNCTION IF EXISTS calculate_lp_tokens_to_mint CASCADE;
DROP FUNCTION IF EXISTS calculate_liquidity_removal CASCADE;
DROP FUNCTION IF EXISTS calculate_impermanent_loss CASCADE;
DROP FUNCTION IF EXISTS calculate_trading_fee_rewards CASCADE;
DROP FUNCTION IF EXISTS calculate_pool_apr CASCADE;
DROP FUNCTION IF EXISTS create_challenge CASCADE;
DROP FUNCTION IF EXISTS process_challenge_response CASCADE;
DROP FUNCTION IF EXISTS fail_expired_challenges CASCADE;
DROP FUNCTION IF EXISTS update_pool_after_trade CASCADE;
DROP FUNCTION IF EXISTS recalculate_pool_price_changes CASCADE;
DROP FUNCTION IF EXISTS get_best_price CASCADE;
EOF

    log_success "All tables dropped."
}

# Reset database (down + up)
migrate_reset() {
    migrate_down
    migrate_up
}

# Show migration status
migrate_status() {
    log_info "Checking database status..."

    echo ""
    echo "=== Tables ==="
    psql "${DATABASE_URL}" -c "
        SELECT tablename, pg_size_pretty(pg_total_relation_size(schemaname||'.'||tablename)) as size
        FROM pg_tables
        WHERE schemaname = 'public'
        ORDER BY tablename;
    " 2>/dev/null || log_warning "Could not query tables (database may not exist)"

    echo ""
    echo "=== Row Counts ==="
    psql "${DATABASE_URL}" -c "
        SELECT 'compute_pools' as table_name, COUNT(*) as rows FROM compute_pools
        UNION ALL SELECT 'compute_nodes', COUNT(*) FROM compute_nodes
        UNION ALL SELECT 'compute_offers', COUNT(*) FROM compute_offers
        UNION ALL SELECT 'pool_memberships', COUNT(*) FROM pool_memberships
        UNION ALL SELECT 'challenges', COUNT(*) FROM challenges
        UNION ALL SELECT 'rewards', COUNT(*) FROM rewards
        UNION ALL SELECT 'trades', COUNT(*) FROM trades
        ORDER BY table_name;
    " 2>/dev/null || log_warning "Tables not found"
}

# Run seed data only
migrate_seed() {
    log_info "Running seed data..."
    run_migration "${MIGRATIONS_DIR}/004_seed_data.sql"
}

# Main command handler
main() {
    local command="${1:-help}"

    case "$command" in
        up)
            migrate_up
            ;;
        down)
            migrate_down
            ;;
        reset)
            migrate_reset
            ;;
        status)
            migrate_status
            ;;
        seed)
            migrate_seed
            ;;
        help|--help|-h)
            echo "Platform Database Migration Script"
            echo ""
            echo "Usage: $0 <command>"
            echo ""
            echo "Commands:"
            echo "  up      Run all pending migrations"
            echo "  down    Drop all tables (use with caution!)"
            echo "  reset   Drop and recreate all tables"
            echo "  status  Show migration status"
            echo "  seed    Run seed data only"
            echo "  help    Show this help message"
            echo ""
            echo "Environment:"
            echo "  DATABASE_URL  PostgreSQL connection string"
            echo "                Default: postgresql://hanzo:hanzo@localhost:5432/platform"
            ;;
        *)
            log_error "Unknown command: $command"
            echo "Run '$0 help' for usage information."
            exit 1
            ;;
    esac
}

main "$@"
