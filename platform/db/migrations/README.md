# Platform Database Migrations

This directory contains PostgreSQL migrations for the Hanzo compute pools platform.

## Migration Files

| File | Description |
|------|-------------|
| `001_initial_schema.sql` | Core tables: compute_pools, compute_nodes, compute_offers, pool_memberships, challenges, rewards |
| `002_trades_and_history.sql` | Trade history, price snapshots, liquidity events, resource allocations, and analytics views |
| `003_functions_and_procedures.sql` | AMM calculation functions, reward distribution, challenge processing, best price routing |
| `004_seed_data.sql` | Initial seed data for development (8 pools, 3 nodes, sample offers/memberships) |

## Schema Overview

### Core Tables

```
compute_pools        - AMM liquidity pools for compute resources
compute_nodes        - Registered compute providers
compute_offers       - Order book entries from providers
pool_memberships     - LP positions and provider stakes
challenges           - SLA verification challenges
rewards              - Reward distributions to participants
```

### Supporting Tables

```
trades               - All executed trades
price_snapshots      - 1-minute price candles
price_snapshots_hourly - Aggregated hourly data
price_snapshots_daily  - Aggregated daily data
liquidity_events     - Add/remove liquidity events
resource_allocations - Active compute allocations
```

### Views

```
v_active_pools       - Active pools with computed metrics
v_user_portfolio     - User liquidity positions
v_node_performance   - Node performance summary
v_pool_leaderboard   - Pool rankings
```

## Resource Types

The platform supports 8 resource types:

| Type | Code | Description |
|------|------|-------------|
| CPU | 0 | CPU compute units |
| GPU | 1 | GPU compute units |
| Memory | 2 | Memory in MB |
| Storage | 3 | Storage in GB |
| Bandwidth | 4 | Bandwidth in Mbps |
| WASM | 5 | WebAssembly execution units |
| Docker | 6 | Docker container slots |
| K8S | 7 | Kubernetes pod slots |

## Running Migrations

### Using psql directly

```bash
# Set your connection string
export DATABASE_URL="postgresql://user:pass@localhost:5432/platform"

# Run all migrations in order
psql $DATABASE_URL -f 001_initial_schema.sql
psql $DATABASE_URL -f 002_trades_and_history.sql
psql $DATABASE_URL -f 003_functions_and_procedures.sql
psql $DATABASE_URL -f 004_seed_data.sql
```

### Using the migration script

```bash
cd /Users/z/work/hanzo/dev/platform/db
./migrate.sh up
```

### Using Docker

```bash
docker compose -f compose.yml exec postgres psql -U hanzo -d platform -f /migrations/001_initial_schema.sql
```

## Key Functions

### AMM Calculations

```sql
-- Calculate swap output
SELECT * FROM calculate_swap_output(pool_id, amount_in, is_buying);

-- Calculate LP tokens for adding liquidity
SELECT calculate_lp_tokens_to_mint(pool_id, resource_amount, token_amount);

-- Calculate amounts for removing liquidity
SELECT * FROM calculate_liquidity_removal(pool_id, lp_tokens);

-- Calculate impermanent loss
SELECT calculate_impermanent_loss(entry_price, current_price);
```

### Price Routing

```sql
-- Get best price across AMM and order book
SELECT * FROM get_best_price('GPU'::resource_type, true, amount);
```

### Rewards

```sql
-- Calculate trading fee rewards
SELECT calculate_trading_fee_rewards(membership_id);

-- Calculate pool APR
SELECT calculate_pool_apr(pool_id);
```

### Challenges

```sql
-- Create a challenge
SELECT create_challenge(node_id, pool_id, 'availability', '{"test": true}'::jsonb);

-- Process challenge response
SELECT process_challenge_response(challenge_id, response_data, score);

-- Auto-fail expired challenges
SELECT fail_expired_challenges();
```

## Numeric Precision

All token and resource amounts are stored as `NUMERIC(78, 0)` to match Ethereum's uint256 with 18 decimal places (wei precision).

Example values:
- 1 token = `1000000000000000000` (10^18)
- 0.001 token = `1000000000000000` (10^15)

## Indexes

The schema includes comprehensive indexes for:
- Primary key lookups
- Foreign key relationships
- Status-based queries
- Time-range queries
- Sorted listings (by volume, APR, etc.)
- Composite indexes for common query patterns

## Development Notes

1. **Seed Data**: Migration 004 includes seed data for development. Remove or modify for production.

2. **Extensions Required**:
   - `uuid-ossp` for UUID generation
   - `pgcrypto` for cryptographic functions

3. **Triggers**: All tables with `updated_at` columns have automatic update triggers.

4. **Constraints**: Business rules are enforced via CHECK constraints (positive amounts, valid percentages, etc.).

## Version

Schema version: 1.0.0
Last updated: 2026-01-24
