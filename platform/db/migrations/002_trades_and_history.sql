-- Migration: 002_trades_and_history.sql
-- Description: Trade history, price snapshots, and analytics tables
-- Created: 2026-01-24
--
-- This migration adds tables for tracking trades, price history,
-- and analytics to support the platform UI components.

-------------------------------------------------------------------------------
-- ENUM TYPES
-------------------------------------------------------------------------------

-- Trade direction
CREATE TYPE trade_direction AS ENUM (
    'buy',          -- Buying compute resources with tokens
    'sell'          -- Selling compute resources for tokens
);

-- Trade execution route
CREATE TYPE trade_route AS ENUM (
    'amm',          -- Executed via AMM pool
    'orderbook',    -- Executed via order book
    'hybrid'        -- Split between AMM and order book
);

-------------------------------------------------------------------------------
-- TRADE HISTORY
-------------------------------------------------------------------------------

-- Trades: All executed trades on the platform
CREATE TABLE trades (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),

    -- Relationships
    pool_id UUID NOT NULL REFERENCES compute_pools(id) ON DELETE CASCADE,
    user_address VARCHAR(66) NOT NULL,

    -- Trade details
    direction trade_direction NOT NULL,
    route trade_route NOT NULL,
    resource_type resource_type NOT NULL,

    -- Amounts
    amount_in NUMERIC(78, 0) NOT NULL,              -- Input amount (tokens or resources)
    amount_out NUMERIC(78, 0) NOT NULL,             -- Output amount (resources or tokens)
    effective_price NUMERIC(78, 0) NOT NULL,        -- Actual price achieved
    price_impact NUMERIC(10, 6) NOT NULL DEFAULT 0, -- Price impact percentage

    -- Fees
    trading_fee NUMERIC(78, 0) NOT NULL DEFAULT 0,
    protocol_fee NUMERIC(78, 0) NOT NULL DEFAULT 0,

    -- Slippage
    slippage_tolerance NUMERIC(10, 6) NOT NULL,
    actual_slippage NUMERIC(10, 6) NOT NULL DEFAULT 0,

    -- Execution details
    offer_ids UUID[],                               -- Matched offers (if orderbook)
    tx_hash VARCHAR(66),                            -- Blockchain transaction hash

    -- Timestamps
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),

    -- Constraints
    CONSTRAINT positive_amounts CHECK (amount_in > 0 AND amount_out > 0),
    CONSTRAINT positive_price CHECK (effective_price > 0)
);

-------------------------------------------------------------------------------
-- PRICE HISTORY
-------------------------------------------------------------------------------

-- Price snapshots for charting (1-minute candles)
CREATE TABLE price_snapshots (
    id BIGSERIAL PRIMARY KEY,

    -- Relationships
    pool_id UUID NOT NULL REFERENCES compute_pools(id) ON DELETE CASCADE,

    -- Time bucket
    bucket_time TIMESTAMPTZ NOT NULL,               -- Start of time bucket
    bucket_interval INTERVAL NOT NULL DEFAULT '1 minute',

    -- OHLCV data
    open_price NUMERIC(78, 0) NOT NULL,
    high_price NUMERIC(78, 0) NOT NULL,
    low_price NUMERIC(78, 0) NOT NULL,
    close_price NUMERIC(78, 0) NOT NULL,
    volume NUMERIC(78, 0) NOT NULL DEFAULT 0,
    trade_count INTEGER NOT NULL DEFAULT 0,

    -- Additional metrics
    liquidity_at_close NUMERIC(78, 0),
    utilization_at_close NUMERIC(5, 2),

    -- Constraints
    UNIQUE (pool_id, bucket_time, bucket_interval)
);

-- Aggregated price snapshots (hourly)
CREATE TABLE price_snapshots_hourly (
    id BIGSERIAL PRIMARY KEY,

    pool_id UUID NOT NULL REFERENCES compute_pools(id) ON DELETE CASCADE,
    bucket_time TIMESTAMPTZ NOT NULL,

    open_price NUMERIC(78, 0) NOT NULL,
    high_price NUMERIC(78, 0) NOT NULL,
    low_price NUMERIC(78, 0) NOT NULL,
    close_price NUMERIC(78, 0) NOT NULL,
    volume NUMERIC(78, 0) NOT NULL DEFAULT 0,
    trade_count INTEGER NOT NULL DEFAULT 0,

    UNIQUE (pool_id, bucket_time)
);

-- Aggregated price snapshots (daily)
CREATE TABLE price_snapshots_daily (
    id BIGSERIAL PRIMARY KEY,

    pool_id UUID NOT NULL REFERENCES compute_pools(id) ON DELETE CASCADE,
    bucket_date DATE NOT NULL,

    open_price NUMERIC(78, 0) NOT NULL,
    high_price NUMERIC(78, 0) NOT NULL,
    low_price NUMERIC(78, 0) NOT NULL,
    close_price NUMERIC(78, 0) NOT NULL,
    volume NUMERIC(78, 0) NOT NULL DEFAULT 0,
    trade_count INTEGER NOT NULL DEFAULT 0,

    -- Daily stats
    unique_traders INTEGER NOT NULL DEFAULT 0,
    avg_trade_size NUMERIC(78, 0),

    UNIQUE (pool_id, bucket_date)
);

-------------------------------------------------------------------------------
-- LIQUIDITY EVENTS
-------------------------------------------------------------------------------

-- Liquidity events: adds, removes, and position changes
CREATE TABLE liquidity_events (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),

    -- Relationships
    pool_id UUID NOT NULL REFERENCES compute_pools(id) ON DELETE CASCADE,
    membership_id UUID NOT NULL REFERENCES pool_memberships(id) ON DELETE CASCADE,
    user_address VARCHAR(66) NOT NULL,

    -- Event type
    event_type VARCHAR(16) NOT NULL,                -- 'add', 'remove', 'claim_rewards'

    -- Amounts
    resource_amount NUMERIC(78, 0) NOT NULL DEFAULT 0,
    token_amount NUMERIC(78, 0) NOT NULL DEFAULT 0,
    lp_tokens_minted NUMERIC(78, 0) NOT NULL DEFAULT 0,
    lp_tokens_burned NUMERIC(78, 0) NOT NULL DEFAULT 0,

    -- Position snapshot
    total_shares_after NUMERIC(78, 0) NOT NULL,
    pool_share_percent NUMERIC(10, 6) NOT NULL,

    -- Transaction
    tx_hash VARCHAR(66),

    -- Timestamps
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),

    -- Constraints
    CONSTRAINT valid_event_type CHECK (event_type IN ('add', 'remove', 'claim_rewards'))
);

-------------------------------------------------------------------------------
-- DEPLOYMENT ALLOCATIONS
-------------------------------------------------------------------------------

-- Resource allocations for deployments
CREATE TABLE resource_allocations (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),

    -- Relationships
    pool_id UUID NOT NULL REFERENCES compute_pools(id) ON DELETE CASCADE,
    node_id UUID NOT NULL REFERENCES compute_nodes(id) ON DELETE CASCADE,
    offer_id UUID REFERENCES compute_offers(id) ON DELETE SET NULL,
    consumer_address VARCHAR(66) NOT NULL,

    -- Allocation details
    resource_type resource_type NOT NULL,
    allocated_amount NUMERIC(78, 0) NOT NULL,
    price_per_unit_hour NUMERIC(78, 0) NOT NULL,

    -- Duration
    start_time TIMESTAMPTZ NOT NULL,
    end_time TIMESTAMPTZ NOT NULL,
    actual_end_time TIMESTAMPTZ,

    -- Payment tracking
    total_cost NUMERIC(78, 0) NOT NULL,
    paid_amount NUMERIC(78, 0) NOT NULL DEFAULT 0,
    escrow_amount NUMERIC(78, 0) NOT NULL DEFAULT 0,

    -- Status
    status VARCHAR(16) NOT NULL DEFAULT 'active',   -- 'pending', 'active', 'completed', 'cancelled', 'disputed'

    -- Performance
    uptime_achieved NUMERIC(5, 2),
    sla_met BOOLEAN,

    -- Timestamps
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),

    -- Constraints
    CONSTRAINT valid_allocation_status CHECK (status IN ('pending', 'active', 'completed', 'cancelled', 'disputed')),
    CONSTRAINT valid_duration CHECK (end_time > start_time)
);

-------------------------------------------------------------------------------
-- INDEXES FOR PERFORMANCE
-------------------------------------------------------------------------------

-- trades indexes
CREATE INDEX idx_trades_pool ON trades(pool_id);
CREATE INDEX idx_trades_user ON trades(user_address);
CREATE INDEX idx_trades_direction ON trades(direction);
CREATE INDEX idx_trades_route ON trades(route);
CREATE INDEX idx_trades_created ON trades(created_at DESC);
CREATE INDEX idx_trades_pool_time ON trades(pool_id, created_at DESC);
CREATE INDEX idx_trades_user_time ON trades(user_address, created_at DESC);
CREATE INDEX idx_trades_tx_hash ON trades(tx_hash) WHERE tx_hash IS NOT NULL;

-- price_snapshots indexes
CREATE INDEX idx_snapshots_pool_time ON price_snapshots(pool_id, bucket_time DESC);
CREATE INDEX idx_snapshots_bucket ON price_snapshots(bucket_time DESC);

CREATE INDEX idx_snapshots_hourly_pool_time ON price_snapshots_hourly(pool_id, bucket_time DESC);
CREATE INDEX idx_snapshots_daily_pool_date ON price_snapshots_daily(pool_id, bucket_date DESC);

-- liquidity_events indexes
CREATE INDEX idx_liquidity_pool ON liquidity_events(pool_id);
CREATE INDEX idx_liquidity_membership ON liquidity_events(membership_id);
CREATE INDEX idx_liquidity_user ON liquidity_events(user_address);
CREATE INDEX idx_liquidity_type ON liquidity_events(event_type);
CREATE INDEX idx_liquidity_created ON liquidity_events(created_at DESC);

-- resource_allocations indexes
CREATE INDEX idx_allocations_pool ON resource_allocations(pool_id);
CREATE INDEX idx_allocations_node ON resource_allocations(node_id);
CREATE INDEX idx_allocations_consumer ON resource_allocations(consumer_address);
CREATE INDEX idx_allocations_status ON resource_allocations(status);
CREATE INDEX idx_allocations_active ON resource_allocations(status, end_time)
    WHERE status = 'active';
CREATE INDEX idx_allocations_time_range ON resource_allocations(start_time, end_time);

-------------------------------------------------------------------------------
-- TRIGGERS
-------------------------------------------------------------------------------

CREATE TRIGGER update_resource_allocations_updated_at
    BEFORE UPDATE ON resource_allocations
    FOR EACH ROW EXECUTE FUNCTION update_updated_at_column();

-------------------------------------------------------------------------------
-- VIEWS FOR COMMON QUERIES
-------------------------------------------------------------------------------

-- Active pools with computed metrics
CREATE VIEW v_active_pools AS
SELECT
    cp.*,
    COALESCE(pm_counts.provider_count, 0) AS provider_count,
    COALESCE(pm_counts.lp_count, 0) AS lp_count,
    COALESCE(t24.trade_count, 0) AS trades_24h,
    COALESCE(t24.unique_traders, 0) AS unique_traders_24h
FROM compute_pools cp
LEFT JOIN (
    SELECT
        pool_id,
        COUNT(*) FILTER (WHERE role = 'provider') AS provider_count,
        COUNT(*) FILTER (WHERE role = 'liquidity') AS lp_count
    FROM pool_memberships
    WHERE is_active = true
    GROUP BY pool_id
) pm_counts ON pm_counts.pool_id = cp.id
LEFT JOIN (
    SELECT
        pool_id,
        COUNT(*) AS trade_count,
        COUNT(DISTINCT user_address) AS unique_traders
    FROM trades
    WHERE created_at > NOW() - INTERVAL '24 hours'
    GROUP BY pool_id
) t24 ON t24.pool_id = cp.id
WHERE cp.status = 'active';

-- User portfolio view
CREATE VIEW v_user_portfolio AS
SELECT
    pm.user_address,
    pm.pool_id,
    cp.name AS pool_name,
    cp.resource_type,
    pm.role,
    pm.liquidity_shares,
    pm.resource_deposited,
    pm.token_deposited,
    pm.entry_price,
    pm.pending_rewards,
    pm.claimed_rewards,
    pm.member_sla_score,
    -- Calculated fields
    CASE
        WHEN cp.total_liquidity > 0
        THEN (pm.liquidity_shares::NUMERIC / cp.total_liquidity::NUMERIC * 100)
        ELSE 0
    END AS pool_share_percent,
    pm.created_at AS position_opened_at
FROM pool_memberships pm
JOIN compute_pools cp ON cp.id = pm.pool_id
WHERE pm.is_active = true;

-- Node performance summary
CREATE VIEW v_node_performance AS
SELECT
    cn.id AS node_id,
    cn.owner_address,
    cn.name,
    cn.status,
    cn.sla_score,
    cn.uptime_percent,
    cn.total_tasks_completed,
    cn.total_tasks_failed,
    COALESCE(co.active_offers, 0) AS active_offers,
    COALESCE(co.total_capacity, 0) AS total_capacity_offered,
    COALESCE(ra.active_allocations, 0) AS active_allocations,
    COALESCE(c.pending_challenges, 0) AS pending_challenges,
    COALESCE(c.failed_challenges_30d, 0) AS failed_challenges_30d
FROM compute_nodes cn
LEFT JOIN (
    SELECT
        node_id,
        COUNT(*) AS active_offers,
        SUM(amount_remaining) AS total_capacity
    FROM compute_offers
    WHERE status = 'active'
    GROUP BY node_id
) co ON co.node_id = cn.id
LEFT JOIN (
    SELECT
        node_id,
        COUNT(*) AS active_allocations
    FROM resource_allocations
    WHERE status = 'active'
    GROUP BY node_id
) ra ON ra.node_id = cn.id
LEFT JOIN (
    SELECT
        node_id,
        COUNT(*) FILTER (WHERE status = 'pending') AS pending_challenges,
        COUNT(*) FILTER (WHERE status = 'failed' AND created_at > NOW() - INTERVAL '30 days') AS failed_challenges_30d
    FROM challenges
    GROUP BY node_id
) c ON c.node_id = cn.id;

-- Pool leaderboard
CREATE VIEW v_pool_leaderboard AS
SELECT
    cp.id,
    cp.name,
    cp.resource_type,
    cp.token_amount AS tvl,
    cp.volume_24h,
    cp.apr,
    cp.utilization,
    cp.avg_sla_score,
    RANK() OVER (ORDER BY cp.volume_24h DESC) AS volume_rank,
    RANK() OVER (ORDER BY cp.token_amount DESC) AS tvl_rank,
    RANK() OVER (ORDER BY cp.apr DESC) AS apr_rank
FROM compute_pools cp
WHERE cp.status = 'active';

-------------------------------------------------------------------------------
-- COMMENTS
-------------------------------------------------------------------------------

COMMENT ON TABLE trades IS 'All executed trades on the compute pools platform';
COMMENT ON TABLE price_snapshots IS 'Price history snapshots (1-minute candles)';
COMMENT ON TABLE price_snapshots_hourly IS 'Aggregated hourly price data';
COMMENT ON TABLE price_snapshots_daily IS 'Aggregated daily price data';
COMMENT ON TABLE liquidity_events IS 'Liquidity add/remove events';
COMMENT ON TABLE resource_allocations IS 'Active compute resource allocations';

COMMENT ON VIEW v_active_pools IS 'Active pools with computed metrics';
COMMENT ON VIEW v_user_portfolio IS 'User liquidity positions across pools';
COMMENT ON VIEW v_node_performance IS 'Compute node performance summary';
COMMENT ON VIEW v_pool_leaderboard IS 'Pool rankings by various metrics';
