-- Migration: 001_initial_schema.sql
-- Description: Core platform database schema for compute pools marketplace
-- Created: 2026-01-24
--
-- This migration creates the foundational tables for the Hanzo compute pools
-- platform, enabling decentralized compute resource trading with AMM liquidity.

-- Enable required extensions
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
CREATE EXTENSION IF NOT EXISTS "pgcrypto";

-------------------------------------------------------------------------------
-- ENUM TYPES
-------------------------------------------------------------------------------

-- Resource types supported by the platform
CREATE TYPE resource_type AS ENUM (
    'CPU',      -- 0: CPU compute units
    'GPU',      -- 1: GPU compute units
    'Memory',   -- 2: Memory in MB
    'Storage',  -- 3: Storage in GB
    'Bandwidth', -- 4: Bandwidth in Mbps
    'WASM',     -- 5: WebAssembly execution units
    'Docker',   -- 6: Docker container slots
    'K8S'       -- 7: Kubernetes pod slots
);

-- Pool status for lifecycle management
CREATE TYPE pool_status AS ENUM (
    'active',       -- Pool is operational and accepting trades
    'paused',       -- Pool temporarily paused (admin action)
    'deprecated',   -- Pool being phased out, no new liquidity
    'closed'        -- Pool permanently closed
);

-- Node status for compute providers
CREATE TYPE node_status AS ENUM (
    'pending',      -- Node registered, awaiting verification
    'active',       -- Node verified and operational
    'suspended',    -- Node temporarily suspended (SLA violation)
    'offline',      -- Node disconnected
    'maintenance',  -- Node in planned maintenance
    'decommissioned' -- Node permanently removed
);

-- Offer status for compute offers
CREATE TYPE offer_status AS ENUM (
    'active',       -- Offer available for matching
    'partially_filled', -- Some capacity consumed
    'filled',       -- Fully consumed
    'expired',      -- Past expiration time
    'cancelled'     -- Manually cancelled by provider
);

-- Challenge status for SLA verification
CREATE TYPE challenge_status AS ENUM (
    'pending',      -- Challenge issued, awaiting response
    'passed',       -- Challenge successfully completed
    'failed',       -- Challenge failed
    'timeout',      -- No response within deadline
    'disputed'      -- Result contested, under review
);

-- Membership role in pool governance
CREATE TYPE membership_role AS ENUM (
    'provider',     -- Compute resource provider
    'liquidity',    -- Liquidity provider (tokens + resources)
    'consumer',     -- Resource consumer
    'operator'      -- Pool operator/admin
);

-------------------------------------------------------------------------------
-- CORE TABLES
-------------------------------------------------------------------------------

-- Compute Pools: AMM-style liquidity pools for compute resources
CREATE TABLE compute_pools (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),

    -- Pool identity
    name VARCHAR(128) NOT NULL,
    slug VARCHAR(64) NOT NULL UNIQUE,
    description TEXT,
    resource_type resource_type NOT NULL,

    -- Pool balances (stored as numeric for precision, represents wei/smallest unit)
    resource_amount NUMERIC(78, 0) NOT NULL DEFAULT 0,  -- Total resource units in pool
    token_amount NUMERIC(78, 0) NOT NULL DEFAULT 0,     -- Total HANZO tokens in pool
    total_liquidity NUMERIC(78, 0) NOT NULL DEFAULT 0,  -- LP token supply

    -- Pricing (k = resource_amount * token_amount for constant product)
    last_price NUMERIC(78, 0) NOT NULL DEFAULT 0,       -- Last trade price (tokens per unit)
    fee_rate NUMERIC(10, 8) NOT NULL DEFAULT 0.003,     -- Trading fee (0.3% default)

    -- Pool metrics
    volume_24h NUMERIC(78, 0) NOT NULL DEFAULT 0,
    volume_7d NUMERIC(78, 0) NOT NULL DEFAULT 0,
    volume_total NUMERIC(78, 0) NOT NULL DEFAULT 0,
    trade_count_24h INTEGER NOT NULL DEFAULT 0,
    trade_count_total BIGINT NOT NULL DEFAULT 0,

    -- Price history for charts
    price_change_1h NUMERIC(10, 6) NOT NULL DEFAULT 0,
    price_change_24h NUMERIC(10, 6) NOT NULL DEFAULT 0,
    price_change_7d NUMERIC(10, 6) NOT NULL DEFAULT 0,

    -- Pool health
    utilization NUMERIC(5, 2) NOT NULL DEFAULT 0,       -- Current utilization percentage
    avg_sla_score NUMERIC(5, 2) NOT NULL DEFAULT 100,   -- Average SLA score of providers
    apr NUMERIC(10, 4) NOT NULL DEFAULT 0,              -- Current APR for LPs

    -- Governance
    status pool_status NOT NULL DEFAULT 'active',
    min_stake NUMERIC(78, 0) NOT NULL DEFAULT 0,        -- Minimum stake to participate

    -- Timestamps
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),

    -- Constraints
    CONSTRAINT positive_resource CHECK (resource_amount >= 0),
    CONSTRAINT positive_token CHECK (token_amount >= 0),
    CONSTRAINT positive_liquidity CHECK (total_liquidity >= 0),
    CONSTRAINT valid_fee_rate CHECK (fee_rate >= 0 AND fee_rate <= 1),
    CONSTRAINT valid_utilization CHECK (utilization >= 0 AND utilization <= 100)
);

-- Compute Nodes: Individual compute providers
CREATE TABLE compute_nodes (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),

    -- Node identity
    owner_address VARCHAR(66) NOT NULL,                 -- Ethereum address (0x + 40 hex)
    name VARCHAR(128),
    description TEXT,

    -- Node capabilities (resources available)
    cpu_cores INTEGER NOT NULL DEFAULT 0,
    cpu_model VARCHAR(128),
    memory_mb BIGINT NOT NULL DEFAULT 0,
    storage_gb BIGINT NOT NULL DEFAULT 0,
    gpu_count INTEGER NOT NULL DEFAULT 0,
    gpu_model VARCHAR(128),
    bandwidth_mbps INTEGER NOT NULL DEFAULT 0,

    -- Container support
    supports_docker BOOLEAN NOT NULL DEFAULT false,
    supports_k8s BOOLEAN NOT NULL DEFAULT false,
    supports_wasm BOOLEAN NOT NULL DEFAULT false,

    -- Node location and network
    region VARCHAR(32),
    availability_zone VARCHAR(32),
    ip_address INET,
    p2p_address VARCHAR(256),                           -- libp2p multiaddress

    -- Attestation
    tee_type VARCHAR(32),                               -- SGX, TDX, SEV, etc.
    attestation_quote BYTEA,
    attestation_verified_at TIMESTAMPTZ,

    -- Performance metrics
    sla_score NUMERIC(5, 2) NOT NULL DEFAULT 100,
    uptime_percent NUMERIC(5, 2) NOT NULL DEFAULT 100,
    avg_response_time_ms INTEGER NOT NULL DEFAULT 0,
    total_tasks_completed BIGINT NOT NULL DEFAULT 0,
    total_tasks_failed BIGINT NOT NULL DEFAULT 0,

    -- Staking
    stake_amount NUMERIC(78, 0) NOT NULL DEFAULT 0,
    stake_locked_until TIMESTAMPTZ,

    -- Status
    status node_status NOT NULL DEFAULT 'pending',
    last_heartbeat_at TIMESTAMPTZ,

    -- Timestamps
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),

    -- Constraints
    CONSTRAINT valid_sla_score CHECK (sla_score >= 0 AND sla_score <= 100),
    CONSTRAINT valid_uptime CHECK (uptime_percent >= 0 AND uptime_percent <= 100),
    CONSTRAINT positive_stake CHECK (stake_amount >= 0)
);

-- Compute Offers: Order book entries from providers
CREATE TABLE compute_offers (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),

    -- Relationships
    node_id UUID NOT NULL REFERENCES compute_nodes(id) ON DELETE CASCADE,
    pool_id UUID REFERENCES compute_pools(id) ON DELETE SET NULL,

    -- Offer details
    resource_type resource_type NOT NULL,
    amount NUMERIC(78, 0) NOT NULL,                     -- Resource units offered
    amount_remaining NUMERIC(78, 0) NOT NULL,           -- Units still available
    price_per_unit NUMERIC(78, 0) NOT NULL,             -- Price in HANZO tokens per unit per hour

    -- Duration constraints
    min_duration_hours INTEGER NOT NULL DEFAULT 1,
    max_duration_hours INTEGER NOT NULL DEFAULT 720,    -- 30 days max

    -- SLA guarantees
    guaranteed_uptime NUMERIC(5, 2) NOT NULL DEFAULT 99.0,
    max_latency_ms INTEGER,

    -- Offer lifecycle
    status offer_status NOT NULL DEFAULT 'active',
    expires_at TIMESTAMPTZ NOT NULL,

    -- Timestamps
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),

    -- Constraints
    CONSTRAINT positive_amount CHECK (amount > 0),
    CONSTRAINT valid_remaining CHECK (amount_remaining >= 0 AND amount_remaining <= amount),
    CONSTRAINT valid_price CHECK (price_per_unit > 0),
    CONSTRAINT valid_duration CHECK (min_duration_hours > 0 AND max_duration_hours >= min_duration_hours),
    CONSTRAINT valid_uptime_guarantee CHECK (guaranteed_uptime >= 0 AND guaranteed_uptime <= 100)
);

-- Pool Memberships: LP positions and provider stakes
CREATE TABLE pool_memberships (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),

    -- Relationships
    pool_id UUID NOT NULL REFERENCES compute_pools(id) ON DELETE CASCADE,
    node_id UUID REFERENCES compute_nodes(id) ON DELETE SET NULL,  -- NULL for pure liquidity providers
    user_address VARCHAR(66) NOT NULL,                  -- Wallet address

    -- Membership type
    role membership_role NOT NULL,

    -- Liquidity position (for liquidity providers)
    liquidity_shares NUMERIC(78, 0) NOT NULL DEFAULT 0, -- LP tokens owned
    resource_deposited NUMERIC(78, 0) NOT NULL DEFAULT 0,
    token_deposited NUMERIC(78, 0) NOT NULL DEFAULT 0,
    entry_price NUMERIC(78, 0) NOT NULL DEFAULT 0,      -- Price at deposit (for IL calc)

    -- Rewards tracking
    pending_rewards NUMERIC(78, 0) NOT NULL DEFAULT 0,
    claimed_rewards NUMERIC(78, 0) NOT NULL DEFAULT 0,
    last_reward_claim_at TIMESTAMPTZ,

    -- Resource commitment (for providers)
    committed_resources NUMERIC(78, 0) NOT NULL DEFAULT 0,
    utilized_resources NUMERIC(78, 0) NOT NULL DEFAULT 0,

    -- Performance
    member_sla_score NUMERIC(5, 2) NOT NULL DEFAULT 100,

    -- Status
    is_active BOOLEAN NOT NULL DEFAULT true,
    joined_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    left_at TIMESTAMPTZ,

    -- Timestamps
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),

    -- Constraints
    CONSTRAINT positive_shares CHECK (liquidity_shares >= 0),
    CONSTRAINT positive_deposited CHECK (resource_deposited >= 0 AND token_deposited >= 0),
    CONSTRAINT valid_member_sla CHECK (member_sla_score >= 0 AND member_sla_score <= 100),

    -- Unique constraint: one membership per user per pool per role
    UNIQUE (pool_id, user_address, role)
);

-- Challenges: SLA verification proofs
CREATE TABLE challenges (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),

    -- Relationships
    node_id UUID NOT NULL REFERENCES compute_nodes(id) ON DELETE CASCADE,
    pool_id UUID REFERENCES compute_pools(id) ON DELETE SET NULL,
    membership_id UUID REFERENCES pool_memberships(id) ON DELETE SET NULL,

    -- Challenge details
    challenge_type VARCHAR(32) NOT NULL,                -- 'availability', 'performance', 'attestation'
    challenge_data JSONB NOT NULL,                      -- Challenge-specific parameters

    -- Response
    response_data JSONB,                                -- Node's response
    response_at TIMESTAMPTZ,

    -- Verification
    verified_by VARCHAR(66),                            -- Verifier address
    verification_proof BYTEA,                           -- Cryptographic proof
    verified_at TIMESTAMPTZ,

    -- Result
    status challenge_status NOT NULL DEFAULT 'pending',
    score NUMERIC(5, 2),                                -- Challenge score (0-100)
    penalty_amount NUMERIC(78, 0) DEFAULT 0,            -- Slashing amount if failed

    -- Timing
    deadline TIMESTAMPTZ NOT NULL,

    -- Timestamps
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),

    -- Constraints
    CONSTRAINT valid_challenge_score CHECK (score IS NULL OR (score >= 0 AND score <= 100)),
    CONSTRAINT non_negative_penalty CHECK (penalty_amount >= 0)
);

-- Rewards: Reward distributions to participants
CREATE TABLE rewards (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),

    -- Relationships
    pool_id UUID NOT NULL REFERENCES compute_pools(id) ON DELETE CASCADE,
    membership_id UUID NOT NULL REFERENCES pool_memberships(id) ON DELETE CASCADE,
    user_address VARCHAR(66) NOT NULL,

    -- Reward details
    reward_type VARCHAR(32) NOT NULL,                   -- 'trading_fee', 'liquidity_mining', 'sla_bonus'
    amount NUMERIC(78, 0) NOT NULL,

    -- Source tracking
    source_tx_hash VARCHAR(66),                         -- Transaction that generated reward
    source_trade_id UUID,                               -- Related trade if applicable

    -- Distribution
    epoch_number BIGINT,                                -- Reward epoch
    epoch_start TIMESTAMPTZ,
    epoch_end TIMESTAMPTZ,

    -- Claim status
    is_claimed BOOLEAN NOT NULL DEFAULT false,
    claimed_at TIMESTAMPTZ,
    claim_tx_hash VARCHAR(66),

    -- Timestamps
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),

    -- Constraints
    CONSTRAINT positive_reward CHECK (amount > 0)
);

-------------------------------------------------------------------------------
-- INDEXES FOR PERFORMANCE
-------------------------------------------------------------------------------

-- compute_pools indexes
CREATE INDEX idx_pools_resource_type ON compute_pools(resource_type);
CREATE INDEX idx_pools_status ON compute_pools(status);
CREATE INDEX idx_pools_volume_24h ON compute_pools(volume_24h DESC);
CREATE INDEX idx_pools_apr ON compute_pools(apr DESC);
CREATE INDEX idx_pools_utilization ON compute_pools(utilization);
CREATE INDEX idx_pools_created_at ON compute_pools(created_at);
CREATE INDEX idx_pools_slug ON compute_pools(slug);

-- compute_nodes indexes
CREATE INDEX idx_nodes_owner ON compute_nodes(owner_address);
CREATE INDEX idx_nodes_status ON compute_nodes(status);
CREATE INDEX idx_nodes_sla_score ON compute_nodes(sla_score DESC);
CREATE INDEX idx_nodes_region ON compute_nodes(region);
CREATE INDEX idx_nodes_capabilities ON compute_nodes(supports_docker, supports_k8s, supports_wasm);
CREATE INDEX idx_nodes_heartbeat ON compute_nodes(last_heartbeat_at) WHERE status = 'active';
CREATE INDEX idx_nodes_stake ON compute_nodes(stake_amount DESC) WHERE stake_amount > 0;

-- compute_offers indexes
CREATE INDEX idx_offers_node ON compute_offers(node_id);
CREATE INDEX idx_offers_pool ON compute_offers(pool_id);
CREATE INDEX idx_offers_resource_type ON compute_offers(resource_type);
CREATE INDEX idx_offers_status ON compute_offers(status);
CREATE INDEX idx_offers_price ON compute_offers(price_per_unit) WHERE status = 'active';
CREATE INDEX idx_offers_expires ON compute_offers(expires_at) WHERE status = 'active';
CREATE INDEX idx_offers_active_by_type ON compute_offers(resource_type, price_per_unit)
    WHERE status = 'active';

-- pool_memberships indexes
CREATE INDEX idx_memberships_pool ON pool_memberships(pool_id);
CREATE INDEX idx_memberships_node ON pool_memberships(node_id);
CREATE INDEX idx_memberships_user ON pool_memberships(user_address);
CREATE INDEX idx_memberships_role ON pool_memberships(role);
CREATE INDEX idx_memberships_active ON pool_memberships(pool_id, is_active) WHERE is_active = true;
CREATE INDEX idx_memberships_shares ON pool_memberships(pool_id, liquidity_shares DESC)
    WHERE liquidity_shares > 0;
CREATE INDEX idx_memberships_rewards ON pool_memberships(pending_rewards DESC)
    WHERE pending_rewards > 0;

-- challenges indexes
CREATE INDEX idx_challenges_node ON challenges(node_id);
CREATE INDEX idx_challenges_pool ON challenges(pool_id);
CREATE INDEX idx_challenges_status ON challenges(status);
CREATE INDEX idx_challenges_deadline ON challenges(deadline) WHERE status = 'pending';
CREATE INDEX idx_challenges_created ON challenges(created_at DESC);
CREATE INDEX idx_challenges_type_status ON challenges(challenge_type, status);

-- rewards indexes
CREATE INDEX idx_rewards_pool ON rewards(pool_id);
CREATE INDEX idx_rewards_membership ON rewards(membership_id);
CREATE INDEX idx_rewards_user ON rewards(user_address);
CREATE INDEX idx_rewards_unclaimed ON rewards(user_address, is_claimed) WHERE is_claimed = false;
CREATE INDEX idx_rewards_type ON rewards(reward_type);
CREATE INDEX idx_rewards_epoch ON rewards(epoch_number);
CREATE INDEX idx_rewards_created ON rewards(created_at DESC);

-------------------------------------------------------------------------------
-- TRIGGERS FOR AUTOMATIC UPDATES
-------------------------------------------------------------------------------

-- Function to update updated_at timestamp
CREATE OR REPLACE FUNCTION update_updated_at_column()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = NOW();
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Apply updated_at triggers
CREATE TRIGGER update_compute_pools_updated_at
    BEFORE UPDATE ON compute_pools
    FOR EACH ROW EXECUTE FUNCTION update_updated_at_column();

CREATE TRIGGER update_compute_nodes_updated_at
    BEFORE UPDATE ON compute_nodes
    FOR EACH ROW EXECUTE FUNCTION update_updated_at_column();

CREATE TRIGGER update_compute_offers_updated_at
    BEFORE UPDATE ON compute_offers
    FOR EACH ROW EXECUTE FUNCTION update_updated_at_column();

CREATE TRIGGER update_pool_memberships_updated_at
    BEFORE UPDATE ON pool_memberships
    FOR EACH ROW EXECUTE FUNCTION update_updated_at_column();

CREATE TRIGGER update_challenges_updated_at
    BEFORE UPDATE ON challenges
    FOR EACH ROW EXECUTE FUNCTION update_updated_at_column();

-------------------------------------------------------------------------------
-- COMMENTS FOR DOCUMENTATION
-------------------------------------------------------------------------------

COMMENT ON TABLE compute_pools IS 'AMM-style liquidity pools for compute resource trading';
COMMENT ON TABLE compute_nodes IS 'Registered compute providers with their capabilities';
COMMENT ON TABLE compute_offers IS 'Order book entries from compute providers';
COMMENT ON TABLE pool_memberships IS 'LP positions and provider stakes in pools';
COMMENT ON TABLE challenges IS 'SLA verification challenges for compute nodes';
COMMENT ON TABLE rewards IS 'Reward distributions to pool participants';

COMMENT ON COLUMN compute_pools.resource_amount IS 'Total resource units in pool (wei precision)';
COMMENT ON COLUMN compute_pools.token_amount IS 'Total HANZO tokens in pool (wei precision)';
COMMENT ON COLUMN compute_pools.fee_rate IS 'Trading fee as decimal (0.003 = 0.3%)';
COMMENT ON COLUMN compute_pools.apr IS 'Current annual percentage rate for LPs';

COMMENT ON COLUMN compute_nodes.sla_score IS 'Service level agreement score (0-100)';
COMMENT ON COLUMN compute_nodes.stake_amount IS 'Staked HANZO tokens (wei precision)';
COMMENT ON COLUMN compute_nodes.tee_type IS 'Trusted execution environment type (SGX, TDX, SEV)';

COMMENT ON COLUMN pool_memberships.liquidity_shares IS 'LP tokens representing pool ownership';
COMMENT ON COLUMN pool_memberships.entry_price IS 'Price at deposit for impermanent loss calculation';

COMMENT ON COLUMN challenges.challenge_type IS 'Type: availability, performance, attestation';
COMMENT ON COLUMN challenges.penalty_amount IS 'Slashing penalty if challenge failed (wei)';

COMMENT ON COLUMN rewards.reward_type IS 'Type: trading_fee, liquidity_mining, sla_bonus';
COMMENT ON COLUMN rewards.epoch_number IS 'Reward distribution epoch number';
