-- Migration: 004_seed_data.sql
-- Description: Initial seed data for compute pools platform
-- Created: 2026-01-24
--
-- This migration seeds initial pools and test data for development.
-- In production, remove or modify this migration.

-------------------------------------------------------------------------------
-- SEED COMPUTE POOLS
-------------------------------------------------------------------------------

-- Insert default compute pools for each resource type
INSERT INTO compute_pools (
    id,
    name,
    slug,
    description,
    resource_type,
    resource_amount,
    token_amount,
    total_liquidity,
    last_price,
    fee_rate,
    utilization,
    avg_sla_score,
    apr,
    status
) VALUES
-- GPU Pool
(
    'a0000000-0000-0000-0000-000000000001'::UUID,
    'GPU Compute',
    'gpu',
    'High-performance GPU compute pool for ML/AI workloads. Supports CUDA and OpenCL.',
    'GPU',
    '1000000000000000000000'::NUMERIC,       -- 1000 GPU units
    '50000000000000000000000000'::NUMERIC,   -- 50M HANZO tokens
    '7071067811865475244'::NUMERIC,          -- sqrt(1000 * 50M) LP tokens
    '50000000000000000000000'::NUMERIC,      -- 50,000 HANZO per GPU unit/hr
    0.003,
    45.5,
    98.5,
    24.5,
    'active'
),
-- CPU Pool
(
    'a0000000-0000-0000-0000-000000000002'::UUID,
    'CPU Compute',
    'cpu',
    'General purpose CPU compute pool for containerized workloads.',
    'CPU',
    '100000000000000000000000'::NUMERIC,     -- 100,000 CPU units
    '10000000000000000000000000'::NUMERIC,   -- 10M HANZO tokens
    '31622776601683793319'::NUMERIC,
    '100000000000000000000'::NUMERIC,        -- 100 HANZO per CPU unit/hr
    0.003,
    62.3,
    99.1,
    18.2,
    'active'
),
-- Memory Pool
(
    'a0000000-0000-0000-0000-000000000003'::UUID,
    'Memory',
    'memory',
    'High-memory compute pool for data-intensive applications.',
    'Memory',
    '1000000000000000000000000'::NUMERIC,    -- 1M GB memory
    '5000000000000000000000000'::NUMERIC,    -- 5M HANZO tokens
    '70710678118654752440'::NUMERIC,
    '5000000000000000000'::NUMERIC,          -- 5 HANZO per GB/hr
    0.003,
    38.7,
    99.5,
    15.8,
    'active'
),
-- Storage Pool
(
    'a0000000-0000-0000-0000-000000000004'::UUID,
    'Storage',
    'storage',
    'Distributed storage pool with high availability and durability.',
    'Storage',
    '10000000000000000000000000'::NUMERIC,   -- 10M GB storage
    '2000000000000000000000000'::NUMERIC,    -- 2M HANZO tokens
    '141421356237309504880'::NUMERIC,
    '200000000000000000'::NUMERIC,           -- 0.2 HANZO per GB/hr
    0.002,
    71.2,
    99.8,
    12.4,
    'active'
),
-- Docker Pool
(
    'a0000000-0000-0000-0000-000000000005'::UUID,
    'Docker Containers',
    'docker',
    'Docker container execution pool with instant deployment.',
    'Docker',
    '50000000000000000000000'::NUMERIC,      -- 50,000 container slots
    '25000000000000000000000000'::NUMERIC,   -- 25M HANZO tokens
    '35355339059327376220'::NUMERIC,
    '500000000000000000000'::NUMERIC,        -- 500 HANZO per container/hr
    0.003,
    55.8,
    98.9,
    21.3,
    'active'
),
-- Kubernetes Pool
(
    'a0000000-0000-0000-0000-000000000006'::UUID,
    'Kubernetes',
    'k8s',
    'Managed Kubernetes pool with auto-scaling and load balancing.',
    'K8S',
    '10000000000000000000000'::NUMERIC,      -- 10,000 pod slots
    '100000000000000000000000000'::NUMERIC,  -- 100M HANZO tokens
    '31622776601683793319'::NUMERIC,
    '10000000000000000000000'::NUMERIC,      -- 10,000 HANZO per pod/hr
    0.003,
    42.1,
    99.2,
    28.7,
    'active'
),
-- WASM Pool
(
    'a0000000-0000-0000-0000-000000000007'::UUID,
    'WebAssembly',
    'wasm',
    'Edge compute pool running WebAssembly modules with near-instant cold starts.',
    'WASM',
    '1000000000000000000000000'::NUMERIC,    -- 1M WASM execution units
    '1000000000000000000000000'::NUMERIC,    -- 1M HANZO tokens
    '1000000000000000000000'::NUMERIC,
    '1000000000000000000'::NUMERIC,          -- 1 HANZO per execution unit
    0.001,
    28.4,
    99.7,
    8.5,
    'active'
),
-- Bandwidth Pool
(
    'a0000000-0000-0000-0000-000000000008'::UUID,
    'Bandwidth',
    'bandwidth',
    'Network bandwidth pool for high-throughput data transfer.',
    'Bandwidth',
    '100000000000000000000000000'::NUMERIC,  -- 100M Mbps
    '500000000000000000000000'::NUMERIC,     -- 500K HANZO tokens
    '223606797749978969640'::NUMERIC,
    '5000000000000000'::NUMERIC,             -- 0.005 HANZO per Mbps/hr
    0.002,
    83.6,
    99.4,
    10.2,
    'active'
);

-------------------------------------------------------------------------------
-- SEED SAMPLE COMPUTE NODES (for development/testing)
-------------------------------------------------------------------------------

INSERT INTO compute_nodes (
    id,
    owner_address,
    name,
    description,
    cpu_cores,
    cpu_model,
    memory_mb,
    storage_gb,
    gpu_count,
    gpu_model,
    bandwidth_mbps,
    supports_docker,
    supports_k8s,
    supports_wasm,
    region,
    availability_zone,
    tee_type,
    sla_score,
    uptime_percent,
    stake_amount,
    status
) VALUES
(
    'b0000000-0000-0000-0000-000000000001'::UUID,
    '0x1234567890123456789012345678901234567890',
    'Alpha-GPU-01',
    'High-performance GPU node with 8x A100',
    128,
    'AMD EPYC 7763',
    524288,
    10000,
    8,
    'NVIDIA A100 80GB',
    10000,
    true,
    true,
    false,
    'us-west-1',
    'us-west-1a',
    'SEV',
    98.7,
    99.95,
    '100000000000000000000000'::NUMERIC,
    'active'
),
(
    'b0000000-0000-0000-0000-000000000002'::UUID,
    '0x2345678901234567890123456789012345678901',
    'Beta-CPU-01',
    'High-core-count CPU node for parallel workloads',
    256,
    'Intel Xeon Platinum 8480+',
    1048576,
    20000,
    0,
    NULL,
    25000,
    true,
    true,
    true,
    'us-east-1',
    'us-east-1b',
    'TDX',
    99.2,
    99.99,
    '50000000000000000000000'::NUMERIC,
    'active'
),
(
    'b0000000-0000-0000-0000-000000000003'::UUID,
    '0x3456789012345678901234567890123456789012',
    'Gamma-Edge-01',
    'Edge node optimized for WASM workloads',
    16,
    'ARM Neoverse V2',
    32768,
    500,
    0,
    NULL,
    1000,
    true,
    false,
    true,
    'eu-west-1',
    'eu-west-1a',
    NULL,
    99.8,
    99.98,
    '10000000000000000000000'::NUMERIC,
    'active'
);

-------------------------------------------------------------------------------
-- SEED SAMPLE OFFERS
-------------------------------------------------------------------------------

INSERT INTO compute_offers (
    id,
    node_id,
    pool_id,
    resource_type,
    amount,
    amount_remaining,
    price_per_unit,
    min_duration_hours,
    max_duration_hours,
    guaranteed_uptime,
    max_latency_ms,
    status,
    expires_at
) VALUES
(
    'c0000000-0000-0000-0000-000000000001'::UUID,
    'b0000000-0000-0000-0000-000000000001'::UUID,
    'a0000000-0000-0000-0000-000000000001'::UUID,
    'GPU',
    '8000000000000000000'::NUMERIC,          -- 8 GPU units
    '8000000000000000000'::NUMERIC,
    '48000000000000000000000'::NUMERIC,      -- 48,000 HANZO (slight discount)
    1,
    720,
    99.5,
    50,
    'active',
    NOW() + INTERVAL '30 days'
),
(
    'c0000000-0000-0000-0000-000000000002'::UUID,
    'b0000000-0000-0000-0000-000000000002'::UUID,
    'a0000000-0000-0000-0000-000000000002'::UUID,
    'CPU',
    '256000000000000000000'::NUMERIC,        -- 256 CPU units
    '256000000000000000000'::NUMERIC,
    '95000000000000000000'::NUMERIC,         -- 95 HANZO (slight discount)
    1,
    168,
    99.9,
    10,
    'active',
    NOW() + INTERVAL '7 days'
),
(
    'c0000000-0000-0000-0000-000000000003'::UUID,
    'b0000000-0000-0000-0000-000000000003'::UUID,
    'a0000000-0000-0000-0000-000000000007'::UUID,
    'WASM',
    '100000000000000000000000'::NUMERIC,     -- 100,000 WASM units
    '100000000000000000000000'::NUMERIC,
    '900000000000000000'::NUMERIC,           -- 0.9 HANZO (10% discount)
    1,
    24,
    99.95,
    5,
    'active',
    NOW() + INTERVAL '14 days'
);

-------------------------------------------------------------------------------
-- SEED SAMPLE POOL MEMBERSHIPS
-------------------------------------------------------------------------------

INSERT INTO pool_memberships (
    id,
    pool_id,
    node_id,
    user_address,
    role,
    liquidity_shares,
    resource_deposited,
    token_deposited,
    entry_price,
    pending_rewards,
    claimed_rewards,
    committed_resources,
    member_sla_score,
    is_active
) VALUES
-- GPU Pool provider
(
    'd0000000-0000-0000-0000-000000000001'::UUID,
    'a0000000-0000-0000-0000-000000000001'::UUID,
    'b0000000-0000-0000-0000-000000000001'::UUID,
    '0x1234567890123456789012345678901234567890',
    'provider',
    '0'::NUMERIC,
    '0'::NUMERIC,
    '0'::NUMERIC,
    '50000000000000000000000'::NUMERIC,
    '0'::NUMERIC,
    '0'::NUMERIC,
    '8000000000000000000'::NUMERIC,
    98.7,
    true
),
-- GPU Pool LP
(
    'd0000000-0000-0000-0000-000000000002'::UUID,
    'a0000000-0000-0000-0000-000000000001'::UUID,
    NULL,
    '0xABCDEF0123456789ABCDEF0123456789ABCDEF01',
    'liquidity',
    '707106781186547524'::NUMERIC,           -- ~10% of pool
    '100000000000000000000'::NUMERIC,        -- 100 GPU units
    '5000000000000000000000000'::NUMERIC,    -- 5M HANZO
    '50000000000000000000000'::NUMERIC,
    '125000000000000000000000'::NUMERIC,     -- 125K pending rewards
    '500000000000000000000000'::NUMERIC,     -- 500K claimed
    '0'::NUMERIC,
    100.0,
    true
),
-- CPU Pool provider
(
    'd0000000-0000-0000-0000-000000000003'::UUID,
    'a0000000-0000-0000-0000-000000000002'::UUID,
    'b0000000-0000-0000-0000-000000000002'::UUID,
    '0x2345678901234567890123456789012345678901',
    'provider',
    '0'::NUMERIC,
    '0'::NUMERIC,
    '0'::NUMERIC,
    '100000000000000000000'::NUMERIC,
    '0'::NUMERIC,
    '0'::NUMERIC,
    '256000000000000000000'::NUMERIC,
    99.2,
    true
);

-------------------------------------------------------------------------------
-- SEED INITIAL PRICE SNAPSHOTS
-------------------------------------------------------------------------------

-- Insert some historical price data for charts (last 24 hours)
INSERT INTO price_snapshots_hourly (pool_id, bucket_time, open_price, high_price, low_price, close_price, volume, trade_count)
SELECT
    cp.id,
    gs.ts,
    cp.last_price * (1 + (random() - 0.5) * 0.1),
    cp.last_price * (1 + random() * 0.05),
    cp.last_price * (1 - random() * 0.05),
    cp.last_price * (1 + (random() - 0.5) * 0.08),
    (cp.volume_24h / 24 * (0.5 + random()))::NUMERIC(78, 0),
    (10 + random() * 50)::INTEGER
FROM compute_pools cp
CROSS JOIN generate_series(
    NOW() - INTERVAL '24 hours',
    NOW(),
    INTERVAL '1 hour'
) AS gs(ts)
WHERE cp.status = 'active';

-------------------------------------------------------------------------------
-- VERIFICATION
-------------------------------------------------------------------------------

-- Display seeded data counts
DO $$
BEGIN
    RAISE NOTICE 'Seed data inserted successfully:';
    RAISE NOTICE '  - Compute pools: %', (SELECT COUNT(*) FROM compute_pools);
    RAISE NOTICE '  - Compute nodes: %', (SELECT COUNT(*) FROM compute_nodes);
    RAISE NOTICE '  - Compute offers: %', (SELECT COUNT(*) FROM compute_offers);
    RAISE NOTICE '  - Pool memberships: %', (SELECT COUNT(*) FROM pool_memberships);
    RAISE NOTICE '  - Price snapshots (hourly): %', (SELECT COUNT(*) FROM price_snapshots_hourly);
END $$;
