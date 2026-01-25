-- Migration: 003_functions_and_procedures.sql
-- Description: Stored functions for AMM calculations and business logic
-- Created: 2026-01-24
--
-- This migration adds PostgreSQL functions for AMM price calculations,
-- reward distributions, and other business logic.

-------------------------------------------------------------------------------
-- AMM CALCULATION FUNCTIONS
-------------------------------------------------------------------------------

-- Calculate output amount using constant product formula (x * y = k)
-- For buying resources: tokens in -> resources out
-- For selling resources: resources in -> tokens out
CREATE OR REPLACE FUNCTION calculate_swap_output(
    p_pool_id UUID,
    p_amount_in NUMERIC(78, 0),
    p_is_buying BOOLEAN
)
RETURNS TABLE (
    amount_out NUMERIC(78, 0),
    price_impact NUMERIC(10, 6),
    trading_fee NUMERIC(78, 0),
    new_price NUMERIC(78, 0)
) AS $$
DECLARE
    v_resource_amount NUMERIC(78, 0);
    v_token_amount NUMERIC(78, 0);
    v_fee_rate NUMERIC(10, 8);
    v_k NUMERIC(156, 0);
    v_amount_in_after_fee NUMERIC(78, 0);
    v_new_resource NUMERIC(78, 0);
    v_new_token NUMERIC(78, 0);
    v_amount_out NUMERIC(78, 0);
    v_old_price NUMERIC(78, 0);
    v_trading_fee NUMERIC(78, 0);
BEGIN
    -- Get pool state
    SELECT resource_amount, token_amount, fee_rate, last_price
    INTO v_resource_amount, v_token_amount, v_fee_rate, v_old_price
    FROM compute_pools
    WHERE id = p_pool_id AND status = 'active';

    IF NOT FOUND THEN
        RAISE EXCEPTION 'Pool not found or not active';
    END IF;

    -- Calculate trading fee
    v_trading_fee := (p_amount_in * v_fee_rate)::NUMERIC(78, 0);
    v_amount_in_after_fee := p_amount_in - v_trading_fee;

    -- Constant product: k = x * y
    v_k := v_resource_amount * v_token_amount;

    IF p_is_buying THEN
        -- Buying resources with tokens
        -- New token reserve = old + input
        v_new_token := v_token_amount + v_amount_in_after_fee;
        -- New resource reserve = k / new_token
        v_new_resource := v_k / v_new_token;
        -- Output = old_resource - new_resource
        v_amount_out := v_resource_amount - v_new_resource;
    ELSE
        -- Selling resources for tokens
        -- New resource reserve = old + input
        v_new_resource := v_resource_amount + v_amount_in_after_fee;
        -- New token reserve = k / new_resource
        v_new_token := v_k / v_new_resource;
        -- Output = old_token - new_token
        v_amount_out := v_token_amount - v_new_token;
    END IF;

    -- Calculate new price (tokens per resource unit)
    new_price := CASE
        WHEN v_new_resource > 0 THEN (v_new_token * 10^18) / v_new_resource
        ELSE 0
    END;

    -- Calculate price impact
    price_impact := CASE
        WHEN v_old_price > 0 THEN
            ABS((new_price - v_old_price)::NUMERIC / v_old_price::NUMERIC * 100)
        ELSE 0
    END;

    amount_out := v_amount_out;
    trading_fee := v_trading_fee;

    RETURN NEXT;
END;
$$ LANGUAGE plpgsql STABLE;

-- Calculate LP tokens to mint for adding liquidity
CREATE OR REPLACE FUNCTION calculate_lp_tokens_to_mint(
    p_pool_id UUID,
    p_resource_amount NUMERIC(78, 0),
    p_token_amount NUMERIC(78, 0)
)
RETURNS NUMERIC(78, 0) AS $$
DECLARE
    v_pool_resource NUMERIC(78, 0);
    v_pool_token NUMERIC(78, 0);
    v_total_liquidity NUMERIC(78, 0);
    v_lp_tokens NUMERIC(78, 0);
BEGIN
    SELECT resource_amount, token_amount, total_liquidity
    INTO v_pool_resource, v_pool_token, v_total_liquidity
    FROM compute_pools
    WHERE id = p_pool_id;

    -- First liquidity provider
    IF v_total_liquidity = 0 THEN
        -- Initial LP tokens = sqrt(resource * token)
        v_lp_tokens := SQRT(p_resource_amount::NUMERIC * p_token_amount::NUMERIC)::NUMERIC(78, 0);
    ELSE
        -- Subsequent: proportional to existing liquidity
        -- LP tokens = min(resource_ratio, token_ratio) * total_liquidity
        v_lp_tokens := LEAST(
            (p_resource_amount * v_total_liquidity) / v_pool_resource,
            (p_token_amount * v_total_liquidity) / v_pool_token
        );
    END IF;

    RETURN v_lp_tokens;
END;
$$ LANGUAGE plpgsql STABLE;

-- Calculate amounts to return when removing liquidity
CREATE OR REPLACE FUNCTION calculate_liquidity_removal(
    p_pool_id UUID,
    p_lp_tokens NUMERIC(78, 0)
)
RETURNS TABLE (
    resource_amount NUMERIC(78, 0),
    token_amount NUMERIC(78, 0),
    share_percent NUMERIC(10, 6)
) AS $$
DECLARE
    v_pool_resource NUMERIC(78, 0);
    v_pool_token NUMERIC(78, 0);
    v_total_liquidity NUMERIC(78, 0);
    v_share NUMERIC(30, 18);
BEGIN
    SELECT cp.resource_amount, cp.token_amount, cp.total_liquidity
    INTO v_pool_resource, v_pool_token, v_total_liquidity
    FROM compute_pools cp
    WHERE cp.id = p_pool_id;

    IF v_total_liquidity = 0 THEN
        RAISE EXCEPTION 'Pool has no liquidity';
    END IF;

    -- Calculate share
    v_share := p_lp_tokens::NUMERIC / v_total_liquidity::NUMERIC;

    resource_amount := (v_pool_resource::NUMERIC * v_share)::NUMERIC(78, 0);
    token_amount := (v_pool_token::NUMERIC * v_share)::NUMERIC(78, 0);
    share_percent := v_share * 100;

    RETURN NEXT;
END;
$$ LANGUAGE plpgsql STABLE;

-- Calculate impermanent loss
CREATE OR REPLACE FUNCTION calculate_impermanent_loss(
    p_entry_price NUMERIC(78, 0),
    p_current_price NUMERIC(78, 0)
)
RETURNS NUMERIC(10, 6) AS $$
DECLARE
    v_price_ratio NUMERIC(30, 18);
    v_sqrt_ratio NUMERIC(30, 18);
    v_il NUMERIC(30, 18);
BEGIN
    IF p_entry_price = 0 THEN
        RETURN 0;
    END IF;

    v_price_ratio := p_current_price::NUMERIC / p_entry_price::NUMERIC;
    v_sqrt_ratio := SQRT(v_price_ratio);

    -- IL = 2 * sqrt(ratio) / (1 + ratio) - 1
    v_il := (2 * v_sqrt_ratio) / (1 + v_price_ratio) - 1;

    -- Return as percentage (negative = loss)
    RETURN v_il * 100;
END;
$$ LANGUAGE plpgsql IMMUTABLE;

-------------------------------------------------------------------------------
-- REWARD CALCULATION FUNCTIONS
-------------------------------------------------------------------------------

-- Calculate trading fee rewards for a liquidity provider
CREATE OR REPLACE FUNCTION calculate_trading_fee_rewards(
    p_membership_id UUID,
    p_from_time TIMESTAMPTZ DEFAULT NULL,
    p_to_time TIMESTAMPTZ DEFAULT NOW()
)
RETURNS NUMERIC(78, 0) AS $$
DECLARE
    v_pool_id UUID;
    v_shares NUMERIC(78, 0);
    v_total_fees NUMERIC(78, 0);
    v_share_of_fees NUMERIC(78, 0);
BEGIN
    -- Get membership details
    SELECT pool_id, liquidity_shares
    INTO v_pool_id, v_shares
    FROM pool_memberships
    WHERE id = p_membership_id AND is_active = true;

    IF NOT FOUND THEN
        RETURN 0;
    END IF;

    -- Calculate total fees in period
    SELECT COALESCE(SUM(trading_fee), 0)
    INTO v_total_fees
    FROM trades
    WHERE pool_id = v_pool_id
      AND created_at >= COALESCE(p_from_time, '-infinity'::TIMESTAMPTZ)
      AND created_at <= p_to_time;

    -- Get user's share of fees (based on time-weighted average, simplified here)
    SELECT CASE
        WHEN cp.total_liquidity > 0 THEN
            (v_shares * v_total_fees) / cp.total_liquidity
        ELSE 0
    END
    INTO v_share_of_fees
    FROM compute_pools cp
    WHERE cp.id = v_pool_id;

    RETURN COALESCE(v_share_of_fees, 0);
END;
$$ LANGUAGE plpgsql STABLE;

-- Calculate pool APR
CREATE OR REPLACE FUNCTION calculate_pool_apr(p_pool_id UUID)
RETURNS NUMERIC(10, 4) AS $$
DECLARE
    v_volume_24h NUMERIC(78, 0);
    v_liquidity NUMERIC(78, 0);
    v_fee_rate NUMERIC(10, 8);
    v_daily_fees NUMERIC;
    v_apr NUMERIC(10, 4);
BEGIN
    SELECT volume_24h, token_amount, fee_rate
    INTO v_volume_24h, v_liquidity, v_fee_rate
    FROM compute_pools
    WHERE id = p_pool_id;

    IF v_liquidity = 0 THEN
        RETURN 0;
    END IF;

    -- Daily fees as percentage of liquidity
    v_daily_fees := (v_volume_24h::NUMERIC * v_fee_rate) / v_liquidity::NUMERIC;

    -- Annualize
    v_apr := v_daily_fees * 365 * 100;

    RETURN COALESCE(v_apr, 0);
END;
$$ LANGUAGE plpgsql STABLE;

-------------------------------------------------------------------------------
-- CHALLENGE FUNCTIONS
-------------------------------------------------------------------------------

-- Create a new challenge for a node
CREATE OR REPLACE FUNCTION create_challenge(
    p_node_id UUID,
    p_pool_id UUID,
    p_challenge_type VARCHAR(32),
    p_challenge_data JSONB,
    p_deadline_minutes INTEGER DEFAULT 5
)
RETURNS UUID AS $$
DECLARE
    v_challenge_id UUID;
BEGIN
    INSERT INTO challenges (
        node_id,
        pool_id,
        challenge_type,
        challenge_data,
        deadline
    ) VALUES (
        p_node_id,
        p_pool_id,
        p_challenge_type,
        p_challenge_data,
        NOW() + (p_deadline_minutes || ' minutes')::INTERVAL
    )
    RETURNING id INTO v_challenge_id;

    RETURN v_challenge_id;
END;
$$ LANGUAGE plpgsql;

-- Process challenge response
CREATE OR REPLACE FUNCTION process_challenge_response(
    p_challenge_id UUID,
    p_response_data JSONB,
    p_score NUMERIC(5, 2)
)
RETURNS BOOLEAN AS $$
DECLARE
    v_node_id UUID;
    v_status challenge_status;
    v_penalty NUMERIC(78, 0) := 0;
BEGIN
    -- Get challenge details
    SELECT node_id, status
    INTO v_node_id, v_status
    FROM challenges
    WHERE id = p_challenge_id;

    IF v_status != 'pending' THEN
        RAISE EXCEPTION 'Challenge is not pending';
    END IF;

    -- Determine pass/fail
    IF p_score >= 80 THEN
        v_status := 'passed';
    ELSE
        v_status := 'failed';
        -- Calculate penalty (simplified)
        v_penalty := 1000000000000000000; -- 1 token penalty
    END IF;

    -- Update challenge
    UPDATE challenges
    SET response_data = p_response_data,
        response_at = NOW(),
        status = v_status,
        score = p_score,
        penalty_amount = v_penalty,
        verified_at = NOW()
    WHERE id = p_challenge_id;

    -- Update node SLA score
    UPDATE compute_nodes
    SET sla_score = (
        SELECT COALESCE(AVG(score), 100)
        FROM challenges
        WHERE node_id = v_node_id
          AND status IN ('passed', 'failed')
          AND created_at > NOW() - INTERVAL '30 days'
    )
    WHERE id = v_node_id;

    RETURN v_status = 'passed';
END;
$$ LANGUAGE plpgsql;

-- Auto-fail expired challenges
CREATE OR REPLACE FUNCTION fail_expired_challenges()
RETURNS INTEGER AS $$
DECLARE
    v_count INTEGER;
BEGIN
    WITH expired AS (
        UPDATE challenges
        SET status = 'timeout',
            penalty_amount = 2000000000000000000 -- 2 token penalty for timeout
        WHERE status = 'pending'
          AND deadline < NOW()
        RETURNING node_id
    )
    SELECT COUNT(*) INTO v_count FROM expired;

    RETURN v_count;
END;
$$ LANGUAGE plpgsql;

-------------------------------------------------------------------------------
-- POOL UPDATE FUNCTIONS
-------------------------------------------------------------------------------

-- Update pool metrics after a trade
CREATE OR REPLACE FUNCTION update_pool_after_trade(
    p_pool_id UUID,
    p_trade_id UUID
)
RETURNS VOID AS $$
DECLARE
    v_trade RECORD;
    v_new_volume_24h NUMERIC(78, 0);
BEGIN
    -- Get trade details
    SELECT * INTO v_trade
    FROM trades
    WHERE id = p_trade_id;

    -- Calculate new 24h volume
    SELECT COALESCE(SUM(amount_in), 0)
    INTO v_new_volume_24h
    FROM trades
    WHERE pool_id = p_pool_id
      AND created_at > NOW() - INTERVAL '24 hours';

    -- Update pool
    UPDATE compute_pools
    SET volume_24h = v_new_volume_24h,
        volume_total = volume_total + v_trade.amount_in,
        trade_count_24h = (
            SELECT COUNT(*)
            FROM trades
            WHERE pool_id = p_pool_id
              AND created_at > NOW() - INTERVAL '24 hours'
        ),
        trade_count_total = trade_count_total + 1,
        last_price = v_trade.effective_price,
        apr = calculate_pool_apr(p_pool_id)
    WHERE id = p_pool_id;
END;
$$ LANGUAGE plpgsql;

-- Recalculate pool price changes
CREATE OR REPLACE FUNCTION recalculate_pool_price_changes(p_pool_id UUID)
RETURNS VOID AS $$
DECLARE
    v_current_price NUMERIC(78, 0);
    v_price_1h NUMERIC(78, 0);
    v_price_24h NUMERIC(78, 0);
    v_price_7d NUMERIC(78, 0);
BEGIN
    SELECT last_price INTO v_current_price
    FROM compute_pools WHERE id = p_pool_id;

    -- Get historical prices
    SELECT close_price INTO v_price_1h
    FROM price_snapshots
    WHERE pool_id = p_pool_id
      AND bucket_time <= NOW() - INTERVAL '1 hour'
    ORDER BY bucket_time DESC LIMIT 1;

    SELECT close_price INTO v_price_24h
    FROM price_snapshots_hourly
    WHERE pool_id = p_pool_id
      AND bucket_time <= NOW() - INTERVAL '24 hours'
    ORDER BY bucket_time DESC LIMIT 1;

    SELECT close_price INTO v_price_7d
    FROM price_snapshots_daily
    WHERE pool_id = p_pool_id
      AND bucket_date <= CURRENT_DATE - INTERVAL '7 days'
    ORDER BY bucket_date DESC LIMIT 1;

    UPDATE compute_pools
    SET price_change_1h = CASE WHEN v_price_1h > 0
        THEN ((v_current_price - v_price_1h)::NUMERIC / v_price_1h::NUMERIC)
        ELSE 0 END,
        price_change_24h = CASE WHEN v_price_24h > 0
        THEN ((v_current_price - v_price_24h)::NUMERIC / v_price_24h::NUMERIC)
        ELSE 0 END,
        price_change_7d = CASE WHEN v_price_7d > 0
        THEN ((v_current_price - v_price_7d)::NUMERIC / v_price_7d::NUMERIC)
        ELSE 0 END
    WHERE id = p_pool_id;
END;
$$ LANGUAGE plpgsql;

-------------------------------------------------------------------------------
-- BEST PRICE ROUTING
-------------------------------------------------------------------------------

-- Get best price across AMM and order book
CREATE OR REPLACE FUNCTION get_best_price(
    p_resource_type resource_type,
    p_is_buying BOOLEAN,
    p_amount NUMERIC(78, 0)
)
RETURNS TABLE (
    pool_id UUID,
    expected_output NUMERIC(78, 0),
    effective_price NUMERIC(78, 0),
    price_impact NUMERIC(10, 6),
    use_amm BOOLEAN,
    route_details JSONB
) AS $$
BEGIN
    -- Get AMM quote from best pool
    RETURN QUERY
    WITH amm_quotes AS (
        SELECT
            cp.id AS pool_id,
            (calculate_swap_output(cp.id, p_amount, p_is_buying)).*
        FROM compute_pools cp
        WHERE cp.resource_type = p_resource_type
          AND cp.status = 'active'
          AND cp.token_amount > 0
          AND cp.resource_amount > 0
    ),
    orderbook_quotes AS (
        SELECT
            co.pool_id,
            SUM(LEAST(co.amount_remaining, p_amount)) AS available_amount,
            AVG(co.price_per_unit) AS avg_price
        FROM compute_offers co
        WHERE co.resource_type = p_resource_type
          AND co.status = 'active'
          AND co.expires_at > NOW()
        GROUP BY co.pool_id
    )
    SELECT
        aq.pool_id,
        aq.amount_out AS expected_output,
        CASE WHEN aq.amount_out > 0
            THEN (p_amount * 10^18) / aq.amount_out
            ELSE 0
        END AS effective_price,
        aq.price_impact,
        true AS use_amm,
        jsonb_build_object(
            'source', 'amm',
            'pool_id', aq.pool_id,
            'trading_fee', aq.trading_fee
        ) AS route_details
    FROM amm_quotes aq
    ORDER BY aq.amount_out DESC
    LIMIT 1;
END;
$$ LANGUAGE plpgsql STABLE;

-------------------------------------------------------------------------------
-- COMMENTS
-------------------------------------------------------------------------------

COMMENT ON FUNCTION calculate_swap_output IS 'Calculate AMM swap output using constant product formula';
COMMENT ON FUNCTION calculate_lp_tokens_to_mint IS 'Calculate LP tokens for adding liquidity';
COMMENT ON FUNCTION calculate_liquidity_removal IS 'Calculate amounts returned when removing liquidity';
COMMENT ON FUNCTION calculate_impermanent_loss IS 'Calculate IL percentage given entry and current price';
COMMENT ON FUNCTION calculate_trading_fee_rewards IS 'Calculate trading fee rewards for a liquidity provider';
COMMENT ON FUNCTION calculate_pool_apr IS 'Calculate current APR for a pool';
COMMENT ON FUNCTION create_challenge IS 'Create a new SLA verification challenge';
COMMENT ON FUNCTION process_challenge_response IS 'Process and score a challenge response';
COMMENT ON FUNCTION fail_expired_challenges IS 'Auto-fail challenges past deadline';
COMMENT ON FUNCTION get_best_price IS 'Get best price across AMM and order book';
