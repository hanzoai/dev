# Platform Phase 1: Pools UI and Deployment Flow Design

## Executive Summary

This document outlines the design for the Pools UI and Deployment Flow for the Hanzo Platform Phase 1. The system integrates with hanzo-node's ComputeDEX smart contracts to provide a user interface for managing compute resource pools, liquidity provision, and application deployment targeting.

---

## 1. Architecture Overview

### 1.1 System Context

```
+------------------+     +-------------------+     +------------------+
|   Platform UI    |---->|   tRPC API Layer  |---->|   hanzo-node     |
|   (Next.js)      |     |   (TypeScript)    |     |   (Rust)         |
+------------------+     +-------------------+     +------------------+
        |                        |                        |
        |                        |                        v
        |                        |              +------------------+
        |                        +------------->| ComputeDEX       |
        |                                       | Smart Contracts  |
        |                                       +------------------+
        |                                                |
        v                                                v
+------------------+                           +------------------+
|   Deployment     |                           |   Liquidity      |
|   Queue (BullMQ) |                           |   Pools (AMM)    |
+------------------+                           +------------------+
```

### 1.2 Integration Points

| Component | Protocol | Port | Purpose |
|-----------|----------|------|---------|
| Platform API | HTTP/tRPC | 3000 | UI backend |
| hanzo-node | HTTP/gRPC | 8080/50051 | Compute operations |
| ComputeDEX | Ethereum JSON-RPC | 8545 | Smart contract interactions |
| Redis/BullMQ | Redis | 6379 | Job queue |

---

## 2. Data Models

### 2.1 Pool Types (from ComputeDEX)

```typescript
// Resource types supported by the DEX
enum ResourceType {
  CPU = 0,
  GPU = 1,
  Memory = 2,
  Storage = 3,
  Bandwidth = 4,
  WASM = 5,
  Docker = 6,
  K8S = 7,
}

// Pool information structure
interface Pool {
  id: string;
  resourceType: ResourceType;
  resourceAmount: bigint;      // Total resource units in pool
  tokenAmount: bigint;         // HANZO tokens in pool
  totalLiquidity: bigint;      // LP token supply
  feeRate: number;             // Trading fee (e.g., 0.003 = 0.3%)
  utilization: number;         // Current utilization percentage
  lastPrice: bigint;           // Last traded price
  volume24h: bigint;           // 24-hour trading volume
}

// User's liquidity position
interface LiquidityPosition {
  poolId: string;
  resourceType: ResourceType;
  liquidityShares: bigint;     // LP tokens held
  resourceAmount: bigint;      // Underlying resource
  tokenAmount: bigint;         // Underlying HANZO tokens
  pendingRewards: bigint;      // Unclaimed rewards
  entryPrice: bigint;          // Price at entry
  impermanentLoss: number;     // Current IL percentage
}

// Order book entry
interface OrderBookEntry {
  orderId: string;
  provider: string;            // Provider address
  resourceType: ResourceType;
  amount: bigint;
  pricePerUnit: bigint;
  duration: number;            // Seconds
  slaScore: number;            // 0-100
  attestation?: string;        // TEE attestation hash
  createdAt: Date;
  expiresAt: Date;
}
```

### 2.2 Deployment Configuration

```typescript
interface DeploymentTarget {
  type: 'local' | 'cloud' | 'pool';
  poolId?: string;             // If type === 'pool'
  region?: string;             // If type === 'cloud'
  serverId?: string;           // If type === 'local'
}

interface DeploymentConfig {
  applicationId: string;
  target: DeploymentTarget;
  resources: {
    cpu: number;               // CPU units requested
    memory: number;            // MB
    storage: number;           // GB
    gpu?: number;              // GPU units (optional)
  };
  scaling: {
    minReplicas: number;
    maxReplicas: number;
    targetCpuUtilization: number;
  };
  budget: {
    maxTokensPerHour: bigint;
    maxTotalTokens: bigint;
  };
}
```

---

## 3. UI Components

### 3.1 Component Hierarchy

```
/app/platform/components/
  /pools/
    PoolsDashboard.tsx          # Main pools overview
    PoolCard.tsx                # Individual pool summary
    PoolDetailView.tsx          # Detailed pool information
    PoolChart.tsx               # Price/volume charts
    AddLiquidityModal.tsx       # Add liquidity dialog
    RemoveLiquidityModal.tsx    # Remove liquidity dialog
    SwapInterface.tsx           # Buy/sell resources
    OrderBook.tsx               # Order book display
    MyPositions.tsx             # User's liquidity positions
  /deployment/
    DeploymentTargetSelector.tsx # Enhanced with pool option
    PoolSelector.tsx            # Pool selection for deployment
    ResourceEstimator.tsx       # Cost/resource estimation
    DeploymentBudget.tsx        # Budget configuration
```

### 3.2 Pools Dashboard Component

```
+------------------------------------------------------------------+
|  Pools Dashboard                                    [Refresh] [+] |
+------------------------------------------------------------------+
|                                                                   |
|  +------------------------+  +------------------------+          |
|  | GPU Pool               |  | CPU Pool               |          |
|  | 1,234.56 HANZO/unit    |  | 0.89 HANZO/unit       |          |
|  | Liquidity: $2.4M       |  | Liquidity: $890K      |          |
|  | 24h Vol: $45K   +2.3%  |  | 24h Vol: $12K  -0.8%  |          |
|  | Utilization: 78%       |  | Utilization: 45%      |          |
|  +------------------------+  +------------------------+          |
|                                                                   |
|  +------------------------+  +------------------------+          |
|  | Memory Pool            |  | Storage Pool           |          |
|  | 0.12 HANZO/GB          |  | 0.05 HANZO/GB/mo      |          |
|  | Liquidity: $450K       |  | Liquidity: $320K      |          |
|  | 24h Vol: $8K    +1.1%  |  | 24h Vol: $5K   +0.5%  |          |
|  | Utilization: 62%       |  | Utilization: 34%      |          |
|  +------------------------+  +------------------------+          |
|                                                                   |
+------------------------------------------------------------------+
|  My Positions                                                     |
+------------------------------------------------------------------+
|  Pool     | Liquidity | Share | Rewards    | IL      | Actions   |
|  ---------|-----------|-------|------------|---------|-----------|
|  GPU      | $12,450   | 0.52% | 45.2 HANZO | -1.2%   | [Manage]  |
|  Memory   | $3,200    | 0.71% | 12.8 HANZO | +0.3%   | [Manage]  |
+------------------------------------------------------------------+
```

### 3.3 Pool Detail View

```
+------------------------------------------------------------------+
|  < Back    GPU Compute Pool                    [Trade] [Provide]  |
+------------------------------------------------------------------+
|                                                                   |
|  +-------------------------------+  +---------------------------+ |
|  |     Price Chart               |  |  Pool Statistics          | |
|  |                               |  |                           | |
|  |     [Line chart area]         |  |  Total Liquidity          | |
|  |                               |  |  $2,456,789               | |
|  |  [1H] [24H] [7D] [30D] [ALL]  |  |                           | |
|  +-------------------------------+  |  24h Volume               | |
|                                     |  $45,234 (+12.3%)         | |
|  +-------------------------------+  |                           | |
|  |     Volume Chart              |  |  Current Price            | |
|  |                               |  |  1,234.56 HANZO/unit      | |
|  |     [Bar chart area]          |  |                           | |
|  |                               |  |  Pool APR                 | |
|  +-------------------------------+  |  18.7%                    | |
|                                     |                           | |
|                                     |  Fee Tier                 | |
|                                     |  0.3%                     | |
|                                     +---------------------------+ |
|                                                                   |
+------------------------------------------------------------------+
|  Order Book                                                       |
+------------------------------------------------------------------+
|  Bids (Buy Orders)         |  Asks (Sell Orders)                 |
|  Price      | Amount       |  Price      | Amount                |
|  1,230.00   | 12.5 units   |  1,235.00   | 8.2 units            |
|  1,228.50   | 25.0 units   |  1,238.00   | 15.5 units           |
|  1,225.00   | 45.0 units   |  1,242.00   | 22.0 units           |
+------------------------------------------------------------------+
```

### 3.4 Add Liquidity Modal

```
+--------------------------------------------------+
|  Add Liquidity to GPU Pool                   [X]  |
+--------------------------------------------------+
|                                                   |
|  You're adding liquidity to earn fees from        |
|  trades in the GPU compute pool.                  |
|                                                   |
|  +---------------------------------------------+ |
|  |  GPU Units                                  | |
|  |  [________________] units                   | |
|  |  Balance: 45.5 units                        | |
|  +---------------------------------------------+ |
|                                                   |
|  +---------------------------------------------+ |
|  |  HANZO Tokens                               | |
|  |  [________________] HANZO                   | |
|  |  Balance: 5,234.56 HANZO                    | |
|  +---------------------------------------------+ |
|                                                   |
|  +---------------------------------------------+ |
|  |  Summary                                    | |
|  |  Share of Pool: ~0.45%                      | |
|  |  Estimated APR: 18.7%                       | |
|  |  Gas Fee: ~0.002 LUX                        | |
|  +---------------------------------------------+ |
|                                                   |
|  [!] Warning: Providing liquidity involves        |
|      impermanent loss risk.                       |
|                                                   |
|  [Cancel]                    [Add Liquidity]      |
+--------------------------------------------------+
```

### 3.5 Swap Interface

```
+--------------------------------------------------+
|  Swap Resources                              [X]  |
+--------------------------------------------------+
|                                                   |
|  [Buy Resources]  [Sell Resources]                |
|                                                   |
|  +---------------------------------------------+ |
|  |  You Pay                                    | |
|  |  [____________1000______] HANZO         [v] | |
|  |  Balance: 5,234.56 HANZO                    | |
|  +---------------------------------------------+ |
|                                                   |
|                    [v]                            |
|                                                   |
|  +---------------------------------------------+ |
|  |  You Receive (estimated)                    | |
|  |  [_____________0.81_____] GPU units     [v] | |
|  |  @ 1,234.56 HANZO per unit                  | |
|  +---------------------------------------------+ |
|                                                   |
|  +---------------------------------------------+ |
|  |  Details                                    | |
|  |  Price Impact: 0.12%                        | |
|  |  Minimum Received: 0.80 GPU units           | |
|  |  Trading Fee: 3.00 HANZO (0.3%)             | |
|  |  Best Route: AMM Pool                       | |
|  +---------------------------------------------+ |
|                                                   |
|  [Cancel]                         [Swap]          |
+--------------------------------------------------+
```

### 3.6 Deployment Pool Selector

```
+--------------------------------------------------+
|  Select Deployment Target                    [X]  |
+--------------------------------------------------+
|                                                   |
|  ( ) Local Docker                                 |
|      Deploy to your local Docker environment      |
|                                                   |
|  ( ) Hanzo Cloud [Recommended]                    |
|      Deploy to Hanzo's global infrastructure      |
|      Region: [us-west-1 v]                        |
|                                                   |
|  (x) Compute Pool                                 |
|      Deploy to decentralized compute providers    |
|                                                   |
|  +---------------------------------------------+ |
|  |  Select Pool                                | |
|  |                                             | |
|  |  [x] GPU Pool                               | |
|  |      Price: 1,234.56 HANZO/unit/hr          | |
|  |      Availability: 156 units                | |
|  |      Avg SLA: 98.5%                         | |
|  |                                             | |
|  |  [ ] CPU Pool                               | |
|  |      Price: 0.89 HANZO/unit/hr              | |
|  |      Availability: 2,450 units              | |
|  |      Avg SLA: 99.1%                         | |
|  |                                             | |
|  |  [ ] Docker Pool                            | |
|  |      Price: 2.50 HANZO/container/hr         | |
|  |      Availability: 89 slots                 | |
|  |      Avg SLA: 97.8%                         | |
|  +---------------------------------------------+ |
|                                                   |
|  +---------------------------------------------+ |
|  |  Resource Requirements                      | |
|  |                                             | |
|  |  CPU: [___4____] cores                      | |
|  |  Memory: [__8192__] MB                      | |
|  |  Storage: [___50___] GB                     | |
|  |  GPU: [___1____] units (optional)           | |
|  +---------------------------------------------+ |
|                                                   |
|  +---------------------------------------------+ |
|  |  Budget                                     | |
|  |                                             | |
|  |  Max hourly: [____100___] HANZO             | |
|  |  Max total:  [___5000___] HANZO             | |
|  |                                             | |
|  |  Estimated cost: ~85 HANZO/hr               | |
|  +---------------------------------------------+ |
|                                                   |
|  [Cancel]                   [Deploy to Pool]      |
+--------------------------------------------------+
```

---

## 4. API Design

### 4.1 tRPC Router: pools.ts

```typescript
// /server/api/routers/pools.ts

import { z } from "zod";
import { createTRPCRouter, protectedProcedure } from "@/server/api/trpc";

export const poolsRouter = createTRPCRouter({
  // Get all pools with current stats
  getAll: protectedProcedure.query(async ({ ctx }) => {
    // Fetch from hanzo-node via gRPC
    return await ctx.hanzoNode.pools.getAll();
  }),

  // Get single pool details
  getById: protectedProcedure
    .input(z.object({ poolId: z.string() }))
    .query(async ({ ctx, input }) => {
      return await ctx.hanzoNode.pools.getById(input.poolId);
    }),

  // Get pool price history
  getPriceHistory: protectedProcedure
    .input(z.object({
      poolId: z.string(),
      timeframe: z.enum(["1H", "24H", "7D", "30D", "ALL"]),
    }))
    .query(async ({ ctx, input }) => {
      return await ctx.hanzoNode.pools.getPriceHistory(input);
    }),

  // Get order book depth
  getOrderBook: protectedProcedure
    .input(z.object({
      resourceType: z.number(),
      levels: z.number().default(10),
    }))
    .query(async ({ ctx, input }) => {
      return await ctx.hanzoNode.pools.getOrderBookDepth(input);
    }),

  // Swap resources
  swap: protectedProcedure
    .input(z.object({
      resourceType: z.number(),
      isBuying: z.boolean(),
      amountIn: z.string(),  // BigInt as string
      minAmountOut: z.string(),
    }))
    .mutation(async ({ ctx, input }) => {
      return await ctx.hanzoNode.pools.swap(input);
    }),

  // Add liquidity
  addLiquidity: protectedProcedure
    .input(z.object({
      resourceType: z.number(),
      resourceAmount: z.string(),
      tokenAmount: z.string(),
    }))
    .mutation(async ({ ctx, input }) => {
      return await ctx.hanzoNode.pools.addLiquidity(input);
    }),

  // Remove liquidity
  removeLiquidity: protectedProcedure
    .input(z.object({
      resourceType: z.number(),
      liquidityAmount: z.string(),
    }))
    .mutation(async ({ ctx, input }) => {
      return await ctx.hanzoNode.pools.removeLiquidity(input);
    }),

  // Get user's positions
  getUserPositions: protectedProcedure.query(async ({ ctx }) => {
    return await ctx.hanzoNode.pools.getUserPositions(ctx.userId);
  }),

  // Claim rewards
  claimRewards: protectedProcedure
    .input(z.object({ poolId: z.string() }))
    .mutation(async ({ ctx, input }) => {
      return await ctx.hanzoNode.pools.claimRewards(input.poolId);
    }),

  // Get best execution price
  getBestPrice: protectedProcedure
    .input(z.object({
      resourceType: z.number(),
      isBuying: z.boolean(),
      amount: z.string(),
    }))
    .query(async ({ ctx, input }) => {
      return await ctx.hanzoNode.pools.getBestPrice(input);
    }),
});
```

### 4.2 tRPC Router: deployment.ts (Enhanced)

```typescript
// /server/api/routers/deployment.ts (additions)

// Add pool deployment support
deployToPool: protectedProcedure
  .input(z.object({
    applicationId: z.string(),
    poolId: z.string(),
    resources: z.object({
      cpu: z.number(),
      memory: z.number(),
      storage: z.number(),
      gpu: z.number().optional(),
    }),
    scaling: z.object({
      minReplicas: z.number().default(1),
      maxReplicas: z.number().default(10),
      targetCpuUtilization: z.number().default(70),
    }),
    budget: z.object({
      maxTokensPerHour: z.string(),
      maxTotalTokens: z.string(),
    }),
  }))
  .mutation(async ({ ctx, input }) => {
    // 1. Validate user has sufficient HANZO tokens
    const balance = await ctx.hanzoNode.tokens.getBalance(ctx.userId);
    if (BigInt(balance) < BigInt(input.budget.maxTotalTokens)) {
      throw new TRPCError({
        code: "BAD_REQUEST",
        message: "Insufficient HANZO token balance",
      });
    }

    // 2. Get best providers from pool
    const providers = await ctx.hanzoNode.pools.matchProviders({
      resourceType: getResourceType(input.poolId),
      requiredAmount: calculateRequiredAmount(input.resources),
      minSlaScore: 95,
    });

    // 3. Create deployment order
    const orderId = await ctx.hanzoNode.pools.createOrder({
      ...input,
      providers,
    });

    // 4. Queue deployment job
    await ctx.deploymentQueue.add("pool-deployment", {
      orderId,
      applicationId: input.applicationId,
      config: input,
    });

    return { orderId, status: "pending" };
  }),

// Get pool deployment status
getPoolDeploymentStatus: protectedProcedure
  .input(z.object({ orderId: z.string() }))
  .query(async ({ ctx, input }) => {
    return await ctx.hanzoNode.pools.getOrderStatus(input.orderId);
  }),

// Cancel pool deployment
cancelPoolDeployment: protectedProcedure
  .input(z.object({ orderId: z.string() }))
  .mutation(async ({ ctx, input }) => {
    return await ctx.hanzoNode.pools.cancelOrder(input.orderId);
  }),
```

---

## 5. Deployment Flow

### 5.1 Flow Diagram

```
User selects "Deploy"
        |
        v
+-------------------+
| Target Selection  |
| - Local Docker    |
| - Hanzo Cloud     |
| - Compute Pool    |<---- NEW
+-------------------+
        |
        | (if Pool selected)
        v
+-------------------+
| Pool Selection    |
| - GPU Pool        |
| - CPU Pool        |
| - Docker Pool     |
+-------------------+
        |
        v
+-------------------+
| Resource Config   |
| - CPU cores       |
| - Memory          |
| - Storage         |
| - GPU (optional)  |
+-------------------+
        |
        v
+-------------------+
| Budget Config     |
| - Max hourly      |
| - Max total       |
+-------------------+
        |
        v
+-------------------+
| Price Estimation  |
| - Query AMM price |
| - Query order book|
| - Show best price |
+-------------------+
        |
        v
+-------------------+
| Confirmation      |
| - Review details  |
| - Sign transaction|
+-------------------+
        |
        v
+-------------------+
| Deployment        |
| - Create order    |
| - Match providers |
| - Start workload  |
+-------------------+
        |
        v
+-------------------+
| Monitoring        |
| - Status updates  |
| - Cost tracking   |
| - SLA metrics     |
+-------------------+
```

### 5.2 State Machine

```typescript
enum PoolDeploymentState {
  CREATED = "created",
  MATCHING = "matching",      // Finding providers
  MATCHED = "matched",        // Providers found
  PROVISIONING = "provisioning", // Setting up resources
  RUNNING = "running",        // Workload active
  SCALING = "scaling",        // Auto-scaling in progress
  STOPPING = "stopping",      // Graceful shutdown
  STOPPED = "stopped",        // Deployment stopped
  FAILED = "failed",          // Deployment failed
}

const transitions: Record<PoolDeploymentState, PoolDeploymentState[]> = {
  [PoolDeploymentState.CREATED]: [PoolDeploymentState.MATCHING, PoolDeploymentState.FAILED],
  [PoolDeploymentState.MATCHING]: [PoolDeploymentState.MATCHED, PoolDeploymentState.FAILED],
  [PoolDeploymentState.MATCHED]: [PoolDeploymentState.PROVISIONING, PoolDeploymentState.FAILED],
  [PoolDeploymentState.PROVISIONING]: [PoolDeploymentState.RUNNING, PoolDeploymentState.FAILED],
  [PoolDeploymentState.RUNNING]: [PoolDeploymentState.SCALING, PoolDeploymentState.STOPPING, PoolDeploymentState.FAILED],
  [PoolDeploymentState.SCALING]: [PoolDeploymentState.RUNNING, PoolDeploymentState.FAILED],
  [PoolDeploymentState.STOPPING]: [PoolDeploymentState.STOPPED],
  [PoolDeploymentState.STOPPED]: [],
  [PoolDeploymentState.FAILED]: [],
};
```

---

## 6. hanzo-node Integration

### 6.1 gRPC Service Definition

```protobuf
// pools.proto

syntax = "proto3";

package hanzo.pools;

service PoolsService {
  // Pool queries
  rpc GetAllPools(Empty) returns (PoolsResponse);
  rpc GetPool(GetPoolRequest) returns (Pool);
  rpc GetPriceHistory(PriceHistoryRequest) returns (PriceHistoryResponse);
  rpc GetOrderBook(OrderBookRequest) returns (OrderBookResponse);

  // Trading
  rpc GetBestPrice(BestPriceRequest) returns (BestPriceResponse);
  rpc Swap(SwapRequest) returns (SwapResponse);

  // Liquidity
  rpc AddLiquidity(AddLiquidityRequest) returns (LiquidityResponse);
  rpc RemoveLiquidity(RemoveLiquidityRequest) returns (LiquidityResponse);
  rpc GetUserPositions(GetUserPositionsRequest) returns (PositionsResponse);
  rpc ClaimRewards(ClaimRewardsRequest) returns (ClaimResponse);

  // Deployment
  rpc MatchProviders(MatchProvidersRequest) returns (ProvidersResponse);
  rpc CreateDeploymentOrder(CreateOrderRequest) returns (OrderResponse);
  rpc GetOrderStatus(GetOrderStatusRequest) returns (OrderStatusResponse);
  rpc CancelOrder(CancelOrderRequest) returns (CancelResponse);

  // Streaming
  rpc StreamPoolUpdates(StreamRequest) returns (stream PoolUpdate);
  rpc StreamOrderStatus(StreamOrderRequest) returns (stream OrderStatusUpdate);
}
```

### 6.2 Platform <-> hanzo-node Communication

```typescript
// /server/services/hanzo-node-client.ts

import { credentials, Metadata } from "@grpc/grpc-js";
import { PoolsServiceClient } from "@/generated/pools";

export class HanzoNodeClient {
  private poolsClient: PoolsServiceClient;

  constructor(config: HanzoNodeConfig) {
    this.poolsClient = new PoolsServiceClient(
      config.grpcEndpoint,
      credentials.createSsl()
    );
  }

  // Pool operations
  async getPools(): Promise<Pool[]> {
    return new Promise((resolve, reject) => {
      this.poolsClient.getAllPools({}, (err, response) => {
        if (err) reject(err);
        else resolve(response.pools);
      });
    });
  }

  async swap(request: SwapRequest): Promise<SwapResponse> {
    const metadata = new Metadata();
    metadata.set("x-user-id", request.userId);

    return new Promise((resolve, reject) => {
      this.poolsClient.swap(request, metadata, (err, response) => {
        if (err) reject(err);
        else resolve(response);
      });
    });
  }

  // Streaming for real-time updates
  streamPoolUpdates(poolIds: string[]): AsyncIterable<PoolUpdate> {
    const stream = this.poolsClient.streamPoolUpdates({ poolIds });

    return {
      [Symbol.asyncIterator]() {
        return {
          async next() {
            return new Promise((resolve, reject) => {
              stream.on("data", (update) => resolve({ value: update, done: false }));
              stream.on("end", () => resolve({ value: undefined, done: true }));
              stream.on("error", reject);
            });
          },
        };
      },
    };
  }
}
```

---

## 7. Real-time Updates

### 7.1 WebSocket Events

```typescript
// Pool-related WebSocket events

interface PoolUpdateEvent {
  type: "POOL_UPDATE";
  poolId: string;
  data: {
    price: string;
    liquidity: string;
    volume24h: string;
    utilization: number;
  };
}

interface PositionUpdateEvent {
  type: "POSITION_UPDATE";
  userId: string;
  poolId: string;
  data: {
    liquidityShares: string;
    pendingRewards: string;
    impermanentLoss: number;
  };
}

interface DeploymentStatusEvent {
  type: "DEPLOYMENT_STATUS";
  orderId: string;
  data: {
    state: PoolDeploymentState;
    progress: number;
    message: string;
    providers?: ProviderInfo[];
    metrics?: DeploymentMetrics;
  };
}

interface TradeEvent {
  type: "TRADE";
  poolId: string;
  data: {
    tradeId: string;
    buyer: string;
    seller: string;
    amount: string;
    price: string;
    timestamp: number;
  };
}
```

### 7.2 WebSocket Server Handler

```typescript
// /server/wss/pools.ts

export function handlePoolsWebSocket(wss: WebSocketServer) {
  wss.on("connection", (ws, req) => {
    const userId = getUserFromRequest(req);

    // Subscribe to pool updates
    ws.on("message", async (message) => {
      const { action, payload } = JSON.parse(message.toString());

      switch (action) {
        case "subscribe_pools":
          await subscribeToPoolUpdates(ws, payload.poolIds);
          break;
        case "subscribe_positions":
          await subscribeToPositionUpdates(ws, userId);
          break;
        case "subscribe_deployment":
          await subscribeToDeploymentStatus(ws, payload.orderId);
          break;
        case "unsubscribe":
          await unsubscribe(ws, payload.subscriptionId);
          break;
      }
    });
  });
}
```

---

## 8. Security Considerations

### 8.1 Authentication & Authorization

- All pool operations require authenticated user session
- Transaction signing uses user's wallet (MetaMask, WalletConnect)
- Rate limiting on swap and liquidity operations
- Budget limits enforced at smart contract level

### 8.2 Transaction Security

```typescript
// Transaction validation before submission
async function validatePoolTransaction(
  tx: PoolTransaction,
  user: User
): Promise<ValidationResult> {
  // 1. Verify user balance
  const balance = await getTokenBalance(user.walletAddress);
  if (BigInt(balance) < BigInt(tx.tokenAmount)) {
    return { valid: false, error: "Insufficient balance" };
  }

  // 2. Check slippage protection
  const currentPrice = await getCurrentPrice(tx.poolId);
  const maxSlippage = 0.05; // 5%
  const priceDeviation = Math.abs(
    (Number(tx.expectedPrice) - Number(currentPrice)) / Number(currentPrice)
  );
  if (priceDeviation > maxSlippage) {
    return { valid: false, error: "Price moved too much" };
  }

  // 3. Verify allowance
  const allowance = await getTokenAllowance(user.walletAddress, POOL_CONTRACT);
  if (BigInt(allowance) < BigInt(tx.tokenAmount)) {
    return { valid: false, error: "Insufficient allowance", needsApproval: true };
  }

  return { valid: true };
}
```

---

## 9. Implementation Plan

### Phase 1a: Core Pool UI (Week 1-2)

1. Create pool data models and types
2. Implement PoolsDashboard component
3. Implement PoolCard component
4. Add pools tRPC router
5. Create HanzoNodeClient for gRPC communication

### Phase 1b: Trading Interface (Week 2-3)

1. Implement SwapInterface component
2. Implement OrderBook component
3. Add price estimation logic
4. Implement transaction signing flow

### Phase 1c: Liquidity Management (Week 3-4)

1. Implement AddLiquidityModal
2. Implement RemoveLiquidityModal
3. Implement MyPositions component
4. Add rewards claiming functionality

### Phase 1d: Deployment Integration (Week 4-5)

1. Enhance DeploymentTargetSelector
2. Implement PoolSelector component
3. Implement ResourceEstimator
4. Implement DeploymentBudget
5. Add pool deployment flow to deployment queue

### Phase 1e: Real-time Updates (Week 5-6)

1. Implement WebSocket handlers for pools
2. Add price streaming
3. Add deployment status streaming
4. Implement position update notifications

---

## 10. Testing Strategy

### 10.1 Unit Tests

```typescript
// Example test: Pool swap calculation
describe("Pool Swap", () => {
  it("calculates correct output amount", async () => {
    const pool = mockPool({ resourceAmount: "1000", tokenAmount: "100000" });
    const result = calculateSwapOutput(pool, "1000", true);
    expect(result.amountOut).toBe("9.9"); // With 0.3% fee
    expect(result.priceImpact).toBeLessThan(0.02);
  });

  it("rejects swap with excessive slippage", async () => {
    const pool = mockPool({ resourceAmount: "100", tokenAmount: "100000" });
    await expect(
      validateSwap(pool, "50000", "40", true)
    ).rejects.toThrow("Price impact too high");
  });
});
```

### 10.2 Integration Tests

```typescript
// Example test: Pool deployment flow
describe("Pool Deployment", () => {
  it("creates deployment order and matches providers", async () => {
    const app = await createTestApplication();

    const result = await api.deployment.deployToPool.mutate({
      applicationId: app.id,
      poolId: "gpu-pool",
      resources: { cpu: 4, memory: 8192, storage: 50, gpu: 1 },
      scaling: { minReplicas: 1, maxReplicas: 5, targetCpuUtilization: 70 },
      budget: { maxTokensPerHour: "100", maxTotalTokens: "5000" },
    });

    expect(result.orderId).toBeDefined();
    expect(result.status).toBe("pending");

    // Wait for matching
    await waitFor(() => {
      const status = api.deployment.getPoolDeploymentStatus.query({ orderId: result.orderId });
      return status.state === "matched";
    });
  });
});
```

---

## 11. Metrics & Monitoring

### 11.1 Key Metrics

| Metric | Description | Alert Threshold |
|--------|-------------|-----------------|
| pool_liquidity_total | Total liquidity across all pools | < $100K |
| pool_utilization | Resource utilization per pool | > 90% |
| swap_volume_24h | 24-hour trading volume | N/A |
| deployment_success_rate | Pool deployment success rate | < 95% |
| provider_sla_score | Average provider SLA score | < 97% |
| transaction_latency_p99 | 99th percentile tx latency | > 5s |

### 11.2 Grafana Dashboard

```
Pool Operations Dashboard
|-------------------------------------------------------------|
| Total Liquidity    | 24h Volume    | Active Deployments     |
| $2.4M              | $156K         | 234                    |
|-------------------------------------------------------------|
| [Pool Liquidity Chart - Area]                               |
|-------------------------------------------------------------|
| [Trading Volume Chart - Bar]                                |
|-------------------------------------------------------------|
| Resource Utilization                                        |
| GPU: ████████░░ 78%  | CPU: ████░░░░░░ 45%                  |
| Mem: ██████░░░░ 62%  | Sto: ███░░░░░░░ 34%                  |
|-------------------------------------------------------------|
```

---

## 12. File Structure

```
/app/platform/
  /components/
    /pools/
      PoolsDashboard.tsx
      PoolCard.tsx
      PoolDetailView.tsx
      PoolChart.tsx
      AddLiquidityModal.tsx
      RemoveLiquidityModal.tsx
      SwapInterface.tsx
      OrderBook.tsx
      MyPositions.tsx
      index.ts
    /deployment/
      DeploymentTargetSelector.tsx  (enhanced)
      PoolSelector.tsx
      ResourceEstimator.tsx
      DeploymentBudget.tsx
  /server/
    /api/
      /routers/
        pools.ts
        deployment.ts  (enhanced)
    /services/
      hanzo-node-client.ts
    /wss/
      pools.ts
      deployment-status.ts
  /lib/
    /pools/
      calculations.ts
      types.ts
      constants.ts
    /deployment/
      pool-deployment.ts
  /hooks/
    usePool.ts
    usePoolPositions.ts
    usePoolPrices.ts
    useDeploymentStatus.ts
```

---

## 13. Dependencies

### New Dependencies

```json
{
  "dependencies": {
    "@grpc/grpc-js": "^1.9.0",
    "@grpc/proto-loader": "^0.7.10",
    "recharts": "^2.12.0",
    "ethers": "^6.9.0",
    "@wagmi/core": "^2.0.0",
    "viem": "^2.0.0"
  }
}
```

### hanzo-node Requirements

- gRPC service implementing PoolsService
- ComputeDEX contract integration
- Real-time event streaming capability
- Provider matching algorithm

---

## 14. Open Questions

1. **Fee Structure**: Should platform take additional fee on top of pool fees?
2. **Provider Selection**: How to handle provider preference (price vs. SLA)?
3. **Failover**: How to handle provider failures mid-deployment?
4. **Custody**: Should platform hold funds or use direct wallet interactions?
5. **Multi-pool Deployments**: Support deploying across multiple pools simultaneously?

---

## Appendix A: ASCII Wireframe Reference

### PoolCard Component

```
+---------------------------+
|  [Icon] GPU Pool     [!]  |
+---------------------------+
|                           |
|  1,234.56 HANZO/unit      |
|  +2.3% (24h)              |
|                           |
|  Liquidity   $2.4M        |
|  Volume 24h  $45K         |
|  Utilization 78%          |
|                           |
|  [Trade]  [Add Liquidity] |
+---------------------------+
```

### Position Row

```
+----------------------------------------------------------------+
| [GPU]  GPU Pool                                                |
|        $12,450 liquidity | 0.52% share | 45.2 HANZO pending    |
|        IL: -1.2%                                               |
|        [Manage] [Claim Rewards]                                |
+----------------------------------------------------------------+
```

---

**Document Version**: 1.0
**Last Updated**: January 2026
**Author**: Claude (Architect Mode)
**Status**: Draft for Review
