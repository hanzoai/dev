# Platform Phase 1: Component Implementation Reference

This document provides code templates for the key UI components described in the design document.

---

## 1. Pool Card Component

```tsx
// /app/platform/components/pools/PoolCard.tsx

import { TrendingUp, TrendingDown, Activity } from "lucide-react";
import {
  Card,
  CardContent,
  CardDescription,
  CardHeader,
  CardTitle,
} from "@/components/ui/card";
import { Badge } from "@/components/ui/badge";
import { Button } from "@/components/ui/button";
import { Progress } from "@/components/ui/progress";
import { formatBigInt, formatPercent } from "@/lib/pools/format";
import type { Pool } from "@/lib/pools/types";

interface PoolCardProps {
  pool: Pool;
  onTrade: (poolId: string) => void;
  onAddLiquidity: (poolId: string) => void;
}

const resourceIcons: Record<string, string> = {
  GPU: "gpu",
  CPU: "cpu",
  Memory: "memory",
  Storage: "storage",
  Bandwidth: "network",
  WASM: "code",
  Docker: "container",
  K8S: "kubernetes",
};

export function PoolCard({ pool, onTrade, onAddLiquidity }: PoolCardProps) {
  const priceChange = pool.priceChange24h;
  const isPositive = priceChange >= 0;

  return (
    <Card className="hover:shadow-md transition-shadow">
      <CardHeader className="flex flex-row items-center justify-between pb-2">
        <div className="flex items-center gap-2">
          <div className="p-2 rounded-lg bg-primary/10">
            <Activity className="h-5 w-5 text-primary" />
          </div>
          <div>
            <CardTitle className="text-lg">{pool.name} Pool</CardTitle>
            <CardDescription>{pool.resourceType}</CardDescription>
          </div>
        </div>
        <Badge variant={pool.utilization > 80 ? "destructive" : "secondary"}>
          {pool.utilization > 80 ? "High Demand" : "Available"}
        </Badge>
      </CardHeader>
      <CardContent className="space-y-4">
        {/* Price Display */}
        <div className="flex items-baseline gap-2">
          <span className="text-2xl font-bold">
            {formatBigInt(pool.lastPrice)} HANZO
          </span>
          <span className="text-sm text-muted-foreground">/ unit</span>
          <span
            className={`flex items-center text-sm ${
              isPositive ? "text-green-500" : "text-red-500"
            }`}
          >
            {isPositive ? (
              <TrendingUp className="h-4 w-4 mr-1" />
            ) : (
              <TrendingDown className="h-4 w-4 mr-1" />
            )}
            {formatPercent(Math.abs(priceChange))}
          </span>
        </div>

        {/* Stats Grid */}
        <div className="grid grid-cols-2 gap-4 text-sm">
          <div>
            <span className="text-muted-foreground">Liquidity</span>
            <p className="font-medium">${formatBigInt(pool.tokenAmount, 2)}</p>
          </div>
          <div>
            <span className="text-muted-foreground">24h Volume</span>
            <p className="font-medium">${formatBigInt(pool.volume24h, 2)}</p>
          </div>
        </div>

        {/* Utilization Bar */}
        <div className="space-y-1">
          <div className="flex justify-between text-sm">
            <span className="text-muted-foreground">Utilization</span>
            <span className="font-medium">{pool.utilization}%</span>
          </div>
          <Progress value={pool.utilization} className="h-2" />
        </div>

        {/* Actions */}
        <div className="flex gap-2 pt-2">
          <Button
            variant="outline"
            className="flex-1"
            onClick={() => onTrade(pool.id)}
          >
            Trade
          </Button>
          <Button className="flex-1" onClick={() => onAddLiquidity(pool.id)}>
            Add Liquidity
          </Button>
        </div>
      </CardContent>
    </Card>
  );
}
```

---

## 2. Pools Dashboard Component

```tsx
// /app/platform/components/pools/PoolsDashboard.tsx

import { useState } from "react";
import { RefreshCcw, Plus, Loader2 } from "lucide-react";
import { Button } from "@/components/ui/button";
import {
  Card,
  CardContent,
  CardDescription,
  CardHeader,
  CardTitle,
} from "@/components/ui/card";
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs";
import { api } from "@/utils/api";
import { PoolCard } from "./PoolCard";
import { MyPositions } from "./MyPositions";
import { SwapInterface } from "./SwapInterface";
import { AddLiquidityModal } from "./AddLiquidityModal";

export function PoolsDashboard() {
  const [selectedPool, setSelectedPool] = useState<string | null>(null);
  const [showSwap, setShowSwap] = useState(false);
  const [showAddLiquidity, setShowAddLiquidity] = useState(false);

  const {
    data: pools,
    isLoading,
    refetch,
  } = api.pools.getAll.useQuery(undefined, {
    refetchInterval: 10000, // Refresh every 10 seconds
  });

  const { data: positions } = api.pools.getUserPositions.useQuery();

  const handleTrade = (poolId: string) => {
    setSelectedPool(poolId);
    setShowSwap(true);
  };

  const handleAddLiquidity = (poolId: string) => {
    setSelectedPool(poolId);
    setShowAddLiquidity(true);
  };

  return (
    <div className="space-y-6">
      {/* Header */}
      <div className="flex items-center justify-between">
        <div>
          <h1 className="text-3xl font-bold">Compute Pools</h1>
          <p className="text-muted-foreground">
            Trade compute resources and provide liquidity
          </p>
        </div>
        <div className="flex gap-2">
          <Button variant="outline" onClick={() => refetch()}>
            <RefreshCcw className="h-4 w-4 mr-2" />
            Refresh
          </Button>
          <Button onClick={() => setShowAddLiquidity(true)}>
            <Plus className="h-4 w-4 mr-2" />
            Add Liquidity
          </Button>
        </div>
      </div>

      {/* Tabs */}
      <Tabs defaultValue="pools">
        <TabsList>
          <TabsTrigger value="pools">All Pools</TabsTrigger>
          <TabsTrigger value="positions">My Positions</TabsTrigger>
        </TabsList>

        <TabsContent value="pools" className="mt-6">
          {isLoading ? (
            <div className="flex items-center justify-center py-12">
              <Loader2 className="h-8 w-8 animate-spin text-muted-foreground" />
            </div>
          ) : pools?.length === 0 ? (
            <Card>
              <CardContent className="flex flex-col items-center justify-center py-12">
                <p className="text-muted-foreground">No pools available</p>
              </CardContent>
            </Card>
          ) : (
            <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 xl:grid-cols-4 gap-4">
              {pools?.map((pool) => (
                <PoolCard
                  key={pool.id}
                  pool={pool}
                  onTrade={handleTrade}
                  onAddLiquidity={handleAddLiquidity}
                />
              ))}
            </div>
          )}
        </TabsContent>

        <TabsContent value="positions" className="mt-6">
          <MyPositions positions={positions || []} />
        </TabsContent>
      </Tabs>

      {/* Modals */}
      <SwapInterface
        open={showSwap}
        onClose={() => setShowSwap(false)}
        poolId={selectedPool}
      />
      <AddLiquidityModal
        open={showAddLiquidity}
        onClose={() => setShowAddLiquidity(false)}
        poolId={selectedPool}
      />
    </div>
  );
}
```

---

## 3. Swap Interface Component

```tsx
// /app/platform/components/pools/SwapInterface.tsx

import { useState, useEffect } from "react";
import { ArrowDown, AlertCircle, Loader2 } from "lucide-react";
import { toast } from "sonner";
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogFooter,
  DialogHeader,
  DialogTitle,
} from "@/components/ui/dialog";
import { Button } from "@/components/ui/button";
import { Input } from "@/components/ui/input";
import { Label } from "@/components/ui/label";
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from "@/components/ui/select";
import { Tabs, TabsList, TabsTrigger } from "@/components/ui/tabs";
import { AlertBlock } from "@/components/shared/alert-block";
import { api } from "@/utils/api";
import { formatBigInt, parseBigInt } from "@/lib/pools/format";
import { calculateSlippage, calculatePriceImpact } from "@/lib/pools/calculations";

interface SwapInterfaceProps {
  open: boolean;
  onClose: () => void;
  poolId: string | null;
}

export function SwapInterface({ open, onClose, poolId }: SwapInterfaceProps) {
  const [isBuying, setIsBuying] = useState(true);
  const [amountIn, setAmountIn] = useState("");
  const [amountOut, setAmountOut] = useState("");
  const [slippage, setSlippage] = useState(0.5);

  const { data: pools } = api.pools.getAll.useQuery();
  const { data: balance } = api.user.getTokenBalance.useQuery();

  const selectedPool = pools?.find((p) => p.id === poolId);

  // Get price quote
  const { data: quote, isLoading: isQuoting } = api.pools.getBestPrice.useQuery(
    {
      resourceType: selectedPool?.resourceType || 0,
      isBuying,
      amount: parseBigInt(amountIn || "0").toString(),
    },
    {
      enabled: !!selectedPool && !!amountIn && parseFloat(amountIn) > 0,
      refetchInterval: 5000,
    }
  );

  // Update output amount when quote changes
  useEffect(() => {
    if (quote) {
      setAmountOut(formatBigInt(quote.expectedOutput));
    }
  }, [quote]);

  // Swap mutation
  const { mutateAsync: swap, isLoading: isSwapping } =
    api.pools.swap.useMutation({
      onSuccess: () => {
        toast.success("Swap successful!");
        onClose();
      },
      onError: (error) => {
        toast.error(error.message || "Swap failed");
      },
    });

  const handleSwap = async () => {
    if (!selectedPool || !amountIn || !quote) return;

    const minAmountOut = calculateSlippage(quote.expectedOutput, slippage);

    await swap({
      resourceType: selectedPool.resourceType,
      isBuying,
      amountIn: parseBigInt(amountIn).toString(),
      minAmountOut: minAmountOut.toString(),
    });
  };

  const priceImpact = quote
    ? calculatePriceImpact(
        parseBigInt(amountIn),
        BigInt(quote.expectedOutput),
        selectedPool?.lastPrice || 0n
      )
    : 0;

  const insufficientBalance = balance
    ? parseBigInt(amountIn) > BigInt(balance)
    : false;

  return (
    <Dialog open={open} onOpenChange={onClose}>
      <DialogContent className="sm:max-w-[425px]">
        <DialogHeader>
          <DialogTitle>Swap Resources</DialogTitle>
          <DialogDescription>
            Trade compute resources on the {selectedPool?.name || "selected"} pool
          </DialogDescription>
        </DialogHeader>

        <div className="space-y-4 py-4">
          {/* Buy/Sell Toggle */}
          <Tabs
            value={isBuying ? "buy" : "sell"}
            onValueChange={(v) => setIsBuying(v === "buy")}
          >
            <TabsList className="grid w-full grid-cols-2">
              <TabsTrigger value="buy">Buy Resources</TabsTrigger>
              <TabsTrigger value="sell">Sell Resources</TabsTrigger>
            </TabsList>
          </Tabs>

          {/* Input Amount */}
          <div className="space-y-2">
            <div className="flex justify-between">
              <Label>You Pay</Label>
              <span className="text-sm text-muted-foreground">
                Balance: {formatBigInt(balance || "0")} {isBuying ? "HANZO" : "units"}
              </span>
            </div>
            <div className="flex gap-2">
              <Input
                type="number"
                placeholder="0.00"
                value={amountIn}
                onChange={(e) => setAmountIn(e.target.value)}
                className="flex-1"
              />
              <Button
                variant="outline"
                size="sm"
                onClick={() =>
                  setAmountIn(formatBigInt(balance || "0"))
                }
              >
                MAX
              </Button>
            </div>
          </div>

          {/* Arrow */}
          <div className="flex justify-center">
            <div className="p-2 rounded-full bg-muted">
              <ArrowDown className="h-4 w-4" />
            </div>
          </div>

          {/* Output Amount */}
          <div className="space-y-2">
            <Label>You Receive (estimated)</Label>
            <div className="flex items-center gap-2 p-3 bg-muted rounded-lg">
              <span className="flex-1 text-lg font-medium">
                {isQuoting ? (
                  <Loader2 className="h-4 w-4 animate-spin" />
                ) : (
                  amountOut || "0.00"
                )}
              </span>
              <span className="text-muted-foreground">
                {isBuying ? "units" : "HANZO"}
              </span>
            </div>
            {quote && (
              <p className="text-sm text-muted-foreground">
                @ {formatBigInt(selectedPool?.lastPrice || 0n)} HANZO per unit
              </p>
            )}
          </div>

          {/* Details */}
          {quote && (
            <div className="space-y-2 p-3 bg-muted/50 rounded-lg text-sm">
              <div className="flex justify-between">
                <span className="text-muted-foreground">Price Impact</span>
                <span className={priceImpact > 1 ? "text-yellow-500" : ""}>
                  {priceImpact.toFixed(2)}%
                </span>
              </div>
              <div className="flex justify-between">
                <span className="text-muted-foreground">Minimum Received</span>
                <span>
                  {formatBigInt(calculateSlippage(quote.expectedOutput, slippage))}{" "}
                  {isBuying ? "units" : "HANZO"}
                </span>
              </div>
              <div className="flex justify-between">
                <span className="text-muted-foreground">Trading Fee</span>
                <span>
                  {formatBigInt(
                    (parseBigInt(amountIn) * 3n) / 1000n
                  )}{" "}
                  {isBuying ? "HANZO" : "units"} (0.3%)
                </span>
              </div>
              <div className="flex justify-between">
                <span className="text-muted-foreground">Best Route</span>
                <span>{quote.useAMM ? "AMM Pool" : "Order Book"}</span>
              </div>
            </div>
          )}

          {/* Warnings */}
          {priceImpact > 5 && (
            <AlertBlock type="warning">
              <AlertCircle className="h-4 w-4" />
              High price impact! Consider trading a smaller amount.
            </AlertBlock>
          )}

          {insufficientBalance && (
            <AlertBlock type="error">
              Insufficient balance for this trade.
            </AlertBlock>
          )}
        </div>

        <DialogFooter>
          <Button variant="outline" onClick={onClose}>
            Cancel
          </Button>
          <Button
            onClick={handleSwap}
            disabled={
              isSwapping || isQuoting || insufficientBalance || !amountIn
            }
          >
            {isSwapping ? (
              <>
                <Loader2 className="h-4 w-4 mr-2 animate-spin" />
                Swapping...
              </>
            ) : (
              "Swap"
            )}
          </Button>
        </DialogFooter>
      </DialogContent>
    </Dialog>
  );
}
```

---

## 4. Add Liquidity Modal

```tsx
// /app/platform/components/pools/AddLiquidityModal.tsx

import { useState, useEffect } from "react";
import { AlertTriangle, Loader2 } from "lucide-react";
import { toast } from "sonner";
import {
  Dialog,
  DialogContent,
  DialogDescription,
  DialogFooter,
  DialogHeader,
  DialogTitle,
} from "@/components/ui/dialog";
import { Button } from "@/components/ui/button";
import { Input } from "@/components/ui/input";
import { Label } from "@/components/ui/label";
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from "@/components/ui/select";
import { AlertBlock } from "@/components/shared/alert-block";
import { api } from "@/utils/api";
import { formatBigInt, parseBigInt } from "@/lib/pools/format";

interface AddLiquidityModalProps {
  open: boolean;
  onClose: () => void;
  poolId: string | null;
}

export function AddLiquidityModal({
  open,
  onClose,
  poolId,
}: AddLiquidityModalProps) {
  const [selectedPoolId, setSelectedPoolId] = useState(poolId || "");
  const [resourceAmount, setResourceAmount] = useState("");
  const [tokenAmount, setTokenAmount] = useState("");

  const { data: pools } = api.pools.getAll.useQuery();
  const { data: tokenBalance } = api.user.getTokenBalance.useQuery();
  const { data: resourceBalance } = api.user.getResourceBalance.useQuery({
    resourceType: pools?.find((p) => p.id === selectedPoolId)?.resourceType || 0,
  });

  const selectedPool = pools?.find((p) => p.id === selectedPoolId);

  // Calculate token amount based on resource amount and pool ratio
  useEffect(() => {
    if (selectedPool && resourceAmount) {
      const ratio =
        BigInt(selectedPool.tokenAmount) / BigInt(selectedPool.resourceAmount);
      const calculatedTokens = parseBigInt(resourceAmount) * ratio;
      setTokenAmount(formatBigInt(calculatedTokens));
    }
  }, [resourceAmount, selectedPool]);

  // Add liquidity mutation
  const { mutateAsync: addLiquidity, isLoading } =
    api.pools.addLiquidity.useMutation({
      onSuccess: () => {
        toast.success("Liquidity added successfully!");
        onClose();
      },
      onError: (error) => {
        toast.error(error.message || "Failed to add liquidity");
      },
    });

  const handleAddLiquidity = async () => {
    if (!selectedPool || !resourceAmount || !tokenAmount) return;

    await addLiquidity({
      resourceType: selectedPool.resourceType,
      resourceAmount: parseBigInt(resourceAmount).toString(),
      tokenAmount: parseBigInt(tokenAmount).toString(),
    });
  };

  // Calculate estimated share and APR
  const estimatedShare = selectedPool
    ? (
        (parseBigInt(tokenAmount) * 100n) /
        (BigInt(selectedPool.tokenAmount) + parseBigInt(tokenAmount))
      ).toString()
    : "0";

  const insufficientTokens = tokenBalance
    ? parseBigInt(tokenAmount) > BigInt(tokenBalance)
    : false;

  const insufficientResources = resourceBalance
    ? parseBigInt(resourceAmount) > BigInt(resourceBalance)
    : false;

  return (
    <Dialog open={open} onOpenChange={onClose}>
      <DialogContent className="sm:max-w-[500px]">
        <DialogHeader>
          <DialogTitle>Add Liquidity</DialogTitle>
          <DialogDescription>
            Provide liquidity to earn fees from trades
          </DialogDescription>
        </DialogHeader>

        <div className="space-y-4 py-4">
          {/* Pool Selection */}
          {!poolId && (
            <div className="space-y-2">
              <Label>Select Pool</Label>
              <Select
                value={selectedPoolId}
                onValueChange={setSelectedPoolId}
              >
                <SelectTrigger>
                  <SelectValue placeholder="Select a pool" />
                </SelectTrigger>
                <SelectContent>
                  {pools?.map((pool) => (
                    <SelectItem key={pool.id} value={pool.id}>
                      {pool.name} Pool
                    </SelectItem>
                  ))}
                </SelectContent>
              </Select>
            </div>
          )}

          {/* Resource Amount */}
          <div className="space-y-2">
            <div className="flex justify-between">
              <Label>{selectedPool?.name || "Resource"} Units</Label>
              <span className="text-sm text-muted-foreground">
                Balance: {formatBigInt(resourceBalance || "0")} units
              </span>
            </div>
            <Input
              type="number"
              placeholder="0.00"
              value={resourceAmount}
              onChange={(e) => setResourceAmount(e.target.value)}
            />
          </div>

          {/* Token Amount */}
          <div className="space-y-2">
            <div className="flex justify-between">
              <Label>HANZO Tokens</Label>
              <span className="text-sm text-muted-foreground">
                Balance: {formatBigInt(tokenBalance || "0")} HANZO
              </span>
            </div>
            <Input
              type="number"
              placeholder="0.00"
              value={tokenAmount}
              onChange={(e) => setTokenAmount(e.target.value)}
            />
          </div>

          {/* Summary */}
          {selectedPool && resourceAmount && (
            <div className="space-y-2 p-4 bg-muted/50 rounded-lg">
              <h4 className="font-medium">Summary</h4>
              <div className="space-y-1 text-sm">
                <div className="flex justify-between">
                  <span className="text-muted-foreground">Share of Pool</span>
                  <span>~{estimatedShare}%</span>
                </div>
                <div className="flex justify-between">
                  <span className="text-muted-foreground">Estimated APR</span>
                  <span className="text-green-500">{selectedPool.apr}%</span>
                </div>
                <div className="flex justify-between">
                  <span className="text-muted-foreground">Gas Fee</span>
                  <span>~0.002 LUX</span>
                </div>
              </div>
            </div>
          )}

          {/* Warning */}
          <AlertBlock type="warning" className="flex items-start gap-2">
            <AlertTriangle className="h-4 w-4 mt-0.5 shrink-0" />
            <div className="text-sm">
              Providing liquidity involves impermanent loss risk. Your share
              value may change based on price movements.
            </div>
          </AlertBlock>

          {/* Validation Errors */}
          {insufficientTokens && (
            <AlertBlock type="error">
              Insufficient HANZO token balance.
            </AlertBlock>
          )}
          {insufficientResources && (
            <AlertBlock type="error">
              Insufficient resource balance.
            </AlertBlock>
          )}
        </div>

        <DialogFooter>
          <Button variant="outline" onClick={onClose}>
            Cancel
          </Button>
          <Button
            onClick={handleAddLiquidity}
            disabled={
              isLoading ||
              !selectedPoolId ||
              !resourceAmount ||
              !tokenAmount ||
              insufficientTokens ||
              insufficientResources
            }
          >
            {isLoading ? (
              <>
                <Loader2 className="h-4 w-4 mr-2 animate-spin" />
                Adding...
              </>
            ) : (
              "Add Liquidity"
            )}
          </Button>
        </DialogFooter>
      </DialogContent>
    </Dialog>
  );
}
```

---

## 5. Enhanced Deployment Target Selector

```tsx
// /app/platform/components/deployment/DeploymentTargetSelector.tsx

import { useState } from "react";
import { Cloud, Server, Network, Loader2 } from "lucide-react";
import {
  Card,
  CardContent,
  CardDescription,
  CardHeader,
  CardTitle,
} from "@/components/ui/card";
import { RadioGroup, RadioGroupItem } from "@/components/ui/radio-group";
import { Label } from "@/components/ui/label";
import { Badge } from "@/components/ui/badge";
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from "@/components/ui/select";
import { api } from "@/utils/api";
import { PoolSelector } from "./PoolSelector";
import { ResourceEstimator } from "./ResourceEstimator";
import { DeploymentBudget } from "./DeploymentBudget";
import { HANZO_CLOUD_CONFIG, isHanzoCloudEnabled } from "@/lib/hanzo-cloud-config";

interface DeploymentTarget {
  type: "local" | "cloud" | "pool";
  region?: string;
  poolId?: string;
  serverId?: string;
}

interface ResourceRequirements {
  cpu: number;
  memory: number;
  storage: number;
  gpu?: number;
}

interface Budget {
  maxTokensPerHour: string;
  maxTotalTokens: string;
}

interface DeploymentTargetSelectorProps {
  onTargetChange: (target: DeploymentTarget) => void;
  onResourcesChange: (resources: ResourceRequirements) => void;
  onBudgetChange: (budget: Budget) => void;
  defaultTarget?: "local" | "cloud" | "pool";
}

export function DeploymentTargetSelector({
  onTargetChange,
  onResourcesChange,
  onBudgetChange,
  defaultTarget = "local",
}: DeploymentTargetSelectorProps) {
  const [selectedTarget, setSelectedTarget] = useState(defaultTarget);
  const [selectedRegion, setSelectedRegion] = useState("us-west-1");
  const [selectedPoolId, setSelectedPoolId] = useState<string | null>(null);
  const [resources, setResources] = useState<ResourceRequirements>({
    cpu: 4,
    memory: 8192,
    storage: 50,
  });
  const [budget, setBudget] = useState<Budget>({
    maxTokensPerHour: "100",
    maxTotalTokens: "5000",
  });

  const cloudEnabled = isHanzoCloudEnabled();
  const { deploymentTargets } = HANZO_CLOUD_CONFIG;

  // Fetch available pools
  const { data: pools, isLoading: isLoadingPools } = api.pools.getAll.useQuery();

  const handleTargetChange = (value: string) => {
    const target = value as "local" | "cloud" | "pool";
    setSelectedTarget(target);
    onTargetChange({
      type: target,
      region: target === "cloud" ? selectedRegion : undefined,
      poolId: target === "pool" ? selectedPoolId || undefined : undefined,
    });
  };

  const handleRegionChange = (value: string) => {
    setSelectedRegion(value);
    if (selectedTarget === "cloud") {
      onTargetChange({ type: "cloud", region: value });
    }
  };

  const handlePoolChange = (poolId: string) => {
    setSelectedPoolId(poolId);
    if (selectedTarget === "pool") {
      onTargetChange({ type: "pool", poolId });
    }
  };

  const handleResourcesChange = (newResources: ResourceRequirements) => {
    setResources(newResources);
    onResourcesChange(newResources);
  };

  const handleBudgetChange = (newBudget: Budget) => {
    setBudget(newBudget);
    onBudgetChange(newBudget);
  };

  return (
    <div className="space-y-6">
      {/* Target Selection Card */}
      <Card>
        <CardHeader>
          <CardTitle>Deployment Target</CardTitle>
          <CardDescription>
            Choose where to deploy your application
          </CardDescription>
        </CardHeader>
        <CardContent className="space-y-6">
          <RadioGroup value={selectedTarget} onValueChange={handleTargetChange}>
            {/* Local Docker */}
            {deploymentTargets.local.enabled && (
              <div className="flex items-start space-x-3 p-4 border rounded-lg hover:bg-muted/50 transition-colors">
                <RadioGroupItem value="local" id="local" className="mt-1" />
                <Label htmlFor="local" className="flex-1 cursor-pointer">
                  <div className="flex items-center space-x-2">
                    <Server className="h-5 w-5 text-muted-foreground" />
                    <span className="font-medium">Local Docker</span>
                    <Badge variant="outline">Self-Hosted</Badge>
                  </div>
                  <p className="text-sm text-muted-foreground mt-1">
                    Deploy to your local Docker environment with full control
                  </p>
                </Label>
              </div>
            )}

            {/* Hanzo Cloud */}
            {cloudEnabled && (
              <div className="flex items-start space-x-3 p-4 border rounded-lg hover:bg-muted/50 transition-colors">
                <RadioGroupItem value="cloud" id="cloud" className="mt-1" />
                <Label htmlFor="cloud" className="flex-1 cursor-pointer">
                  <div className="flex items-center space-x-2">
                    <Cloud className="h-5 w-5 text-primary" />
                    <span className="font-medium">Hanzo Cloud</span>
                    <Badge>Recommended</Badge>
                  </div>
                  <p className="text-sm text-muted-foreground mt-1">
                    Deploy to Hanzo's global infrastructure with auto-scaling
                  </p>
                </Label>
              </div>
            )}

            {/* Compute Pool - NEW */}
            <div className="flex items-start space-x-3 p-4 border rounded-lg hover:bg-muted/50 transition-colors">
              <RadioGroupItem value="pool" id="pool" className="mt-1" />
              <Label htmlFor="pool" className="flex-1 cursor-pointer">
                <div className="flex items-center space-x-2">
                  <Network className="h-5 w-5 text-purple-500" />
                  <span className="font-medium">Compute Pool</span>
                  <Badge variant="secondary">Decentralized</Badge>
                </div>
                <p className="text-sm text-muted-foreground mt-1">
                  Deploy to decentralized compute providers with HANZO tokens
                </p>
              </Label>
            </div>
          </RadioGroup>

          {/* Region Selection (Cloud) */}
          {selectedTarget === "cloud" && deploymentTargets.hanzoCloud.regions && (
            <div className="pt-4 border-t">
              <Label htmlFor="region" className="text-sm font-medium">
                Select Region
              </Label>
              <Select value={selectedRegion} onValueChange={handleRegionChange}>
                <SelectTrigger id="region" className="mt-2">
                  <SelectValue placeholder="Select a region" />
                </SelectTrigger>
                <SelectContent>
                  {deploymentTargets.hanzoCloud.regions.map((region) => (
                    <SelectItem key={region.id} value={region.id}>
                      {region.name}
                      <span className="text-xs text-muted-foreground ml-2">
                        ({region.id})
                      </span>
                    </SelectItem>
                  ))}
                </SelectContent>
              </Select>
            </div>
          )}

          {/* Pool Selection */}
          {selectedTarget === "pool" && (
            <div className="pt-4 border-t">
              {isLoadingPools ? (
                <div className="flex items-center justify-center py-8">
                  <Loader2 className="h-6 w-6 animate-spin text-muted-foreground" />
                </div>
              ) : (
                <PoolSelector
                  pools={pools || []}
                  selectedPoolId={selectedPoolId}
                  onPoolSelect={handlePoolChange}
                />
              )}
            </div>
          )}
        </CardContent>
      </Card>

      {/* Resource Requirements (for Pool deployments) */}
      {selectedTarget === "pool" && selectedPoolId && (
        <ResourceEstimator
          poolId={selectedPoolId}
          resources={resources}
          onResourcesChange={handleResourcesChange}
        />
      )}

      {/* Budget Configuration (for Pool deployments) */}
      {selectedTarget === "pool" && selectedPoolId && (
        <DeploymentBudget
          poolId={selectedPoolId}
          resources={resources}
          budget={budget}
          onBudgetChange={handleBudgetChange}
        />
      )}
    </div>
  );
}
```

---

## 6. Pool Selector Component

```tsx
// /app/platform/components/deployment/PoolSelector.tsx

import { Check, Activity, Zap, HardDrive, Cpu, Container, Box } from "lucide-react";
import { cn } from "@/lib/utils";
import type { Pool } from "@/lib/pools/types";
import { formatBigInt } from "@/lib/pools/format";

interface PoolSelectorProps {
  pools: Pool[];
  selectedPoolId: string | null;
  onPoolSelect: (poolId: string) => void;
}

const poolIcons: Record<string, React.ReactNode> = {
  GPU: <Zap className="h-5 w-5" />,
  CPU: <Cpu className="h-5 w-5" />,
  Memory: <Activity className="h-5 w-5" />,
  Storage: <HardDrive className="h-5 w-5" />,
  Docker: <Container className="h-5 w-5" />,
  K8S: <Box className="h-5 w-5" />,
};

export function PoolSelector({
  pools,
  selectedPoolId,
  onPoolSelect,
}: PoolSelectorProps) {
  return (
    <div className="space-y-3">
      <h4 className="text-sm font-medium">Select Compute Pool</h4>
      <div className="space-y-2">
        {pools.map((pool) => {
          const isSelected = pool.id === selectedPoolId;
          const Icon = poolIcons[pool.resourceType] || Activity;

          return (
            <button
              key={pool.id}
              onClick={() => onPoolSelect(pool.id)}
              className={cn(
                "w-full flex items-center gap-4 p-4 rounded-lg border transition-all text-left",
                isSelected
                  ? "border-primary bg-primary/5"
                  : "border-border hover:border-primary/50 hover:bg-muted/50"
              )}
            >
              {/* Selection Indicator */}
              <div
                className={cn(
                  "flex h-5 w-5 shrink-0 items-center justify-center rounded-full border",
                  isSelected
                    ? "border-primary bg-primary text-primary-foreground"
                    : "border-muted-foreground"
                )}
              >
                {isSelected && <Check className="h-3 w-3" />}
              </div>

              {/* Pool Icon */}
              <div
                className={cn(
                  "p-2 rounded-lg",
                  isSelected ? "bg-primary/10" : "bg-muted"
                )}
              >
                {Icon}
              </div>

              {/* Pool Info */}
              <div className="flex-1 min-w-0">
                <div className="flex items-center gap-2">
                  <span className="font-medium">{pool.name} Pool</span>
                </div>
                <div className="flex items-center gap-4 mt-1 text-sm text-muted-foreground">
                  <span>{formatBigInt(pool.lastPrice)} HANZO/unit/hr</span>
                  <span>|</span>
                  <span>{formatBigInt(pool.resourceAmount)} units available</span>
                </div>
              </div>

              {/* SLA Score */}
              <div className="text-right shrink-0">
                <div className="text-sm font-medium">{pool.avgSlaScore}%</div>
                <div className="text-xs text-muted-foreground">Avg SLA</div>
              </div>
            </button>
          );
        })}
      </div>
    </div>
  );
}
```

---

## 7. Resource Estimator Component

```tsx
// /app/platform/components/deployment/ResourceEstimator.tsx

import { useEffect, useState } from "react";
import { Cpu, MemoryStick, HardDrive, Zap, Loader2 } from "lucide-react";
import {
  Card,
  CardContent,
  CardDescription,
  CardHeader,
  CardTitle,
} from "@/components/ui/card";
import { Input } from "@/components/ui/input";
import { Label } from "@/components/ui/label";
import { Slider } from "@/components/ui/slider";
import { api } from "@/utils/api";
import { formatBigInt } from "@/lib/pools/format";

interface ResourceRequirements {
  cpu: number;
  memory: number;
  storage: number;
  gpu?: number;
}

interface ResourceEstimatorProps {
  poolId: string;
  resources: ResourceRequirements;
  onResourcesChange: (resources: ResourceRequirements) => void;
}

export function ResourceEstimator({
  poolId,
  resources,
  onResourcesChange,
}: ResourceEstimatorProps) {
  const { data: pool } = api.pools.getById.useQuery({ poolId });
  const { data: estimate, isLoading } = api.pools.getBestPrice.useQuery(
    {
      resourceType: pool?.resourceType || 0,
      isBuying: true,
      amount: calculateTotalResources(resources).toString(),
    },
    {
      enabled: !!pool,
    }
  );

  const updateResource = (key: keyof ResourceRequirements, value: number) => {
    onResourcesChange({ ...resources, [key]: value });
  };

  return (
    <Card>
      <CardHeader>
        <CardTitle>Resource Requirements</CardTitle>
        <CardDescription>
          Configure the resources needed for your deployment
        </CardDescription>
      </CardHeader>
      <CardContent className="space-y-6">
        {/* CPU */}
        <div className="space-y-3">
          <div className="flex items-center justify-between">
            <Label className="flex items-center gap-2">
              <Cpu className="h-4 w-4" />
              CPU Cores
            </Label>
            <span className="text-sm font-medium">{resources.cpu} cores</span>
          </div>
          <Slider
            value={[resources.cpu]}
            onValueChange={([value]) => updateResource("cpu", value)}
            min={1}
            max={32}
            step={1}
          />
        </div>

        {/* Memory */}
        <div className="space-y-3">
          <div className="flex items-center justify-between">
            <Label className="flex items-center gap-2">
              <MemoryStick className="h-4 w-4" />
              Memory
            </Label>
            <span className="text-sm font-medium">
              {resources.memory >= 1024
                ? `${(resources.memory / 1024).toFixed(1)} GB`
                : `${resources.memory} MB`}
            </span>
          </div>
          <Slider
            value={[resources.memory]}
            onValueChange={([value]) => updateResource("memory", value)}
            min={512}
            max={65536}
            step={512}
          />
        </div>

        {/* Storage */}
        <div className="space-y-3">
          <div className="flex items-center justify-between">
            <Label className="flex items-center gap-2">
              <HardDrive className="h-4 w-4" />
              Storage
            </Label>
            <span className="text-sm font-medium">{resources.storage} GB</span>
          </div>
          <Slider
            value={[resources.storage]}
            onValueChange={([value]) => updateResource("storage", value)}
            min={10}
            max={500}
            step={10}
          />
        </div>

        {/* GPU (Optional) */}
        {pool?.resourceType === "GPU" && (
          <div className="space-y-3">
            <div className="flex items-center justify-between">
              <Label className="flex items-center gap-2">
                <Zap className="h-4 w-4" />
                GPU Units
              </Label>
              <span className="text-sm font-medium">
                {resources.gpu || 0} units
              </span>
            </div>
            <Slider
              value={[resources.gpu || 0]}
              onValueChange={([value]) => updateResource("gpu", value)}
              min={0}
              max={8}
              step={1}
            />
          </div>
        )}

        {/* Cost Estimate */}
        <div className="pt-4 border-t">
          <div className="flex items-center justify-between">
            <span className="text-sm text-muted-foreground">
              Estimated hourly cost
            </span>
            {isLoading ? (
              <Loader2 className="h-4 w-4 animate-spin" />
            ) : (
              <span className="text-lg font-bold">
                ~{formatBigInt(estimate?.expectedOutput || "0")} HANZO/hr
              </span>
            )}
          </div>
        </div>
      </CardContent>
    </Card>
  );
}

function calculateTotalResources(resources: ResourceRequirements): bigint {
  // Simplified calculation - in production this would be more complex
  return BigInt(
    resources.cpu * 10 +
      Math.floor(resources.memory / 1024) * 5 +
      Math.floor(resources.storage / 10) +
      (resources.gpu || 0) * 100
  );
}
```

---

## 8. Utility Functions

```typescript
// /lib/pools/format.ts

export function formatBigInt(value: string | bigint, decimals: number = 2): string {
  const bn = typeof value === "string" ? BigInt(value) : value;
  const divisor = BigInt(10 ** 18);
  const whole = bn / divisor;
  const fraction = bn % divisor;

  if (decimals === 0) {
    return whole.toString();
  }

  const fractionStr = fraction.toString().padStart(18, "0").slice(0, decimals);
  return `${whole}.${fractionStr}`;
}

export function parseBigInt(value: string): bigint {
  const [whole, fraction = ""] = value.split(".");
  const paddedFraction = fraction.padEnd(18, "0").slice(0, 18);
  return BigInt(whole) * BigInt(10 ** 18) + BigInt(paddedFraction);
}

export function formatPercent(value: number): string {
  return `${(value * 100).toFixed(2)}%`;
}

export function formatUSD(value: bigint): string {
  const usd = Number(formatBigInt(value, 2));
  return new Intl.NumberFormat("en-US", {
    style: "currency",
    currency: "USD",
  }).format(usd);
}


// /lib/pools/calculations.ts

export function calculateSlippage(amount: string | bigint, slippagePercent: number): bigint {
  const bn = typeof amount === "string" ? BigInt(amount) : amount;
  const slippageBps = BigInt(Math.floor(slippagePercent * 100));
  return bn - (bn * slippageBps) / 10000n;
}

export function calculatePriceImpact(
  amountIn: bigint,
  amountOut: bigint,
  spotPrice: bigint
): number {
  if (spotPrice === 0n || amountIn === 0n) return 0;

  const expectedOut = (amountIn * BigInt(10 ** 18)) / spotPrice;
  if (expectedOut === 0n) return 0;

  const impact = Number(((expectedOut - amountOut) * 10000n) / expectedOut);
  return impact / 100; // Convert to percentage
}

export function calculateImpermanentLoss(
  priceRatio: number // currentPrice / entryPrice
): number {
  // IL = 2 * sqrt(priceRatio) / (1 + priceRatio) - 1
  const sqrtRatio = Math.sqrt(priceRatio);
  return (2 * sqrtRatio) / (1 + priceRatio) - 1;
}

export function calculateAPR(
  volume24h: bigint,
  liquidity: bigint,
  feeRate: number = 0.003
): number {
  if (liquidity === 0n) return 0;

  const dailyFees = Number(volume24h) * feeRate;
  const dailyReturn = dailyFees / Number(liquidity);
  return dailyReturn * 365 * 100; // Annual percentage
}


// /lib/pools/types.ts

export enum ResourceType {
  CPU = 0,
  GPU = 1,
  Memory = 2,
  Storage = 3,
  Bandwidth = 4,
  WASM = 5,
  Docker = 6,
  K8S = 7,
}

export interface Pool {
  id: string;
  name: string;
  resourceType: ResourceType;
  resourceAmount: string;
  tokenAmount: string;
  totalLiquidity: string;
  feeRate: number;
  utilization: number;
  lastPrice: string;
  volume24h: string;
  priceChange24h: number;
  avgSlaScore: number;
  apr: number;
}

export interface LiquidityPosition {
  poolId: string;
  resourceType: ResourceType;
  liquidityShares: string;
  resourceAmount: string;
  tokenAmount: string;
  pendingRewards: string;
  entryPrice: string;
  impermanentLoss: number;
}

export interface OrderBookEntry {
  orderId: string;
  provider: string;
  resourceType: ResourceType;
  amount: string;
  pricePerUnit: string;
  duration: number;
  slaScore: number;
  attestation?: string;
  createdAt: Date;
  expiresAt: Date;
}

export interface SwapQuote {
  expectedOutput: string;
  priceImpact: number;
  tradingFee: string;
  useAMM: boolean;
  minOutput: string;
}
```

---

## Summary

This implementation reference provides:

1. **PoolCard** - Display individual pool information with actions
2. **PoolsDashboard** - Main dashboard with tabs for pools and positions
3. **SwapInterface** - Modal for trading resources
4. **AddLiquidityModal** - Modal for providing liquidity
5. **DeploymentTargetSelector** - Enhanced with pool deployment option
6. **PoolSelector** - Select pool for deployment
7. **ResourceEstimator** - Configure deployment resources
8. **Utility functions** - Formatting and calculations

All components follow the existing platform patterns using:
- Radix UI primitives
- Tailwind CSS styling
- tRPC for API calls
- Sonner for toast notifications
- Consistent card and modal patterns

**Document Version**: 1.0
**Last Updated**: January 2026
