import * as React from 'react';
import {
  Cpu,
  HardDrive,
  Database,
  Server,
  Wifi,
  Box,
  Container,
  Cloud,
  AlertTriangle,
  Info,
  ArrowRight,
} from 'lucide-react';
import { cn } from '@/lib/utils';
import { parseBigInt, formatTokenAmount, applySlippage } from '@/lib/utils';
import { Modal, ModalFooter } from '@/components/ui/modal';
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import { Badge } from '@/components/ui/badge';
import {
  ResourceType,
  ResourceTypeLabels,
  ResourceTypeUnits,
  AddLiquidityParams,
} from '@/types';

/** Icon mapping for resource types */
const ResourceIcons: Record<ResourceType, React.ComponentType<{ className?: string }>> = {
  [ResourceType.CPU]: Cpu,
  [ResourceType.GPU]: HardDrive,
  [ResourceType.Memory]: Database,
  [ResourceType.Storage]: Server,
  [ResourceType.Bandwidth]: Wifi,
  [ResourceType.WASM]: Box,
  [ResourceType.Docker]: Container,
  [ResourceType.K8S]: Cloud,
};

export interface CreatePoolModalProps {
  isOpen: boolean;
  onClose: () => void;
  onSubmit: (params: AddLiquidityParams) => Promise<void>;
  userResourceBalances: Record<ResourceType, bigint>;
  userTokenBalance: bigint;
  existingPools: ResourceType[];
}

type Step = 'select-type' | 'configure' | 'confirm';

export function CreatePoolModal({
  isOpen,
  onClose,
  onSubmit,
  userResourceBalances,
  userTokenBalance,
  existingPools,
}: CreatePoolModalProps) {
  const [step, setStep] = React.useState<Step>('select-type');
  const [selectedType, setSelectedType] = React.useState<ResourceType | null>(null);
  const [resourceAmount, setResourceAmount] = React.useState('');
  const [tokenAmount, setTokenAmount] = React.useState('');
  const [slippage, setSlippage] = React.useState('0.5');
  const [loading, setLoading] = React.useState(false);
  const [error, setError] = React.useState<string | null>(null);

  // Reset state when modal closes
  React.useEffect(() => {
    if (!isOpen) {
      setStep('select-type');
      setSelectedType(null);
      setResourceAmount('');
      setTokenAmount('');
      setError(null);
    }
  }, [isOpen]);

  const availableTypes = Object.values(ResourceType)
    .filter((type) => typeof type === 'number')
    .filter((type) => !existingPools.includes(type as ResourceType)) as ResourceType[];

  const selectedLabel = selectedType !== null ? ResourceTypeLabels[selectedType] : '';
  const selectedUnit = selectedType !== null ? ResourceTypeUnits[selectedType] : '';
  const userResourceBalance = selectedType !== null ? userResourceBalances[selectedType] : 0n;

  const resourceAmountBigInt = resourceAmount ? parseBigInt(resourceAmount, 18) : 0n;
  const tokenAmountBigInt = tokenAmount ? parseBigInt(tokenAmount, 18) : 0n;
  const slippageNum = parseFloat(slippage) || 0.5;

  const validation = React.useMemo(() => {
    if (!selectedType) return { valid: false, error: 'Select a resource type' };
    if (resourceAmountBigInt <= 0n) return { valid: false, error: 'Enter resource amount' };
    if (tokenAmountBigInt <= 0n) return { valid: false, error: 'Enter token amount' };
    if (resourceAmountBigInt > userResourceBalance) {
      return { valid: false, error: `Insufficient ${selectedLabel} balance` };
    }
    if (tokenAmountBigInt > userTokenBalance) {
      return { valid: false, error: 'Insufficient HANZO balance' };
    }
    if (slippageNum <= 0 || slippageNum > 50) {
      return { valid: false, error: 'Invalid slippage (0.01-50%)' };
    }
    return { valid: true, error: null };
  }, [
    selectedType,
    resourceAmountBigInt,
    tokenAmountBigInt,
    userResourceBalance,
    userTokenBalance,
    selectedLabel,
    slippageNum,
  ]);

  const handleSubmit = async () => {
    if (!validation.valid || selectedType === null) return;

    setLoading(true);
    setError(null);

    try {
      await onSubmit({
        resourceType: selectedType,
        resourceAmount: resourceAmountBigInt,
        tokenAmount: tokenAmountBigInt,
        slippageTolerance: slippageNum / 100,
      });
      onClose();
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Failed to create pool');
    } finally {
      setLoading(false);
    }
  };

  const setMaxResource = () => {
    setResourceAmount(formatTokenAmount(userResourceBalance, 18, 18));
  };

  const setMaxToken = () => {
    setTokenAmount(formatTokenAmount(userTokenBalance, 18, 18));
  };

  return (
    <Modal
      isOpen={isOpen}
      onClose={onClose}
      title={
        step === 'select-type'
          ? 'Create New Pool'
          : step === 'configure'
          ? `Create ${selectedLabel} Pool`
          : 'Confirm Pool Creation'
      }
      description={
        step === 'select-type'
          ? 'Select a resource type to create a new liquidity pool.'
          : step === 'configure'
          ? `Configure initial liquidity for the ${selectedLabel} pool.`
          : 'Review and confirm your pool creation.'
      }
      className="max-w-lg"
    >
      {step === 'select-type' && (
        <div className="space-y-4">
          {availableTypes.length === 0 ? (
            <div className="rounded-lg border border-dashed p-6 text-center">
              <p className="text-muted-foreground">
                All pool types have already been created.
              </p>
            </div>
          ) : (
            <div className="grid grid-cols-2 gap-3">
              {availableTypes.map((type) => {
                const Icon = ResourceIcons[type];
                const label = ResourceTypeLabels[type];
                const balance = userResourceBalances[type];

                return (
                  <button
                    key={type}
                    className={cn(
                      'flex flex-col items-center gap-2 rounded-lg border p-4 transition-colors hover:bg-accent',
                      selectedType === type && 'border-primary bg-accent'
                    )}
                    onClick={() => setSelectedType(type)}
                  >
                    <Icon className="h-8 w-8" />
                    <span className="font-medium">{label}</span>
                    <span className="text-xs text-muted-foreground">
                      Balance: {formatTokenAmount(balance, 18, 2)}
                    </span>
                  </button>
                );
              })}
            </div>
          )}

          <ModalFooter>
            <Button variant="outline" onClick={onClose}>
              Cancel
            </Button>
            <Button
              disabled={selectedType === null}
              onClick={() => setStep('configure')}
            >
              Continue
              <ArrowRight className="ml-2 h-4 w-4" />
            </Button>
          </ModalFooter>
        </div>
      )}

      {step === 'configure' && selectedType !== null && (
        <div className="space-y-4">
          {/* Resource Amount Input */}
          <div className="space-y-2">
            <div className="flex items-center justify-between">
              <label className="text-sm font-medium">{selectedLabel} Amount</label>
              <Button
                variant="ghost"
                size="sm"
                className="h-6 px-2 text-xs"
                onClick={setMaxResource}
              >
                Max
              </Button>
            </div>
            <Input
              type="number"
              placeholder="0.00"
              value={resourceAmount}
              onChange={(e) => setResourceAmount(e.target.value)}
              rightElement={
                <span className="text-sm text-muted-foreground">{selectedUnit}</span>
              }
              hint={`Balance: ${formatTokenAmount(userResourceBalance)} ${selectedUnit}`}
            />
          </div>

          {/* Token Amount Input */}
          <div className="space-y-2">
            <div className="flex items-center justify-between">
              <label className="text-sm font-medium">HANZO Amount</label>
              <Button
                variant="ghost"
                size="sm"
                className="h-6 px-2 text-xs"
                onClick={setMaxToken}
              >
                Max
              </Button>
            </div>
            <Input
              type="number"
              placeholder="0.00"
              value={tokenAmount}
              onChange={(e) => setTokenAmount(e.target.value)}
              rightElement={
                <span className="text-sm text-muted-foreground">HANZO</span>
              }
              hint={`Balance: ${formatTokenAmount(userTokenBalance)} HANZO`}
            />
          </div>

          {/* Slippage Tolerance */}
          <div className="space-y-2">
            <label className="text-sm font-medium">Slippage Tolerance</label>
            <div className="flex gap-2">
              {['0.1', '0.5', '1.0'].map((value) => (
                <Button
                  key={value}
                  variant={slippage === value ? 'secondary' : 'outline'}
                  size="sm"
                  onClick={() => setSlippage(value)}
                >
                  {value}%
                </Button>
              ))}
              <Input
                type="number"
                placeholder="Custom"
                value={slippage}
                onChange={(e) => setSlippage(e.target.value)}
                className="w-24"
                rightElement={<span className="text-sm text-muted-foreground">%</span>}
              />
            </div>
          </div>

          {/* Initial Price Display */}
          {resourceAmountBigInt > 0n && tokenAmountBigInt > 0n && (
            <div className="rounded-lg bg-muted p-4">
              <p className="text-sm text-muted-foreground">Initial Price</p>
              <p className="text-lg font-semibold">
                {formatTokenAmount(
                  (tokenAmountBigInt * BigInt(10 ** 18)) / resourceAmountBigInt,
                  18,
                  4
                )}{' '}
                HANZO/{selectedUnit}
              </p>
            </div>
          )}

          {/* Warning */}
          <div className="flex items-start gap-2 rounded-lg border border-yellow-500/50 bg-yellow-500/10 p-3">
            <AlertTriangle className="mt-0.5 h-4 w-4 shrink-0 text-yellow-500" />
            <p className="text-sm text-yellow-700 dark:text-yellow-300">
              Creating a new pool involves risk. Initial price will be determined by
              the ratio of assets you provide.
            </p>
          </div>

          <ModalFooter>
            <Button variant="outline" onClick={() => setStep('select-type')}>
              Back
            </Button>
            <Button
              disabled={!validation.valid}
              onClick={() => setStep('confirm')}
            >
              Review
              <ArrowRight className="ml-2 h-4 w-4" />
            </Button>
          </ModalFooter>
        </div>
      )}

      {step === 'confirm' && selectedType !== null && (
        <div className="space-y-4">
          {/* Summary */}
          <div className="space-y-3 rounded-lg border p-4">
            <div className="flex justify-between">
              <span className="text-muted-foreground">{selectedLabel}</span>
              <span className="font-medium">
                {formatTokenAmount(resourceAmountBigInt)} {selectedUnit}
              </span>
            </div>
            <div className="flex justify-between">
              <span className="text-muted-foreground">HANZO</span>
              <span className="font-medium">
                {formatTokenAmount(tokenAmountBigInt)} HANZO
              </span>
            </div>
            <div className="border-t pt-3">
              <div className="flex justify-between">
                <span className="text-muted-foreground">Initial Price</span>
                <span className="font-medium">
                  {formatTokenAmount(
                    (tokenAmountBigInt * BigInt(10 ** 18)) / resourceAmountBigInt,
                    18,
                    4
                  )}{' '}
                  HANZO/{selectedUnit}
                </span>
              </div>
              <div className="flex justify-between">
                <span className="text-muted-foreground">Slippage Tolerance</span>
                <span className="font-medium">{slippage}%</span>
              </div>
            </div>
          </div>

          {/* Info */}
          <div className="flex items-start gap-2 rounded-lg bg-muted p-3">
            <Info className="mt-0.5 h-4 w-4 shrink-0 text-muted-foreground" />
            <p className="text-sm text-muted-foreground">
              You will receive LP tokens representing your share of the pool.
              These can be redeemed for the underlying assets at any time.
            </p>
          </div>

          {/* Error Display */}
          {error && (
            <div className="rounded-lg bg-destructive/10 p-3 text-sm text-destructive">
              {error}
            </div>
          )}

          <ModalFooter>
            <Button variant="outline" onClick={() => setStep('configure')}>
              Back
            </Button>
            <Button loading={loading} onClick={handleSubmit}>
              Create Pool
            </Button>
          </ModalFooter>
        </div>
      )}
    </Modal>
  );
}

export default CreatePoolModal;
