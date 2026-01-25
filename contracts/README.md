# Hanzo AI Compute Rewards Contracts

Smart contracts for the AI Compute Contribution Rewards system as specified in HIP-XXX.

## Overview

The `AIRewards` contract is an ERC20 token that implements:

- **Minting for verified compute contributions** - Providers earn AI tokens for contributing GPU/CPU compute to the network
- **Staking for pool operators** - Minimum stake requirement (1000 AI) with lock periods
- **Quality-weighted rewards** - Rewards based on quality score, trust score, uptime, and stake multipliers
- **Hardware attestation** - NVTrust integration for Sybil resistance
- **Vesting schedules** - 50% immediate, 25% over 7 days, 25% over 30 days
- **Burn mechanism** - 10% of emissions burned for deflationary pressure
- **Governance features** - Role-based access control for validators, slashers, and governance

## Architecture

```
AIRewards.sol
├── ERC20 (OpenZeppelin)
├── ERC20Burnable (burn mechanism)
├── ERC20Permit (gasless approvals)
├── AccessControl (role-based permissions)
├── ReentrancyGuard (security)
└── Pausable (emergency controls)
```

## Key Features

### Emission Schedule

| Year | Daily Emission | Annual Cap |
|------|----------------|------------|
| 1    | 1,000,000 AI   | 10%        |
| 2    | 750,000 AI     | 7.5%       |
| 3    | 500,000 AI     | 5%         |
| 4+   | 250,000 AI     | 3%         |

### Reward Allocation

| Allocation | Percentage |
|------------|------------|
| Providers  | 70%        |
| Treasury   | 20%        |
| Burn       | 10%        |

### Multipliers

- **Quality Multiplier**: 0.5x - 2.0x based on SLA compliance, job success rate
- **Trust Multiplier**: 0.6x - 1.0x based on hardware attestation (H100 = 0.95x, B200 = 1.0x)
- **Uptime Multiplier**: 0.7x - 1.2x based on uptime ratio (bonus for consecutive 99%+ uptime)
- **Stake Multiplier**: 1.0x - 1.5x based on staked amount (diminishing returns above 1M AI)

## Building

```bash
# Install dependencies
forge install

# Build contracts
forge build

# Run tests
forge test

# Run tests with verbosity
forge test -vv

# Run fuzz tests with more iterations
forge test --match-test testFuzz -vvv

# Generate gas report
forge test --gas-report
```

## Deployment

```bash
# Deploy to local network
forge script script/Deploy.s.sol --fork-url http://localhost:8545 --broadcast

# Deploy to testnet (example: Sepolia)
forge script script/Deploy.s.sol --rpc-url $SEPOLIA_RPC_URL --broadcast --verify
```

## Contract Addresses

| Network    | Address |
|------------|---------|
| Hanzo EVM  | TBD     |
| Zoo EVM    | TBD     |
| Lux C-Chain| TBD     |

## Roles

| Role | Purpose |
|------|---------|
| `DEFAULT_ADMIN_ROLE` | Contract admin, can grant/revoke roles |
| `MINTER_ROLE` | Can mint tokens (for rewards distribution) |
| `VALIDATOR_ROLE` | Can update quality/uptime scores |
| `GOVERNANCE_ROLE` | Can update protocol parameters |
| `SLASHER_ROLE` | Can slash malicious providers |

## Security

- Reentrancy protection on all state-changing functions
- Role-based access control for privileged operations
- Minimum stake duration (7 days) to prevent flash loan attacks
- Hardware attestation prevents Sybil attacks
- Emergency pause functionality

## References

- [HIP-XXX: AI Compute Contribution Rewards](../docs/HIP-AI-COMPUTE-REWARDS.md)
- [HIP-004: Hamiltonian Market Maker](../docs/HIP-004.md)
- [ZIP-002: Proof of AI](../docs/ZIP-002.md)

## License

MIT
