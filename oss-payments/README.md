# OSS Payment System

Automated tracking and distribution of payments to open-source authors.

## Overview

Modern software projects depend on hundreds of open-source packages, yet the authors of these packages are rarely compensated. This system:

1. **Scans** your project dependencies across multiple ecosystems (npm, Cargo, PyPI, Go)
2. **Attributes** contributions to their authors using registry metadata and git analysis
3. **Calculates** fair distribution weights based on direct/transitive status, usage, criticality, and maintenance activity
4. **Distributes** payments through GitHub Sponsors, Open Collective, and cryptocurrency

## Installation

```bash
npm install @hanzo/oss-payments
# or
pnpm add @hanzo/oss-payments
```

## Quick Start

### CLI Usage

```bash
# Scan a project
oss-payments scan ./my-project --project "My Project"

# Preview distribution
oss-payments preview <project-id> --amount 500

# Execute distribution
oss-payments distribute <project-id> --amount 500 --confirm
```

### Programmatic Usage

```typescript
import { createOSSPaymentService } from '@hanzo/oss-payments'

const service = createOSSPaymentService({
  database: { connectionString: process.env.DATABASE_URL },
  github: { token: process.env.GITHUB_TOKEN },
  registries: { npm: {}, crates: {} },
  distribution: { minPaymentThreshold: 0.50 },
})

// Register and scan a project
const project = await service.registerProject('my-project')
const scan = await service.scanDirectory(project.id, './my-project')

// Preview distribution
const preview = await service.previewDistribution({
  projectId: project.id,
  totalAmount: 500,
  periodStart: new Date('2024-01-01'),
  periodEnd: new Date('2024-01-31'),
})

console.log(`Would distribute to ${preview.uniqueAuthors} authors`)

// Create distribution
const distribution = await service.createDistribution({
  projectId: project.id,
  totalAmount: 500,
  periodStart: new Date('2024-01-01'),
  periodEnd: new Date('2024-01-31'),
})
```

## Supported Ecosystems

| Ecosystem | Manifest | Lockfile |
|-----------|----------|----------|
| npm/pnpm | package.json | package-lock.json, pnpm-lock.yaml |
| Cargo (Rust) | Cargo.toml | Cargo.lock |
| PyPI (Python) | pyproject.toml | uv.lock, requirements.txt |
| Go | go.mod | go.sum |

## Distribution Algorithm

Payments are weighted based on:

- **Direct vs Transitive**: Direct dependencies get 1.5x weight; transitive deps are weighted inversely by depth
- **Usage Frequency**: Log-scaled based on import/require counts
- **Criticality**: Security packages (crypto, auth) get 2x; core packages (react, express) get 1.5x
- **Maintenance Activity**: Active (1.5x), Recent (1.0x), Stale (0.7x), Abandoned (0.5x)

Author shares within a package are calculated based on:
- 60% weight to manifest-listed authors
- 40% weight to top git contributors (proportional to commits)

## Configuration

```typescript
interface OSSPaymentConfig {
  database: {
    connectionString: string
    maxConnections?: number
  }
  github: {
    token: string        // Required for contributor analysis
    rateLimit?: number   // Requests per second (default: 5)
  }
  registries: {
    npm?: { token?: string }
    crates?: { token?: string }
    pypi?: { token?: string }
  }
  distribution: {
    minPaymentThreshold?: Decimal  // Default: $0.50
    maxPaymentsPerBatch?: number   // Default: 100
    weights?: WeightingFactors
    excludedLicenses?: string[]
    excludedPackages?: string[]
  }
}
```

## Database Setup

Run migrations to set up the PostgreSQL schema:

```bash
npm run db:migrate
```

## API Integration

The system provides both CLI and programmatic APIs. For REST API integration:

```typescript
import { createOSSPaymentService, createAPI } from '@hanzo/oss-payments'
import express from 'express'

const app = express()
const service = createOSSPaymentService(config)
const api = createAPI(service)

app.post('/api/oss/projects', async (req, res) => {
  const project = await api.createProject(req.body)
  res.json(project)
})

app.post('/api/oss/projects/:id/scan', async (req, res) => {
  const result = await api.scanProject(req.params.id, req.body)
  res.json(result)
})
```

## Payment Methods

- **GitHub Sponsors**: Via GitHub's GraphQL API
- **Open Collective**: Via Open Collective API
- **Cryptocurrency**: Ethereum, Bitcoin, Lux Network
- **Stripe**: For direct bank transfers

## Architecture

```
+-------------------+     +-------------------+     +-------------------+
|   Dependency      |     |   Attribution     |     |   Distribution    |
|   Scanner         |---->|   Engine          |---->|   Calculator      |
+-------------------+     +-------------------+     +-------------------+
        |                         |                         |
        v                         v                         v
+-------------------+     +-------------------+     +-------------------+
|   Package         |     |   Author          |     |   Payment         |
|   Registry APIs   |     |   Registry        |     |   Processor       |
+-------------------+     +-------------------+     +-------------------+
```

## Development

```bash
# Install dependencies
pnpm install

# Build
pnpm build

# Run tests
pnpm test

# Type check
pnpm typecheck

# Lint
pnpm lint
```

## License

MIT - Hanzo AI
