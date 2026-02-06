# OSS Author Payment Tracking and Distribution System

## Design Document

**Version**: 1.0
**Author**: Hanzo AI Architecture Team
**Date**: 2026-01-24
**Status**: Draft

---

## 1. Executive Summary

This document outlines the architecture for an automated system that tracks open-source software dependencies, attributes contributions to their authors, and distributes payments fairly. The system integrates with Hanzo Commerce for payment processing and leverages the Lux Network for crypto-native settlement.

## 2. Problem Statement

Modern software projects depend on hundreds of open-source packages, yet the authors of these packages are rarely compensated. Current funding mechanisms (GitHub Sponsors, Open Collective) are fragmented and require manual setup by each maintainer.

**Goals**:

1. Automatically identify all OSS dependencies in a project
2. Attribute contributions to their authors
3. Calculate fair distribution weights
4. Process payments through fiat and crypto channels
5. Provide transparency and auditability

## 3. Research Findings

### 3.1 Dependency Tracking Patterns

**Rust/Cargo Ecosystem**:

```toml
# Cargo.toml metadata provides:
[package]
name = "serde"
authors = ["Erick Tryzelaar <erick.tryzelaar@gmail.com>", "David Tolnay <dtolnay@gmail.com>"]
repository = "https://github.com/serde-rs/serde"
license = "MIT OR Apache-2.0"
```

**Node.js/npm Ecosystem**:

```json
{
  "name": "package-name",
  "author": "Author Name <email@example.com>",
  "contributors": [...],
  "funding": {
    "type": "github",
    "url": "https://github.com/sponsors/author"
  },
  "repository": "https://github.com/org/repo"
}
```

**GitHub FUNDING.yml Standard**:

```yaml
github: [dtolnay]
open_collective: project-name
ko_fi: username
custom: ["https://example.com/donate"]
```

### 3.2 Existing Hanzo Infrastructure

| Component            | Location                     | Capabilities                              |
| -------------------- | ---------------------------- | ----------------------------------------- |
| **Commerce**         | `/hanzo/commerce`            | Payment processing, wallets, transactions |
| **PaymentProcessor** | `commerce/payment/processor` | Stripe, PayPal, Bitcoin, Ethereum         |
| **CryptoProcessor**  | `commerce/payment/processor` | Address generation, balance checking      |
| **Wallet Model**     | `commerce/models/wallet`     | Multi-chain account management            |
| **Events System**    | `commerce/events`            | ClickHouse-based analytics                |
| **Lux Network**      | `/lux/wallet`                | Blockchain settlement layer               |

### 3.3 Attribution Challenges

1. **Transitive Dependencies**: A project with 50 direct dependencies may have 500+ transitive ones
2. **Multi-Author Packages**: Fair split between contributors
3. **Abandoned Packages**: Should inactive maintainers receive funds?
4. **Corporate vs Individual**: Different payment preferences
5. **Identity Verification**: Linking GitHub to payment addresses

---

## 4. System Architecture

### 4.1 High-Level Architecture

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
|   (crates.io,     |     |   (PostgreSQL)    |     |   (Commerce)      |
|   npm, pypi)      |     |                   |     |                   |
+-------------------+     +-------------------+     +-------------------+
                                  |                         |
                                  v                         v
                          +-------------------+     +-------------------+
                          |   Funding         |     |   Settlement      |
                          |   Sources         |     |   Layer           |
                          |   (GitHub,        |     |   (Lux Network,   |
                          |   FUNDING.yml)    |     |   Stripe)         |
                          +-------------------+     +-------------------+
```

### 4.2 Component Details

#### 4.2.1 Dependency Scanner

**Purpose**: Extract dependency graphs from project manifests

**Supported Formats**:

- `Cargo.toml` + `Cargo.lock` (Rust)
- `package.json` + `package-lock.json` / `pnpm-lock.yaml` (Node.js)
- `pyproject.toml` + `uv.lock` / `requirements.txt` (Python)
- `go.mod` + `go.sum` (Go)

**Data Model**:

```rust
struct Dependency {
    name: String,
    version: String,
    ecosystem: Ecosystem,  // Cargo, Npm, PyPI, Go
    is_direct: bool,
    depth: u32,            // 0 = direct, 1 = first transitive, etc.
    usage_count: u64,      // Times referenced in codebase
    last_updated: DateTime,
}

struct DependencyGraph {
    project_id: String,
    root_dependencies: Vec<Dependency>,
    all_dependencies: HashMap<String, Dependency>,
    total_count: u32,
    scan_timestamp: DateTime,
}
```

#### 4.2.2 Attribution Engine

**Purpose**: Map dependencies to their authors and funding information

**Data Sources**:

1. Package registry metadata (crates.io, npm, PyPI)
2. GitHub repository FUNDING.yml files
3. Package manifest author/contributors fields
4. GitHub Sponsors API
5. Open Collective API

**Data Model**:

```rust
struct Author {
    id: Uuid,
    name: String,
    emails: Vec<String>,
    github_username: Option<String>,
    payment_addresses: Vec<PaymentAddress>,
    verified: bool,
    last_active: DateTime,
}

struct PaymentAddress {
    address_type: PaymentType,  // GithubSponsors, OpenCollective, Ethereum, Bitcoin, Lux
    address: String,
    verified: bool,
    preferred: bool,
}

struct PackageAttribution {
    package_name: String,
    ecosystem: Ecosystem,
    authors: Vec<AuthorShare>,
    funding_urls: Vec<String>,
    license: String,
    repository_url: Option<String>,
}

struct AuthorShare {
    author_id: Uuid,
    share_percentage: f64,  // 0.0 to 1.0
    role: ContributorRole,  // Primary, Maintainer, Contributor
}
```

**Attribution Algorithm**:

```python
def calculate_author_shares(package: Package) -> List[AuthorShare]:
    """
    Determine fair share distribution among package authors.

    Factors:
    1. Listed in 'authors' field = higher weight
    2. GitHub commit history = proportional weight
    3. Listed maintainers = maintenance weight
    4. FUNDING.yml presence = intent signal
    """
    shares = []

    # Primary authors from manifest (60% weight)
    manifest_authors = package.get_manifest_authors()
    manifest_weight = 0.6 / len(manifest_authors) if manifest_authors else 0

    for author in manifest_authors:
        shares.append(AuthorShare(
            author_id=resolve_author(author),
            share_percentage=manifest_weight,
            role=ContributorRole.Primary
        ))

    # GitHub contributors (40% weight, commit-proportional)
    if package.repository_url:
        contributors = get_github_contributors(package.repository_url)
        total_commits = sum(c.commit_count for c in contributors)

        for contributor in contributors[:20]:  # Top 20 contributors
            share = 0.4 * (contributor.commit_count / total_commits)
            existing = find_by_github(shares, contributor.username)
            if existing:
                existing.share_percentage += share
            else:
                shares.append(AuthorShare(
                    author_id=resolve_author(contributor),
                    share_percentage=share,
                    role=ContributorRole.Contributor
                ))

    # Normalize to 100%
    total = sum(s.share_percentage for s in shares)
    for share in shares:
        share.share_percentage /= total

    return shares
```

#### 4.2.3 Distribution Calculator

**Purpose**: Calculate fair payment distribution across all dependencies

**Weighting Factors**:

| Factor                    | Weight      | Description                                  |
| ------------------------- | ----------- | -------------------------------------------- |
| **Direct Dependency**     | 1.5x        | Explicitly chosen by developer               |
| **Transitive Depth**      | 1.0 / depth | Deeper = less weight                         |
| **Usage Frequency**       | log(n)      | Import/require counts in codebase            |
| **Criticality**           | 1.0-3.0x    | Security, performance, or core functionality |
| **Maintenance Activity**  | 0.5-1.5x    | Active vs abandoned                          |
| **License Compatibility** | 1.0x or 0x  | Exclude non-OSS                              |

**Distribution Algorithm**:

```python
def calculate_distribution(
    project: Project,
    total_budget: Decimal,
    period: TimePeriod
) -> List[Payment]:
    """
    Calculate payment distribution for all OSS dependencies.
    """
    graph = scan_dependencies(project)
    attributions = get_attributions(graph)

    # Calculate raw scores
    scores = {}
    for dep in graph.all_dependencies.values():
        base_score = 1.0

        # Depth weighting (direct deps worth more)
        if dep.is_direct:
            base_score *= 1.5
        else:
            base_score *= 1.0 / dep.depth

        # Usage frequency (logarithmic)
        base_score *= math.log10(max(dep.usage_count, 1) + 1)

        # Maintenance activity
        days_since_update = (now() - dep.last_updated).days
        if days_since_update < 30:
            base_score *= 1.5
        elif days_since_update < 180:
            base_score *= 1.0
        elif days_since_update < 365:
            base_score *= 0.7
        else:
            base_score *= 0.5  # Abandoned penalty

        # Criticality assessment
        if is_security_critical(dep):
            base_score *= 2.0
        elif is_core_functionality(dep):
            base_score *= 1.5

        scores[dep.name] = base_score

    # Normalize scores to percentages
    total_score = sum(scores.values())
    percentages = {k: v / total_score for k, v in scores.items()}

    # Generate payments
    payments = []
    for dep_name, percentage in percentages.items():
        attribution = attributions[dep_name]
        dep_budget = total_budget * Decimal(percentage)

        for author_share in attribution.authors:
            amount = dep_budget * Decimal(author_share.share_percentage)

            if amount >= MIN_PAYMENT_THRESHOLD:  # e.g., $0.50
                payments.append(Payment(
                    recipient_id=author_share.author_id,
                    amount=amount,
                    package=dep_name,
                    period=period,
                    attribution_details=author_share
                ))

    # Batch small payments
    return consolidate_payments(payments)
```

#### 4.2.4 Payment Processor Integration

**Integration with Hanzo Commerce**:

```go
// /hanzo/commerce/oss/processor.go

package oss

import (
    "context"
    "github.com/hanzoai/commerce/payment/processor"
    "github.com/hanzoai/commerce/models/types/currency"
)

type OSSPaymentService struct {
    fiatProcessor   processor.PaymentProcessor      // Stripe
    cryptoProcessor processor.CryptoProcessor       // Lux/Ethereum
    db              *db.DB
}

// ProcessOSSPayments handles batch distribution to OSS authors
func (s *OSSPaymentService) ProcessOSSPayments(
    ctx context.Context,
    distribution Distribution,
) (*BatchResult, error) {
    results := &BatchResult{
        Successful: make([]PaymentResult, 0),
        Failed:     make([]PaymentFailure, 0),
    }

    for _, payment := range distribution.Payments {
        author, err := s.db.GetAuthor(ctx, payment.RecipientID)
        if err != nil {
            results.Failed = append(results.Failed, PaymentFailure{
                Payment: payment,
                Reason:  "author_not_found",
            })
            continue
        }

        // Choose payment method based on author preference
        preferredAddress := author.GetPreferredPaymentAddress()

        switch preferredAddress.Type {
        case PaymentTypeGithubSponsors:
            err = s.processGithubSponsorship(ctx, payment, author)

        case PaymentTypeOpenCollective:
            err = s.processOpenCollective(ctx, payment, author)

        case PaymentTypeLux, PaymentTypeEthereum:
            result, err := s.cryptoProcessor.Charge(ctx, processor.PaymentRequest{
                Amount:   currency.CentsFromDecimal(payment.Amount),
                Currency: currency.USD,
                Metadata: map[string]interface{}{
                    "oss_package":   payment.Package,
                    "period":        payment.Period,
                    "recipient":     author.Name,
                },
            })
            if err == nil {
                err = s.transferToAddress(ctx, result.TransactionID, preferredAddress.Address)
            }

        case PaymentTypeStripe:
            _, err = s.fiatProcessor.Charge(ctx, processor.PaymentRequest{
                Amount:     currency.CentsFromDecimal(payment.Amount),
                Currency:   currency.USD,
                CustomerID: author.StripeCustomerID,
            })
        }

        if err != nil {
            results.Failed = append(results.Failed, PaymentFailure{
                Payment: payment,
                Reason:  err.Error(),
            })
        } else {
            results.Successful = append(results.Successful, PaymentResult{
                Payment:       payment,
                TransactionID: generateTxID(),
            })
        }
    }

    // Record in events system
    s.emitDistributionEvent(ctx, distribution, results)

    return results, nil
}
```

---

## 5. Data Models

### 5.1 Database Schema (PostgreSQL)

```sql
-- Authors and their payment information
CREATE TABLE oss_authors (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    name VARCHAR(255) NOT NULL,
    github_username VARCHAR(255) UNIQUE,
    email VARCHAR(255),
    verified BOOLEAN DEFAULT FALSE,
    verification_date TIMESTAMP,
    created_at TIMESTAMP DEFAULT NOW(),
    updated_at TIMESTAMP DEFAULT NOW()
);

CREATE TABLE oss_payment_addresses (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    author_id UUID REFERENCES oss_authors(id),
    address_type VARCHAR(50) NOT NULL,  -- 'github_sponsors', 'open_collective', 'ethereum', 'bitcoin', 'lux', 'stripe'
    address VARCHAR(500) NOT NULL,
    verified BOOLEAN DEFAULT FALSE,
    preferred BOOLEAN DEFAULT FALSE,
    created_at TIMESTAMP DEFAULT NOW(),
    UNIQUE(author_id, address_type, address)
);

-- Package attribution
CREATE TABLE oss_packages (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    name VARCHAR(255) NOT NULL,
    ecosystem VARCHAR(50) NOT NULL,  -- 'cargo', 'npm', 'pypi', 'go'
    version VARCHAR(100),
    repository_url TEXT,
    homepage_url TEXT,
    license VARCHAR(100),
    last_fetched TIMESTAMP,
    UNIQUE(name, ecosystem)
);

CREATE TABLE oss_package_authors (
    package_id UUID REFERENCES oss_packages(id),
    author_id UUID REFERENCES oss_authors(id),
    share_percentage DECIMAL(5,4) NOT NULL,  -- 0.0000 to 1.0000
    role VARCHAR(50) NOT NULL,  -- 'primary', 'maintainer', 'contributor'
    PRIMARY KEY (package_id, author_id)
);

-- Project dependency tracking
CREATE TABLE oss_projects (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    organization_id UUID,
    name VARCHAR(255) NOT NULL,
    repository_url TEXT,
    last_scan TIMESTAMP,
    created_at TIMESTAMP DEFAULT NOW()
);

CREATE TABLE oss_project_dependencies (
    project_id UUID REFERENCES oss_projects(id),
    package_id UUID REFERENCES oss_packages(id),
    version VARCHAR(100),
    is_direct BOOLEAN NOT NULL,
    depth INTEGER NOT NULL,
    usage_count INTEGER DEFAULT 0,
    first_added TIMESTAMP DEFAULT NOW(),
    last_seen TIMESTAMP DEFAULT NOW(),
    PRIMARY KEY (project_id, package_id)
);

-- Payment distributions
CREATE TABLE oss_distributions (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    project_id UUID REFERENCES oss_projects(id),
    total_amount DECIMAL(12,2) NOT NULL,
    currency VARCHAR(10) DEFAULT 'USD',
    period_start DATE NOT NULL,
    period_end DATE NOT NULL,
    status VARCHAR(50) DEFAULT 'pending',  -- 'pending', 'processing', 'completed', 'failed'
    created_at TIMESTAMP DEFAULT NOW(),
    processed_at TIMESTAMP
);

CREATE TABLE oss_payments (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    distribution_id UUID REFERENCES oss_distributions(id),
    author_id UUID REFERENCES oss_authors(id),
    package_id UUID REFERENCES oss_packages(id),
    amount DECIMAL(12,4) NOT NULL,
    payment_type VARCHAR(50) NOT NULL,
    transaction_id VARCHAR(255),
    status VARCHAR(50) DEFAULT 'pending',
    error_message TEXT,
    created_at TIMESTAMP DEFAULT NOW(),
    processed_at TIMESTAMP
);

-- Indexes for common queries
CREATE INDEX idx_packages_ecosystem ON oss_packages(ecosystem);
CREATE INDEX idx_dependencies_project ON oss_project_dependencies(project_id);
CREATE INDEX idx_payments_distribution ON oss_payments(distribution_id);
CREATE INDEX idx_payments_author ON oss_payments(author_id);
CREATE INDEX idx_distributions_status ON oss_distributions(status);
```

### 5.2 Events Schema (ClickHouse)

```sql
-- Event tracking for analytics
CREATE TABLE oss_events (
    event_id UUID,
    event_type String,  -- 'scan', 'attribution', 'distribution', 'payment'
    project_id UUID,
    organization_id UUID,
    data String,  -- JSON
    timestamp DateTime DEFAULT now()
) ENGINE = MergeTree()
ORDER BY (timestamp, event_type);

-- Distribution analytics
CREATE TABLE oss_distribution_analytics (
    distribution_id UUID,
    project_id UUID,
    period_start Date,
    total_amount Decimal(12,2),
    unique_packages UInt32,
    unique_authors UInt32,
    payment_count UInt32,
    success_count UInt32,
    failure_count UInt32,
    timestamp DateTime DEFAULT now()
) ENGINE = SummingMergeTree()
ORDER BY (project_id, period_start);
```

---

## 6. API Design

### 6.1 REST API Endpoints

```yaml
openapi: 3.0.0
info:
  title: OSS Payment API
  version: 1.0.0

paths:
  /api/v1/oss/projects:
    post:
      summary: Register a project for OSS tracking
      requestBody:
        content:
          application/json:
            schema:
              type: object
              properties:
                name: { type: string }
                repository_url: { type: string }

  /api/v1/oss/projects/{project_id}/scan:
    post:
      summary: Scan project dependencies
      parameters:
        - name: project_id
          in: path
          required: true
      requestBody:
        content:
          multipart/form-data:
            schema:
              type: object
              properties:
                cargo_lock: { type: string, format: binary }
                package_lock: { type: string, format: binary }
                pyproject_toml: { type: string, format: binary }
      responses:
        "202":
          description: Scan initiated

  /api/v1/oss/projects/{project_id}/dependencies:
    get:
      summary: Get dependency graph
      parameters:
        - name: project_id
          in: path
          required: true
        - name: include_transitive
          in: query
          schema: { type: boolean, default: true }

  /api/v1/oss/projects/{project_id}/distribution:
    post:
      summary: Calculate and execute payment distribution
      requestBody:
        content:
          application/json:
            schema:
              type: object
              properties:
                total_amount:
                  type: number
                  description: Total amount to distribute (USD)
                period_start: { type: string, format: date }
                period_end: { type: string, format: date }
                dry_run:
                  type: boolean
                  default: false

  /api/v1/oss/distributions/{distribution_id}:
    get:
      summary: Get distribution details and status

  /api/v1/oss/authors/{author_id}:
    get:
      summary: Get author profile and payment history
    put:
      summary: Update author payment preferences

  /api/v1/oss/authors/claim:
    post:
      summary: Author claims their packages
      description: Verify ownership via GitHub OAuth
      requestBody:
        content:
          application/json:
            schema:
              type: object
              properties:
                github_code: { type: string }
                payment_addresses:
                  type: array
                  items:
                    type: object
                    properties:
                      type: { type: string }
                      address: { type: string }
```

### 6.2 CLI Integration

```bash
# Add to code-cli for developer workflows

# Scan current project
code oss scan

# View dependency distribution
code oss deps --show-authors

# Configure payment budget
code oss budget set 100 --currency usd --period monthly

# Execute distribution
code oss distribute --dry-run
code oss distribute --confirm

# View payment history
code oss payments --period 2026-01
```

---

## 7. Implementation Plan

### Phase 1: Foundation (2 weeks)

1. **Database Schema Setup**

   - Create PostgreSQL tables
   - Create ClickHouse events tables
   - Set up migrations

2. **Dependency Scanner**

   - Implement Cargo.lock parser
   - Implement package-lock.json parser
   - Build dependency graph construction

3. **Package Registry Integration**
   - crates.io API client
   - npm registry API client
   - GitHub API client for FUNDING.yml

### Phase 2: Attribution Engine (2 weeks)

1. **Author Resolution**

   - GitHub username to profile mapping
   - Email deduplication
   - Payment address discovery

2. **Share Calculation**

   - Implement attribution algorithm
   - GitHub contributor fetching
   - Share normalization

3. **Author Registry**
   - CRUD operations
   - Verification flow
   - Payment preference management

### Phase 3: Distribution System (2 weeks)

1. **Scoring Algorithm**

   - Implement weighting factors
   - Criticality assessment
   - Activity scoring

2. **Payment Generation**

   - Distribution calculation
   - Payment batching
   - Threshold handling

3. **Commerce Integration**
   - Stripe payment processing
   - Crypto payment processing
   - Transaction recording

### Phase 4: API & CLI (1 week)

1. **REST API**

   - Endpoint implementation
   - Authentication/authorization
   - Documentation

2. **CLI Commands**
   - Scan integration
   - Distribution commands
   - Reporting

### Phase 5: Testing & Launch (1 week)

1. **Testing**

   - Unit tests
   - Integration tests
   - End-to-end tests

2. **Documentation**
   - API documentation
   - User guides
   - Author onboarding

---

## 8. Security Considerations

### 8.1 Payment Security

- All crypto transactions require multi-sig or threshold signatures
- Fiat payments use Stripe's PCI-compliant infrastructure
- Payment amounts are rate-limited per period
- Anomaly detection for unusual distribution patterns

### 8.2 Author Verification

- GitHub OAuth for identity verification
- Email confirmation for payment addresses
- Crypto address verification via signature
- Manual review for high-value recipients

### 8.3 Data Privacy

- Author emails encrypted at rest
- Payment addresses optionally public
- GDPR-compliant data handling
- Audit logs for all access

---

## 9. Metrics & Monitoring

### 9.1 Key Metrics

| Metric               | Description                        | Target  |
| -------------------- | ---------------------------------- | ------- |
| Scan Coverage        | % of dependencies with attribution | >95%    |
| Payment Success Rate | Successful / Total payments        | >99%    |
| Author Coverage      | Authors with payment addresses     | >70%    |
| Distribution Latency | Time from trigger to completion    | <1 hour |
| Author Onboarding    | New authors claiming packages      | Growing |

### 9.2 Alerting

- Payment failures > 5%
- Scan errors
- API latency > 2s
- Distribution stuck in processing

---

## 10. Future Enhancements

### 10.1 Advanced Attribution

- Machine learning for contribution impact assessment
- Code-level dependency analysis (which functions are used)
- Security vulnerability response weighting

### 10.2 Community Features

- Leaderboards for top-supported packages
- Sponsor badges for projects
- Public distribution transparency

### 10.3 Ecosystem Expansion

- Additional package managers (Maven, NuGet)
- Enterprise self-hosted deployments
- Integration with existing platforms (thanks.dev, tea.xyz)

---

## 11. Appendix

### A. Existing Funding Information Sources

1. **GitHub FUNDING.yml**: `github`, `open_collective`, `ko_fi`, `patreon`, `tidelift`, `custom`
2. **npm funding**: `npm fund` command, `funding` field in package.json
3. **crates.io**: No native funding field, relies on FUNDING.yml
4. **PyPI**: No native funding field, relies on project URLs

### B. Sample Distribution Calculation

For a project with 100 dependencies and $500 monthly budget:

| Package | Depth | Usage | Activity | Score | Amount |
| ------- | ----- | ----- | -------- | ----- | ------ |
| tokio   | 0     | 45    | Active   | 8.25  | $41.25 |
| serde   | 0     | 120   | Active   | 11.22 | $56.10 |
| rand    | 1     | 12    | Active   | 3.12  | $15.60 |
| ...     | ...   | ...   | ...      | ...   | ...    |

### C. Integration with Lux Network

The system integrates with Lux Network for crypto-native settlements:

1. **Multi-chain Support**: Native LUX tokens, plus Ethereum, Bitcoin
2. **Low Fees**: Batch transactions reduce per-payment costs
3. **Fast Finality**: Sub-second confirmation times
4. **Cross-chain**: Support for authors preferring different chains

---

_This document is maintained as part of the Hanzo Dev project. Updates should be reflected in LLM.md._
