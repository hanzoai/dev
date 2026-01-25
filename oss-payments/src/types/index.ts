/**
 * OSS Payment System - Core Type Definitions
 *
 * These types form the foundation of the payment tracking and distribution system.
 */

import { z } from 'zod'
import type { Decimal } from 'decimal.js'

// ============================================================================
// Enums
// ============================================================================

export enum Ecosystem {
  Cargo = 'cargo',
  Npm = 'npm',
  PyPI = 'pypi',
  Go = 'go',
}

export enum PaymentType {
  GithubSponsors = 'github_sponsors',
  OpenCollective = 'open_collective',
  KoFi = 'ko_fi',
  Patreon = 'patreon',
  Tidelift = 'tidelift',
  Ethereum = 'ethereum',
  Bitcoin = 'bitcoin',
  Lux = 'lux',
  Stripe = 'stripe',
  Custom = 'custom',
}

export enum ContributorRole {
  Primary = 'primary',
  Maintainer = 'maintainer',
  Contributor = 'contributor',
}

export enum DistributionStatus {
  Pending = 'pending',
  Processing = 'processing',
  Completed = 'completed',
  Failed = 'failed',
  PartiallyCompleted = 'partially_completed',
}

export enum PaymentStatus {
  Pending = 'pending',
  Processing = 'processing',
  Completed = 'completed',
  Failed = 'failed',
  Skipped = 'skipped',
}

// ============================================================================
// Zod Schemas for Validation
// ============================================================================

export const DependencySchema = z.object({
  name: z.string().min(1),
  version: z.string(),
  ecosystem: z.nativeEnum(Ecosystem),
  isDirect: z.boolean(),
  depth: z.number().int().min(0),
  usageCount: z.number().int().min(0).default(0),
  lastUpdated: z.date().optional(),
  repositoryUrl: z.string().url().optional(),
  license: z.string().optional(),
})

export const AuthorSchema = z.object({
  id: z.string().uuid(),
  name: z.string().min(1),
  emails: z.array(z.string().email()),
  githubUsername: z.string().optional(),
  verified: z.boolean().default(false),
  verificationDate: z.date().optional(),
  lastActive: z.date().optional(),
  createdAt: z.date(),
  updatedAt: z.date(),
})

export const PaymentAddressSchema = z.object({
  id: z.string().uuid(),
  authorId: z.string().uuid(),
  addressType: z.nativeEnum(PaymentType),
  address: z.string().min(1),
  verified: z.boolean().default(false),
  preferred: z.boolean().default(false),
  createdAt: z.date(),
})

export const AuthorShareSchema = z.object({
  authorId: z.string().uuid(),
  sharePercentage: z.number().min(0).max(1),
  role: z.nativeEnum(ContributorRole),
})

export const PackageAttributionSchema = z.object({
  id: z.string().uuid(),
  packageName: z.string().min(1),
  ecosystem: z.nativeEnum(Ecosystem),
  version: z.string().optional(),
  authors: z.array(AuthorShareSchema),
  fundingUrls: z.array(z.string().url()),
  license: z.string().optional(),
  repositoryUrl: z.string().url().optional(),
  homepageUrl: z.string().url().optional(),
  lastFetched: z.date().optional(),
})

export const FundingInfoSchema = z.object({
  github: z.array(z.string()).optional(),
  openCollective: z.string().optional(),
  koFi: z.string().optional(),
  patreon: z.string().optional(),
  tidelift: z.string().optional(),
  custom: z.array(z.string().url()).optional(),
})

// ============================================================================
// TypeScript Types (derived from schemas)
// ============================================================================

export type Dependency = z.infer<typeof DependencySchema>
export type Author = z.infer<typeof AuthorSchema>
export type PaymentAddress = z.infer<typeof PaymentAddressSchema>
export type AuthorShare = z.infer<typeof AuthorShareSchema>
export type PackageAttribution = z.infer<typeof PackageAttributionSchema>
export type FundingInfo = z.infer<typeof FundingInfoSchema>

// ============================================================================
// Complex Types
// ============================================================================

export interface DependencyGraph {
  projectId: string
  projectName: string
  rootDependencies: Dependency[]
  allDependencies: Map<string, Dependency>
  totalCount: number
  scanTimestamp: Date
  ecosystems: Ecosystem[]
}

export interface Project {
  id: string
  organizationId?: string
  name: string
  repositoryUrl?: string
  lastScan?: Date
  createdAt: Date
  updatedAt: Date
}

export interface Distribution {
  id: string
  projectId: string
  totalAmount: Decimal
  currency: string
  periodStart: Date
  periodEnd: Date
  status: DistributionStatus
  payments: Payment[]
  createdAt: Date
  processedAt?: Date
}

export interface Payment {
  id: string
  distributionId: string
  authorId: string
  packageId: string
  packageName: string
  amount: Decimal
  paymentType: PaymentType
  transactionId?: string
  status: PaymentStatus
  errorMessage?: string
  createdAt: Date
  processedAt?: Date
}

export interface PaymentResult {
  payment: Payment
  transactionId: string
  success: boolean
  errorMessage?: string
}

export interface BatchResult {
  distributionId: string
  successful: PaymentResult[]
  failed: PaymentResult[]
  skipped: PaymentResult[]
  totalProcessed: number
  totalAmount: Decimal
  successAmount: Decimal
}

// ============================================================================
// Configuration Types
// ============================================================================

export interface WeightingFactors {
  directDependencyMultiplier: number      // Default: 1.5
  transitiveDepthDivisor: number          // Default: 1.0 (divides by depth)
  usageFrequencyLogBase: number           // Default: 10 (log10)
  criticalityMultipliers: {
    security: number                       // Default: 2.0
    core: number                           // Default: 1.5
    normal: number                         // Default: 1.0
  }
  maintenanceActivityMultipliers: {
    active: number                         // Default: 1.5 (< 30 days)
    recent: number                         // Default: 1.0 (< 180 days)
    stale: number                          // Default: 0.7 (< 365 days)
    abandoned: number                      // Default: 0.5 (>= 365 days)
  }
}

export interface DistributionConfig {
  minPaymentThreshold: Decimal            // Default: $0.50
  maxPaymentsPerBatch: number             // Default: 100
  weights: WeightingFactors
  excludedLicenses: string[]              // e.g., proprietary licenses
  excludedPackages: string[]              // Blacklisted packages
}

export interface OSSPaymentConfig {
  database: {
    connectionString: string
    maxConnections: number
  }
  github: {
    token: string
    rateLimit: number
  }
  registries: {
    npm?: { token?: string }
    crates?: { token?: string }
    pypi?: { token?: string }
  }
  payments: {
    stripe?: { secretKey: string }
    lux?: { endpoint: string; privateKey: string }
  }
  distribution: DistributionConfig
}

// ============================================================================
// API Types
// ============================================================================

export interface ScanRequest {
  projectId: string
  manifestFiles: {
    cargoLock?: string
    packageLock?: string
    pnpmLock?: string
    pyprojectToml?: string
    uvLock?: string
    goMod?: string
    goSum?: string
  }
}

export interface ScanResult {
  projectId: string
  graph: DependencyGraph
  newPackages: number
  updatedPackages: number
  scanDuration: number
}

export interface DistributionRequest {
  projectId: string
  totalAmount: number
  currency?: string
  periodStart: Date
  periodEnd: Date
  dryRun?: boolean
}

export interface DistributionPreview {
  payments: Array<{
    authorName: string
    authorId: string
    packageName: string
    amount: Decimal
    percentage: number
  }>
  totalAmount: Decimal
  uniqueAuthors: number
  uniquePackages: number
}

// ============================================================================
// Git Analysis Types
// ============================================================================

export interface GitContributor {
  name: string
  email: string
  username?: string
  commitCount: number
  additions: number
  deletions: number
  firstCommit: Date
  lastCommit: Date
}

export interface GitAnalysisResult {
  repositoryUrl: string
  totalCommits: number
  contributors: GitContributor[]
  analyzedAt: Date
}

// ============================================================================
// Registry API Response Types
// ============================================================================

export interface NpmPackageInfo {
  name: string
  version: string
  description?: string
  author?: string | { name: string; email?: string; url?: string }
  contributors?: Array<string | { name: string; email?: string; url?: string }>
  maintainers?: Array<{ name: string; email?: string }>
  repository?: { type?: string; url: string } | string
  homepage?: string
  license?: string
  funding?: string | { type: string; url: string } | Array<{ type: string; url: string }>
}

export interface CratesPackageInfo {
  name: string
  version: string
  description?: string
  authors?: string[]
  repository?: string
  homepage?: string
  license?: string
  documentation?: string
}

export interface PyPIPackageInfo {
  name: string
  version: string
  summary?: string
  author?: string
  author_email?: string
  maintainer?: string
  maintainer_email?: string
  home_page?: string
  project_urls?: Record<string, string>
  license?: string
}
