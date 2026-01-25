/**
 * Distribution Calculator
 *
 * Calculates fair payment distribution across all OSS dependencies.
 */

import Decimal from 'decimal.js'
import {
  type Dependency,
  type DependencyGraph,
  type Distribution,
  type Payment,
  type PackageAttribution,
  type WeightingFactors,
  type DistributionConfig,
  DistributionStatus,
  PaymentStatus,
  PaymentType,
  type DistributionPreview,
} from '../types/index.js'
import { generateId, createLogger, daysSince, toDecimal, sumDecimals } from '../utils/index.js'

const logger = createLogger('distribution')

// Default weighting factors
const DEFAULT_WEIGHTS: WeightingFactors = {
  directDependencyMultiplier: 1.5,
  transitiveDepthDivisor: 1.0,
  usageFrequencyLogBase: 10,
  criticalityMultipliers: {
    security: 2.0,
    core: 1.5,
    normal: 1.0,
  },
  maintenanceActivityMultipliers: {
    active: 1.5,      // < 30 days since update
    recent: 1.0,      // < 180 days
    stale: 0.7,       // < 365 days
    abandoned: 0.5,   // >= 365 days
  },
}

// Default distribution config
const DEFAULT_CONFIG: DistributionConfig = {
  minPaymentThreshold: new Decimal('0.50'),
  maxPaymentsPerBatch: 100,
  weights: DEFAULT_WEIGHTS,
  excludedLicenses: [],
  excludedPackages: [],
}

// Security-critical package patterns
const SECURITY_PACKAGES = [
  /^crypto/i,
  /^openssl/i,
  /^tls/i,
  /^ssh/i,
  /^auth/i,
  /^jwt/i,
  /^bcrypt/i,
  /^argon2/i,
  /^scrypt/i,
  /^passport/i,
  /^helmet/i,
  /^cors/i,
  /^csrf/i,
  /^sanitize/i,
  /^validator/i,
]

// Core infrastructure package patterns
const CORE_PACKAGES = [
  /^react$/i,
  /^vue$/i,
  /^angular/i,
  /^svelte/i,
  /^express$/i,
  /^fastify$/i,
  /^koa$/i,
  /^next$/i,
  /^nuxt$/i,
  /^webpack$/i,
  /^vite$/i,
  /^esbuild$/i,
  /^typescript$/i,
  /^babel/i,
  /^eslint$/i,
  /^prettier$/i,
  /^jest$/i,
  /^vitest$/i,
  /^mocha$/i,
  /^tokio$/i,
  /^serde$/i,
  /^async-std$/i,
  /^reqwest$/i,
  /^hyper$/i,
  /^actix/i,
  /^axum$/i,
  /^diesel$/i,
  /^sqlx$/i,
]

export interface DistributionInput {
  projectId: string
  graph: DependencyGraph
  attributions: Map<string, PackageAttribution>
  totalBudget: Decimal
  periodStart: Date
  periodEnd: Date
  currency?: string
}

export interface CalculatedPayment {
  authorId: string
  authorName?: string
  packageName: string
  packageKey: string
  amount: Decimal
  percentage: number
  score: number
}

export class DistributionCalculator {
  private config: DistributionConfig

  constructor(config: Partial<DistributionConfig> = {}) {
    this.config = {
      ...DEFAULT_CONFIG,
      ...config,
      weights: {
        ...DEFAULT_WEIGHTS,
        ...config.weights,
      },
    }
  }

  /**
   * Calculate distribution for a dependency graph
   */
  calculate(input: DistributionInput): Distribution {
    const { projectId, graph, attributions, totalBudget, periodStart, periodEnd, currency = 'USD' } = input

    logger.info(`Calculating distribution for project ${projectId}`, {
      totalDeps: graph.totalCount,
      budget: totalBudget.toString(),
    })

    // Calculate scores for all dependencies
    const scores = this.calculateScores(graph)

    // Normalize scores to percentages
    const totalScore = Array.from(scores.values()).reduce((sum, s) => sum + s, 0)
    const percentages = new Map<string, number>()
    for (const [key, score] of scores) {
      percentages.set(key, score / totalScore)
    }

    // Generate payments
    const payments: Payment[] = []
    const calculatedPayments: CalculatedPayment[] = []

    for (const [depKey, percentage] of percentages) {
      const attribution = attributions.get(depKey)
      if (!attribution) {
        logger.debug(`No attribution for ${depKey}, skipping`)
        continue
      }

      const depBudget = totalBudget.mul(percentage)

      for (const authorShare of attribution.authors) {
        const amount = depBudget.mul(authorShare.sharePercentage)

        // Skip payments below threshold
        if (amount.lt(this.config.minPaymentThreshold)) {
          logger.debug(`Payment below threshold: ${amount.toString()} for ${depKey}`)
          continue
        }

        const payment: Payment = {
          id: generateId(),
          distributionId: '', // Will be set when distribution is created
          authorId: authorShare.authorId,
          packageId: attribution.id,
          packageName: attribution.packageName,
          amount,
          paymentType: PaymentType.GithubSponsors, // Default, will be resolved later
          status: PaymentStatus.Pending,
          createdAt: new Date(),
        }

        payments.push(payment)

        calculatedPayments.push({
          authorId: authorShare.authorId,
          packageName: attribution.packageName,
          packageKey: depKey,
          amount,
          percentage: percentage * authorShare.sharePercentage * 100,
          score: scores.get(depKey) ?? 0,
        })
      }
    }

    // Sort by amount descending
    payments.sort((a, b) => b.amount.cmp(a.amount))
    calculatedPayments.sort((a, b) => b.amount.cmp(a.amount))

    // Consolidate small payments if needed
    const consolidatedPayments = this.consolidatePayments(payments)

    // Create distribution
    const distribution: Distribution = {
      id: generateId(),
      projectId,
      totalAmount: totalBudget,
      currency,
      periodStart,
      periodEnd,
      status: DistributionStatus.Pending,
      payments: consolidatedPayments,
      createdAt: new Date(),
    }

    // Update payment distribution IDs
    for (const payment of distribution.payments) {
      payment.distributionId = distribution.id
    }

    logger.info(`Distribution calculated`, {
      distributionId: distribution.id,
      paymentCount: distribution.payments.length,
      totalAllocated: sumDecimals(distribution.payments.map((p) => p.amount)).toString(),
    })

    return distribution
  }

  /**
   * Preview distribution without creating it
   */
  preview(input: DistributionInput): DistributionPreview {
    const { graph, attributions, totalBudget } = input

    // Calculate scores
    const scores = this.calculateScores(graph)
    const totalScore = Array.from(scores.values()).reduce((sum, s) => sum + s, 0)

    // Generate preview entries
    const entries: Array<{
      authorName: string
      authorId: string
      packageName: string
      amount: Decimal
      percentage: number
    }> = []

    const authorTotals = new Map<string, { amount: Decimal; count: number }>()

    for (const [depKey, score] of scores) {
      const percentage = score / totalScore
      const attribution = attributions.get(depKey)
      if (!attribution) continue

      const depBudget = totalBudget.mul(percentage)

      for (const authorShare of attribution.authors) {
        const amount = depBudget.mul(authorShare.sharePercentage)
        if (amount.lt(this.config.minPaymentThreshold)) continue

        entries.push({
          authorId: authorShare.authorId,
          authorName: authorShare.authorId, // Would be resolved from author registry
          packageName: attribution.packageName,
          amount,
          percentage: percentage * authorShare.sharePercentage * 100,
        })

        const existing = authorTotals.get(authorShare.authorId)
        if (existing) {
          existing.amount = existing.amount.plus(amount)
          existing.count++
        } else {
          authorTotals.set(authorShare.authorId, { amount, count: 1 })
        }
      }
    }

    return {
      payments: entries.sort((a, b) => b.amount.cmp(a.amount)),
      totalAmount: sumDecimals(entries.map((e) => e.amount)),
      uniqueAuthors: authorTotals.size,
      uniquePackages: new Set(entries.map((e) => e.packageName)).size,
    }
  }

  /**
   * Calculate scores for all dependencies
   */
  private calculateScores(graph: DependencyGraph): Map<string, number> {
    const scores = new Map<string, number>()
    const weights = this.config.weights

    for (const [name, dep] of graph.allDependencies) {
      // Skip excluded packages
      if (this.isExcluded(dep)) {
        continue
      }

      let score = 1.0

      // Direct vs transitive weighting
      if (dep.isDirect) {
        score *= weights.directDependencyMultiplier
      } else if (dep.depth > 0) {
        score *= weights.transitiveDepthDivisor / dep.depth
      }

      // Usage frequency (logarithmic)
      if (dep.usageCount > 0) {
        score *= Math.log10(dep.usageCount + 1) / Math.log10(weights.usageFrequencyLogBase)
      }

      // Maintenance activity
      if (dep.lastUpdated) {
        const daysSinceUpdate = daysSince(dep.lastUpdated)
        if (daysSinceUpdate < 30) {
          score *= weights.maintenanceActivityMultipliers.active
        } else if (daysSinceUpdate < 180) {
          score *= weights.maintenanceActivityMultipliers.recent
        } else if (daysSinceUpdate < 365) {
          score *= weights.maintenanceActivityMultipliers.stale
        } else {
          score *= weights.maintenanceActivityMultipliers.abandoned
        }
      }

      // Criticality assessment
      if (this.isSecurityCritical(dep)) {
        score *= weights.criticalityMultipliers.security
      } else if (this.isCoreFunctionality(dep)) {
        score *= weights.criticalityMultipliers.core
      } else {
        score *= weights.criticalityMultipliers.normal
      }

      const key = `${dep.ecosystem}:${name}`
      scores.set(key, Math.max(score, 0.01)) // Minimum score to avoid zero payments
    }

    return scores
  }

  /**
   * Check if package should be excluded
   */
  private isExcluded(dep: Dependency): boolean {
    // Check excluded packages list
    if (this.config.excludedPackages.includes(dep.name)) {
      return true
    }

    // Check excluded licenses
    if (dep.license && this.config.excludedLicenses.includes(dep.license)) {
      return true
    }

    return false
  }

  /**
   * Check if package is security-critical
   */
  private isSecurityCritical(dep: Dependency): boolean {
    return SECURITY_PACKAGES.some((pattern) => pattern.test(dep.name))
  }

  /**
   * Check if package is core infrastructure
   */
  private isCoreFunctionality(dep: Dependency): boolean {
    return CORE_PACKAGES.some((pattern) => pattern.test(dep.name))
  }

  /**
   * Consolidate payments to the same author
   */
  private consolidatePayments(payments: Payment[]): Payment[] {
    const authorPayments = new Map<string, Payment[]>()

    for (const payment of payments) {
      const existing = authorPayments.get(payment.authorId)
      if (existing) {
        existing.push(payment)
      } else {
        authorPayments.set(payment.authorId, [payment])
      }
    }

    const consolidated: Payment[] = []

    for (const [authorId, authorPaymentList] of authorPayments) {
      if (authorPaymentList.length === 1) {
        // Single payment, keep as-is
        consolidated.push(authorPaymentList[0]!)
      } else {
        // Multiple payments - could consolidate or keep separate
        // For now, keep separate for transparency
        consolidated.push(...authorPaymentList)
      }
    }

    return consolidated
  }
}

/**
 * Create a distribution calculator with custom config
 */
export function createDistributionCalculator(
  config: Partial<DistributionConfig> = {}
): DistributionCalculator {
  return new DistributionCalculator(config)
}
