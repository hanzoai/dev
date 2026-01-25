/**
 * Distribution Calculator Tests
 */

import { describe, it, expect } from 'vitest'
import Decimal from 'decimal.js'
import { DistributionCalculator } from '../src/distribution/calculator.js'
import type { DependencyGraph, Dependency, PackageAttribution, AuthorShare } from '../src/types/index.js'
import { Ecosystem, ContributorRole, PaymentType } from '../src/types/index.js'

function createDependency(
  name: string,
  overrides: Partial<Dependency> = {}
): Dependency {
  return {
    name,
    version: '1.0.0',
    ecosystem: Ecosystem.Npm,
    isDirect: true,
    depth: 0,
    usageCount: 1,
    ...overrides,
  }
}

function createAttribution(
  name: string,
  authors: AuthorShare[],
  ecosystem: Ecosystem = Ecosystem.Npm
): PackageAttribution {
  return {
    id: `pkg-${name}`,
    packageName: name,
    ecosystem,
    authors,
    fundingUrls: [],
  }
}

describe('DistributionCalculator', () => {
  const calculator = new DistributionCalculator()

  it('calculates distribution for simple case', () => {
    const graph: DependencyGraph = {
      projectId: 'test-project',
      projectName: 'Test Project',
      rootDependencies: [createDependency('lodash'), createDependency('express')],
      allDependencies: new Map([
        ['lodash', createDependency('lodash')],
        ['express', createDependency('express')],
      ]),
      totalCount: 2,
      scanTimestamp: new Date(),
      ecosystems: [Ecosystem.Npm],
    }

    const attributions = new Map<string, PackageAttribution>([
      ['npm:lodash', createAttribution('lodash', [
        { authorId: 'author-1', sharePercentage: 1.0, role: ContributorRole.Primary },
      ])],
      ['npm:express', createAttribution('express', [
        { authorId: 'author-2', sharePercentage: 0.6, role: ContributorRole.Primary },
        { authorId: 'author-3', sharePercentage: 0.4, role: ContributorRole.Maintainer },
      ])],
    ])

    const distribution = calculator.calculate({
      projectId: 'test-project',
      graph,
      attributions,
      totalBudget: new Decimal(100),
      periodStart: new Date('2024-01-01'),
      periodEnd: new Date('2024-01-31'),
    })

    expect(distribution.projectId).toBe('test-project')
    expect(distribution.totalAmount.toNumber()).toBe(100)
    expect(distribution.payments.length).toBe(3)

    // Check total allocated is close to budget (may be slightly less due to thresholds)
    const totalAllocated = distribution.payments.reduce(
      (sum, p) => sum.plus(p.amount),
      new Decimal(0)
    )
    expect(totalAllocated.toNumber()).toBeGreaterThan(90)
    expect(totalAllocated.toNumber()).toBeLessThanOrEqual(100.01) // Allow floating point variance
  })

  it('weights direct dependencies higher', () => {
    const graph: DependencyGraph = {
      projectId: 'test',
      projectName: 'Test',
      rootDependencies: [createDependency('direct')],
      allDependencies: new Map([
        ['direct', createDependency('direct', { isDirect: true, depth: 0 })],
        ['transitive', createDependency('transitive', { isDirect: false, depth: 2 })],
      ]),
      totalCount: 2,
      scanTimestamp: new Date(),
      ecosystems: [Ecosystem.Npm],
    }

    const attributions = new Map<string, PackageAttribution>([
      ['npm:direct', createAttribution('direct', [
        { authorId: 'author-1', sharePercentage: 1.0, role: ContributorRole.Primary },
      ])],
      ['npm:transitive', createAttribution('transitive', [
        { authorId: 'author-2', sharePercentage: 1.0, role: ContributorRole.Primary },
      ])],
    ])

    const distribution = calculator.calculate({
      projectId: 'test',
      graph,
      attributions,
      totalBudget: new Decimal(100),
      periodStart: new Date(),
      periodEnd: new Date(),
    })

    // Direct dependency should get more
    const directPayment = distribution.payments.find((p) => p.packageName === 'direct')
    const transitivePayment = distribution.payments.find((p) => p.packageName === 'transitive')

    expect(directPayment).toBeDefined()
    expect(transitivePayment).toBeDefined()
    expect(directPayment!.amount.toNumber()).toBeGreaterThan(transitivePayment!.amount.toNumber())
  })

  it('weights security-critical packages higher', () => {
    const graph: DependencyGraph = {
      projectId: 'test',
      projectName: 'Test',
      rootDependencies: [
        createDependency('crypto-lib'),
        createDependency('utils'),
      ],
      allDependencies: new Map([
        ['crypto-lib', createDependency('crypto-lib')],
        ['utils', createDependency('utils')],
      ]),
      totalCount: 2,
      scanTimestamp: new Date(),
      ecosystems: [Ecosystem.Npm],
    }

    const attributions = new Map<string, PackageAttribution>([
      ['npm:crypto-lib', createAttribution('crypto-lib', [
        { authorId: 'author-1', sharePercentage: 1.0, role: ContributorRole.Primary },
      ])],
      ['npm:utils', createAttribution('utils', [
        { authorId: 'author-2', sharePercentage: 1.0, role: ContributorRole.Primary },
      ])],
    ])

    const distribution = calculator.calculate({
      projectId: 'test',
      graph,
      attributions,
      totalBudget: new Decimal(100),
      periodStart: new Date(),
      periodEnd: new Date(),
    })

    const cryptoPayment = distribution.payments.find((p) => p.packageName === 'crypto-lib')
    const utilsPayment = distribution.payments.find((p) => p.packageName === 'utils')

    expect(cryptoPayment).toBeDefined()
    expect(utilsPayment).toBeDefined()
    // crypto-lib matches security pattern, should get more
    expect(cryptoPayment!.amount.toNumber()).toBeGreaterThan(utilsPayment!.amount.toNumber())
  })

  it('filters payments below threshold', () => {
    const graph: DependencyGraph = {
      projectId: 'test',
      projectName: 'Test',
      rootDependencies: Array.from({ length: 200 }, (_, i) =>
        createDependency(`pkg-${i}`)
      ),
      allDependencies: new Map(
        Array.from({ length: 200 }, (_, i) => [
          `pkg-${i}`,
          createDependency(`pkg-${i}`),
        ])
      ),
      totalCount: 200,
      scanTimestamp: new Date(),
      ecosystems: [Ecosystem.Npm],
    }

    const attributions = new Map<string, PackageAttribution>(
      Array.from({ length: 200 }, (_, i) => [
        `npm:pkg-${i}`,
        createAttribution(`pkg-${i}`, [
          { authorId: `author-${i}`, sharePercentage: 1.0, role: ContributorRole.Primary },
        ]),
      ])
    )

    // With $10 budget and 200 packages, most payments would be below $0.50 threshold
    const distribution = calculator.calculate({
      projectId: 'test',
      graph,
      attributions,
      totalBudget: new Decimal(10),
      periodStart: new Date(),
      periodEnd: new Date(),
    })

    // Should have fewer payments due to threshold filtering
    expect(distribution.payments.length).toBeLessThan(200)

    // All payments should be at or above threshold
    for (const payment of distribution.payments) {
      expect(payment.amount.toNumber()).toBeGreaterThanOrEqual(0.50)
    }
  })

  it('generates preview correctly', () => {
    const graph: DependencyGraph = {
      projectId: 'test',
      projectName: 'Test',
      rootDependencies: [createDependency('lodash')],
      allDependencies: new Map([
        ['lodash', createDependency('lodash')],
      ]),
      totalCount: 1,
      scanTimestamp: new Date(),
      ecosystems: [Ecosystem.Npm],
    }

    const attributions = new Map<string, PackageAttribution>([
      ['npm:lodash', createAttribution('lodash', [
        { authorId: 'author-1', sharePercentage: 0.7, role: ContributorRole.Primary },
        { authorId: 'author-2', sharePercentage: 0.3, role: ContributorRole.Maintainer },
      ])],
    ])

    const preview = calculator.preview({
      projectId: 'test',
      graph,
      attributions,
      totalBudget: new Decimal(100),
      periodStart: new Date(),
      periodEnd: new Date(),
    })

    expect(preview.uniqueAuthors).toBe(2)
    expect(preview.uniquePackages).toBe(1)
    expect(preview.payments.length).toBe(2)

    const primary = preview.payments.find((p) => p.percentage > 50)
    const maintainer = preview.payments.find((p) => p.percentage < 50)

    expect(primary?.amount.toNumber()).toBeCloseTo(70, 0)
    expect(maintainer?.amount.toNumber()).toBeCloseTo(30, 0)
  })
})
