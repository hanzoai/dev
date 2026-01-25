/**
 * OSS Payment Service
 *
 * Main orchestration service for the OSS payment system.
 */

import { readFile } from 'fs/promises'
import { resolve, basename } from 'path'
import glob from 'fast-glob'
import Decimal from 'decimal.js'
import type {
  DependencyGraph,
  Dependency,
  Project,
  Distribution,
  PackageAttribution,
  OSSPaymentConfig,
  ScanRequest,
  ScanResult,
  DistributionRequest,
  DistributionPreview,
  Ecosystem,
} from '../types/index.js'
import {
  createScanner,
  detectEcosystem,
  getManifestPatterns,
  getLockfilePatterns,
} from '../scanners/index.js'
import { AttributionEngine } from '../attribution/index.js'
import { DistributionCalculator } from '../distribution/index.js'
import { generateId, createLogger, toDecimal } from '../utils/index.js'

const logger = createLogger('oss-service')

export class OSSPaymentService {
  private config: OSSPaymentConfig
  private attributionEngine: AttributionEngine
  private distributionCalculator: DistributionCalculator
  private projects: Map<string, Project> = new Map()
  private graphs: Map<string, DependencyGraph> = new Map()
  private attributions: Map<string, Map<string, PackageAttribution>> = new Map()

  constructor(config: OSSPaymentConfig) {
    this.config = config

    this.attributionEngine = new AttributionEngine({
      github: config.github,
      npm: config.registries.npm,
      crates: config.registries.crates,
    })

    this.distributionCalculator = new DistributionCalculator(config.distribution)
  }

  /**
   * Register a new project for tracking
   */
  async registerProject(
    name: string,
    repositoryUrl?: string,
    organizationId?: string
  ): Promise<Project> {
    const project: Project = {
      id: generateId(),
      name,
      repositoryUrl,
      organizationId,
      createdAt: new Date(),
      updatedAt: new Date(),
    }

    this.projects.set(project.id, project)
    logger.info(`Registered project: ${name}`, { projectId: project.id })

    return project
  }

  /**
   * Get project by ID
   */
  getProject(projectId: string): Project | undefined {
    return this.projects.get(projectId)
  }

  /**
   * Scan a project directory for dependencies
   */
  async scanDirectory(
    projectId: string,
    directory: string
  ): Promise<ScanResult> {
    const project = this.projects.get(projectId)
    if (!project) {
      throw new Error(`Project not found: ${projectId}`)
    }

    logger.info(`Scanning directory for project ${project.name}`, { directory })

    const startTime = Date.now()
    const allDependencies = new Map<string, Dependency>()
    const rootDependencies: Dependency[] = []
    const ecosystems = new Set<Ecosystem>()

    // Find manifest files
    const manifestPatterns = getManifestPatterns()
    const lockfilePatterns = getLockfilePatterns()

    const manifests = await glob(manifestPatterns, { cwd: directory, absolute: true })
    const lockfiles = await glob(lockfilePatterns, { cwd: directory, absolute: true })

    // Build lockfile map
    const lockfileMap = new Map<string, string>()
    for (const lockfile of lockfiles) {
      const name = basename(lockfile)
      lockfileMap.set(name, lockfile)
    }

    // Process each manifest
    for (const manifest of manifests) {
      const ecosystem = detectEcosystem(basename(manifest))
      if (!ecosystem) continue

      ecosystems.add(ecosystem)

      try {
        const scanner = createScanner(ecosystem)
        const manifestContent = await readFile(manifest, 'utf-8')

        // Find corresponding lockfile
        let lockfileContent: string | undefined
        const lockfilePath = lockfileMap.get(scanner.lockFileName)
        if (lockfilePath) {
          lockfileContent = await readFile(lockfilePath, 'utf-8')
        }

        const deps = await scanner.scan(manifestContent, lockfileContent)

        for (const dep of deps) {
          const key = `${dep.ecosystem}:${dep.name}`
          if (!allDependencies.has(key)) {
            allDependencies.set(key, dep)
          }
          if (dep.isDirect) {
            rootDependencies.push(dep)
          }
        }

        logger.debug(`Scanned ${basename(manifest)}`, { depCount: deps.length })
      } catch (error) {
        logger.error(`Failed to scan ${manifest}`, { error })
      }
    }

    // Create dependency graph
    const graph: DependencyGraph = {
      projectId,
      projectName: project.name,
      rootDependencies,
      allDependencies,
      totalCount: allDependencies.size,
      scanTimestamp: new Date(),
      ecosystems: Array.from(ecosystems),
    }

    this.graphs.set(projectId, graph)

    // Update project
    project.lastScan = new Date()
    project.updatedAt = new Date()

    const scanDuration = Date.now() - startTime

    logger.info(`Scan complete for ${project.name}`, {
      totalDeps: graph.totalCount,
      directDeps: rootDependencies.length,
      ecosystems: graph.ecosystems,
      duration: scanDuration,
    })

    return {
      projectId,
      graph,
      newPackages: allDependencies.size, // Simplified - would compare with previous scan
      updatedPackages: 0,
      scanDuration,
    }
  }

  /**
   * Scan with provided manifest content
   */
  async scanManifests(request: ScanRequest): Promise<ScanResult> {
    const { projectId, manifestFiles } = request
    const project = this.projects.get(projectId)
    if (!project) {
      throw new Error(`Project not found: ${projectId}`)
    }

    logger.info(`Scanning manifests for project ${project.name}`)

    const startTime = Date.now()
    const allDependencies = new Map<string, Dependency>()
    const rootDependencies: Dependency[] = []
    const ecosystems = new Set<Ecosystem>()

    // Process Cargo
    if (manifestFiles.cargoLock) {
      ecosystems.add('cargo' as Ecosystem)
      // Would need Cargo.toml content too for full parsing
    }

    // Process npm
    if (manifestFiles.packageLock || manifestFiles.pnpmLock) {
      const scanner = createScanner('npm' as Ecosystem)
      ecosystems.add('npm' as Ecosystem)
      // Would need package.json content
      try {
        const lockContent = manifestFiles.packageLock ?? manifestFiles.pnpmLock ?? ''
        const deps = await scanner.scan('{}', lockContent) // Empty manifest
        for (const dep of deps) {
          const key = `${dep.ecosystem}:${dep.name}`
          allDependencies.set(key, dep)
        }
      } catch (error) {
        logger.error('Failed to parse npm lockfile', { error })
      }
    }

    // Process Python
    if (manifestFiles.pyprojectToml) {
      const scanner = createScanner('pypi' as Ecosystem)
      ecosystems.add('pypi' as Ecosystem)
      try {
        const lockContent = manifestFiles.uvLock
        const deps = await scanner.scan(manifestFiles.pyprojectToml, lockContent)
        for (const dep of deps) {
          const key = `${dep.ecosystem}:${dep.name}`
          allDependencies.set(key, dep)
          if (dep.isDirect) rootDependencies.push(dep)
        }
      } catch (error) {
        logger.error('Failed to parse Python manifest', { error })
      }
    }

    // Process Go
    if (manifestFiles.goMod) {
      const scanner = createScanner('go' as Ecosystem)
      ecosystems.add('go' as Ecosystem)
      try {
        const deps = await scanner.scan(manifestFiles.goMod, manifestFiles.goSum)
        for (const dep of deps) {
          const key = `${dep.ecosystem}:${dep.name}`
          allDependencies.set(key, dep)
          if (dep.isDirect) rootDependencies.push(dep)
        }
      } catch (error) {
        logger.error('Failed to parse Go manifest', { error })
      }
    }

    const graph: DependencyGraph = {
      projectId,
      projectName: project.name,
      rootDependencies,
      allDependencies,
      totalCount: allDependencies.size,
      scanTimestamp: new Date(),
      ecosystems: Array.from(ecosystems),
    }

    this.graphs.set(projectId, graph)
    project.lastScan = new Date()

    return {
      projectId,
      graph,
      newPackages: allDependencies.size,
      updatedPackages: 0,
      scanDuration: Date.now() - startTime,
    }
  }

  /**
   * Get dependency graph for a project
   */
  getDependencyGraph(projectId: string): DependencyGraph | undefined {
    return this.graphs.get(projectId)
  }

  /**
   * Attribute all dependencies for a project
   */
  async attributeProject(projectId: string): Promise<Map<string, PackageAttribution>> {
    const graph = this.graphs.get(projectId)
    if (!graph) {
      throw new Error(`No scan data for project: ${projectId}`)
    }

    logger.info(`Attributing ${graph.totalCount} dependencies for project ${projectId}`)

    const attributions = new Map<string, PackageAttribution>()
    const dependencies = Array.from(graph.allDependencies.values())

    // Process in batches to respect rate limits
    const results = await this.attributionEngine.attributeDependencies(dependencies)

    for (const [key, result] of results) {
      attributions.set(key, result.attribution)
    }

    this.attributions.set(projectId, attributions)

    logger.info(`Attribution complete`, {
      projectId,
      attributedCount: attributions.size,
    })

    return attributions
  }

  /**
   * Calculate distribution preview (dry run)
   */
  async previewDistribution(request: DistributionRequest): Promise<DistributionPreview> {
    const { projectId, totalAmount, periodStart, periodEnd } = request

    const graph = this.graphs.get(projectId)
    if (!graph) {
      throw new Error(`No scan data for project: ${projectId}`)
    }

    let attributions = this.attributions.get(projectId)
    if (!attributions) {
      // Auto-attribute if not done yet
      attributions = await this.attributeProject(projectId)
    }

    return this.distributionCalculator.preview({
      projectId,
      graph,
      attributions,
      totalBudget: toDecimal(totalAmount),
      periodStart,
      periodEnd,
    })
  }

  /**
   * Calculate and create distribution
   */
  async createDistribution(request: DistributionRequest): Promise<Distribution> {
    const { projectId, totalAmount, currency, periodStart, periodEnd, dryRun } = request

    if (dryRun) {
      const preview = await this.previewDistribution(request)
      // Convert preview to distribution format for dry run
      throw new Error('Use previewDistribution for dry runs')
    }

    const graph = this.graphs.get(projectId)
    if (!graph) {
      throw new Error(`No scan data for project: ${projectId}`)
    }

    let attributions = this.attributions.get(projectId)
    if (!attributions) {
      attributions = await this.attributeProject(projectId)
    }

    const distribution = this.distributionCalculator.calculate({
      projectId,
      graph,
      attributions,
      totalBudget: toDecimal(totalAmount),
      periodStart,
      periodEnd,
      currency,
    })

    logger.info(`Distribution created`, {
      distributionId: distribution.id,
      projectId,
      totalAmount: distribution.totalAmount.toString(),
      paymentCount: distribution.payments.length,
    })

    return distribution
  }

  /**
   * Get distribution by ID
   */
  getDistribution(_distributionId: string): Distribution | undefined {
    // Would be retrieved from database
    return undefined
  }

  /**
   * List all projects
   */
  listProjects(): Project[] {
    return Array.from(this.projects.values())
  }
}

/**
 * Create an OSS Payment Service instance
 */
export function createOSSPaymentService(config: OSSPaymentConfig): OSSPaymentService {
  return new OSSPaymentService(config)
}
