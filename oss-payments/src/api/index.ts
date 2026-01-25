/**
 * API Module
 *
 * REST API handlers for the OSS Payment System.
 * This module provides the API layer - integrate with your preferred framework.
 */

import type {
  ScanRequest,
  ScanResult,
  DistributionRequest,
  DistributionPreview,
  Distribution,
  Project,
  Author,
} from '../types/index.js'
import type { OSSPaymentService } from '../core/index.js'

/**
 * API Request Handlers
 *
 * These functions can be integrated with any HTTP framework (Express, Fastify, etc.)
 */
export class OSSPaymentAPI {
  constructor(private service: OSSPaymentService) {}

  // ============================================================================
  // Project Endpoints
  // ============================================================================

  /**
   * POST /api/v1/oss/projects
   * Register a new project
   */
  async createProject(body: {
    name: string
    repositoryUrl?: string
    organizationId?: string
  }): Promise<Project> {
    return this.service.registerProject(
      body.name,
      body.repositoryUrl,
      body.organizationId
    )
  }

  /**
   * GET /api/v1/oss/projects
   * List all projects
   */
  async listProjects(): Promise<Project[]> {
    return this.service.listProjects()
  }

  /**
   * GET /api/v1/oss/projects/:projectId
   * Get project details
   */
  async getProject(projectId: string): Promise<Project | null> {
    return this.service.getProject(projectId) ?? null
  }

  // ============================================================================
  // Scan Endpoints
  // ============================================================================

  /**
   * POST /api/v1/oss/projects/:projectId/scan
   * Scan project dependencies from uploaded manifest files
   */
  async scanProject(projectId: string, manifests: ScanRequest['manifestFiles']): Promise<ScanResult> {
    return this.service.scanManifests({
      projectId,
      manifestFiles: manifests,
    })
  }

  /**
   * POST /api/v1/oss/projects/:projectId/scan/directory
   * Scan project dependencies from a local directory
   */
  async scanDirectory(projectId: string, directory: string): Promise<ScanResult> {
    return this.service.scanDirectory(projectId, directory)
  }

  // ============================================================================
  // Dependencies Endpoints
  // ============================================================================

  /**
   * GET /api/v1/oss/projects/:projectId/dependencies
   * Get project dependency graph
   */
  async getDependencies(
    projectId: string,
    options: { includeTransitive?: boolean } = {}
  ): Promise<{
    total: number
    direct: number
    transitive: number
    ecosystems: string[]
    dependencies: Array<{
      name: string
      version: string
      ecosystem: string
      isDirect: boolean
      depth: number
    }>
  }> {
    const graph = this.service.getDependencyGraph(projectId)
    if (!graph) {
      throw new Error('Project not scanned. Run scan first.')
    }

    const { includeTransitive = true } = options
    const deps = Array.from(graph.allDependencies.values())
      .filter((d) => includeTransitive || d.isDirect)
      .map((d) => ({
        name: d.name,
        version: d.version,
        ecosystem: d.ecosystem,
        isDirect: d.isDirect,
        depth: d.depth,
      }))

    return {
      total: graph.totalCount,
      direct: graph.rootDependencies.length,
      transitive: graph.totalCount - graph.rootDependencies.length,
      ecosystems: graph.ecosystems,
      dependencies: deps,
    }
  }

  // ============================================================================
  // Distribution Endpoints
  // ============================================================================

  /**
   * POST /api/v1/oss/projects/:projectId/distribution/preview
   * Preview payment distribution (dry run)
   */
  async previewDistribution(
    projectId: string,
    body: Omit<DistributionRequest, 'projectId'>
  ): Promise<DistributionPreview> {
    return this.service.previewDistribution({
      projectId,
      ...body,
    })
  }

  /**
   * POST /api/v1/oss/projects/:projectId/distribution
   * Create and optionally execute a distribution
   */
  async createDistribution(
    projectId: string,
    body: Omit<DistributionRequest, 'projectId'>
  ): Promise<Distribution> {
    if (body.dryRun) {
      throw new Error('Use preview endpoint for dry runs')
    }

    return this.service.createDistribution({
      projectId,
      ...body,
    })
  }

  /**
   * GET /api/v1/oss/distributions/:distributionId
   * Get distribution details
   */
  async getDistribution(distributionId: string): Promise<Distribution | null> {
    return this.service.getDistribution(distributionId) ?? null
  }

  // ============================================================================
  // Author Endpoints
  // ============================================================================

  /**
   * GET /api/v1/oss/authors/:authorId
   * Get author profile
   */
  async getAuthor(_authorId: string): Promise<Author | null> {
    // Would be implemented with database queries
    return null
  }

  /**
   * POST /api/v1/oss/authors/claim
   * Author claims their packages (requires GitHub OAuth)
   */
  async claimPackages(_body: {
    githubCode: string
    paymentAddresses?: Array<{ type: string; address: string }>
  }): Promise<{ authorId: string; claimedPackages: string[] }> {
    // Would implement GitHub OAuth verification
    throw new Error('Not implemented - requires OAuth setup')
  }
}

/**
 * Create API instance
 */
export function createAPI(service: OSSPaymentService): OSSPaymentAPI {
  return new OSSPaymentAPI(service)
}

/**
 * OpenAPI schema fragment for documentation
 */
export const openAPISpec = {
  openapi: '3.0.0',
  info: {
    title: 'OSS Payment API',
    version: '1.0.0',
    description: 'API for tracking and distributing payments to OSS authors',
  },
  paths: {
    '/api/v1/oss/projects': {
      post: {
        summary: 'Register a project for OSS tracking',
        requestBody: {
          content: {
            'application/json': {
              schema: {
                type: 'object',
                properties: {
                  name: { type: 'string' },
                  repositoryUrl: { type: 'string' },
                },
                required: ['name'],
              },
            },
          },
        },
        responses: {
          '201': { description: 'Project created' },
        },
      },
      get: {
        summary: 'List all projects',
        responses: {
          '200': { description: 'List of projects' },
        },
      },
    },
    '/api/v1/oss/projects/{projectId}/scan': {
      post: {
        summary: 'Scan project dependencies',
        parameters: [
          { name: 'projectId', in: 'path', required: true, schema: { type: 'string' } },
        ],
        responses: {
          '202': { description: 'Scan initiated' },
        },
      },
    },
    '/api/v1/oss/projects/{projectId}/distribution': {
      post: {
        summary: 'Create payment distribution',
        requestBody: {
          content: {
            'application/json': {
              schema: {
                type: 'object',
                properties: {
                  totalAmount: { type: 'number' },
                  periodStart: { type: 'string', format: 'date' },
                  periodEnd: { type: 'string', format: 'date' },
                  dryRun: { type: 'boolean', default: false },
                },
                required: ['totalAmount', 'periodStart', 'periodEnd'],
              },
            },
          },
        },
        responses: {
          '201': { description: 'Distribution created' },
        },
      },
    },
  },
}
