/**
 * Base Scanner Interface
 *
 * Defines the contract for all ecosystem-specific dependency scanners.
 */

import type { Dependency, Ecosystem } from '../types/index.js'

export interface ScannerOptions {
  includeDevDependencies?: boolean
  maxDepth?: number
  excludePatterns?: string[]
}

export interface ParsedManifest {
  name?: string
  version?: string
  dependencies: Map<string, string>
  devDependencies?: Map<string, string>
  repositoryUrl?: string
}

export interface ParsedLockfile {
  packages: Map<string, LockfilePackage>
}

export interface LockfilePackage {
  name: string
  version: string
  dependencies?: string[]
  integrity?: string
  resolved?: string
}

export abstract class DependencyScanner {
  abstract readonly ecosystem: Ecosystem
  abstract readonly manifestFileName: string
  abstract readonly lockFileName: string

  protected options: ScannerOptions

  constructor(options: ScannerOptions = {}) {
    this.options = {
      includeDevDependencies: false,
      maxDepth: Infinity,
      excludePatterns: [],
      ...options,
    }
  }

  /**
   * Parse the manifest file (package.json, Cargo.toml, etc.)
   */
  abstract parseManifest(content: string): Promise<ParsedManifest>

  /**
   * Parse the lock file (package-lock.json, Cargo.lock, etc.)
   */
  abstract parseLockfile(content: string): Promise<ParsedLockfile>

  /**
   * Scan dependencies from manifest and optionally lockfile
   */
  async scan(
    manifestContent: string,
    lockfileContent?: string
  ): Promise<Dependency[]> {
    const manifest = await this.parseManifest(manifestContent)
    const lockfile = lockfileContent
      ? await this.parseLockfile(lockfileContent)
      : undefined

    const dependencies: Dependency[] = []
    const seen = new Set<string>()

    // Process direct dependencies
    for (const [name, versionSpec] of manifest.dependencies) {
      if (this.shouldExclude(name)) continue

      const version = this.resolveVersion(name, versionSpec, lockfile)
      const dep = this.createDependency(name, version, true, 0)
      dependencies.push(dep)
      seen.add(name)
    }

    // Process dev dependencies if enabled
    if (this.options.includeDevDependencies && manifest.devDependencies) {
      for (const [name, versionSpec] of manifest.devDependencies) {
        if (this.shouldExclude(name) || seen.has(name)) continue

        const version = this.resolveVersion(name, versionSpec, lockfile)
        const dep = this.createDependency(name, version, true, 0)
        dependencies.push(dep)
        seen.add(name)
      }
    }

    // Process transitive dependencies from lockfile
    if (lockfile) {
      const transitive = this.extractTransitiveDependencies(
        dependencies,
        lockfile,
        seen
      )
      dependencies.push(...transitive)
    }

    return dependencies
  }

  /**
   * Extract transitive dependencies with depth tracking
   */
  protected extractTransitiveDependencies(
    directDeps: Dependency[],
    lockfile: ParsedLockfile,
    seen: Set<string>
  ): Dependency[] {
    const transitive: Dependency[] = []
    const queue: Array<{ name: string; depth: number }> = []

    // Initialize queue with direct dependency children
    for (const dep of directDeps) {
      const lockPkg = lockfile.packages.get(dep.name)
      if (lockPkg?.dependencies) {
        for (const child of lockPkg.dependencies) {
          if (!seen.has(child)) {
            queue.push({ name: child, depth: 1 })
          }
        }
      }
    }

    // BFS to discover all transitive dependencies
    while (queue.length > 0) {
      const item = queue.shift()
      if (!item) continue

      const { name, depth } = item

      if (seen.has(name)) continue
      if (depth > (this.options.maxDepth ?? Infinity)) continue
      if (this.shouldExclude(name)) continue

      seen.add(name)

      const lockPkg = lockfile.packages.get(name)
      if (lockPkg) {
        const dep = this.createDependency(name, lockPkg.version, false, depth)
        transitive.push(dep)

        // Add children to queue
        if (lockPkg.dependencies) {
          for (const child of lockPkg.dependencies) {
            if (!seen.has(child)) {
              queue.push({ name: child, depth: depth + 1 })
            }
          }
        }
      }
    }

    return transitive
  }

  /**
   * Resolve version from lockfile or use spec
   */
  protected resolveVersion(
    name: string,
    versionSpec: string,
    lockfile?: ParsedLockfile
  ): string {
    if (lockfile) {
      const lockPkg = lockfile.packages.get(name)
      if (lockPkg) {
        return lockPkg.version
      }
    }
    // Strip version range prefixes
    return versionSpec.replace(/^[\^~>=<]+/, '')
  }

  /**
   * Create a Dependency object
   */
  protected createDependency(
    name: string,
    version: string,
    isDirect: boolean,
    depth: number
  ): Dependency {
    return {
      name,
      version,
      ecosystem: this.ecosystem,
      isDirect,
      depth,
      usageCount: 0,
    }
  }

  /**
   * Check if package should be excluded
   */
  protected shouldExclude(name: string): boolean {
    const patterns = this.options.excludePatterns ?? []
    return patterns.some((pattern) => {
      if (pattern.includes('*')) {
        const regex = new RegExp('^' + pattern.replace(/\*/g, '.*') + '$')
        return regex.test(name)
      }
      return name === pattern
    })
  }
}
