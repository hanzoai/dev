/**
 * NPM/Node.js Dependency Scanner
 *
 * Parses package.json and package-lock.json / pnpm-lock.yaml
 */

import { Ecosystem } from '../types/index.js'
import {
  DependencyScanner,
  type ParsedManifest,
  type ParsedLockfile,
  type LockfilePackage,
  type ScannerOptions,
} from './base.js'
import YAML from 'yaml'

export class NpmScanner extends DependencyScanner {
  readonly ecosystem = Ecosystem.Npm
  readonly manifestFileName = 'package.json'
  readonly lockFileName = 'package-lock.json'

  constructor(options: ScannerOptions = {}) {
    super(options)
  }

  async parseManifest(content: string): Promise<ParsedManifest> {
    const pkg = JSON.parse(content) as {
      name?: string
      version?: string
      dependencies?: Record<string, string>
      devDependencies?: Record<string, string>
      repository?: string | { type?: string; url?: string }
    }

    const dependencies = new Map<string, string>()
    const devDependencies = new Map<string, string>()

    if (pkg.dependencies) {
      for (const [name, version] of Object.entries(pkg.dependencies)) {
        dependencies.set(name, version)
      }
    }

    if (pkg.devDependencies) {
      for (const [name, version] of Object.entries(pkg.devDependencies)) {
        devDependencies.set(name, version)
      }
    }

    let repositoryUrl: string | undefined
    if (typeof pkg.repository === 'string') {
      repositoryUrl = pkg.repository
    } else if (pkg.repository?.url) {
      repositoryUrl = pkg.repository.url
    }

    return {
      name: pkg.name,
      version: pkg.version,
      dependencies,
      devDependencies,
      repositoryUrl,
    }
  }

  async parseLockfile(content: string): Promise<ParsedLockfile> {
    // Detect if it's pnpm-lock.yaml or package-lock.json
    if (content.trim().startsWith('lockfileVersion')) {
      return this.parsePnpmLockfile(content)
    }
    return this.parsePackageLock(content)
  }

  private parsePackageLock(content: string): ParsedLockfile {
    const lock = JSON.parse(content) as {
      lockfileVersion?: number
      packages?: Record<string, PackageLockEntry>
      dependencies?: Record<string, LegacyDependency>
    }

    const packages = new Map<string, LockfilePackage>()

    // Handle lockfile v2/v3 format (packages field)
    if (lock.packages) {
      for (const [path, entry] of Object.entries(lock.packages)) {
        // Skip root package
        if (path === '') continue

        // Extract package name from path (e.g., "node_modules/lodash")
        const name = this.extractPackageName(path)
        if (!name) continue

        packages.set(name, {
          name,
          version: entry.version ?? '',
          dependencies: entry.dependencies
            ? Object.keys(entry.dependencies)
            : undefined,
          integrity: entry.integrity,
          resolved: entry.resolved,
        })
      }
    }

    // Handle lockfile v1 format (dependencies field)
    if (lock.dependencies && packages.size === 0) {
      this.parseLegacyDependencies(lock.dependencies, packages)
    }

    return { packages }
  }

  private extractPackageName(path: string): string | undefined {
    // Handle scoped packages: "node_modules/@scope/name"
    // Handle regular packages: "node_modules/name"
    const match = path.match(/node_modules\/(@[^/]+\/[^/]+|[^/]+)$/)
    return match?.[1]
  }

  private parseLegacyDependencies(
    deps: Record<string, LegacyDependency>,
    packages: Map<string, LockfilePackage>,
    prefix: string = ''
  ): void {
    for (const [name, entry] of Object.entries(deps)) {
      const fullName = prefix ? `${prefix}/${name}` : name

      packages.set(name, {
        name,
        version: entry.version,
        dependencies: entry.requires ? Object.keys(entry.requires) : undefined,
        integrity: entry.integrity,
        resolved: entry.resolved,
      })

      // Recurse into nested dependencies
      if (entry.dependencies) {
        this.parseLegacyDependencies(entry.dependencies, packages, fullName)
      }
    }
  }

  private parsePnpmLockfile(content: string): ParsedLockfile {
    const lock = YAML.parse(content) as {
      lockfileVersion?: string | number
      packages?: Record<string, PnpmPackageEntry>
      snapshots?: Record<string, PnpmPackageEntry>
    }

    const packages = new Map<string, LockfilePackage>()

    // pnpm v9+ uses packages, older uses snapshots
    const pkgSource = lock.packages ?? lock.snapshots ?? {}

    for (const [key, entry] of Object.entries(pkgSource)) {
      // Parse package specifier: "@scope/name@version" or "name@version"
      const { name, version } = this.parsePnpmPackageKey(key)
      if (!name) continue

      packages.set(name, {
        name,
        version,
        dependencies: entry.dependencies
          ? Object.keys(entry.dependencies)
          : undefined,
        integrity: entry.resolution?.integrity,
      })
    }

    return { packages }
  }

  private parsePnpmPackageKey(key: string): { name: string; version: string } {
    // Handle format: "/@scope/name@version" or "/name@version" or "@scope/name@version"
    const cleaned = key.startsWith('/') ? key.slice(1) : key

    // Find the last @ that separates name from version
    const lastAtIndex = cleaned.lastIndexOf('@')
    if (lastAtIndex <= 0) {
      // No version, or @ is at position 0 (scoped package without version)
      return { name: cleaned, version: '' }
    }

    // Check if this @ is part of a scoped package name
    const beforeAt = cleaned.slice(0, lastAtIndex)
    if (beforeAt.startsWith('@') && !beforeAt.includes('/')) {
      // It's @scope@version format (malformed), skip
      return { name: cleaned, version: '' }
    }

    return {
      name: beforeAt,
      version: cleaned.slice(lastAtIndex + 1),
    }
  }
}

// Type definitions for lockfile structures
interface PackageLockEntry {
  version?: string
  resolved?: string
  integrity?: string
  dependencies?: Record<string, string>
  devDependencies?: Record<string, string>
}

interface LegacyDependency {
  version: string
  resolved?: string
  integrity?: string
  requires?: Record<string, string>
  dependencies?: Record<string, LegacyDependency>
}

interface PnpmPackageEntry {
  dependencies?: Record<string, string>
  devDependencies?: Record<string, string>
  resolution?: {
    integrity?: string
    tarball?: string
  }
}
