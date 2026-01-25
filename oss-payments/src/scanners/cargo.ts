/**
 * Cargo/Rust Dependency Scanner
 *
 * Parses Cargo.toml and Cargo.lock
 */

import { Ecosystem } from '../types/index.js'
import {
  DependencyScanner,
  type ParsedManifest,
  type ParsedLockfile,
  type LockfilePackage,
  type ScannerOptions,
} from './base.js'
import TOML from 'toml'

export class CargoScanner extends DependencyScanner {
  readonly ecosystem = Ecosystem.Cargo
  readonly manifestFileName = 'Cargo.toml'
  readonly lockFileName = 'Cargo.lock'

  constructor(options: ScannerOptions = {}) {
    super(options)
  }

  async parseManifest(content: string): Promise<ParsedManifest> {
    const cargo = TOML.parse(content) as CargoToml

    const dependencies = new Map<string, string>()
    const devDependencies = new Map<string, string>()

    // Parse regular dependencies
    if (cargo.dependencies) {
      for (const [name, spec] of Object.entries(cargo.dependencies)) {
        const version = this.extractVersion(spec)
        dependencies.set(name, version)
      }
    }

    // Parse dev dependencies
    if (cargo['dev-dependencies']) {
      for (const [name, spec] of Object.entries(cargo['dev-dependencies'])) {
        const version = this.extractVersion(spec)
        devDependencies.set(name, version)
      }
    }

    // Parse build dependencies (treat as regular deps for attribution)
    if (cargo['build-dependencies']) {
      for (const [name, spec] of Object.entries(cargo['build-dependencies'])) {
        if (!dependencies.has(name)) {
          const version = this.extractVersion(spec)
          dependencies.set(name, version)
        }
      }
    }

    // Parse workspace dependencies
    if (cargo.workspace?.dependencies) {
      for (const [name, spec] of Object.entries(cargo.workspace.dependencies)) {
        if (!dependencies.has(name)) {
          const version = this.extractVersion(spec)
          dependencies.set(name, version)
        }
      }
    }

    // Extract repository URL
    let repositoryUrl: string | undefined
    if (cargo.package?.repository) {
      repositoryUrl = cargo.package.repository
    }

    return {
      name: cargo.package?.name,
      version: cargo.package?.version,
      dependencies,
      devDependencies,
      repositoryUrl,
    }
  }

  async parseLockfile(content: string): Promise<ParsedLockfile> {
    const lock = TOML.parse(content) as CargoLock

    const packages = new Map<string, LockfilePackage>()

    if (lock.package) {
      for (const pkg of lock.package) {
        // Skip workspace members (usually the root package)
        if (pkg.source === undefined && pkg.checksum === undefined) {
          // Could be a workspace member or path dependency
          // Include it if it has a version (might be published later)
        }

        packages.set(pkg.name, {
          name: pkg.name,
          version: pkg.version,
          dependencies: pkg.dependencies?.map((dep) => this.parseDependencyString(dep)),
          integrity: pkg.checksum,
        })
      }
    }

    return { packages }
  }

  private extractVersion(spec: CargoDependencySpec): string {
    if (typeof spec === 'string') {
      return spec
    }
    if (typeof spec === 'object' && spec !== null) {
      return spec.version ?? '*'
    }
    return '*'
  }

  private parseDependencyString(dep: string): string {
    // Format: "name version" or "name version (source)"
    const parts = dep.split(' ')
    return parts[0] ?? dep
  }
}

// Type definitions for Cargo structures
interface CargoToml {
  package?: {
    name?: string
    version?: string
    authors?: string[]
    repository?: string
    homepage?: string
    license?: string
  }
  dependencies?: Record<string, CargoDependencySpec>
  'dev-dependencies'?: Record<string, CargoDependencySpec>
  'build-dependencies'?: Record<string, CargoDependencySpec>
  workspace?: {
    members?: string[]
    dependencies?: Record<string, CargoDependencySpec>
  }
}

type CargoDependencySpec =
  | string
  | {
      version?: string
      path?: string
      git?: string
      branch?: string
      tag?: string
      rev?: string
      features?: string[]
      optional?: boolean
      default_features?: boolean
    }

interface CargoLock {
  version?: number
  package?: Array<{
    name: string
    version: string
    source?: string
    checksum?: string
    dependencies?: string[]
  }>
}
