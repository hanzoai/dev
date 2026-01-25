/**
 * Python Dependency Scanner
 *
 * Parses pyproject.toml and uv.lock / requirements.txt
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

export class PythonScanner extends DependencyScanner {
  readonly ecosystem = Ecosystem.PyPI
  readonly manifestFileName = 'pyproject.toml'
  readonly lockFileName = 'uv.lock'

  constructor(options: ScannerOptions = {}) {
    super(options)
  }

  async parseManifest(content: string): Promise<ParsedManifest> {
    const pyproject = TOML.parse(content) as PyProjectToml

    const dependencies = new Map<string, string>()
    const devDependencies = new Map<string, string>()

    // PEP 621 style dependencies
    if (pyproject.project?.dependencies) {
      for (const dep of pyproject.project.dependencies) {
        const { name, version } = this.parsePep508(dep)
        dependencies.set(name, version)
      }
    }

    // Optional dependencies (often includes dev deps)
    if (pyproject.project?.['optional-dependencies']) {
      for (const [group, deps] of Object.entries(
        pyproject.project['optional-dependencies']
      )) {
        const isDevGroup = ['dev', 'test', 'testing', 'development'].includes(
          group.toLowerCase()
        )
        for (const dep of deps) {
          const { name, version } = this.parsePep508(dep)
          if (isDevGroup) {
            devDependencies.set(name, version)
          } else if (!dependencies.has(name)) {
            dependencies.set(name, version)
          }
        }
      }
    }

    // Poetry style dependencies
    if (pyproject.tool?.poetry?.dependencies) {
      for (const [name, spec] of Object.entries(
        pyproject.tool.poetry.dependencies
      )) {
        if (name === 'python') continue // Skip python version constraint
        const version = this.extractPoetryVersion(spec)
        dependencies.set(name, version)
      }
    }

    if (pyproject.tool?.poetry?.['dev-dependencies']) {
      for (const [name, spec] of Object.entries(
        pyproject.tool.poetry['dev-dependencies']
      )) {
        const version = this.extractPoetryVersion(spec)
        devDependencies.set(name, version)
      }
    }

    // uv style dependencies
    if (pyproject.tool?.uv?.['dev-dependencies']) {
      for (const dep of pyproject.tool.uv['dev-dependencies']) {
        const { name, version } = this.parsePep508(dep)
        devDependencies.set(name, version)
      }
    }

    // Extract repository URL
    let repositoryUrl: string | undefined
    if (pyproject.project?.urls?.Repository) {
      repositoryUrl = pyproject.project.urls.Repository
    } else if (pyproject.project?.urls?.repository) {
      repositoryUrl = pyproject.project.urls.repository
    } else if (pyproject.project?.urls?.Source) {
      repositoryUrl = pyproject.project.urls.Source
    } else if (pyproject.tool?.poetry?.repository) {
      repositoryUrl = pyproject.tool.poetry.repository
    }

    return {
      name: pyproject.project?.name ?? pyproject.tool?.poetry?.name,
      version: pyproject.project?.version ?? pyproject.tool?.poetry?.version,
      dependencies,
      devDependencies,
      repositoryUrl,
    }
  }

  async parseLockfile(content: string): Promise<ParsedLockfile> {
    // Detect format: uv.lock (TOML) or requirements.txt (plain)
    if (content.trim().startsWith('version =')) {
      return this.parseUvLock(content)
    }
    return this.parseRequirementsTxt(content)
  }

  private parseUvLock(content: string): ParsedLockfile {
    const lock = TOML.parse(content) as UvLock

    const packages = new Map<string, LockfilePackage>()

    if (lock.package) {
      for (const pkg of lock.package) {
        const dependencies = pkg.dependencies?.map((dep) => {
          const { name } = this.parsePep508(dep)
          return name
        })

        packages.set(this.normalizePyPiName(pkg.name), {
          name: this.normalizePyPiName(pkg.name),
          version: pkg.version,
          dependencies,
        })
      }
    }

    return { packages }
  }

  private parseRequirementsTxt(content: string): ParsedLockfile {
    const packages = new Map<string, LockfilePackage>()

    for (const line of content.split('\n')) {
      const trimmed = line.trim()

      // Skip comments and empty lines
      if (!trimmed || trimmed.startsWith('#') || trimmed.startsWith('-')) {
        continue
      }

      const { name, version } = this.parsePep508(trimmed)
      if (name) {
        packages.set(this.normalizePyPiName(name), {
          name: this.normalizePyPiName(name),
          version,
        })
      }
    }

    return { packages }
  }

  private parsePep508(spec: string): { name: string; version: string } {
    // Parse PEP 508 dependency specifier
    // Examples: "requests>=2.0", "flask[async]~=2.0", "package @ https://..."

    // Remove environment markers (everything after ;)
    const withoutMarkers = spec.split(';')[0]?.trim() ?? spec

    // Remove URL specifications
    const withoutUrl = withoutMarkers.split('@')[0]?.trim() ?? withoutMarkers

    // Remove extras (content in brackets)
    const withoutExtras = withoutUrl.replace(/\[.*?\]/g, '').trim()

    // Split on version specifiers
    const match = withoutExtras.match(/^([a-zA-Z0-9_-]+)(.*)?$/)
    if (match) {
      const name = match[1] ?? ''
      const versionPart = match[2]?.trim() ?? '*'

      // Extract version number from specifier
      const versionMatch = versionPart.match(/[>=<~!]+\s*([0-9][0-9.a-zA-Z]*)/)
      const version = versionMatch?.[1] ?? (versionPart || '*')

      return { name, version }
    }

    return { name: withoutExtras, version: '*' }
  }

  private extractPoetryVersion(
    spec: string | PoetryDependencySpec
  ): string {
    if (typeof spec === 'string') {
      return spec.replace(/^[\^~>=<]+/, '')
    }
    if (typeof spec === 'object' && spec !== null) {
      if (spec.version) {
        return spec.version.replace(/^[\^~>=<]+/, '')
      }
    }
    return '*'
  }

  private normalizePyPiName(name: string): string {
    // PyPI normalizes names: replace . and _ with -, lowercase
    return name.toLowerCase().replace(/[._]/g, '-')
  }
}

// Type definitions for Python structures
interface PyProjectToml {
  project?: {
    name?: string
    version?: string
    dependencies?: string[]
    'optional-dependencies'?: Record<string, string[]>
    urls?: Record<string, string>
  }
  tool?: {
    poetry?: {
      name?: string
      version?: string
      repository?: string
      dependencies?: Record<string, string | PoetryDependencySpec>
      'dev-dependencies'?: Record<string, string | PoetryDependencySpec>
    }
    uv?: {
      'dev-dependencies'?: string[]
    }
  }
}

interface PoetryDependencySpec {
  version?: string
  python?: string
  path?: string
  git?: string
  branch?: string
  tag?: string
  extras?: string[]
  optional?: boolean
}

interface UvLock {
  version?: number
  package?: Array<{
    name: string
    version: string
    source?: {
      registry?: string
    }
    dependencies?: string[]
  }>
}
