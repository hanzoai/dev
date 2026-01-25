/**
 * Dependency Scanners - Unified Export
 */

export { DependencyScanner, type ScannerOptions, type ParsedManifest, type ParsedLockfile } from './base.js'
export { NpmScanner } from './npm.js'
export { CargoScanner } from './cargo.js'
export { PythonScanner } from './python.js'
export { GoScanner } from './go.js'

import { Ecosystem } from '../types/index.js'
import { DependencyScanner, type ScannerOptions } from './base.js'
import { NpmScanner } from './npm.js'
import { CargoScanner } from './cargo.js'
import { PythonScanner } from './python.js'
import { GoScanner } from './go.js'

/**
 * Factory function to create scanner for a specific ecosystem
 */
export function createScanner(
  ecosystem: Ecosystem,
  options: ScannerOptions = {}
): DependencyScanner {
  switch (ecosystem) {
    case Ecosystem.Npm:
      return new NpmScanner(options)
    case Ecosystem.Cargo:
      return new CargoScanner(options)
    case Ecosystem.PyPI:
      return new PythonScanner(options)
    case Ecosystem.Go:
      return new GoScanner(options)
    default:
      throw new Error(`Unsupported ecosystem: ${ecosystem}`)
  }
}

/**
 * Detect ecosystem from file names
 */
export function detectEcosystem(fileName: string): Ecosystem | undefined {
  const lower = fileName.toLowerCase()

  if (lower === 'package.json' || lower === 'package-lock.json' || lower === 'pnpm-lock.yaml') {
    return Ecosystem.Npm
  }
  if (lower === 'cargo.toml' || lower === 'cargo.lock') {
    return Ecosystem.Cargo
  }
  if (lower === 'pyproject.toml' || lower === 'uv.lock' || lower === 'requirements.txt') {
    return Ecosystem.PyPI
  }
  if (lower === 'go.mod' || lower === 'go.sum') {
    return Ecosystem.Go
  }

  return undefined
}

/**
 * Get all manifest file patterns for glob searching
 */
export function getManifestPatterns(): string[] {
  return [
    'package.json',
    'Cargo.toml',
    'pyproject.toml',
    'go.mod',
  ]
}

/**
 * Get all lockfile patterns for glob searching
 */
export function getLockfilePatterns(): string[] {
  return [
    'package-lock.json',
    'pnpm-lock.yaml',
    'yarn.lock',
    'Cargo.lock',
    'uv.lock',
    'requirements.txt',
    'go.sum',
  ]
}
