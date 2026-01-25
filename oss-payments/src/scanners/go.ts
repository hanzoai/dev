/**
 * Go Dependency Scanner
 *
 * Parses go.mod and go.sum
 */

import { Ecosystem } from '../types/index.js'
import {
  DependencyScanner,
  type ParsedManifest,
  type ParsedLockfile,
  type LockfilePackage,
  type ScannerOptions,
} from './base.js'

export class GoScanner extends DependencyScanner {
  readonly ecosystem = Ecosystem.Go
  readonly manifestFileName = 'go.mod'
  readonly lockFileName = 'go.sum'

  constructor(options: ScannerOptions = {}) {
    super(options)
  }

  async parseManifest(content: string): Promise<ParsedManifest> {
    const dependencies = new Map<string, string>()

    let moduleName: string | undefined
    let moduleVersion: string | undefined
    let inRequireBlock = false
    let inReplaceBlock = false

    const replacements = new Map<string, string>()

    for (const line of content.split('\n')) {
      const trimmed = line.trim()

      // Skip comments and empty lines
      if (trimmed.startsWith('//') || !trimmed) {
        continue
      }

      // Parse module declaration
      if (trimmed.startsWith('module ')) {
        moduleName = trimmed.slice(7).trim()
        continue
      }

      // Parse go version (not really a version, but useful)
      if (trimmed.startsWith('go ')) {
        moduleVersion = trimmed.slice(3).trim()
        continue
      }

      // Handle block starts
      if (trimmed === 'require (') {
        inRequireBlock = true
        continue
      }
      if (trimmed === 'replace (') {
        inReplaceBlock = true
        continue
      }
      if (trimmed === ')') {
        inRequireBlock = false
        inReplaceBlock = false
        continue
      }

      // Handle single-line require
      if (trimmed.startsWith('require ') && !inRequireBlock) {
        const dep = this.parseRequireLine(trimmed.slice(8))
        if (dep) {
          dependencies.set(dep.path, dep.version)
        }
        continue
      }

      // Handle single-line replace
      if (trimmed.startsWith('replace ') && !inReplaceBlock) {
        const replacement = this.parseReplaceLine(trimmed.slice(8))
        if (replacement) {
          replacements.set(replacement.original, replacement.replacement)
        }
        continue
      }

      // Parse require block entries
      if (inRequireBlock) {
        const dep = this.parseRequireLine(trimmed)
        if (dep) {
          dependencies.set(dep.path, dep.version)
        }
      }

      // Parse replace block entries
      if (inReplaceBlock) {
        const replacement = this.parseReplaceLine(trimmed)
        if (replacement) {
          replacements.set(replacement.original, replacement.replacement)
        }
      }
    }

    // Apply replacements - remove replaced modules (they're local or forked)
    // Note: We keep replaced dependencies in the list but could filter them out
    // for original in replacements.keys(): dependencies.delete(original)

    return {
      name: moduleName,
      version: moduleVersion,
      dependencies,
      repositoryUrl: moduleName ? `https://${moduleName}` : undefined,
    }
  }

  async parseLockfile(content: string): Promise<ParsedLockfile> {
    const packages = new Map<string, LockfilePackage>()
    const seen = new Set<string>()

    for (const line of content.split('\n')) {
      const trimmed = line.trim()
      if (!trimmed) continue

      // go.sum format: "module version hash" or "module version/go.mod hash"
      const parts = trimmed.split(/\s+/)
      if (parts.length < 3) continue

      const modulePath = parts[0]
      const versionFull = parts[1]
      const hash = parts[2]

      if (!modulePath || !versionFull || !hash) continue

      // Skip go.mod entries (we want the actual module)
      if (versionFull.endsWith('/go.mod')) continue

      // Normalize version (remove /go.mod suffix if present)
      const version = versionFull.replace('/go.mod', '')

      // Use module path as key, avoiding duplicates
      if (!seen.has(modulePath)) {
        seen.add(modulePath)
        packages.set(modulePath, {
          name: modulePath,
          version,
          integrity: hash,
        })
      }
    }

    return { packages }
  }

  private parseRequireLine(
    line: string
  ): { path: string; version: string } | undefined {
    // Format: "github.com/user/repo v1.0.0" or with // indirect comment
    const parts = line.split(/\s+/)
    const path = parts[0]
    let version = parts[1]

    if (!path || !version) return undefined

    // Remove indirect marker if present
    version = version.replace(/\s*\/\/.*$/, '')

    return { path, version }
  }

  private parseReplaceLine(
    line: string
  ): { original: string; replacement: string } | undefined {
    // Format: "old/path => new/path" or "old/path v1.0.0 => new/path v2.0.0"
    const parts = line.split('=>')
    if (parts.length !== 2) return undefined

    const original = parts[0]?.trim().split(/\s+/)[0]
    const replacement = parts[1]?.trim().split(/\s+/)[0]

    if (!original || !replacement) return undefined

    return { original, replacement }
  }
}
