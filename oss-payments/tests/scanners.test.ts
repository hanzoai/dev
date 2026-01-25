/**
 * Scanner Tests
 */

import { describe, it, expect } from 'vitest'
import { NpmScanner } from '../src/scanners/npm.js'
import { CargoScanner } from '../src/scanners/cargo.js'
import { PythonScanner } from '../src/scanners/python.js'
import { GoScanner } from '../src/scanners/go.js'
import { Ecosystem } from '../src/types/index.js'

describe('NpmScanner', () => {
  const scanner = new NpmScanner()

  it('parses package.json', async () => {
    const manifest = JSON.stringify({
      name: 'test-project',
      version: '1.0.0',
      dependencies: {
        'lodash': '^4.17.21',
        'express': '~4.18.0',
      },
      devDependencies: {
        'jest': '29.0.0',
      },
    })

    const result = await scanner.parseManifest(manifest)

    expect(result.name).toBe('test-project')
    expect(result.version).toBe('1.0.0')
    expect(result.dependencies.size).toBe(2)
    expect(result.dependencies.get('lodash')).toBe('^4.17.21')
    expect(result.dependencies.get('express')).toBe('~4.18.0')
    expect(result.devDependencies?.size).toBe(1)
  })

  it('parses package-lock.json v2', async () => {
    const lockfile = JSON.stringify({
      lockfileVersion: 2,
      packages: {
        '': { name: 'test', version: '1.0.0' },
        'node_modules/lodash': { version: '4.17.21' },
        'node_modules/@types/node': { version: '20.0.0' },
      },
    })

    const result = await scanner.parseLockfile(lockfile)

    expect(result.packages.size).toBe(2)
    expect(result.packages.get('lodash')?.version).toBe('4.17.21')
    expect(result.packages.get('@types/node')?.version).toBe('20.0.0')
  })

  it('scans dependencies with lockfile', async () => {
    const manifest = JSON.stringify({
      dependencies: { 'lodash': '^4.17.0' },
    })

    const lockfile = JSON.stringify({
      lockfileVersion: 2,
      packages: {
        '': {},
        'node_modules/lodash': {
          version: '4.17.21',
          dependencies: ['lodash.debounce'],
        },
        'node_modules/lodash.debounce': { version: '4.0.8' },
      },
    })

    const deps = await scanner.scan(manifest, lockfile)

    expect(deps.length).toBeGreaterThanOrEqual(1)
    expect(deps.find((d) => d.name === 'lodash')).toBeDefined()
    expect(deps.find((d) => d.name === 'lodash')?.isDirect).toBe(true)
  })
})

describe('CargoScanner', () => {
  const scanner = new CargoScanner()

  it('parses Cargo.toml', async () => {
    const manifest = `
[package]
name = "test-crate"
version = "0.1.0"
repository = "https://github.com/test/test-crate"

[dependencies]
serde = "1.0"
tokio = { version = "1.0", features = ["full"] }

[dev-dependencies]
criterion = "0.4"
`

    const result = await scanner.parseManifest(manifest)

    expect(result.name).toBe('test-crate')
    expect(result.version).toBe('0.1.0')
    expect(result.repositoryUrl).toBe('https://github.com/test/test-crate')
    expect(result.dependencies.size).toBe(2)
    expect(result.dependencies.get('serde')).toBe('1.0')
    expect(result.dependencies.get('tokio')).toBe('1.0')
    expect(result.devDependencies?.size).toBe(1)
  })

  it('parses Cargo.lock', async () => {
    const lockfile = `
[[package]]
name = "serde"
version = "1.0.193"
source = "registry+https://github.com/rust-lang/crates.io-index"
checksum = "abcdef"
dependencies = [
  "serde_derive 1.0.193"
]

[[package]]
name = "serde_derive"
version = "1.0.193"
`

    const result = await scanner.parseLockfile(lockfile)

    expect(result.packages.size).toBe(2)
    expect(result.packages.get('serde')?.version).toBe('1.0.193')
    expect(result.packages.get('serde')?.dependencies).toContain('serde_derive')
  })
})

describe('PythonScanner', () => {
  const scanner = new PythonScanner()

  it('parses pyproject.toml (PEP 621)', async () => {
    const manifest = `
[project]
name = "test-project"
version = "1.0.0"
dependencies = [
  "requests>=2.28.0",
  "click~=8.0",
]

[project.optional-dependencies]
dev = ["pytest>=7.0"]

[project.urls]
Repository = "https://github.com/test/project"
`

    const result = await scanner.parseManifest(manifest)

    expect(result.name).toBe('test-project')
    expect(result.version).toBe('1.0.0')
    expect(result.dependencies.size).toBe(2)
    expect(result.dependencies.has('requests')).toBe(true)
    expect(result.dependencies.has('click')).toBe(true)
    expect(result.devDependencies?.size).toBe(1)
    expect(result.repositoryUrl).toBe('https://github.com/test/project')
  })

  it('parses uv.lock', async () => {
    const lockfile = `
version = 1

[[package]]
name = "requests"
version = "2.31.0"
dependencies = ["charset-normalizer"]

[[package]]
name = "charset-normalizer"
version = "3.3.2"
`

    const result = await scanner.parseLockfile(lockfile)

    expect(result.packages.size).toBe(2)
    expect(result.packages.get('requests')?.version).toBe('2.31.0')
    expect(result.packages.get('charset-normalizer')?.version).toBe('3.3.2')
  })

  it('parses requirements.txt', async () => {
    const lockfile = `
# Requirements
requests==2.31.0
click>=8.0.0
pytest  # testing
-e ./local_package
`

    const result = await scanner.parseLockfile(lockfile)

    expect(result.packages.size).toBeGreaterThanOrEqual(2)
    expect(result.packages.get('requests')?.version).toBe('2.31.0')
    expect(result.packages.get('click')).toBeDefined()
    // Note: pytest is also parsed since it doesn't have a # comment marker
  })
})

describe('GoScanner', () => {
  const scanner = new GoScanner()

  it('parses go.mod', async () => {
    const manifest = `
module github.com/test/project

go 1.21

require (
	github.com/gin-gonic/gin v1.9.1
	golang.org/x/text v0.14.0 // indirect
)

require github.com/stretchr/testify v1.8.4
`

    const result = await scanner.parseManifest(manifest)

    expect(result.name).toBe('github.com/test/project')
    expect(result.version).toBe('1.21')
    expect(result.dependencies.size).toBe(3)
    expect(result.dependencies.get('github.com/gin-gonic/gin')).toBe('v1.9.1')
    expect(result.dependencies.get('golang.org/x/text')).toBe('v0.14.0')
    expect(result.dependencies.get('github.com/stretchr/testify')).toBe('v1.8.4')
  })

  it('parses go.sum', async () => {
    const lockfile = `
github.com/gin-gonic/gin v1.9.1 h1:abc123=
github.com/gin-gonic/gin v1.9.1/go.mod h1:def456=
golang.org/x/text v0.14.0 h1:xyz789=
`

    const result = await scanner.parseLockfile(lockfile)

    expect(result.packages.size).toBe(2)
    expect(result.packages.get('github.com/gin-gonic/gin')?.version).toBe('v1.9.1')
    expect(result.packages.get('golang.org/x/text')?.version).toBe('v0.14.0')
  })
})

describe('createScanner', () => {
  it('creates correct scanner for each ecosystem', async () => {
    const { createScanner } = await import('../src/scanners/index.js')

    expect(createScanner(Ecosystem.Npm)).toBeInstanceOf(NpmScanner)
    expect(createScanner(Ecosystem.Cargo)).toBeInstanceOf(CargoScanner)
    expect(createScanner(Ecosystem.PyPI)).toBeInstanceOf(PythonScanner)
    expect(createScanner(Ecosystem.Go)).toBeInstanceOf(GoScanner)
  })
})
