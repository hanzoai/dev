/**
 * CLI Module
 *
 * Command-line interface for the OSS Payment System.
 */

import { Command } from 'commander'
import ora from 'ora'
import { toDecimal } from '../utils/index.js'
import { createOSSPaymentService } from '../core/index.js'
import type { OSSPaymentConfig } from '../types/index.js'

export function createCLI() {
  const program = new Command()

  program
    .name('oss-payments')
    .description('OSS Author Payment Tracking and Distribution System')
    .version('1.0.0')

  // Scan command
  program
    .command('scan')
    .description('Scan a project directory for dependencies')
    .argument('[directory]', 'Directory to scan', '.')
    .option('-p, --project <name>', 'Project name', 'default')
    .option('--include-dev', 'Include dev dependencies')
    .option('--json', 'Output as JSON')
    .action(async (directory, options) => {
      const spinner = ora('Scanning dependencies...').start()

      try {
        const config = loadConfig()
        const service = createOSSPaymentService(config)

        // Register or get project
        const project = await service.registerProject(options.project)

        // Scan directory
        const result = await service.scanDirectory(project.id, directory)

        spinner.succeed(`Found ${result.graph.totalCount} dependencies`)

        if (options.json) {
          console.log(JSON.stringify({
            projectId: project.id,
            totalDependencies: result.graph.totalCount,
            directDependencies: result.graph.rootDependencies.length,
            ecosystems: result.graph.ecosystems,
            scanDuration: result.scanDuration,
          }, null, 2))
        } else {
          console.log(`\nProject: ${project.name}`)
          console.log(`Direct dependencies: ${result.graph.rootDependencies.length}`)
          console.log(`Total dependencies: ${result.graph.totalCount}`)
          console.log(`Ecosystems: ${result.graph.ecosystems.join(', ')}`)
          console.log(`Scan duration: ${result.scanDuration}ms`)

          // Show top 10 direct dependencies
          console.log(`\nTop direct dependencies:`)
          for (const dep of result.graph.rootDependencies.slice(0, 10)) {
            console.log(`  - ${dep.name}@${dep.version} (${dep.ecosystem})`)
          }
          if (result.graph.rootDependencies.length > 10) {
            console.log(`  ... and ${result.graph.rootDependencies.length - 10} more`)
          }
        }
      } catch (error) {
        spinner.fail('Scan failed')
        console.error(error)
        process.exit(1)
      }
    })

  // Attribution command
  program
    .command('attribute')
    .description('Attribute dependencies to their authors')
    .argument('<projectId>', 'Project ID to attribute')
    .option('--json', 'Output as JSON')
    .action(async (projectId, options) => {
      const spinner = ora('Attributing dependencies...').start()

      try {
        const config = loadConfig()
        const service = createOSSPaymentService(config)

        const attributions = await service.attributeProject(projectId)

        spinner.succeed(`Attributed ${attributions.size} packages`)

        if (options.json) {
          const data = Array.from(attributions.entries()).map(([key, attr]) => ({
            package: key,
            authors: attr.authors.map((a) => ({
              id: a.authorId,
              share: a.sharePercentage,
              role: a.role,
            })),
            fundingUrls: attr.fundingUrls,
          }))
          console.log(JSON.stringify(data, null, 2))
        } else {
          console.log(`\nAttribution Summary:`)
          let totalAuthors = 0
          for (const [key, attr] of attributions) {
            console.log(`\n${key}:`)
            for (const author of attr.authors) {
              console.log(`  - ${author.authorId}: ${(author.sharePercentage * 100).toFixed(1)}% (${author.role})`)
              totalAuthors++
            }
            if (attr.fundingUrls.length > 0) {
              console.log(`  Funding: ${attr.fundingUrls[0]}`)
            }
          }
          console.log(`\nTotal unique author entries: ${totalAuthors}`)
        }
      } catch (error) {
        spinner.fail('Attribution failed')
        console.error(error)
        process.exit(1)
      }
    })

  // Preview distribution command
  program
    .command('preview')
    .description('Preview payment distribution (dry run)')
    .argument('<projectId>', 'Project ID')
    .option('-a, --amount <amount>', 'Total amount to distribute (USD)', '100')
    .option('--start <date>', 'Period start date', new Date().toISOString().split('T')[0])
    .option('--end <date>', 'Period end date', new Date().toISOString().split('T')[0])
    .option('--json', 'Output as JSON')
    .action(async (projectId, options) => {
      const spinner = ora('Calculating distribution...').start()

      try {
        const config = loadConfig()
        const service = createOSSPaymentService(config)

        const preview = await service.previewDistribution({
          projectId,
          totalAmount: parseFloat(options.amount),
          periodStart: new Date(options.start),
          periodEnd: new Date(options.end),
        })

        spinner.succeed('Distribution preview calculated')

        if (options.json) {
          console.log(JSON.stringify({
            totalAmount: preview.totalAmount.toString(),
            uniqueAuthors: preview.uniqueAuthors,
            uniquePackages: preview.uniquePackages,
            payments: preview.payments.map((p) => ({
              ...p,
              amount: p.amount.toString(),
            })),
          }, null, 2))
        } else {
          console.log(`\nDistribution Preview`)
          console.log(`===================`)
          console.log(`Total Budget: $${options.amount}`)
          console.log(`Allocated: $${preview.totalAmount.toFixed(2)}`)
          console.log(`Unique Authors: ${preview.uniqueAuthors}`)
          console.log(`Unique Packages: ${preview.uniquePackages}`)
          console.log(`\nTop Recipients:`)

          for (const payment of preview.payments.slice(0, 20)) {
            console.log(
              `  $${payment.amount.toFixed(2).padStart(8)} (${payment.percentage.toFixed(2)}%) - ${payment.authorName} for ${payment.packageName}`
            )
          }
          if (preview.payments.length > 20) {
            console.log(`  ... and ${preview.payments.length - 20} more payments`)
          }
        }
      } catch (error) {
        spinner.fail('Preview failed')
        console.error(error)
        process.exit(1)
      }
    })

  // Distribute command
  program
    .command('distribute')
    .description('Execute payment distribution')
    .argument('<projectId>', 'Project ID')
    .option('-a, --amount <amount>', 'Total amount to distribute (USD)', '100')
    .option('--start <date>', 'Period start date')
    .option('--end <date>', 'Period end date')
    .option('--dry-run', 'Preview without executing')
    .option('--confirm', 'Confirm execution without prompting')
    .action(async (projectId, options) => {
      if (options.dryRun) {
        // Redirect to preview
        console.log('Running in dry-run mode (use "preview" command for detailed preview)')
      }

      const spinner = ora('Creating distribution...').start()

      try {
        const config = loadConfig()
        const service = createOSSPaymentService(config)

        const distribution = await service.createDistribution({
          projectId,
          totalAmount: parseFloat(options.amount),
          periodStart: options.start ? new Date(options.start) : new Date(),
          periodEnd: options.end ? new Date(options.end) : new Date(),
        })

        spinner.succeed(`Distribution created: ${distribution.id}`)

        console.log(`\nDistribution Summary`)
        console.log(`===================`)
        console.log(`ID: ${distribution.id}`)
        console.log(`Total: $${distribution.totalAmount.toString()}`)
        console.log(`Payments: ${distribution.payments.length}`)
        console.log(`Status: ${distribution.status}`)

        console.log(`\nTo process payments, use:`)
        console.log(`  oss-payments process ${distribution.id}`)
      } catch (error) {
        spinner.fail('Distribution failed')
        console.error(error)
        process.exit(1)
      }
    })

  // List projects command
  program
    .command('projects')
    .description('List registered projects')
    .option('--json', 'Output as JSON')
    .action(async (options) => {
      try {
        const config = loadConfig()
        const service = createOSSPaymentService(config)

        const projects = service.listProjects()

        if (options.json) {
          console.log(JSON.stringify(projects, null, 2))
        } else {
          if (projects.length === 0) {
            console.log('No projects registered. Use "oss-payments scan" to scan a project.')
          } else {
            console.log('Registered Projects:')
            for (const project of projects) {
              console.log(`\n  ${project.name}`)
              console.log(`    ID: ${project.id}`)
              console.log(`    Last Scan: ${project.lastScan?.toISOString() ?? 'Never'}`)
              if (project.repositoryUrl) {
                console.log(`    Repository: ${project.repositoryUrl}`)
              }
            }
          }
        }
      } catch (error) {
        console.error('Failed to list projects:', error)
        process.exit(1)
      }
    })

  // Config command
  program
    .command('config')
    .description('Show or update configuration')
    .option('--show', 'Show current configuration')
    .option('--init', 'Initialize configuration file')
    .action(async (options) => {
      if (options.init) {
        console.log('Creating default configuration...')
        console.log(`
# OSS Payment Configuration
# Save as ~/.oss-payments/config.json or set OSS_PAYMENT_CONFIG env var

{
  "database": {
    "connectionString": "postgresql://user:pass@localhost:5432/oss_payments",
    "maxConnections": 10
  },
  "github": {
    "token": "ghp_your_token_here",
    "rateLimit": 5
  },
  "registries": {
    "npm": {},
    "crates": {}
  },
  "distribution": {
    "minPaymentThreshold": "0.50",
    "maxPaymentsPerBatch": 100
  }
}
`)
      } else {
        try {
          const config = loadConfig()
          console.log('Current configuration:')
          console.log(JSON.stringify({
            ...config,
            github: { ...config.github, token: '***' },
          }, null, 2))
        } catch (error) {
          console.error('No configuration found. Run "oss-payments config --init" to create one.')
        }
      }
    })

  return program
}

/**
 * Load configuration from environment or file
 */
function loadConfig(): OSSPaymentConfig {
  // Try environment variables first
  const githubToken = process.env['GITHUB_TOKEN'] ?? process.env['GH_TOKEN']
  if (!githubToken) {
    console.warn('Warning: GITHUB_TOKEN not set. GitHub API calls will be limited.')
  }

  return {
    database: {
      connectionString: process.env['DATABASE_URL'] ?? 'postgresql://localhost:5432/oss_payments',
      maxConnections: 10,
    },
    github: {
      token: githubToken ?? '',
      rateLimit: 5,
    },
    registries: {
      npm: {
        token: process.env['NPM_TOKEN'],
      },
      crates: {},
    },
    payments: {},
    distribution: {
      minPaymentThreshold: toDecimal('0.50'),
      maxPaymentsPerBatch: 100,
      weights: {
        directDependencyMultiplier: 1.5,
        transitiveDepthDivisor: 1.0,
        usageFrequencyLogBase: 10,
        criticalityMultipliers: {
          security: 2.0,
          core: 1.5,
          normal: 1.0,
        },
        maintenanceActivityMultipliers: {
          active: 1.5,
          recent: 1.0,
          stale: 0.7,
          abandoned: 0.5,
        },
      },
      excludedLicenses: [],
      excludedPackages: [],
    },
  }
}

export { loadConfig }
