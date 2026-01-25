/**
 * External Integrations - Unified Export
 */

export { GitHubClient, type GitHubConfig, type GitHubUser, type SponsorshipTier } from './github.js'
export { NpmRegistryClient, type NpmRegistryConfig, type PackageAuthorInfo as NpmAuthorInfo } from './npm-registry.js'
export { CratesRegistryClient, type CratesRegistryConfig, type PackageAuthorInfo as CratesAuthorInfo } from './crates-registry.js'
export { OpenCollectiveClient, type OpenCollectiveConfig, type Collective, type ContributionResult } from './open-collective.js'
