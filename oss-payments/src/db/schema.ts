/**
 * Database Schema
 *
 * Drizzle ORM schema definitions for the OSS Payment System.
 */

import {
  pgTable,
  uuid,
  varchar,
  text,
  boolean,
  timestamp,
  integer,
  decimal,
  date,
  uniqueIndex,
  index,
  primaryKey,
} from 'drizzle-orm/pg-core'
import { relations } from 'drizzle-orm'

// ============================================================================
// Authors Table
// ============================================================================

export const ossAuthors = pgTable('oss_authors', {
  id: uuid('id').primaryKey().defaultRandom(),
  name: varchar('name', { length: 255 }).notNull(),
  githubUsername: varchar('github_username', { length: 255 }).unique(),
  email: varchar('email', { length: 255 }),
  verified: boolean('verified').default(false).notNull(),
  verificationDate: timestamp('verification_date'),
  createdAt: timestamp('created_at').defaultNow().notNull(),
  updatedAt: timestamp('updated_at').defaultNow().notNull(),
}, (table) => ({
  githubIdx: uniqueIndex('idx_authors_github').on(table.githubUsername),
  emailIdx: index('idx_authors_email').on(table.email),
}))

export const ossAuthorsRelations = relations(ossAuthors, ({ many }) => ({
  paymentAddresses: many(ossPaymentAddresses),
  packageAuthors: many(ossPackageAuthors),
  payments: many(ossPayments),
}))

// ============================================================================
// Payment Addresses Table
// ============================================================================

export const ossPaymentAddresses = pgTable('oss_payment_addresses', {
  id: uuid('id').primaryKey().defaultRandom(),
  authorId: uuid('author_id').notNull().references(() => ossAuthors.id, { onDelete: 'cascade' }),
  addressType: varchar('address_type', { length: 50 }).notNull(), // github_sponsors, open_collective, ethereum, etc.
  address: varchar('address', { length: 500 }).notNull(),
  verified: boolean('verified').default(false).notNull(),
  preferred: boolean('preferred').default(false).notNull(),
  createdAt: timestamp('created_at').defaultNow().notNull(),
}, (table) => ({
  authorTypeIdx: uniqueIndex('idx_payment_addresses_unique').on(
    table.authorId,
    table.addressType,
    table.address
  ),
}))

export const ossPaymentAddressesRelations = relations(ossPaymentAddresses, ({ one }) => ({
  author: one(ossAuthors, {
    fields: [ossPaymentAddresses.authorId],
    references: [ossAuthors.id],
  }),
}))

// ============================================================================
// Packages Table
// ============================================================================

export const ossPackages = pgTable('oss_packages', {
  id: uuid('id').primaryKey().defaultRandom(),
  name: varchar('name', { length: 255 }).notNull(),
  ecosystem: varchar('ecosystem', { length: 50 }).notNull(), // cargo, npm, pypi, go
  version: varchar('version', { length: 100 }),
  repositoryUrl: text('repository_url'),
  homepageUrl: text('homepage_url'),
  license: varchar('license', { length: 100 }),
  lastFetched: timestamp('last_fetched'),
  createdAt: timestamp('created_at').defaultNow().notNull(),
  updatedAt: timestamp('updated_at').defaultNow().notNull(),
}, (table) => ({
  uniquePackage: uniqueIndex('idx_packages_unique').on(table.name, table.ecosystem),
  ecosystemIdx: index('idx_packages_ecosystem').on(table.ecosystem),
}))

export const ossPackagesRelations = relations(ossPackages, ({ many }) => ({
  authors: many(ossPackageAuthors),
  projectDependencies: many(ossProjectDependencies),
  payments: many(ossPayments),
}))

// ============================================================================
// Package Authors Table (Many-to-Many)
// ============================================================================

export const ossPackageAuthors = pgTable('oss_package_authors', {
  packageId: uuid('package_id').notNull().references(() => ossPackages.id, { onDelete: 'cascade' }),
  authorId: uuid('author_id').notNull().references(() => ossAuthors.id, { onDelete: 'cascade' }),
  sharePercentage: decimal('share_percentage', { precision: 5, scale: 4 }).notNull(), // 0.0000 to 1.0000
  role: varchar('role', { length: 50 }).notNull(), // primary, maintainer, contributor
  createdAt: timestamp('created_at').defaultNow().notNull(),
}, (table) => ({
  pk: primaryKey({ columns: [table.packageId, table.authorId] }),
}))

export const ossPackageAuthorsRelations = relations(ossPackageAuthors, ({ one }) => ({
  package: one(ossPackages, {
    fields: [ossPackageAuthors.packageId],
    references: [ossPackages.id],
  }),
  author: one(ossAuthors, {
    fields: [ossPackageAuthors.authorId],
    references: [ossAuthors.id],
  }),
}))

// ============================================================================
// Projects Table
// ============================================================================

export const ossProjects = pgTable('oss_projects', {
  id: uuid('id').primaryKey().defaultRandom(),
  organizationId: uuid('organization_id'),
  name: varchar('name', { length: 255 }).notNull(),
  repositoryUrl: text('repository_url'),
  lastScan: timestamp('last_scan'),
  createdAt: timestamp('created_at').defaultNow().notNull(),
  updatedAt: timestamp('updated_at').defaultNow().notNull(),
}, (table) => ({
  orgIdx: index('idx_projects_org').on(table.organizationId),
}))

export const ossProjectsRelations = relations(ossProjects, ({ many }) => ({
  dependencies: many(ossProjectDependencies),
  distributions: many(ossDistributions),
}))

// ============================================================================
// Project Dependencies Table
// ============================================================================

export const ossProjectDependencies = pgTable('oss_project_dependencies', {
  projectId: uuid('project_id').notNull().references(() => ossProjects.id, { onDelete: 'cascade' }),
  packageId: uuid('package_id').notNull().references(() => ossPackages.id, { onDelete: 'cascade' }),
  version: varchar('version', { length: 100 }),
  isDirect: boolean('is_direct').notNull(),
  depth: integer('depth').notNull(),
  usageCount: integer('usage_count').default(0),
  firstAdded: timestamp('first_added').defaultNow().notNull(),
  lastSeen: timestamp('last_seen').defaultNow().notNull(),
}, (table) => ({
  pk: primaryKey({ columns: [table.projectId, table.packageId] }),
  projectIdx: index('idx_deps_project').on(table.projectId),
}))

export const ossProjectDependenciesRelations = relations(ossProjectDependencies, ({ one }) => ({
  project: one(ossProjects, {
    fields: [ossProjectDependencies.projectId],
    references: [ossProjects.id],
  }),
  package: one(ossPackages, {
    fields: [ossProjectDependencies.packageId],
    references: [ossPackages.id],
  }),
}))

// ============================================================================
// Distributions Table
// ============================================================================

export const ossDistributions = pgTable('oss_distributions', {
  id: uuid('id').primaryKey().defaultRandom(),
  projectId: uuid('project_id').notNull().references(() => ossProjects.id, { onDelete: 'cascade' }),
  totalAmount: decimal('total_amount', { precision: 12, scale: 2 }).notNull(),
  currency: varchar('currency', { length: 10 }).default('USD').notNull(),
  periodStart: date('period_start').notNull(),
  periodEnd: date('period_end').notNull(),
  status: varchar('status', { length: 50 }).default('pending').notNull(), // pending, processing, completed, failed
  createdAt: timestamp('created_at').defaultNow().notNull(),
  processedAt: timestamp('processed_at'),
}, (table) => ({
  projectIdx: index('idx_distributions_project').on(table.projectId),
  statusIdx: index('idx_distributions_status').on(table.status),
}))

export const ossDistributionsRelations = relations(ossDistributions, ({ one, many }) => ({
  project: one(ossProjects, {
    fields: [ossDistributions.projectId],
    references: [ossProjects.id],
  }),
  payments: many(ossPayments),
}))

// ============================================================================
// Payments Table
// ============================================================================

export const ossPayments = pgTable('oss_payments', {
  id: uuid('id').primaryKey().defaultRandom(),
  distributionId: uuid('distribution_id').notNull().references(() => ossDistributions.id, { onDelete: 'cascade' }),
  authorId: uuid('author_id').notNull().references(() => ossAuthors.id),
  packageId: uuid('package_id').notNull().references(() => ossPackages.id),
  amount: decimal('amount', { precision: 12, scale: 4 }).notNull(),
  paymentType: varchar('payment_type', { length: 50 }).notNull(), // github_sponsors, open_collective, ethereum, etc.
  transactionId: varchar('transaction_id', { length: 255 }),
  status: varchar('status', { length: 50 }).default('pending').notNull(), // pending, processing, completed, failed, skipped
  errorMessage: text('error_message'),
  createdAt: timestamp('created_at').defaultNow().notNull(),
  processedAt: timestamp('processed_at'),
}, (table) => ({
  distributionIdx: index('idx_payments_distribution').on(table.distributionId),
  authorIdx: index('idx_payments_author').on(table.authorId),
  statusIdx: index('idx_payments_status').on(table.status),
}))

export const ossPaymentsRelations = relations(ossPayments, ({ one }) => ({
  distribution: one(ossDistributions, {
    fields: [ossPayments.distributionId],
    references: [ossDistributions.id],
  }),
  author: one(ossAuthors, {
    fields: [ossPayments.authorId],
    references: [ossAuthors.id],
  }),
  package: one(ossPackages, {
    fields: [ossPayments.packageId],
    references: [ossPackages.id],
  }),
}))

// ============================================================================
// Export all tables
// ============================================================================

export const tables = {
  ossAuthors,
  ossPaymentAddresses,
  ossPackages,
  ossPackageAuthors,
  ossProjects,
  ossProjectDependencies,
  ossDistributions,
  ossPayments,
}
