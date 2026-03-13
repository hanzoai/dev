/**
 * @hanzo/zap — Hanzo-branded ZAP SDK
 *
 * Thin wrapper over @zap-proto/zap (canonical ZAP protocol).
 *
 * @packageDocumentation
 */

// Re-export everything from canonical @zap-proto/zap
export * from '@zap-proto/zap';

// ── Backward-compat aliases ──────────────────────────────────────────
import { Client, Server } from '@zap-proto/zap';
export { Client as ZapClient };
export { Server as ZapServer };
