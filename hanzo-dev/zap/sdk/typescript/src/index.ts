/**
 * @hanzo/zap — Hanzo-branded ZAP SDK
 *
 * Thin wrapper over @zap-proto/zap (the canonical native ZAP wire runtime).
 * Re-exports the canonical surface verbatim — builder, client (ZapClient),
 * envelope, pipeline, view, and wire — so `@hanzo/zap` stays byte-compatible
 * with github.com/zap-proto/go and the luxfi/zap transport.
 *
 * @packageDocumentation
 */

export * from "@zap-proto/zap";
