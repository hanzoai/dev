---
name: ui
description: >
  Design, build, or port user interfaces on the Hanzo stack (@hanzo/ui, @hanzo/gui,
  @hanzo/design tokens) while achieving measured visual parity across mobile, duo,
  laptop, and desktop viewports. Use when creating or refactoring UI screens,
  porting a reference implementation, or fixing layout and responsive regressions.
license: MIT
---

# UI (@hanzo/ui + Visual Parity by Measurement)

Treat the reference UI or specification as an executable visual contract, not inspiration. Preserve working business logic, data model, semantics, and accessibility while making every visible state converge across mobile, duo, laptop, and desktop viewports.

## 1. Substrate Rules (@hanzo/ui + @hanzo/gui + @hanzo/design)

- Import layout and controls from `@hanzo/ui` and `@hanzo/gui` (`YStack`, `XStack`, `Box`, `View`, `Text`, `Button`, `Input`, `Textarea`, `Card`, `Separator`).
- Use semantic design tokens (`$ink`, `$soft`, `$faint`, `$edge`, `$sunken`, `$hover`, `$1`–`$8`) so light and dark themes resolve automatically without hardcoded hex overrides.
- Typography uses the canonical `Zen` and `Zen Mono` variable font families (`$sans`, `$mono`, `$heading`).

## 2. Convergence Order

Never begin by polishing individual components. Converge in dependency order:

1. capture conditions
2. shell geometry
3. typography metrics
4. spacing and control primitives
5. screen topology / information architecture
6. compound components
7. state families and data-dependent regions (`loading · empty · failure · populated-minimal · populated-realistic`)
8. responsive layouts (`mobile · duo · laptop · desktop`)
9. pixel-level cleanup

A lower-level mismatch invalidates measurements below it. If the shell shifts every child, fix the shell and remeasure. If typography changes wrapping, fix type before vertical spacing.

## 3. Hard Verification Rules

- **Never validate only the state that happened to render**: A data-driven screen is scored once per state (`loading`, `empty`, `failure`, `populated`), each entered deliberately by a deterministic fixture or live API seed.
- **Never equate DOM presence with visual presence**: Verify rendered text rects and computed visibility (`hasRenderedTextRect && intersectsScope && intersectsAllClippingAncestors && !hiddenByAncestor`).
- **Never let a screen converge by hiding**: Material the reference shows must never be suppressed behind `opacity: 0`, offscreen transforms, zero-height clips, or closed disclosures to pass a census.
