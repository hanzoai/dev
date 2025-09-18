#!/usr/bin/env python3
"""Helper script that performs sustained heavy math for roughly one minute."""

from __future__ import annotations

import os
import time


def _positive_int(env_value: str, default: int) -> int:
    """Parse a positive integer from the environment or fall back to default."""
    try:
        parsed = int(env_value)
    except ValueError:
        return default
    return parsed if parsed > 0 else default


def _heavy_transform(seed: int, scale: int) -> int:
    """Iteratively mix the seed with modular multiplications to grow large numbers."""
    modulus = (1 << 1024) - 109  # large odd modulus guarantees big intermediates
    value = seed or 1
    base = 65537 + (scale * 17)
    for counter in range(1, scale + 1):
        value = (value * (base + counter)) % modulus
        value ^= (value >> 7)
    return value


def main() -> None:
    target_seconds = _positive_int(os.getenv("LONG_RUN_TOTAL_SECONDS", "60"), 60)
    interval_seconds = _positive_int(os.getenv("LONG_RUN_INTERVAL_SECONDS", "5"), 5)
    work_scale = _positive_int(os.getenv("LONG_RUN_WORK_SCALE", "250000"), 250000)

    steps = max(1, target_seconds // interval_seconds)
    if steps * interval_seconds < target_seconds:
        steps += 1

    start = time.perf_counter()
    accumulator = 1

    for step in range(1, steps + 1):
        chunk_scale = work_scale + (step * 257)
        chunk_start = time.perf_counter()
        accumulator = _heavy_transform(accumulator, chunk_scale)
        compute_elapsed = time.perf_counter() - chunk_start
        total_elapsed = time.perf_counter() - start

        checksum = accumulator & ((1 << 64) - 1)
        print(
            f"[step {step}/{steps}] t={total_elapsed:6.2f}s "
            f"compute={compute_elapsed:5.2f}s checksum=0x{checksum:016x}",
            flush=True,
        )

        target_elapsed = step * interval_seconds
        now = time.perf_counter()
        remaining = target_elapsed - (now - start)
        if step < steps and remaining > 0:
            time.sleep(remaining)

    final_digest = pow(accumulator, 5, (1 << 2048) - 159)
    total_duration = time.perf_counter() - start
    digest_bits = final_digest.bit_length()

    print(
        f"Completed in {total_duration:6.2f}s with final digest bit length {digest_bits}.",
        flush=True,
    )


if __name__ == "__main__":
    main()
