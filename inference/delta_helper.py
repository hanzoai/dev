#!/usr/bin/env python3
"""
Helper for generating GSPO training deltas in the format expected by hanzod-gym.
Produces delta.bin and delta.meta.json for LoRA/BitDelta updates.
"""

import json
import struct
import hashlib
import numpy as np
from pathlib import Path
from typing import Dict, List, Optional, Tuple
import torch
import safetensors.torch


class DeltaHelper:
    """Generate and manage GSPO training deltas."""

    def __init__(self, job_dir: str = "/work"):
        self.job_dir = Path(job_dir)
        self.job_dir.mkdir(parents=True, exist_ok=True)

    def create_lora_delta(
        self,
        lora_weights: Dict[str, torch.Tensor],
        step: int,
        samples_seen: int,
        loss: float,
        norm: Optional[float] = None
    ) -> Tuple[Path, Path]:
        """
        Create delta.bin and delta.meta.json from LoRA weights.

        Args:
            lora_weights: Dictionary of LoRA weight tensors
            step: Training step number
            samples_seen: Number of samples processed
            loss: Current training loss
            norm: Optional L2 norm of delta

        Returns:
            Tuple of (delta_bin_path, meta_json_path)
        """
        # Calculate norm if not provided
        if norm is None:
            norm = self._calculate_norm(lora_weights)

        # Serialize weights to binary
        delta_bin_path = self.job_dir / "delta.bin"
        self._write_delta_binary(lora_weights, delta_bin_path)

        # Create metadata
        meta = {
            "step": step,
            "samples_seen": samples_seen,
            "loss": float(loss),
            "norm": float(norm),
            "num_params": sum(w.numel() for w in lora_weights.values()),
            "layer_shapes": {k: list(v.shape) for k, v in lora_weights.items()},
            "checksum": self._compute_checksum(delta_bin_path)
        }

        meta_json_path = self.job_dir / "delta.meta.json"
        with open(meta_json_path, 'w') as f:
            json.dump(meta, f, indent=2)

        return delta_bin_path, meta_json_path

    def create_bitdelta(
        self,
        base_weights: Dict[str, torch.Tensor],
        fine_weights: Dict[str, torch.Tensor],
        compression_ratio: float = 0.1,
        step: int = 0,
        samples_seen: int = 0,
        loss: float = 0.0
    ) -> Tuple[Path, Path]:
        """
        Create BitDelta compressed difference between base and fine-tuned weights.

        Args:
            base_weights: Original model weights
            fine_weights: Fine-tuned model weights
            compression_ratio: Fraction of weights to keep (top-k magnitude)
            step: Training step
            samples_seen: Number of samples
            loss: Training loss

        Returns:
            Tuple of (delta_bin_path, meta_json_path)
        """
        deltas = {}
        indices = {}

        for key in base_weights.keys():
            if key not in fine_weights:
                continue

            # Compute difference
            delta = fine_weights[key] - base_weights[key]

            # Select top-k by magnitude
            flat_delta = delta.flatten()
            k = max(1, int(len(flat_delta) * compression_ratio))

            topk_values, topk_indices = torch.topk(
                flat_delta.abs(), k, largest=True
            )

            # Store sparse representation
            deltas[key] = flat_delta[topk_indices]
            indices[key] = topk_indices

        # Write binary format
        delta_bin_path = self.job_dir / "delta.bin"
        self._write_bitdelta_binary(deltas, indices, delta_bin_path)

        # Create metadata
        meta = {
            "type": "bitdelta",
            "step": step,
            "samples_seen": samples_seen,
            "loss": float(loss),
            "compression_ratio": compression_ratio,
            "num_params": sum(d.numel() for d in deltas.values()),
            "layer_info": {
                k: {
                    "shape": list(base_weights[k].shape),
                    "sparse_count": len(indices[k])
                }
                for k in deltas.keys()
            },
            "checksum": self._compute_checksum(delta_bin_path)
        }

        meta_json_path = self.job_dir / "delta.meta.json"
        with open(meta_json_path, 'w') as f:
            json.dump(meta, f, indent=2)

        return delta_bin_path, meta_json_path

    def _write_delta_binary(self, weights: Dict[str, torch.Tensor], path: Path):
        """Write weights to binary format."""
        with open(path, 'wb') as f:
            # Header: version + num_layers
            f.write(struct.pack('II', 1, len(weights)))

            for key, tensor in weights.items():
                # Write key
                key_bytes = key.encode('utf-8')
                f.write(struct.pack('I', len(key_bytes)))
                f.write(key_bytes)

                # Write tensor info
                shape = tensor.shape
                f.write(struct.pack('I', len(shape)))
                for dim in shape:
                    f.write(struct.pack('I', dim))

                # Write tensor data (as float32)
                data = tensor.cpu().float().numpy()
                f.write(data.tobytes())

    def _write_bitdelta_binary(
        self,
        deltas: Dict[str, torch.Tensor],
        indices: Dict[str, torch.Tensor],
        path: Path
    ):
        """Write sparse BitDelta to binary format."""
        with open(path, 'wb') as f:
            # Header: version + format_type + num_layers
            f.write(struct.pack('III', 1, 2, len(deltas)))  # 2 = bitdelta format

            for key in deltas.keys():
                # Write key
                key_bytes = key.encode('utf-8')
                f.write(struct.pack('I', len(key_bytes)))
                f.write(key_bytes)

                # Write sparse data
                values = deltas[key].cpu().float().numpy()
                idx = indices[key].cpu().long().numpy()

                f.write(struct.pack('I', len(values)))
                f.write(values.tobytes())
                f.write(idx.tobytes())

    def _calculate_norm(self, weights: Dict[str, torch.Tensor]) -> float:
        """Calculate L2 norm of weight dictionary."""
        total = 0.0
        for tensor in weights.values():
            total += torch.sum(tensor ** 2).item()
        return np.sqrt(total)

    def _compute_checksum(self, path: Path) -> str:
        """Compute SHA256 checksum of file."""
        sha256 = hashlib.sha256()
        with open(path, 'rb') as f:
            for chunk in iter(lambda: f.read(4096), b''):
                sha256.update(chunk)
        return sha256.hexdigest()

    def load_delta(self, delta_path: Path, meta_path: Path) -> Tuple[Dict, Dict]:
        """Load delta from disk."""
        with open(meta_path, 'r') as f:
            meta = json.load(f)

        weights = {}
        with open(delta_path, 'rb') as f:
            version, num_layers = struct.unpack('II', f.read(8))

            for _ in range(num_layers):
                # Read key
                key_len = struct.unpack('I', f.read(4))[0]
                key = f.read(key_len).decode('utf-8')

                # Read shape
                shape_len = struct.unpack('I', f.read(4))[0]
                shape = []
                for _ in range(shape_len):
                    shape.append(struct.unpack('I', f.read(4))[0])

                # Read data
                num_elements = np.prod(shape)
                data = np.frombuffer(f.read(num_elements * 4), dtype=np.float32)
                weights[key] = torch.from_numpy(data).reshape(shape)

        return weights, meta

    def merge_deltas(self, delta_list: List[Dict[str, torch.Tensor]]) -> Dict[str, torch.Tensor]:
        """Average multiple deltas (for aggregation)."""
        if not delta_list:
            return {}

        merged = {}
        for key in delta_list[0].keys():
            stacked = torch.stack([d[key] for d in delta_list])
            merged[key] = torch.mean(stacked, dim=0)

        return merged


# Example usage for GSPO trainer integration
if __name__ == "__main__":
    helper = DeltaHelper(job_dir="/work/job-001")

    # Example: Create LoRA delta after training step
    lora_weights = {
        "q_proj.lora_A": torch.randn(768, 32),
        "q_proj.lora_B": torch.randn(32, 768),
        "v_proj.lora_A": torch.randn(768, 32),
        "v_proj.lora_B": torch.randn(32, 768),
    }

    delta_path, meta_path = helper.create_lora_delta(
        lora_weights=lora_weights,
        step=100,
        samples_seen=1600,
        loss=2.34
    )

    print(f"Delta saved to: {delta_path}")
    print(f"Meta saved to: {meta_path}")

    # Load and verify
    loaded_weights, loaded_meta = helper.load_delta(delta_path, meta_path)
    print(f"Loaded {len(loaded_weights)} weight tensors")
    print(f"Meta: {json.dumps(loaded_meta, indent=2)}")