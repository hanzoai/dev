#!/usr/bin/env python3
"""Test script to verify model loading works"""

import os
import sys
import torch
from transformers import AutoTokenizer, AutoModelForCausalLM

MODEL_NAME = os.getenv("MODEL_NAME", "Qwen/Qwen2.5-1.5B-Instruct")
CACHE_DIR = os.getenv("TRANSFORMERS_CACHE", "/root/.cache/huggingface")

print(f"Testing model: {MODEL_NAME}")
print(f"Cache dir: {CACHE_DIR}")
print(f"Python: {sys.version}")
print(f"PyTorch: {torch.__version__}")
print(f"CUDA available: {torch.cuda.is_available()}")
print(f"MPS available: {torch.backends.mps.is_available() if hasattr(torch.backends, 'mps') else False}")

try:
    print("\n1. Loading tokenizer...")
    tokenizer = AutoTokenizer.from_pretrained(
        MODEL_NAME,
        trust_remote_code=True,
        cache_dir=CACHE_DIR
    )
    print(f"✓ Tokenizer loaded: {tokenizer.__class__.__name__}")

    print("\n2. Loading model (this may take a while)...")
    model = AutoModelForCausalLM.from_pretrained(
        MODEL_NAME,
        torch_dtype=torch.float32,
        trust_remote_code=True,
        cache_dir=CACHE_DIR
    )
    print(f"✓ Model loaded: {model.__class__.__name__}")
    print(f"   Model size: {sum(p.numel() for p in model.parameters()) / 1e9:.2f}B parameters")

    print("\n3. Testing inference...")
    prompt = "What is 2+2?"
    inputs = tokenizer(prompt, return_tensors="pt")

    with torch.no_grad():
        outputs = model.generate(
            **inputs,
            max_new_tokens=10,
            temperature=0.7,
            do_sample=True
        )

    response = tokenizer.decode(outputs[0][inputs.input_ids.shape[1]:], skip_special_tokens=True)
    print(f"Prompt: {prompt}")
    print(f"Response: {response}")

    print("\n✓ All tests passed!")

except Exception as e:
    print(f"\n✗ Error: {e}")
    import traceback
    traceback.print_exc()
    sys.exit(1)