#!/usr/bin/env python3
"""
Minimal inference server for Qwen3 model.
Provides OpenAI-compatible API on port 8080.
"""

import os
import json
import time
from typing import Optional, List, Dict, Any
from contextlib import asynccontextmanager

from fastapi import FastAPI, HTTPException
from fastapi.responses import StreamingResponse
from pydantic import BaseModel
import torch
from transformers import AutoTokenizer, AutoModelForCausalLM, TextStreamer
import uvicorn


# Configuration from environment
MODEL_NAME = os.getenv("MODEL_NAME", "Qwen/Qwen2.5-1.5B-Instruct")
HF_TOKEN = os.getenv("HF_TOKEN", None)
DEVICE = os.getenv("DEVICE", "cpu")
MAX_MEMORY = os.getenv("MAX_MEMORY", "4GB")
PORT = int(os.getenv("PORT", "8080"))
CACHE_DIR = os.getenv("TRANSFORMERS_CACHE", "/root/.cache/huggingface")


# Global model and tokenizer
model = None
tokenizer = None


# Request/Response models
class InferenceRequest(BaseModel):
    model: str
    prompt: str
    temperature: float = 0.7
    max_tokens: int = 512
    stream: bool = False
    top_p: float = 0.95

class ChatMessage(BaseModel):
    role: str
    content: str

class ChatRequest(BaseModel):
    model: str
    messages: List[ChatMessage]
    temperature: float = 0.7
    max_tokens: int = 512
    stream: bool = False


@asynccontextmanager
async def lifespan(app: FastAPI):
    """Load model on startup, cleanup on shutdown."""
    global model, tokenizer

    print(f"Loading model {MODEL_NAME} from Hugging Face...")
    print(f"Cache directory: {CACHE_DIR}")
    print(f"Device: {DEVICE}")
    
    try:
        # Load tokenizer with HF token if provided
        tokenizer_kwargs = {
            "trust_remote_code": True,
            "use_fast": True,
            "cache_dir": CACHE_DIR
        }
        if HF_TOKEN:
            tokenizer_kwargs["token"] = HF_TOKEN
            
        print("Downloading/loading tokenizer...")
        tokenizer = AutoTokenizer.from_pretrained(
            MODEL_NAME,
            **tokenizer_kwargs
        )
        
        # Set padding token if not set
        if tokenizer.pad_token is None:
            tokenizer.pad_token = tokenizer.eos_token

        # Load model with appropriate settings
        model_kwargs = {
            "trust_remote_code": True,
            "cache_dir": CACHE_DIR
        }
        if HF_TOKEN:
            model_kwargs["token"] = HF_TOKEN
            
        print(f"Downloading/loading model (this may take a few minutes on first run)...")
        
        if DEVICE == "cuda" and torch.cuda.is_available():
            print("Using CUDA with float16")
            model = AutoModelForCausalLM.from_pretrained(
                MODEL_NAME,
                torch_dtype=torch.float16,
                device_map="auto",
                max_memory={0: MAX_MEMORY},
                low_cpu_mem_usage=True,
                **model_kwargs
            )
        elif DEVICE == "mps" and torch.backends.mps.is_available():
            print("Using Apple Metal Performance Shaders")
            model = AutoModelForCausalLM.from_pretrained(
                MODEL_NAME,
                torch_dtype=torch.float16,
                **model_kwargs
            )
            model = model.to("mps")
        else:
            print(f"Using CPU mode")
            model = AutoModelForCausalLM.from_pretrained(
                MODEL_NAME,
                torch_dtype=torch.float32,
                **model_kwargs
            )
            model = model.to(DEVICE)

        model.eval()
        print(f"Model loaded successfully on {DEVICE}")

    except Exception as e:
        print(f"Error loading model: {e}")
        raise

    yield

    # Cleanup
    if model:
        del model
    if tokenizer:
        del tokenizer
    torch.cuda.empty_cache() if torch.cuda.is_available() else None


app = FastAPI(title="Hanzod Inference", lifespan=lifespan)


@app.get("/health")
async def health():
    """Health check endpoint."""
    return {
        "status": "healthy",
        "model": MODEL_NAME,
        "device": DEVICE,
        "model_loaded": model is not None,
        "cache_dir": CACHE_DIR,
        "has_token": HF_TOKEN is not None
    }

@app.get("/v1/models")
async def list_models():
    """List available models."""
    return {
        "models": [
            {
                "id": MODEL_NAME,
                "object": "model",
                "owned_by": "huggingface",
                "loaded": model is not None
            }
        ]
    }


@app.post("/v1/inference")
async def inference(request: InferenceRequest):
    """Simple inference endpoint."""
    if not model or not tokenizer:
        raise HTTPException(status_code=503, detail="Model not loaded")

    try:
        # Tokenize input
        device = "mps" if DEVICE == "mps" else DEVICE
        inputs = tokenizer(request.prompt, return_tensors="pt")
        if device != "cpu":
            inputs = {k: v.to(device) for k, v in inputs.items()}

        # Generate
        with torch.no_grad():
            outputs = model.generate(
                **inputs,
                max_new_tokens=request.max_tokens,
                temperature=request.temperature,
                top_p=request.top_p,
                do_sample=True,
                pad_token_id=tokenizer.eos_token_id
            )

        # Decode response
        response_text = tokenizer.decode(outputs[0][inputs.input_ids.shape[1]:], skip_special_tokens=True)

        return {
            "response": response_text,
            "model": request.model,
            "usage": {
                "prompt_tokens": inputs.input_ids.shape[1],
                "completion_tokens": outputs.shape[1] - inputs.input_ids.shape[1],
                "total_tokens": outputs.shape[1]
            }
        }

    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@app.post("/v1/chat/completions")
async def chat_completions(request: ChatRequest):
    """OpenAI-compatible chat completions endpoint."""
    if not model or not tokenizer:
        raise HTTPException(status_code=503, detail="Model not loaded")

    try:
        # Format messages for Qwen model
        # Qwen uses specific chat template
        messages = [{"role": msg.role, "content": msg.content} for msg in request.messages]
        
        # Use the tokenizer's chat template if available
        if hasattr(tokenizer, 'apply_chat_template'):
            prompt = tokenizer.apply_chat_template(
                messages,
                tokenize=False,
                add_generation_prompt=True
            )
        else:
            # Fallback to simple format
            prompt = ""
            for msg in messages:
                if msg["role"] == "system":
                    prompt += f"System: {msg['content']}\n"
                elif msg["role"] == "user":
                    prompt += f"User: {msg['content']}\n"
                elif msg["role"] == "assistant":
                    prompt += f"Assistant: {msg['content']}\n"
            prompt += "Assistant: "

        # Tokenize
        device = "mps" if DEVICE == "mps" else DEVICE
        inputs = tokenizer(prompt, return_tensors="pt")
        if device != "cpu":
            inputs = {k: v.to(device) for k, v in inputs.items()}

        # Generate
        with torch.no_grad():
            outputs = model.generate(
                **inputs,
                max_new_tokens=request.max_tokens,
                temperature=request.temperature,
                do_sample=True,
                pad_token_id=tokenizer.eos_token_id
            )

        # Decode
        response_text = tokenizer.decode(outputs[0][inputs.input_ids.shape[1]:], skip_special_tokens=True)

        # Format OpenAI-style response
        return {
            "id": f"chatcmpl-{int(time.time())}",
            "object": "chat.completion",
            "created": int(time.time()),
            "model": request.model,
            "choices": [
                {
                    "index": 0,
                    "message": {
                        "role": "assistant",
                        "content": response_text
                    },
                    "finish_reason": "stop"
                }
            ],
            "usage": {
                "prompt_tokens": inputs.input_ids.shape[1],
                "completion_tokens": outputs.shape[1] - inputs.input_ids.shape[1],
                "total_tokens": outputs.shape[1]
            }
        }

    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@app.post("/v1/embeddings")
async def embeddings(text: str):
    """Generate embeddings (using last hidden states)."""
    if not model or not tokenizer:
        raise HTTPException(status_code=503, detail="Model not loaded")

    try:
        inputs = tokenizer(text, return_tensors="pt", truncation=True, max_length=512).to(DEVICE)

        with torch.no_grad():
            outputs = model(**inputs, output_hidden_states=True)
            # Use mean pooling of last hidden states
            embeddings = outputs.hidden_states[-1].mean(dim=1).squeeze().cpu().numpy().tolist()

        return {
            "embedding": embeddings,
            "model": MODEL_NAME
        }

    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


if __name__ == "__main__":
    uvicorn.run(app, host="0.0.0.0", port=PORT)