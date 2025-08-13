#!/bin/bash

echo "Testing IIFE handling in browser JavaScript execution..."

# Test 1: IIFE with return statement
echo "Test 1: IIFE pattern"
cat test_iife.js | codex -b "navigate to https://example.com and run this JavaScript:" 2>&1 | grep -A5 "JavaScript result"

echo ""
echo "Test 2: Normal code without IIFE"
cat test_normal.js | codex -b "navigate to https://example.com and run this JavaScript:" 2>&1 | grep -A5 "JavaScript result"

echo ""
echo "Test 3: Top-level return"
echo "return { test: 'direct-return' }" | codex -b "navigate to https://example.com and run this JavaScript:" 2>&1 | grep -A5 "JavaScript result"