#!/bin/bash

echo "🐝 Hanzo Dev Swarm Demo"
echo "======================"
echo ""
echo "This demo will add copyright headers to 5 test files in parallel"
echo ""
echo "Files before:"
echo "-------------"
head -n 1 test-swarm/*.{js,ts,py,md,json} 2>/dev/null

echo ""
echo "Running swarm with 5 agents..."
echo ""

# Run the swarm command
node dist/cli/dev.js --claude --swarm 5 -p "Add this copyright header at the very top of each file: '// Copyright 2025 Hanzo Industries Inc.' (use # for Python, // for JS/TS/JSON)"

echo ""
echo "Files after:"
echo "------------"
head -n 2 test-swarm/*.{js,ts,py,md,json} 2>/dev/null