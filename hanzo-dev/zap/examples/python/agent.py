#!/usr/bin/env python3
"""ZAP Agent Example - Python

Demonstrates a complete ZAP agent with tool execution in Python.

Usage:
    pip install hanzo-tools
    python agent.py
"""

import asyncio
import json
from pathlib import Path

# Import from hanzo-tools (Python SDK)
try:
    from hanzo_tools.core import BaseTool, PermissionManager
    from hanzo_tools.fs import FsTool
    from hanzo_tools.vcs import VcsTool
    from hanzo_tools.shell import ShellTool
except ImportError:
    print("Install hanzo-tools: pip install hanzo-tools")
    print("Or: pip install hanzo-tools-fs hanzo-tools-vcs hanzo-tools-shell")
    exit(1)


class ZapAgent:
    """ZAP-compatible agent using hanzo-tools."""

    def __init__(self, cwd: str = "."):
        self.cwd = Path(cwd).resolve()
        self.permission_manager = PermissionManager(
            allowed_paths=[self.cwd],
            deny_patterns=[".git/objects", ".env", "*.pem"]
        )

        # Initialize tools
        self.fs_tool = FsTool(permission_manager=self.permission_manager)
        self.vcs_tool = VcsTool(cwd=str(self.cwd))
        self.shell_tool = ShellTool(cwd=str(self.cwd))

        self.tools = {
            "fs": self.fs_tool,
            "vcs": self.vcs_tool,
            "shell": self.shell_tool,
        }

    async def execute(self, tool_name: str, action: str, **params) -> dict:
        """Execute a tool action with parameters.

        Args:
            tool_name: Name of the tool (fs, vcs, shell)
            action: Action to perform
            **params: Tool-specific parameters

        Returns:
            Tool execution result
        """
        tool = self.tools.get(tool_name)
        if not tool:
            raise ValueError(f"Unknown tool: {tool_name}")

        # Build the unified call parameters
        call_params = {"action": action, **params}

        # Execute via the tool's call method
        result = await tool.call(ctx=None, **call_params)
        return {"content": result, "error": None}

    def list_tools(self) -> list[str]:
        """List available tools and their actions."""
        return [
            "fs.read", "fs.write", "fs.list", "fs.stat", "fs.apply_patch",
            "vcs.status", "vcs.diff", "vcs.commit", "vcs.log", "vcs.branch",
            "shell.run",
        ]


async def main():
    print("ZAP Python Agent Example")
    print("========================")
    print()

    agent = ZapAgent(cwd=".")

    # List available tools
    tools = agent.list_tools()
    print(f"Available tools ({len(tools)}):")
    for tool in tools:
        print(f"  - {tool}")
    print()

    # Example 1: Read a file
    print("Example 1: Read file")
    try:
        result = await agent.execute("fs", "read", path="Cargo.toml")
        content = result["content"]
        print(f"  Result: {len(content) if content else 0} chars read")
    except Exception as e:
        print(f"  Error: {e}")
    print()

    # Example 2: List directory
    print("Example 2: List directory")
    try:
        result = await agent.execute("fs", "list", path=".")
        print(f"  Result: {result['content'][:200]}...")
    except Exception as e:
        print(f"  Error: {e}")
    print()

    # Example 3: Git status
    print("Example 3: Git status")
    try:
        result = await agent.execute("vcs", "status")
        print(f"  Result: {result['content'][:200]}...")
    except Exception as e:
        print(f"  Error: {e}")
    print()

    # Example 4: Permission check
    print("Example 4: Permission check")
    is_allowed = agent.permission_manager.is_path_allowed("/etc/passwd")
    print(f"  /etc/passwd allowed: {is_allowed}")
    is_allowed = agent.permission_manager.is_path_allowed("./src/main.rs")
    print(f"  ./src/main.rs allowed: {is_allowed}")


if __name__ == "__main__":
    asyncio.run(main())
