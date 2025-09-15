#!/usr/bin/env python3
"""
Comprehensive test fix script for all Rust compilation errors
"""

import os
import re
import subprocess

def fix_file(filepath, fixes):
    """Apply a list of fixes to a file"""
    try:
        with open(filepath, 'r') as f:
            content = f.read()
        
        original = content
        for pattern, replacement in fixes:
            content = re.sub(pattern, replacement, content, flags=re.MULTILINE | re.DOTALL)
        
        if content != original:
            with open(filepath, 'w') as f:
                f.write(content)
            print(f"  Fixed: {filepath}")
            return True
    except Exception as e:
        print(f"  Error fixing {filepath}: {e}")
    return False

def main():
    os.chdir('/Users/z/work/hanzo/dev/src/rs')
    
    # Fix patterns
    fixes = []
    
    # 1. Fix SandboxPolicy missing allow_git_writes
    print("1. Fixing SandboxPolicy missing allow_git_writes...")
    sandbox_fix = [
        (r'(SandboxPolicy::WorkspaceWrite\s*\{[^}]*exclude_slash_tmp:\s*(?:true|false))\s*(\})',
         r'\1,\n                allow_git_writes: false\2'),
    ]
    
    files = subprocess.check_output(['grep', '-r', '-l', 'SandboxPolicy::WorkspaceWrite', '.'], 
                                   stderr=subprocess.DEVNULL).decode().strip().split('\n')
    for f in files:
        if f and f.endswith('.rs'):
            fix_file(f, sandbox_fix)
    
    # 2. Fix Event missing event_seq and order
    print("2. Fixing Event missing event_seq and order...")
    event_fix = [
        (r'(Event\s*\{\s*id:\s*[^,]+,)(\s*msg:)',
         r'\1\n            event_seq: 0,\n            order: 0,\2'),
    ]
    
    files = subprocess.check_output(['grep', '-r', '-l', 'Event {', '.'], 
                                   stderr=subprocess.DEVNULL).decode().strip().split('\n')
    for f in files:
        if f and f.endswith('.rs'):
            fix_file(f, event_fix)
    
    # 3. Fix Config missing fields
    print("3. Fixing Config missing fields...")
    
    # Find files with Config initialization
    config_files = [
        'core/src/config.rs',
        'core/tests/suite/config.rs',
        'tui/src/main.rs',
    ]
    
    config_fix = [
        # Add missing fields to Config struct initialization
        (r'(Config\s*\{[^}]*)(agents:\s*[^,]+,)?([^}]*)\}',
         lambda m: m.group(1) + (m.group(2) if m.group(2) else 'agents: vec![],\n            ') + 
                   'include_view_image_tool: false,\n            ' + m.group(3) + '}' 
         if 'include_view_image_tool' not in m.group(0) else m.group(0)),
    ]
    
    for f in config_files:
        if os.path.exists(f):
            fix_file(f, config_fix)
    
    # 4. Fix imports
    print("4. Fixing imports...")
    import_fixes = [
        (r'use dev_core::ResponseEvent;', 'use dev_protocol::models::ResponseEvent;'),
        (r'use dev_core::ResponseItem;', 'use dev_protocol::models::ResponseItem;'),
        (r'use dev_core::Prompt;', 'use dev_protocol::models::Prompt;'),
        (r'use dev_core::ModelClient;', 'use dev_protocol::models::ModelClient;'),
        (r'use dev_core::models::LocalShellAction;', 'use dev_protocol::models::LocalShellAction;'),
        (r'use dev_core::models::LocalShellExecAction;', 'use dev_protocol::models::LocalShellExecAction;'),
        (r'use dev_core::models::LocalShellStatus;', 'use dev_protocol::models::LocalShellStatus;'),
        (r'use dev_core::models::ReasoningItemContent;', 'use dev_protocol::models::ReasoningItemContent;'),
    ]
    
    # Apply import fixes to all .rs files
    for root, dirs, files in os.walk('.'):
        # Skip target directory
        if 'target' in root:
            continue
        for file in files:
            if file.endswith('.rs'):
                filepath = os.path.join(root, file)
                fix_file(filepath, import_fixes)
    
    # 5. Fix FileChange::Delete has no field content
    print("5. Fixing FileChange::Delete...")
    filechange_files = [
        'core/src/turn_diff_tracker.rs',
    ]
    
    for f in filechange_files:
        if os.path.exists(f):
            # Read file and fix Delete variant
            with open(f, 'r') as file:
                lines = file.readlines()
            
            fixed = False
            for i, line in enumerate(lines):
                if 'FileChange::Delete' in line and 'content:' in line:
                    # Remove content field from Delete variant
                    lines[i] = re.sub(r'content:\s*[^,\}]+,?\s*', '', line)
                    fixed = True
            
            if fixed:
                with open(f, 'w') as file:
                    file.writelines(lines)
                print(f"  Fixed: {f}")
    
    print("\nRunning cargo check to verify fixes...")
    result = subprocess.run(['cargo', 'check', '--all-targets'], 
                          capture_output=True, text=True)
    
    # Count remaining errors
    errors = len(re.findall(r'^error\[E\d+\]:', result.stderr, re.MULTILINE))
    print(f"\nRemaining compilation errors: {errors}")
    
    if errors > 0:
        print("\nTop remaining errors:")
        error_lines = [line for line in result.stderr.split('\n') if line.startswith('error[E')]
        for line in error_lines[:10]:
            print(f"  {line}")

if __name__ == '__main__':
    main()