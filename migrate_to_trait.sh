#!/bin/bash
# Script to migrate from enum-based to trait-based HistoryCell

echo "Starting migration to trait-based HistoryCell..."

# Backup the old files
cp codex-rs/tui/src/history_cell.rs codex-rs/tui/src/history_cell.rs.backup
cp codex-rs/tui/src/chatwidget.rs codex-rs/tui/src/chatwidget.rs.backup

# Replace history_cell.rs with the new version
cp codex-rs/tui/src/history_cell_new.rs codex-rs/tui/src/history_cell.rs

echo "Files backed up and new history_cell.rs in place"
echo "Now manually updating chatwidget.rs..."