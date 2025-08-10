# Theme Testing Guide

## Testing the /theme Command

The theme switching functionality has been successfully implemented. Here's how to test it:

### 1. Build and Install
```bash
npm run build:quick
```

### 2. Run Codex TUI
```bash
codex
```

### 3. Test Theme Switching
- Type `/theme` in the compose field
- Press Enter to open the theme selection UI
- Use arrow keys (↑↓) to navigate between themes
- Press Enter to select a theme
- Press Esc to cancel

### Available Themes

1. **Carbon Night** - Sleek modern dark theme (default)
2. **Photon Light** - Clean professional light theme
3. **Shinobi Dusk** - Japanese-inspired twilight
4. **OLED Black Pro** - True black for OLED displays
5. **Amber Terminal** - Retro amber CRT aesthetic
6. **Aurora Flux** - Northern lights inspired
7. **Charcoal Rainbow** - High-contrast accessible
8. **Zen Garden** - Calm and peaceful
9. **Paper Light Pro** - Premium paper-like light

### What to Verify

1. **Theme Selection UI**:
   - The popup appears when typing `/theme`
   - All 9 themes are listed with descriptions
   - Current theme is marked with "(current)"
   - Arrow keys navigate the list
   - Selected item is highlighted with ">" and cyan color

2. **Theme Application**:
   - When a theme is selected, colors change immediately
   - A confirmation message appears: "✓ Theme changed to [Theme Name]"
   - The new theme persists across different views

3. **Configuration**:
   - Theme changes are logged (check terminal output)
   - To persist themes, add to config.toml:
   ```toml
   [tui.theme]
   name = "carbon-night"  # or any other theme name
   ```

### Features Implemented

- ✅ Complete theme system with 9 predefined themes
- ✅ Dynamic theme switching during active sessions
- ✅ `/theme` slash command
- ✅ Interactive theme selection UI
- ✅ Immediate visual feedback
- ✅ Theme configuration in config.toml
- ✅ Custom color overrides support

### Technical Implementation

- **Theme Module** (`tui/src/theme.rs`): Core theme definitions and switching logic
- **Colors Module** (`tui/src/colors.rs`): Dynamic color functions using current theme
- **Theme Selection View** (`tui/src/bottom_pane/theme_selection_view.rs`): Interactive UI
- **Config Types** (`core/src/config_types.rs`): Theme configuration structures
- **Slash Command** (`tui/src/slash_command.rs`): `/theme` command integration