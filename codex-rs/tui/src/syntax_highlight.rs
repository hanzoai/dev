use once_cell::sync::OnceCell;
use ratatui::text::{Line, Span};

// syntect imports
use syntect::easy::HighlightLines;
use syntect::highlighting::{Style as SynStyle, Theme, ThemeSet};
use syntect::parsing::{SyntaxReference, SyntaxSet};
use syntect::util::LinesWithEndings;

// Convert a ratatui Color to RGB tuple; mirrored locally to avoid exposing internals.
fn color_to_rgb(c: ratatui::style::Color) -> (u8, u8, u8) {
    use ratatui::style::Color;
    match c {
        Color::Rgb(r, g, b) => (r, g, b),
        Color::Black => (0, 0, 0),
        Color::White => (255, 255, 255),
        Color::Gray => (192, 192, 192),
        Color::DarkGray => (128, 128, 128),
        Color::Red => (205, 49, 49),
        Color::Green => (13, 188, 121),
        Color::Yellow => (229, 229, 16),
        Color::Blue => (36, 114, 200),
        Color::Magenta => (188, 63, 188),
        Color::Cyan => (17, 168, 205),
        Color::LightRed => (255, 102, 102),
        Color::LightGreen => (102, 255, 178),
        Color::LightYellow => (255, 255, 102),
        Color::LightBlue => (102, 153, 255),
        Color::LightMagenta => (255, 102, 255),
        Color::LightCyan => (102, 255, 255),
        Color::Indexed(i) => (i, i, i),
        Color::Reset => (255, 255, 255),
    }
}

fn relative_luminance(rgb: (u8, u8, u8)) -> f32 {
    (0.2126 * rgb.0 as f32 + 0.7152 * rgb.1 as f32 + 0.0722 * rgb.2 as f32) / 255.0
}

fn is_light_bg() -> bool {
    let bg = crate::colors::background();
    let lum = relative_luminance(color_to_rgb(bg));
    lum >= 0.6
}

static PS: OnceCell<SyntaxSet> = OnceCell::new();
static THEMES: OnceCell<ThemeSet> = OnceCell::new();

fn syntax_set() -> &'static SyntaxSet {
    PS.get_or_init(|| SyntaxSet::load_defaults_newlines())
}

fn themes() -> &'static ThemeSet {
    THEMES.get_or_init(ThemeSet::load_defaults)
}

fn current_theme_name<'a>(_ts: &'a ThemeSet) -> &'a str {
    // Restrict to Solarized only; pick based on current UI background brightness.
    if is_light_bg() { "Solarized (light)" } else { "Solarized (dark)" }
}

fn blending_enabled() -> bool { false }

fn default_theme<'a>() -> &'a Theme {
    // Use the currently selected theme (rotatable via Ctrl+Y)
    let ts = themes();
    let name = current_theme_name(ts);
    // Prefer the Solarized themes; fall back to first available if missing.
    ts.themes
        .get(name)
        .or_else(|| ts.themes.get(if is_light_bg() { "Solarized (light)" } else { "Solarized (dark)" }))
        .unwrap_or_else(|| ts.themes.values().next().expect("at least one syntect theme"))
}

fn try_syntax_for_lang<'a>(ps: &'a SyntaxSet, lang: &str) -> Option<&'a SyntaxReference> {
    // Try token, then extension, then name; return None if not found.
    let lang = normalize_lang(lang);
    ps.find_syntax_by_token(lang)
        .or_else(|| ps.find_syntax_by_extension(lang))
        .or_else(|| ps.find_syntax_by_name(lang))
}

fn syntax_for_lang<'a>(ps: &'a SyntaxSet, lang: &str) -> &'a SyntaxReference {
    try_syntax_for_lang(ps, lang).unwrap_or_else(|| ps.find_syntax_plain_text())
}

fn span_from_syn((SynStyle { foreground, font_style, .. }, text): (SynStyle, &str)) -> Span<'static> {
    use ratatui::style::{Color, Modifier, Style};
    // Map syntect Style to ratatui Style
    let fg = adjust_color(Color::Rgb(foreground.r, foreground.g, foreground.b));
    let mut style = Style::default().fg(fg);
    if font_style.contains(syntect::highlighting::FontStyle::BOLD) {
        style = style.add_modifier(Modifier::BOLD);
    }
    if font_style.contains(syntect::highlighting::FontStyle::ITALIC) {
        style = style.add_modifier(Modifier::ITALIC);
    }
    if font_style.contains(syntect::highlighting::FontStyle::UNDERLINE) {
        style = style.add_modifier(Modifier::UNDERLINED);
    }
    // Strip a single trailing newline from syntect's line so our Line has no '\n'
    let content = if let Some(stripped) = text.strip_suffix('\n') { stripped } else { text };
    Span::styled(content.to_string(), style)
}

/// Highlight a code block into ratatui Lines while preserving exact text.
pub(crate) fn highlight_code_block(content: &str, lang: Option<&str>) -> Vec<Line<'static>> {
    let ps = syntax_set();
    let theme = default_theme();
    // Resolve language
    let syntax = {
        // Prefer the provided language when it maps to a known syntax.
        if let Some(l) = lang.and_then(|l| if l.trim().is_empty() { None } else { Some(l) }) {
            if let Some(s) = try_syntax_for_lang(ps, l) {
                s
            } else {
                // Unknown language label: try a best-effort auto-detect before
                // falling back to plain text.
                autodetect_lang(content)
                    .map(|dl| syntax_for_lang(ps, dl))
                    .unwrap_or_else(|| ps.find_syntax_plain_text())
            }
        } else {
            // No label provided: attempt auto-detection, otherwise plain text.
            autodetect_lang(content)
                .map(|dl| syntax_for_lang(ps, dl))
                .unwrap_or_else(|| ps.find_syntax_plain_text())
        }
    };

    let mut highlighter = HighlightLines::new(syntax, theme);
    let mut out: Vec<Line<'static>> = Vec::new();
    for line in LinesWithEndings::from(content) {
        // syntect returns (Style, &str) pairs; convert to ratatui Spans
        let ranges = highlighter.highlight_line(line, ps).unwrap_or_else(|_| vec![(SynStyle::default(), line)]);
        let spans: Vec<Span<'static>> = ranges.into_iter().map(span_from_syn).collect();
        out.push(Line::from(spans));
    }
    // When `content` ends with a trailing newline, syntect yields an extra
    // empty "line" at the end. Historically our renderer did not emit that
    // final empty line, which also avoids introducing a stray blank row
    // outside the code block background. Trim a single trailing empty Line.
    if content.ends_with('\n') {
        if let Some(last) = out.last() {
            let is_empty = last.spans.is_empty() || last.spans.iter().all(|s| s.content.is_empty());
            if is_empty { out.pop(); }
        }
    }
    out
}

// --- Color adaptation helpers ---
use ratatui::style::Color;

fn mix_rgb(a: (u8, u8, u8), b: (u8, u8, u8), t: f32) -> (u8, u8, u8) {
    let t = t.clamp(0.0, 1.0);
    let inv = 1.0 - t;
    let r = (a.0 as f32 * inv + b.0 as f32 * t).round() as u8;
    let g = (a.1 as f32 * inv + b.1 as f32 * t).round() as u8;
    let bl = (a.2 as f32 * inv + b.2 as f32 * t).round() as u8;
    (r, g, bl)
}

fn contrast_ratio(l1: f32, l2: f32) -> f32 {
    let (a, b) = if l1 > l2 { (l1, l2) } else { (l2, l1) };
    (a + 0.05) / (b + 0.05)
}

fn adjust_color(c: Color) -> Color {
    // Blend syntect foreground toward our theme text color to reduce mismatch,
    // then ensure a minimum contrast vs our background.
    if !blending_enabled() {
        return c; // raw syntect color
    }
    let text = crate::colors::text();
    let bg = crate::colors::background();
    let (cr, cg, cb) = color_to_rgb(c);
    let (tr, tg, tb) = color_to_rgb(text);
    let (br, bgc, bb) = color_to_rgb(bg);
    let base_mix = if is_light_bg() { 0.35 } else { 0.25 };
    let (mut r, mut g, mut b) = mix_rgb((cr, cg, cb), (tr, tg, tb), base_mix);

    // Enforce a modest contrast so colors don't look washed out.
    let mut l_fg = relative_luminance((r, g, b));
    let l_bg = relative_luminance((br, bgc, bb));
    let mut ratio = contrast_ratio(l_fg, l_bg);
    let target = if is_light_bg() { 3.0 } else { 2.5 };
    if ratio < target {
        // Move further toward text color until ratio is met or we cap iterations.
        let mut t = base_mix;
        for _ in 0..4 {
            t = (t + 0.20).min(0.80);
            let mixed = mix_rgb((cr, cg, cb), (tr, tg, tb), t);
            l_fg = relative_luminance(mixed);
            ratio = contrast_ratio(l_fg, l_bg);
            r = mixed.0; g = mixed.1; b = mixed.2;
            if ratio >= target { break; }
        }
    }
    Color::Rgb(r, g, b)
}

// --- Language aliasing ---
fn normalize_lang(lang: &str) -> &str {
    let l = lang.trim().trim_matches(|c: char| c == '.' || c == '#').to_ascii_lowercase();
    match l.as_str() {
        // Shells
        "sh" | "bash" | "zsh" | "shell" | "console" | "shellsession" => "bash",
        // Web
        "html" | "htm" | "xhtml" => "html",
        "xml" => "xml",
        "css" => "css",
        "scss" | "sass" => "scss",
        // JS/TS
        "js" | "javascript" => "javascript",
        "mjs" | "cjs" => "javascript",
        "jsx" => "jsx",
        "ts" | "typescript" => "typescript",
        "tsx" => "tsx",
        // Data/config
        "json" => "json",
        "yaml" | "yml" => "yaml",
        "toml" => "toml",
        "ini" | "cfg" | "conf" | "dotenv" | ".env" => "ini",
        // Rust and friends
        "rs" | "rust" => "rust",
        // Python
        "py" | "python" | "py3" => "python",
        // C-family
        "c" => "c",
        "h" => "c",
        "cpp" | "c++" | "cxx" | "cc" | "hpp" | "hh" => "cpp",
        "objc" | "objective-c" | "m" | "mm" => "objective-c",
        "cs" | "csharp" => "cs",
        // Other popular
        "go" => "go",
        "rb" | "ruby" => "ruby",
        "java" => "java",
        "scala" => "scala",
        "kt" | "kts" | "kotlin" => "kotlin",
        "swift" => "swift",
        "php" => "php",
        "dart" => "dart",
        "lua" => "lua",
        "r" => "r",
        "hs" | "haskell" => "haskell",
        // Infra / devops
        "docker" | "dockerfile" => "docker",
        "make" | "makefile" | "mk" => "make",
        "cmake" | "cmakelists.txt" => "cmake",
        "nix" => "nix",
        "tf" | "terraform" => "terraform",
        "hcl" => "hcl",
        // Markup / docs
        "md" | "markdown" => "markdown",
        // Data formats and misc languages
        "sql" => "sql",
        "proto" | "protobuf" => "protobuf",
        "graphql" | "gql" => "graphql",
        "plantuml" | "puml" => "plantuml",
        // Fallback
        other => Box::leak(other.to_string().into_boxed_str()),
    }
}

// Theme cycling removed; we always use Solarized matching UI brightness.

// --- Lightweight language auto-detection ---
// Heuristics only; must be fast and avoid extra deps.
fn autodetect_lang(content: &str) -> Option<&'static str> {
    let s = content.trim_start();
    if s.is_empty() {
        return None;
    }

    // 1) Shebang on first line
    if let Some(first) = s.lines().next() {
        if let Some(rest) = first.strip_prefix("#!") {
            let l = rest.to_ascii_lowercase();
            if l.contains("bash") || l.contains("sh") || l.contains("zsh") {
                return Some("bash");
            }
            if l.contains("python") {
                return Some("python");
            }
            if l.contains("node") || l.contains("deno") {
                return Some("javascript");
            }
            if l.contains("ruby") {
                return Some("ruby");
            }
        }
    }

    // Narrow to a small sample for sniffing
    let sample: String = s.lines().take(24).collect::<Vec<_>>().join("\n");
    let lower = sample.to_ascii_lowercase();
    let trimmed_all = s.trim();

    // 2) Diff/patch
    if trimmed_all.starts_with("diff --git ")
        || sample.lines().take(6).any(|l| l.starts_with("--- "))
            && sample.lines().take(6).any(|l| l.starts_with("+++ "))
    {
        return Some("diff");
    }

    // 3) JSON (parse to be certain when it looks like JSON)
    if (trimmed_all.starts_with('{') && trimmed_all.ends_with('}'))
        || (trimmed_all.starts_with('[') && trimmed_all.ends_with(']'))
    {
        if serde_json::from_str::<serde_json::Value>(trimmed_all).is_ok() {
            return Some("json");
        }
    }

    // 4) HTML/XML
    if trimmed_all.starts_with('<') && trimmed_all.contains('>') {
        let l = lower.trim_start();
        if l.starts_with("<!doctype html") || l.starts_with("<html") || l.starts_with("<head") || l.starts_with("<body") {
            return Some("html");
        }
        if l.starts_with("<?xml") || l.starts_with("<svg") {
            return Some("xml");
        }
    }

    // 5) Rust (common keywords + braces)
    let rust_hits = ["fn ", "let ", "mod ", "impl ", "use ", "pub "]
        .iter()
        .filter(|k| lower.contains(**k))
        .count();
    if rust_hits >= 2 && sample.contains('{') {
        return Some("rust");
    }

    // 6) Python (def/class/import and minimal punctuation typical of Python)
    if (lower.contains("def ") || lower.contains("class ") || lower.contains("import "))
        && !sample.contains(";")
    {
        return Some("python");
    }

    // 7) Shell-ish commands (very loose; prefer only when signals are clear)
    if sample.lines().take(8).any(|l| l.trim_start().starts_with("$ "))
        || sample.contains(" && ")
        || sample.contains(" | ")
        || sample.lines().take(8).any(|l| l.trim_start().starts_with("echo "))
    {
        return Some("bash");
    }

    // 8) SQL
    if ["select ", "insert ", "update ", "delete ", "create ", "alter ", "drop "]
        .iter()
        .any(|kw| lower.contains(*kw))
        && sample.contains(';')
    {
        return Some("sql");
    }

    // 9) INI / TOML / YAML (rough heuristics)
    if let Some(first_line) = s.lines().find(|l| !l.trim().is_empty()) {
        let fl = first_line.trim();
        if fl.starts_with('[') && fl.ends_with(']') {
            if s.contains("[[") || s.contains("]]") || s.contains("=\"") {
                return Some("toml");
            }
            if s.lines().any(|l| l.contains('=')) && !s.contains(": ") {
                return Some("ini");
            }
        }
    }
    let yaml_signals = sample
        .lines()
        .map(|l| l.trim())
        .filter(|l| !l.is_empty())
        .take(20)
        .filter(|l| l.starts_with("- ") || (l.contains(':') && !l.contains("://") && !l.contains("::") && !l.contains(":=")))
        .count();
    if yaml_signals >= 3 {
        return Some("yaml");
    }

    None
}
