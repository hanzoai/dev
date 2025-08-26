use ratatui::style::{Modifier, Style};
use ratatui::text::{Line, Span};
use regex_lite::Regex;

/// Custom markdown renderer with full control over spacing and styling
pub struct MarkdownRenderer {
    lines: Vec<Line<'static>>,
    current_line: Vec<Span<'static>>,
    in_code_block: bool,
    code_block_lang: Option<String>,
    #[allow(dead_code)]
    list_depth: usize,
    bold_first_sentence: bool,
    first_sentence_done: bool,
}

impl MarkdownRenderer {
    pub fn new() -> Self {
        Self {
            lines: Vec::new(),
            current_line: Vec::new(),
            in_code_block: false,
            code_block_lang: None,
            list_depth: 0,
            bold_first_sentence: false,
            first_sentence_done: false,
        }
    }

    pub fn render(text: &str) -> Vec<Line<'static>> {
        let mut renderer = Self::new();
        renderer.process_text(text);
        renderer.finish();
        renderer.lines
    }
    
    pub fn render_with_bold_first_sentence(text: &str) -> Vec<Line<'static>> {
        let mut renderer = Self::new();
        renderer.bold_first_sentence = true;
        renderer.process_text(text);
        renderer.finish();
        renderer.lines
    }

    fn process_text(&mut self, text: &str) {
        let lines: Vec<&str> = text.lines().collect();
        let mut i = 0;
        
        while i < lines.len() {
            let line = lines[i];
            
            // Handle code blocks
            if line.trim_start().starts_with("```") {
                self.handle_code_fence(line);
                i += 1;
                continue;
            }
            
            if self.in_code_block {
                self.add_code_line(line);
                i += 1;
                continue;
            }
            
            // Handle headings
            if let Some(heading) = self.parse_heading(line) {
                self.flush_current_line();
                // Do not auto-insert spacing before headings; preserve exactly what the
                // assistant returned. Only explicit blank lines in the source should render.
                self.lines.push(heading);
                i += 1;
                continue;
            }
            
            // Handle lists
            if let Some(list_item) = self.parse_list_item(line) {
                self.flush_current_line();
                self.lines.push(list_item);
                i += 1;
                continue;
            }
            
            // Handle blank lines
            if line.trim().is_empty() {
                self.flush_current_line();
                // Don't add multiple consecutive blank lines
                if !self.is_last_line_blank() {
                    self.lines.push(Line::from(""));
                }
                i += 1;
                continue;
            }
            
            // Regular text with inline formatting
            self.process_inline_text(line);
            i += 1;
        }
    }
    
    fn handle_code_fence(&mut self, line: &str) {
        let trimmed = line.trim_start();
        if self.in_code_block {
            // Closing fence
            self.in_code_block = false;
            self.code_block_lang = None;
        } else {
            // Opening fence
            self.flush_current_line();
            self.in_code_block = true;
            // Extract language if present
            let lang = trimmed.trim_start_matches("```").trim();
            self.code_block_lang = if lang.is_empty() {
                None
            } else {
                Some(lang.to_string())
            };
        }
    }
    
    fn add_code_line(&mut self, line: &str) {
        // Preserve exact indentation in code blocks
        let code_style = Style::default().fg(crate::colors::function());
        self.lines.push(Line::from(Span::styled(
            line.to_string(),
            code_style,
        )));
    }
    
    fn parse_heading(&self, line: &str) -> Option<Line<'static>> {
        let trimmed = line.trim_start();
        
        // Count heading level
        let mut level = 0;
        for ch in trimmed.chars() {
            if ch == '#' {
                level += 1;
            } else {
                break;
            }
        }
        
        if level == 0 || level > 6 {
            return None;
        }
        
        // Must have space after #
        if !trimmed.chars().nth(level).map_or(false, |c| c == ' ') {
            return None;
        }
        
        let heading_text = trimmed[level..].trim();
        
        // Style based on heading level
        let style = match level {
            1 => Style::default()
                .fg(crate::colors::primary())
                .add_modifier(Modifier::BOLD),
            2 => Style::default().fg(crate::colors::primary()),
            3 => Style::default()
                .fg(crate::colors::text_bright())
                .add_modifier(Modifier::BOLD),
            4 => Style::default().fg(crate::colors::text_bright()),
            5 => Style::default().fg(crate::colors::text_dim()),
            _ => Style::default().fg(crate::colors::text_dim()),
        };
        
        // Include the # symbols in the output for clarity
        let full_heading = format!("{} {}", "#".repeat(level), heading_text);
        Some(Line::from(Span::styled(full_heading, style)))
    }
    
    fn parse_list_item(&mut self, line: &str) -> Option<Line<'static>> {
        let trimmed = line.trim_start();
        
        // Check for unordered list markers
        if trimmed.starts_with("- ") || trimmed.starts_with("* ") || trimmed.starts_with("+ ") {
            let indent = line.len() - trimmed.len();
            let content = &trimmed[2..];
            let styled_content = self.process_inline_spans(content);
            // Determine nesting level from indent (2 spaces per level approximation)
            let level = (indent / 2) + 1;
            let bullet = match level {
                1 => "◦",
                2 => "·",
                3 => "∘",
                _ => "⋅",
            };

            let mut spans = vec![
                Span::raw(" ".repeat(indent)),
                // Render bullet using standard text color for consistency
                Span::styled(bullet, Style::default().fg(crate::colors::text())),
                Span::raw(" "),
            ];
            spans.extend(styled_content);
            
            return Some(Line::from(spans));
        }
        
        // Check for ordered list markers (1. 2. etc)
        if let Some(dot_pos) = trimmed.find(". ") {
            let number_part = &trimmed[..dot_pos];
            if number_part.chars().all(|c| c.is_ascii_digit()) && !number_part.is_empty() {
                let indent = line.len() - trimmed.len();
                let content = &trimmed[dot_pos + 2..];
                let styled_content = self.process_inline_spans(content);
                
                let mut spans = vec![
                    Span::raw(" ".repeat(indent)),
                    Span::styled(
                        format!("{}.", number_part),
                        Style::default().fg(crate::colors::primary()),
                    ),
                    Span::raw(" "),
                ];
                spans.extend(styled_content);
                
                return Some(Line::from(spans));
            }
        }
        
        None
    }
    
    fn process_inline_text(&mut self, text: &str) {
        let spans = self.process_inline_spans(text);
        self.current_line.extend(spans);
        // If bold-first is enabled, restrict bolding to the first rendered line.
        // Even when no sentence terminator is present, only the first line should be bold.
        if self.bold_first_sentence && !self.first_sentence_done {
            self.first_sentence_done = true;
        }
        self.flush_current_line();
    }
    
    fn process_inline_spans(&mut self, text: &str) -> Vec<Span<'static>> {
        let mut spans = Vec::new();
        let chars: Vec<char> = text.chars().collect();
        let mut i = 0;
        let mut current_text = String::new();
        
        while i < chars.len() {
            // Check for inline code
            if chars[i] == '`' {
                // Flush current text
                if !current_text.is_empty() {
                    spans.push(Span::raw(current_text.clone()));
                    current_text.clear();
                }
                
                // Find closing backtick
                let mut j = i + 1;
                let mut code_content = String::new();
                while j < chars.len() && chars[j] != '`' {
                    code_content.push(chars[j]);
                    j += 1;
                }
                
                if j < chars.len() {
                    // Found closing backtick — render code without surrounding backticks
                    spans.push(Span::styled(
                        code_content,
                        Style::default().fg(crate::colors::function()),
                    ));
                    i = j + 1;
                } else {
                    // No closing backtick, treat as regular text
                    current_text.push('`');
                    i += 1;
                }
                continue;
            }
            
            // Check for bold (**text** or __text__)
            if i + 1 < chars.len() && 
               ((chars[i] == '*' && chars[i + 1] == '*') || 
                (chars[i] == '_' && chars[i + 1] == '_')) {
                // Flush current text
                if !current_text.is_empty() {
                    spans.push(Span::raw(current_text.clone()));
                    current_text.clear();
                }
                
                let marker = chars[i];
                // Find closing markers
                let mut j = i + 2;
                let mut bold_content = String::new();
                while j + 1 < chars.len() {
                    if chars[j] == marker && chars[j + 1] == marker {
                        // Found closing markers
                        // Assistant messages render with bright bold; others stay dim
                        let bold_color = if self.bold_first_sentence {
                            crate::colors::text_bright()
                        } else {
                            crate::colors::text_dim()
                        };
                        spans.push(Span::styled(
                            bold_content,
                            Style::default()
                                .fg(bold_color)
                                .add_modifier(Modifier::BOLD),
                        ));
                        i = j + 2;
                        break;
                    }
                    bold_content.push(chars[j]);
                    j += 1;
                }
                
                if j + 1 >= chars.len() {
                    // No closing markers, treat as regular text
                    current_text.push(marker);
                    current_text.push(marker);
                    i += 2;
                }
                continue;
            }
            
            // Check for italic (*text* or _text_)
            if (chars[i] == '*' || chars[i] == '_') &&
               (i == 0 || !chars[i - 1].is_alphanumeric()) &&
               (i + 1 < chars.len() && chars[i + 1] != ' ' && chars[i + 1] != chars[i]) {
                // Flush current text
                if !current_text.is_empty() {
                    spans.push(Span::raw(current_text.clone()));
                    current_text.clear();
                }
                
                let marker = chars[i];
                // Find closing marker
                let mut j = i + 1;
                let mut italic_content = String::new();
                while j < chars.len() {
                    if chars[j] == marker &&
                       (j + 1 >= chars.len() || !chars[j + 1].is_alphanumeric()) {
                        // Found closing marker
                        spans.push(Span::styled(
                            italic_content,
                            Style::default().add_modifier(Modifier::ITALIC),
                        ));
                        i = j + 1;
                        break;
                    }
                    italic_content.push(chars[j]);
                    j += 1;
                }
                
                if j >= chars.len() {
                    // No closing marker, treat as regular text
                    current_text.push(marker);
                    i += 1;
                }
                continue;
            }
            
            // Regular character
            current_text.push(chars[i]);
            i += 1;
        }
        
        // Flush any remaining text
        if !current_text.is_empty() {
            // Apply first sentence bolding if enabled
            if self.bold_first_sentence && !self.first_sentence_done {
                let sentence_end = current_text.find(|c: char| c == '.' || c == '!' || c == '?' || c == ':');
                if let Some(end) = sentence_end {
                    // Split at sentence end (including punctuation)
                    let end_with_punct = end + 1;
                    let first_sentence = &current_text[..end_with_punct];
                    spans.push(Span::styled(
                        first_sentence.to_string(),
                        Style::default()
                            .fg(crate::colors::text_bright())
                            .add_modifier(Modifier::BOLD),
                    ));
                    self.first_sentence_done = true;
                    
                    // Add the rest without bold
                    if end_with_punct < current_text.len() {
                        spans.push(Span::raw(current_text[end_with_punct..].to_string()));
                    }
                } else {
                    // No sentence end found, bold the entire text
                    spans.push(Span::styled(
                        current_text,
                        Style::default()
                            .fg(crate::colors::text_bright())
                            .add_modifier(Modifier::BOLD),
                    ));
                }
            } else {
                spans.push(Span::raw(current_text));
            }
        }
        
        spans
    }
    
    fn flush_current_line(&mut self) {
        if !self.current_line.is_empty() {
            // Autolink URLs and markdown links inside the accumulated spans.
            let linked = autolink_spans(std::mem::take(&mut self.current_line));
            self.lines.push(Line::from(linked));
            self.current_line.clear();
        }
    }
    
    fn is_last_line_blank(&self) -> bool {
        if let Some(last) = self.lines.last() {
            last.spans.is_empty() || 
            last.spans.iter().all(|s| s.content.trim().is_empty())
        } else {
            false
        }
    }
    
    fn finish(&mut self) {
        self.flush_current_line();
    }
}

// Turn inline markdown links and bare URLs into OSC 8 hyperlinks.
// This runs late, on the already-styled spans for a line, so it preserves
// bold/italic styling while adding underlines for links and embedding the
// hyperlink escape sequences in the text.
fn autolink_spans(spans: Vec<Span<'static>>) -> Vec<Span<'static>> {
    // Patterns: markdown [label](target), explicit http(s) URLs, and plain domains.
    // Keep conservative to avoid false positives.
    static MD_LINK_RE: once_cell::sync::OnceCell<Regex> = once_cell::sync::OnceCell::new();
    static EXPL_URL_RE: once_cell::sync::OnceCell<Regex> = once_cell::sync::OnceCell::new();
    static DOMAIN_RE: once_cell::sync::OnceCell<Regex> = once_cell::sync::OnceCell::new();
    let md_re = MD_LINK_RE
        .get_or_init(|| Regex::new(r"\[([^\]]+)\]\(([^)\s]+)\)").unwrap());
    let url_re = EXPL_URL_RE
        .get_or_init(|| Regex::new(r"(?i)\bhttps?://[^\s)]+").unwrap());
    let dom_re = DOMAIN_RE.get_or_init(|| {
        // Conservative bare-domain matcher (no scheme). Examples:
        //   apps.shopify.com
        //   foo.example.io/path?x=1
        // It intentionally over-matches a bit; we further filter below.
        Regex::new(r"\b([a-z0-9](?:[a-z0-9-]*[a-z0-9])?(?:\.[a-z0-9](?:[a-z0-9-]*[a-z0-9])?)+(?:/[\w\-./?%&=#]*)?)")
            .unwrap()
    });

    // Trim common trailing punctuation from a detected URL/domain, returning
    // (core, trailing). The trailing part will be emitted as normal text (not
    // hyperlinked) so that tokens like "example.com." don’t include the period.
    fn split_trailing_punct(s: &str) -> (&str, &str) {
        let bytes = s.as_bytes();
        let mut end = bytes.len();
        while end > 0 {
            let ch = bytes[end - 1] as char;
            if ")]}>'\".,!?;:".contains(ch) { // common trailing punctuation
                end -= 1;
                continue;
            }
            break;
        }
        (&s[..end], &s[end..])
    }

    // Additional heuristic to avoid false positives like "e.g." or
    // "filename.rs" by requiring a well-known TLD. Precision over recall.
    fn is_probable_domain(dom: &str) -> bool {
        if dom.contains('@') { return false; }
        let dot_count = dom.matches('.').count();
        if dot_count == 0 { return false; }

        // Extract the final label (candidate TLD) and normalize.
        let tld = dom
            .rsplit_once('.')
            .map(|(_, t)| t)
            .unwrap_or("")
            .trim_matches(|c: char| !c.is_ascii_alphanumeric())
            .to_ascii_lowercase();

        // Small allowlist of popular TLDs. This intentionally excludes
        // language/file extensions like `.rs`, `.ts`, `.php`, etc.
        const ALLOWED_TLDS: &[&str] = &[
            "com","org","net","edu","gov","io","ai","app","dev","co","us","uk","ca","de","fr","jp","cn","in","au"
        ];

        ALLOWED_TLDS.contains(&tld.as_str())
    }

    let mut out: Vec<Span<'static>> = Vec::with_capacity(spans.len());
    for s in spans {
        // Skip autolinking inside inline code spans (we style code with the
        // theme's function color). This avoids linking snippets like
        // `curl https://example.com` or code identifiers containing dots.
        if s.style.fg == Some(crate::colors::function()) {
            out.push(s);
            continue;
        }
        let text = s.content.clone();
        let mut cursor = 0usize;
        let mut changed = false;

        // Scan left-to-right, preferring markdown links first (explicit intent),
        // then explicit URLs, then bare domains.
        while cursor < text.len() {
            let after = &text[cursor..];

            // 1) markdown links
            if let Some(m) = md_re.find(after) {
                let start = cursor + m.start();
                let end = cursor + m.end();
                // Push any plain prefix
                if cursor < start {
                    let mut span = s.clone();
                    span.content = text[cursor..start].to_string().into();
                    out.push(span);
                }
                let caps = md_re.captures(&text[start..end]).unwrap();
                let label = caps.get(1).unwrap().as_str();
                let target = caps.get(2).unwrap().as_str();
                out.push(hyperlink_span(label, target, &s));
                cursor = end;
                changed = true;
                continue;
            }

            // 2) explicit http(s) URLs
            if let Some(m) = url_re.find(after) {
                let start = cursor + m.start();
                let end = cursor + m.end();
                if cursor < start {
                    let mut span = s.clone();
                    span.content = text[cursor..start].to_string().into();
                    out.push(span);
                }
                let raw = &text[start..end];
                let (core, trailing) = split_trailing_punct(raw);
                out.push(hyperlink_span(core, core, &s));
                if !trailing.is_empty() {
                    let mut span = s.clone();
                    span.content = trailing.to_string().into();
                    out.push(span);
                }
                cursor = start + core.len() + trailing.len();
                changed = true;
                continue;
            }

            // 3) bare domain (assume https)
            if let Some(m) = dom_re.find(after) {
                let start = cursor + m.start();
                let end = cursor + m.end();
                if cursor < start {
                    let mut span = s.clone();
                    span.content = text[cursor..start].to_string().into();
                    out.push(span);
                }
                let raw = &text[start..end];
                let (core_dom, trailing) = split_trailing_punct(raw);
                if is_probable_domain(core_dom) {
                    let target = format!("https://{core_dom}");
                    out.push(hyperlink_span(core_dom, &target, &s));
                    if !trailing.is_empty() {
                        let mut span = s.clone();
                        span.content = trailing.to_string().into();
                        out.push(span);
                    }
                    cursor = start + core_dom.len() + trailing.len();
                    changed = true;
                    continue;
                }
                // Not a probable domain; emit raw text
                let mut span = s.clone();
                span.content = raw.to_string().into();
                out.push(span);
                cursor = end;
                changed = true;
                continue;
            }

            // No more matches
            break;
        }

        if changed {
            if cursor < text.len() {
                let mut span = s.clone();
                span.content = text[cursor..].to_string().into();
                out.push(span);
            }
        } else {
            out.push(s);
        }
    }

    out
}

fn hyperlink_span(label: &str, target: &str, base: &Span<'static>) -> Span<'static> {
    // Use OSC 8 with BEL terminators (widely supported; matches existing code).
    let content = format!(
        "\u{1b}]8;;{target}\u{7}{label}\u{1b}]8;;\u{7}"
    );
    let mut style = base.style;
    style.add_modifier.insert(Modifier::UNDERLINED);
    Span::styled(content, style)
}

#[cfg(test)]
mod tests {
    use super::*;
    use ratatui::style::Modifier;
    
    #[test]
    fn test_bold_first_sentence() {
        let text = "This is the first sentence. Here is the second sentence. And this is the third.";
        let lines = MarkdownRenderer::render_with_bold_first_sentence(text);
        
        assert!(!lines.is_empty(), "Should have rendered lines");
        
        // Check first line contains bold text
        let first_line = &lines[0];
        let mut found_bold = false;
        let mut bold_text = String::new();
        
        for span in &first_line.spans {
            if span.style.add_modifier.contains(Modifier::BOLD) {
                found_bold = true;
                bold_text.push_str(&span.content);
            }
        }
        
        assert!(found_bold, "Should have found bold text");
        assert_eq!(bold_text, "This is the first sentence.", "First sentence should be bold");
    }
    
    #[test]
    fn test_no_bold_without_flag() {
        let text = "This is the first sentence. Here is the second sentence.";
        let lines = MarkdownRenderer::render(text);
        
        assert!(!lines.is_empty(), "Should have rendered lines");
        
        // Check no bold text
        let first_line = &lines[0];
        let mut found_bold = false;
        
        for span in &first_line.spans {
            if span.style.add_modifier.contains(Modifier::BOLD) {
                found_bold = true;
            }
        }
        
        assert!(!found_bold, "Should not have bold text when flag is not set");
    }
}
