use ratatui::style::{Modifier, Style};
use ratatui::text::{Line, Span};

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
                            crate::colors::text_dim()
                        } else {
                            crate::colors::text_bright()
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
            self.lines.push(Line::from(self.current_line.clone()));
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
