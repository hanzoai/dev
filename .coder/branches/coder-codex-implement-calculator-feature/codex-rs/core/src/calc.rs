//! Simple arithmetic expression evaluator for CLI/TUI calculator.
//!
//! - Deterministic f64 operations
//! - Pratt parser with support for: numbers, unary +/- , binary + - * / ^, parentheses
//! - Right-associative exponentiation (^)
//! - Division-by-zero error
//! - Depth guard to avoid pathological inputs

#[derive(Debug, Clone, Copy)]
pub struct EvalOptions {
    pub precision: usize,
    pub raw: bool,
}

impl Default for EvalOptions {
    fn default() -> Self {
        Self {
            precision: 6,
            raw: false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CalcError {
    Empty,
    InvalidNumber { pos: usize },
    Unexpected { pos: usize, found: char },
    MismatchParen { pos: usize },
    DivideByZero { pos: usize },
    TrailingInput { pos: usize },
    MaxDepth,
    Overflow,
}

/// Evaluate expression with default options.
pub fn evaluate(expr: &str) -> Result<f64, CalcError> {
    Parser::new(expr)?.evaluate()
}

/// Evaluate expression with explicit options. Options currently do not affect
/// the numerical result (only formatting); provided for symmetry and future use.
pub fn evaluate_with_opts(expr: &str, _opts: &EvalOptions) -> Result<f64, CalcError> {
    evaluate(expr)
}

/// Format a floating value with given precision.
/// - When `raw` is true, always shows exactly `precision` digits after decimal.
/// - When `raw` is false, trims trailing zeros and a trailing decimal point.
pub fn format_value(v: f64, precision: usize, raw: bool) -> String {
    if !v.is_finite() {
        return if raw {
            String::from("nan")
        } else {
            String::from("nan")
        };
    }
    // Normalize -0.0 to 0.0 for display
    let v = if v == 0.0 { 0.0 } else { v };

    if raw {
        return format!("{:.*}", precision, v);
    }

    let s = format!("{:.*}", precision, v);
    // Trim trailing zeros and optional trailing '.'
    if let Some(dot_idx) = s.find('.') {
        let (int_part, frac_part) = s.split_at(dot_idx);
        let mut frac = &frac_part[1..]; // skip '.'
        // remove trailing zeros
        frac = frac.trim_end_matches('0');
        if frac.is_empty() {
            int_part.to_string()
        } else {
            format!("{int_part}.{frac}")
        }
    } else {
        s
    }
}

// ---------------- Parser -----------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TokKind {
    Num,
    Plus,
    Minus,
    Star,
    Slash,
    Caret,
    LParen,
    RParen,
    Eof,
}

#[derive(Debug, Clone, Copy)]
struct Token {
    kind: TokKind,
    pos: usize,
    // For numbers only
    value: f64,
}

struct Lexer<'a> {
    s: &'a [u8],
    i: usize,
    len: usize,
}

impl<'a> Lexer<'a> {
    fn new(s: &'a str) -> Self {
        Self {
            s: s.as_bytes(),
            i: 0,
            len: s.len(),
        }
    }

    fn peek(&self) -> Option<u8> {
        self.s.get(self.i).copied()
    }

    fn bump(&mut self) -> Option<u8> {
        let b = self.peek()?;
        self.i += 1;
        Some(b)
    }

    fn skip_ws(&mut self) {
        while let Some(b) = self.peek() {
            if b.is_ascii_whitespace() {
                self.i += 1;
            } else {
                break;
            }
        }
    }

    fn number(&mut self, start: usize) -> Result<Token, CalcError> {
        let mut end = self.i;
        // integer part
        while let Some(b) = self.peek() {
            if (b as char).is_ascii_digit() {
                self.i += 1;
                end = self.i;
            } else {
                break;
            }
        }
        // optional fractional part
        if self.peek() == Some(b'.') {
            self.i += 1;
            let mut saw_digit = false;
            while let Some(b) = self.peek() {
                if (b as char).is_ascii_digit() {
                    self.i += 1;
                    end = self.i;
                    saw_digit = true;
                } else {
                    break;
                }
            }
            if !saw_digit {
                // e.g. "1." is valid (treat as integer)
                end = self.i;
            }
        }

        // optional exponent part
        if matches!(self.peek(), Some(b'e' | b'E')) {
            let save = self.i;
            self.i += 1; // consume e/E
            if matches!(self.peek(), Some(b'+' | b'-')) {
                self.i += 1;
            }
            let mut saw_digit = false;
            while let Some(b) = self.peek() {
                if (b as char).is_ascii_digit() {
                    self.i += 1;
                    saw_digit = true;
                } else {
                    break;
                }
            }
            if !saw_digit {
                // rollback; invalid exponent, treat as no exponent
                self.i = save;
            } else {
                end = self.i;
            }
        }

        let text = std::str::from_utf8(&self.s[start..end]).unwrap_or("");
        match text.parse::<f64>() {
            Ok(v) => Ok(Token {
                kind: TokKind::Num,
                pos: start,
                value: v,
            }),
            Err(_) => Err(CalcError::InvalidNumber { pos: start }),
        }
    }

    fn next_token(&mut self) -> Result<Token, CalcError> {
        self.skip_ws();
        let pos = self.i;
        match self.bump() {
            None => Ok(Token {
                kind: TokKind::Eof,
                pos,
                value: 0.0,
            }),
            Some(b'+') => Ok(Token {
                kind: TokKind::Plus,
                pos,
                value: 0.0,
            }),
            Some(b'-') => Ok(Token {
                kind: TokKind::Minus,
                pos,
                value: 0.0,
            }),
            Some(b'*') => Ok(Token {
                kind: TokKind::Star,
                pos,
                value: 0.0,
            }),
            Some(b'/') => Ok(Token {
                kind: TokKind::Slash,
                pos,
                value: 0.0,
            }),
            Some(b'^') => Ok(Token {
                kind: TokKind::Caret,
                pos,
                value: 0.0,
            }),
            Some(b'(') => Ok(Token {
                kind: TokKind::LParen,
                pos,
                value: 0.0,
            }),
            Some(b')') => Ok(Token {
                kind: TokKind::RParen,
                pos,
                value: 0.0,
            }),
            Some(b) if (b as char).is_ascii_digit() || b == b'.' => {
                // number starting with digit or '.'
                let start = if b == b'.' {
                    // allow leading '.'
                    pos
                } else {
                    pos
                };
                // rewind one, number() expects current at first digit/dot consumed
                self.i = pos;
                self.number(start)
            }
            Some(other) => Err(CalcError::Unexpected {
                pos,
                found: other as char,
            }),
        }
    }
}

struct Parser<'a> {
    lex: Lexer<'a>,
    cur: Token,
    depth: usize,
    // max recursive calls (inclusive)
    max_depth: usize,
}

impl<'a> Parser<'a> {
    fn new(src: &'a str) -> Result<Self, CalcError> {
        let mut lex = Lexer::new(src);
        let first = lex.next_token()?;
        Ok(Self {
            lex,
            cur: first,
            depth: 0,
            max_depth: 64,
        })
    }

    fn bump(&mut self) -> Result<(), CalcError> {
        self.cur = self.lex.next_token()?;
        Ok(())
    }

    fn expect(&mut self, kind: TokKind) -> Result<Token, CalcError> {
        if self.cur.kind == kind {
            let t = self.cur;
            self.bump()?;
            Ok(t)
        } else {
            Err(match kind {
                TokKind::RParen => CalcError::MismatchParen { pos: self.cur.pos },
                _ => CalcError::Unexpected {
                    pos: self.cur.pos,
                    found: self.found_char(),
                },
            })
        }
    }

    fn found_char(&self) -> char {
        match self.cur.kind {
            TokKind::Plus => '+',
            TokKind::Minus => '-',
            TokKind::Star => '*',
            TokKind::Slash => '/',
            TokKind::Caret => '^',
            TokKind::LParen => '(',
            TokKind::RParen => ')',
            TokKind::Eof => '\0',
            TokKind::Num => '#',
        }
    }

    fn evaluate(mut self) -> Result<f64, CalcError> {
        // Empty check
        if matches!(self.cur.kind, TokKind::Eof) {
            return Err(CalcError::Empty);
        }
        let v = self.expr(0)?;
        if !matches!(self.cur.kind, TokKind::Eof) {
            return Err(CalcError::TrailingInput { pos: self.cur.pos });
        }
        if !v.is_finite() {
            return Err(CalcError::Overflow);
        }
        Ok(v)
    }

    fn expr(&mut self, min_bp: u8) -> Result<f64, CalcError> {
        self.check_depth()?;
        // Prefix
        let mut lhs = match self.cur.kind {
            TokKind::Num => {
                let v = self.cur.value;
                self.bump()?;
                v
            }
            TokKind::Minus => {
                // Unary - : parse with rbp that allows '^' to bind tighter but blocks * and +
                let _op = self.cur;
                self.bump()?;
                let rhs = self.expr(6)?; // rbp for prefix
                -rhs
            }
            TokKind::Plus => {
                // Unary +
                self.bump()?;
                self.expr(6)?
            }
            TokKind::LParen => {
                self.bump()?;
                let v = match self.expr(0) {
                    Ok(v) => v,
                    Err(CalcError::Unexpected { found: '\0', .. }) | Err(CalcError::Empty) => {
                        return Err(CalcError::MismatchParen { pos: self.cur.pos })
                    }
                    Err(e) => return Err(e),
                };
                self.expect(TokKind::RParen)?;
                v
            }
            _ => {
                return Err(CalcError::Unexpected {
                    pos: self.cur.pos,
                    found: self.found_char(),
                });
            }
        };

        // Infix loop
        loop {
            self.check_depth()?;
            let (lbp, rbp, kind) = match self.cur.kind {
                TokKind::Plus => (1, 2, TokKind::Plus),
                TokKind::Minus => (1, 2, TokKind::Minus),
                TokKind::Star => (3, 4, TokKind::Star),
                TokKind::Slash => (3, 4, TokKind::Slash),
                TokKind::Caret => (7, 7, TokKind::Caret), // right-assoc
                _ => break,
            };
            if lbp < min_bp {
                break;
            }
            let op_pos = self.cur.pos;
            self.bump()?;
            let mut rhs = self.expr(rbp)?;
            match kind {
                TokKind::Plus => lhs += rhs,
                TokKind::Minus => lhs -= rhs,
                TokKind::Star => lhs *= rhs,
                TokKind::Slash => {
                    if rhs == 0.0 {
                        return Err(CalcError::DivideByZero { pos: op_pos });
                    }
                    lhs /= rhs
                }
                TokKind::Caret => {
                    // powf: handle negative bases as well
                    rhs = lhs.powf(rhs);
                    lhs = rhs;
                }
                _ => unreachable!(),
            }
            if !lhs.is_finite() {
                return Err(CalcError::Overflow);
            }
        }

        Ok(lhs)
    }

    fn check_depth(&mut self) -> Result<(), CalcError> {
        self.depth += 1;
        if self.depth > self.max_depth {
            return Err(CalcError::MaxDepth);
        }
        Ok(())
    }
}

// ---------------- Tests -----------------

#[cfg(test)]
mod tests {
    use super::*;

    fn fmt(v: f64) -> String {
        format_value(v, 6, false)
    }

    #[test]
    fn arithmetic_basics() {
        assert_eq!(evaluate("1+2").unwrap(), 3.0);
        assert_eq!(evaluate("1+2*3").unwrap(), 7.0);
        assert_eq!(evaluate("(1+2)*3").unwrap(), 9.0);
        assert_eq!(evaluate("2^3").unwrap(), 8.0);
    }

    #[test]
    fn associativity_and_precedence() {
        // Right-associative exponent
        assert_eq!(evaluate("2^3^2").unwrap(), 512.0); // 2^(3^2)
        // Unary binds looser than '^' so -3^2 == -(3^2)
        assert_eq!(evaluate("-3^2").unwrap(), -9.0);
        // Unary binds tighter than * and +
        assert_eq!(evaluate("-3*2").unwrap(), -6.0);
        assert_eq!(evaluate("1+-2").unwrap(), -1.0);
    }

    #[test]
    fn division_by_zero() {
        let e = evaluate("1/0").unwrap_err();
        assert!(matches!(e, CalcError::DivideByZero { .. }));
    }

    #[test]
    fn syntax_errors() {
        assert!(matches!(evaluate("").unwrap_err(), CalcError::Empty));
        assert!(matches!(
            evaluate("(").unwrap_err(),
            CalcError::MismatchParen { .. }
        ));
        assert!(matches!(
            evaluate("1 1").unwrap_err(),
            CalcError::TrailingInput { .. }
        ));
        assert!(matches!(
            evaluate("@").unwrap_err(),
            CalcError::Unexpected { .. }
        ));
        assert!(matches!(evaluate("1e").unwrap_err(), CalcError::Unexpected { .. }));
    }

    #[test]
    fn formatting() {
        assert_eq!(format_value(1.23456789, 6, false), "1.234568");
        assert_eq!(format_value(1.2000, 6, false), "1.2");
        assert_eq!(format_value(1.0, 6, false), "1");
        assert_eq!(format_value(0.0, 6, true), "0.000000");
        assert_eq!(format_value(0.0, 3, true), "0.000");
    }
}
