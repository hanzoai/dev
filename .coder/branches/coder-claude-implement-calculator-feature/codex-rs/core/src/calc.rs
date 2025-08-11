//! Simple arithmetic calculator with Pratt parsing.

use std::fmt;

/// Options for evaluation and formatting.
#[derive(Debug, Clone)]
pub struct EvalOptions {
    /// Number of decimal places for output formatting.
    pub precision: usize,
    /// If true, output raw f64 format (no trimming).
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

/// Calculator errors with position information.
#[derive(Debug, Clone, PartialEq)]
pub enum CalcError {
    /// Empty expression.
    Empty,
    /// Invalid number at position.
    InvalidNumber { pos: usize },
    /// Unexpected character/token.
    Unexpected { pos: usize, found: String },
    /// Mismatched parentheses.
    MismatchParen { pos: usize },
    /// Division by zero.
    DivideByZero { pos: usize },
    /// Trailing input after expression.
    TrailingInput { pos: usize },
    /// Maximum recursion depth exceeded.
    MaxDepth,
    /// Arithmetic overflow.
    Overflow,
}

impl fmt::Display for CalcError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CalcError::Empty => write!(f, "empty expression"),
            CalcError::InvalidNumber { pos } => write!(f, "invalid number at position {}", pos),
            CalcError::Unexpected { pos, found } => {
                write!(f, "unexpected '{}' at position {}", found, pos)
            }
            CalcError::MismatchParen { pos } => {
                write!(f, "mismatched parenthesis at position {}", pos)
            }
            CalcError::DivideByZero { pos } => write!(f, "division by zero at position {}", pos),
            CalcError::TrailingInput { pos } => write!(f, "trailing input at position {}", pos),
            CalcError::MaxDepth => write!(f, "maximum recursion depth exceeded"),
            CalcError::Overflow => write!(f, "arithmetic overflow"),
        }
    }
}

impl std::error::Error for CalcError {}

/// Evaluate an arithmetic expression and return the result.
pub fn evaluate(expr: &str) -> Result<f64, CalcError> {
    evaluate_with_opts(expr, &EvalOptions::default())
}

/// Evaluate an arithmetic expression with options.
pub fn evaluate_with_opts(expr: &str, _opts: &EvalOptions) -> Result<f64, CalcError> {
    let mut parser = Parser::new(expr)?;
    parser.parse()
}

/// Format a numeric value with given precision and raw flag.
pub fn format_value(v: f64, precision: usize, raw: bool) -> String {
    if raw {
        v.to_string()
    } else {
        let formatted = format!("{:.prec$}", v, prec = precision);
        // Trim trailing zeros and decimal point if integer
        if formatted.contains('.') {
            let trimmed = formatted.trim_end_matches('0').trim_end_matches('.');
            if trimmed.is_empty() {
                "0".to_string()
            } else {
                trimmed.to_string()
            }
        } else {
            formatted
        }
    }
}

// Token types for the parser
#[derive(Debug, Clone, PartialEq)]
enum Token {
    Number(f64),
    Plus,
    Minus,
    Star,
    Slash,
    Caret,
    LeftParen,
    RightParen,
    End,
}

// Lexer for tokenizing the expression
struct Lexer<'a> {
    input: &'a str,
    chars: std::str::CharIndices<'a>,
    current: Option<(usize, char)>,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Self {
        let mut chars = input.char_indices();
        let current = chars.next();
        Self {
            input,
            chars,
            current,
        }
    }

    fn peek(&self) -> Option<char> {
        self.current.map(|(_, ch)| ch)
    }

    fn pos(&self) -> usize {
        self.current.map(|(pos, _)| pos).unwrap_or(self.input.len())
    }

    fn advance(&mut self) {
        self.current = self.chars.next();
    }

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.peek() {
            if ch.is_whitespace() {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn next_token(&mut self) -> Result<(Token, usize), CalcError> {
        self.skip_whitespace();

        let pos = self.pos();
        match self.peek() {
            None => Ok((Token::End, pos)),
            Some('+') => {
                self.advance();
                Ok((Token::Plus, pos))
            }
            Some('-') => {
                self.advance();
                Ok((Token::Minus, pos))
            }
            Some('*') => {
                self.advance();
                Ok((Token::Star, pos))
            }
            Some('/') => {
                self.advance();
                Ok((Token::Slash, pos))
            }
            Some('^') => {
                self.advance();
                Ok((Token::Caret, pos))
            }
            Some('(') => {
                self.advance();
                Ok((Token::LeftParen, pos))
            }
            Some(')') => {
                self.advance();
                Ok((Token::RightParen, pos))
            }
            Some(ch) if ch.is_ascii_digit() || ch == '.' => self.parse_number(),
            Some(ch) => Err(CalcError::Unexpected {
                pos,
                found: ch.to_string(),
            }),
        }
    }

    fn parse_number(&mut self) -> Result<(Token, usize), CalcError> {
        let start_pos = self.pos();
        let start = start_pos;
        let mut has_dot = false;

        // Collect digits and at most one decimal point
        while let Some(ch) = self.peek() {
            if ch.is_ascii_digit() {
                self.advance();
            } else if ch == '.' && !has_dot {
                has_dot = true;
                self.advance();
            } else {
                break;
            }
        }

        let end = self.pos();
        let num_str = &self.input[start..end];

        num_str
            .parse::<f64>()
            .map(|n| (Token::Number(n), start_pos))
            .map_err(|_| CalcError::InvalidNumber { pos: start_pos })
    }
}

// Pratt parser for expression evaluation
struct Parser<'a> {
    lexer: Lexer<'a>,
    current: (Token, usize),
    depth: usize,
}

impl<'a> Parser<'a> {
    const MAX_DEPTH: usize = 100;

    fn new(input: &'a str) -> Result<Self, CalcError> {
        let mut lexer = Lexer::new(input);
        let current = lexer.next_token()?;
        Ok(Self {
            lexer,
            current,
            depth: 0,
        })
    }

    fn parse(&mut self) -> Result<f64, CalcError> {
        if matches!(self.current.0, Token::End) {
            return Err(CalcError::Empty);
        }

        let result = self.expression(0)?;

        if !matches!(self.current.0, Token::End) {
            return Err(CalcError::TrailingInput { pos: self.current.1 });
        }

        Ok(result)
    }

    fn advance(&mut self) -> Result<(), CalcError> {
        self.current = self.lexer.next_token()?;
        Ok(())
    }

    fn expression(&mut self, min_bp: u8) -> Result<f64, CalcError> {
        self.depth += 1;
        if self.depth > Self::MAX_DEPTH {
            return Err(CalcError::MaxDepth);
        }

        let mut left = self.parse_prefix()?;

        loop {
            let op = match &self.current.0 {
                Token::Plus | Token::Minus | Token::Star | Token::Slash | Token::Caret => {
                    self.current.clone()
                }
                _ => break,
            };

            let (left_bp, right_bp) = binding_power(&op.0);
            if left_bp < min_bp {
                break;
            }

            self.advance()?;
            let right = self.expression(right_bp)?;

            left = apply_op(left, &op.0, right, op.1)?;
        }

        self.depth -= 1;
        Ok(left)
    }

    fn parse_prefix(&mut self) -> Result<f64, CalcError> {
        match &self.current.0 {
            Token::Number(n) => {
                let val = *n;
                self.advance()?;
                Ok(val)
            }
            Token::Plus => {
                let _pos = self.current.1;
                self.advance()?;
                self.parse_prefix()
            }
            Token::Minus => {
                let _pos = self.current.1;
                self.advance()?;
                let val = self.parse_prefix()?;
                Ok(-val)
            }
            Token::LeftParen => {
                let open_pos = self.current.1;
                self.advance()?;
                let val = self.expression(0)?;
                if !matches!(self.current.0, Token::RightParen) {
                    return Err(CalcError::MismatchParen { pos: open_pos });
                }
                self.advance()?;
                Ok(val)
            }
            _ => Err(CalcError::Unexpected {
                pos: self.current.1,
                found: format!("{:?}", self.current.0),
            }),
        }
    }
}

fn binding_power(op: &Token) -> (u8, u8) {
    match op {
        Token::Plus | Token::Minus => (1, 2),
        Token::Star | Token::Slash => (3, 4),
        Token::Caret => (6, 5), // Right associative
        _ => (0, 0),
    }
}

fn apply_op(left: f64, op: &Token, right: f64, pos: usize) -> Result<f64, CalcError> {
    let result = match op {
        Token::Plus => left + right,
        Token::Minus => left - right,
        Token::Star => left * right,
        Token::Slash => {
            if right == 0.0 {
                return Err(CalcError::DivideByZero { pos });
            }
            left / right
        }
        Token::Caret => left.powf(right),
        _ => unreachable!(),
    };

    if result.is_infinite() || result.is_nan() {
        Err(CalcError::Overflow)
    } else {
        Ok(result)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_arithmetic() {
        assert_eq!(evaluate("2 + 2").unwrap(), 4.0);
        assert_eq!(evaluate("10 - 3").unwrap(), 7.0);
        assert_eq!(evaluate("4 * 5").unwrap(), 20.0);
        assert_eq!(evaluate("15 / 3").unwrap(), 5.0);
        assert_eq!(evaluate("2 ^ 3").unwrap(), 8.0);
    }

    #[test]
    fn test_precedence() {
        assert_eq!(evaluate("2 + 3 * 4").unwrap(), 14.0);
        assert_eq!(evaluate("10 - 2 * 3").unwrap(), 4.0);
        assert_eq!(evaluate("2 * 3 + 4").unwrap(), 10.0);
        assert_eq!(evaluate("2 ^ 3 * 2").unwrap(), 16.0);
        assert_eq!(evaluate("2 * 2 ^ 3").unwrap(), 16.0);
    }

    #[test]
    fn test_associativity() {
        assert_eq!(evaluate("10 - 5 - 2").unwrap(), 3.0);
        assert_eq!(evaluate("20 / 4 / 2").unwrap(), 2.5);
        assert_eq!(evaluate("2 ^ 2 ^ 3").unwrap(), 256.0); // Right associative
    }

    #[test]
    fn test_parentheses() {
        assert_eq!(evaluate("(2 + 3) * 4").unwrap(), 20.0);
        assert_eq!(evaluate("2 * (3 + 4)").unwrap(), 14.0);
        assert_eq!(evaluate("((2 + 3) * (4 + 5))").unwrap(), 45.0);
        assert_eq!(evaluate("2 ^ (3 * 2)").unwrap(), 64.0);
    }

    #[test]
    fn test_unary() {
        assert_eq!(evaluate("-5").unwrap(), -5.0);
        assert_eq!(evaluate("+5").unwrap(), 5.0);
        assert_eq!(evaluate("-(2 + 3)").unwrap(), -5.0);
        assert_eq!(evaluate("-2 * 3").unwrap(), -6.0);
        assert_eq!(evaluate("2 * -3").unwrap(), -6.0);
    }

    #[test]
    fn test_decimals() {
        assert_eq!(evaluate("3.14").unwrap(), 3.14);
        assert_eq!(evaluate("2.5 + 1.5").unwrap(), 4.0);
        assert_eq!(evaluate("10.0 / 4.0").unwrap(), 2.5);
        assert_eq!(evaluate(".5 + .5").unwrap(), 1.0);
    }

    #[test]
    fn test_whitespace() {
        assert_eq!(evaluate("  2  +  2  ").unwrap(), 4.0);
        assert_eq!(evaluate("2+2").unwrap(), 4.0);
        assert_eq!(evaluate(" ( 2 + 3 ) * 4 ").unwrap(), 20.0);
    }

    #[test]
    fn test_errors() {
        assert!(matches!(evaluate(""), Err(CalcError::Empty)));
        assert!(matches!(evaluate("2 +"), Err(CalcError::Unexpected { .. })));
        assert_eq!(evaluate("2 + + 3").unwrap(), 5.0); // Double unary plus is valid
        assert!(matches!(evaluate("(2 + 3"), Err(CalcError::MismatchParen { .. })));
        assert!(matches!(evaluate("2 + 3)"), Err(CalcError::TrailingInput { .. })));
        assert!(matches!(evaluate("5 / 0"), Err(CalcError::DivideByZero { .. })));
        assert!(matches!(evaluate("abc"), Err(CalcError::Unexpected { .. })));
        assert!(matches!(evaluate("2 3"), Err(CalcError::TrailingInput { .. })));
    }

    #[test]
    fn test_format_value() {
        assert_eq!(format_value(3.14159, 2, false), "3.14");
        assert_eq!(format_value(3.0, 2, false), "3");
        assert_eq!(format_value(3.100, 3, false), "3.1");
        assert_eq!(format_value(0.0, 2, false), "0");
        assert_eq!(format_value(3.14159, 2, true), "3.14159");
        assert_eq!(format_value(123.456789, 4, false), "123.4568");
        assert_eq!(format_value(1000.0, 2, false), "1000");
    }

    #[test]
    fn test_complex_expressions() {
        assert_eq!(
            evaluate("(2 + 3) * (4 - 1) / 5 + 2 ^ 2").unwrap(),
            7.0
        );
        assert_eq!(
            evaluate("3.14 * 2 ^ 2 + 1.86 * 2").unwrap(),
            16.28
        );
    }
}