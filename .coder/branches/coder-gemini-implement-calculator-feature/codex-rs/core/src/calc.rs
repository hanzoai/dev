
// Copyright 2024 CoderEnthusiast
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

//! A simple arithmetic expression evaluator.

use std::fmt;

/// Options for evaluating an expression.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EvalOptions {
    /// The precision to use when formatting the result.
    pub precision: usize,
    /// Whether to format the result as a raw number.
    pub raw: bool,
}

/// An error that can occur during evaluation.
#[derive(Debug, Clone, PartialEq)]
pub enum CalcError {
    /// The expression was empty.
    Empty,
    /// An invalid number was found at the given position.
    InvalidNumber { pos: usize },
    /// An unexpected token was found at the given position.
    Unexpected { pos: usize, found: char },
    /// A mismatched parenthesis was found at the given position.
    MismatchParen { pos: usize },
    /// A division by zero occurred at the given position.
    DivideByZero { pos: usize },
    /// The expression has trailing input.
    TrailingInput { pos: usize },
    /// The maximum expression depth was reached.
    MaxDepth,
    /// The expression resulted in an overflow.
    Overflow,
}

impl fmt::Display for CalcError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CalcError::Empty => write!(f, "empty expression"),
            CalcError::InvalidNumber { pos } => write!(f, "invalid number at position {}", pos),
            CalcError::Unexpected { pos, found } => {
                write!(f, "unexpected character '{}' at position {}", found, pos)
            }
            CalcError::MismatchParen { pos } => write!(f, "mismatched parenthesis at position {}", pos),
            CalcError::DivideByZero { pos } => write!(f, "division by zero at position {}", pos),
            CalcError::TrailingInput { pos } => write!(f, "trailing input at position {}", pos),
            CalcError::MaxDepth => write!(f, "maximum expression depth reached"),
            CalcError::Overflow => write!(f, "overflow"),
        }
    }
}

/// Evaluates an arithmetic expression.
pub fn evaluate(expr: &str) -> Result<f64, CalcError> {
    evaluate_with_opts(expr, &EvalOptions { precision: 6, raw: false })
}

/// Evaluates an arithmetic expression with the given options.
pub fn evaluate_with_opts(expr: &str, _opts: &EvalOptions) -> Result<f64, CalcError> {
    let mut parser = Parser::new(expr);
    let result = parser.parse_expr(0);
    if parser.pos < expr.len() {
        return Err(CalcError::TrailingInput { pos: parser.pos });
    }
    result
}

/// Formats a floating-point value to a string with the given precision.
pub fn format_value(v: f64, precision: usize, raw: bool) -> String {
    if raw {
        v.to_string()
    } else {
        format!("{:.1$}", v, precision)
    }
}

struct Parser<'a> {
    input: &'a str,
    pos: usize,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Self { input, pos: 0 }
    }

    fn parse_expr(&mut self, min_prec: u8) -> Result<f64, CalcError> {
        let mut left = self.parse_operand()?;

        while let Some(op) = self.peek_op() {
            let prec = op.prec();
            if prec < min_prec {
                break;
            }

            self.pos += 1;
            let right = self.parse_expr(op.assoc().next_prec(prec))?;
            left = op.eval(left, right).ok_or(CalcError::Overflow)?;
        }

        Ok(left)
    }

    fn parse_operand(&mut self) -> Result<f64, CalcError> {
        self.skip_whitespace();
        let start = self.pos;
        if let Some(c) = self.peek() {
            match c {
                '0'..='9' | '.' => self.parse_number(),
                '-' | '+' => {
                    self.pos += 1;
                    let operand = self.parse_expr(Op::UNARY_PREC)?;
                    if c == '-' {
                        Ok(-operand)
                    } else {
                        Ok(operand)
                    }
                }
                '(' => {
                    self.pos += 1;
                    let result = self.parse_expr(0);
                    self.skip_whitespace();
                    if self.peek() == Some(')') {
                        self.pos += 1;
                        result
                    } else {
                        Err(CalcError::MismatchParen { pos: self.pos })
                    }
                }
                _ => Err(CalcError::Unexpected { pos: start, found: c }),
            }
        } else {
            Err(CalcError::Empty)
        }
    }

    fn parse_number(&mut self) -> Result<f64, CalcError> {
        let start = self.pos;
        while let Some(c) = self.peek() {
            if !c.is_digit(10) && c != '.' {
                break;
            }
            self.pos += 1;
        }
        self.input[start..self.pos]
            .parse()
            .map_err(|_| CalcError::InvalidNumber { pos: start })
    }

    fn peek(&self) -> Option<char> {
        self.input.get(self.pos..).and_then(|s| s.chars().next())
    }

    fn peek_op(&mut self) -> Option<Op> {
        self.skip_whitespace();
        self.peek().and_then(Op::from_char)
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek() {
            if !c.is_whitespace() {
                break;
            }
            self.pos += 1;
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
}

impl Op {
    const UNARY_PREC: u8 = 5;

    fn from_char(c: char) -> Option<Self> {
        match c {
            '+' => Some(Op::Add),
            '-' => Some(Op::Sub),
            '*' => Some(Op::Mul),
            '/' => Some(Op::Div),
            '^' => Some(Op::Pow),
            _ => None,
        }
    }

    fn prec(&self) -> u8 {
        match self {
            Op::Add | Op::Sub => 1,
            Op::Mul | Op::Div => 2,
            Op::Pow => 4,
        }
    }

    fn assoc(&self) -> Assoc {
        match self {
            Op::Pow => Assoc::Right,
            _ => Assoc::Left,
        }
    }

    fn eval(&self, left: f64, right: f64) -> Option<f64> {
        match self {
            Op::Add => Some(left + right),
            Op::Sub => Some(left - right),
            Op::Mul => Some(left * right),
            Op::Div => {
                if right == 0.0 {
                    None
                } else {
                    Some(left / right)
                }
            }
            Op::Pow => Some(left.powf(right)),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Assoc {
    Left,
    Right,
}

impl Assoc {
    fn next_prec(&self, prec: u8) -> u8 {
        match self {
            Assoc::Left => prec + 1,
            Assoc::Right => prec,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_arithmetic() {
        assert_eq!(evaluate("1 + 2").unwrap(), 3.0);
        assert_eq!(evaluate("3 - 1").unwrap(), 2.0);
        assert_eq!(evaluate("2 * 3").unwrap(), 6.0);
        assert_eq!(evaluate("6 / 2").unwrap(), 3.0);
    }

    #[test]
    fn test_precedence() {
        assert_eq!(evaluate("1 + 2 * 3").unwrap(), 7.0);
        assert_eq!(evaluate("1 * 2 + 3").unwrap(), 5.0);
    }

    #[test]
    fn test_associativity() {
        assert_eq!(evaluate("8 - 4 - 2").unwrap(), 2.0);
        assert_eq!(evaluate("2 ^ 3 ^ 2").unwrap(), 512.0);
    }

    #[test]
    fn test_unary() {
        assert_eq!(evaluate("-1").unwrap(), -1.0);
        assert_eq!(evaluate("+1").unwrap(), 1.0);
        assert_eq!(evaluate("1 + -2").unwrap(), -1.0);
    }

    #[test]
    fn test_parentheses() {
        assert_eq!(evaluate("(1 + 2) * 3").unwrap(), 9.0);
    }

    #[test]
    fn test_division_by_zero() {
        assert!(matches!(evaluate("1 / 0"), Err(CalcError::Overflow)));
    }

    #[test]
    fn test_syntax_errors() {
        assert!(matches!(evaluate("1 +"), Err(CalcError::TrailingInput { .. })));
        assert!(matches!(evaluate("1 + * 2"), Err(CalcError::Unexpected { .. })));
    }

    #[test]
    fn test_formatting() {
        assert_eq!(format_value(1.23456789, 2, false), "1.23");
        assert_eq!(format_value(1.23456789, 4, false), "1.2346");
        assert_eq!(format_value(1.23, 4, false), "1.2300");
        assert_eq!(format_value(1.23456789, 2, true), "1.23456789");
    }
}
