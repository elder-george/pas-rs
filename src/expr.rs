use crate::parse::{expect, ident, sequence_with_sep};
use crate::token::{tokenize_to_vec, Token};

#[derive(Debug, PartialEq)]
pub(crate) enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Equal,
    NotEqual,
    GreaterEqual,
    Greater,
    LessEqual,
    Less,
    And,
    Or,
}

impl BinaryOp {
    pub(crate) fn parse(ts: &[Token]) -> Result<(&[Token], Self), String> {
        let op = Self::from_token(&ts[0])?;
        Ok((&ts[1..], op))
    }

    pub(crate) fn from_token(tk: &Token) -> Result<Self, String> {
        match tk {
            Token::Plus => Ok(Self::Add),
            Token::Minus => Ok(Self::Sub),
            Token::Star => Ok(Self::Mul),
            Token::Slash => Ok(Self::Div),
            Token::Equal => Ok(Self::Equal),
            Token::NotEqual => Ok(Self::NotEqual),
            Token::Greater => Ok(Self::Greater),
            Token::GreaterEqual => Ok(Self::GreaterEqual),
            Token::Less => Ok(Self::Less),
            Token::LessEqual => Ok(Self::LessEqual),
            Token::And => Ok(Self::And),
            Token::Or => Ok(Self::Or),
            _ => Err(format!("expected a binary operator but got {:?}", tk)),
        }
    }
}

#[derive(Debug, PartialEq)]
pub(crate) enum UnaryOp {
    Neg,
    Not,
}

impl UnaryOp {
    pub(crate) fn parse(ts: &[Token]) -> Result<(&[Token], Self), String> {
        let op = match ts[0] {
            Token::Minus => Ok(Self::Neg),
            Token::Not => Ok(Self::Not),
            _ => Err(format!("expected a unary operator but got {:?}", ts[0])),
        }?;
        Ok((&ts[1..], op))
    }
}

#[cfg(test)]
mod test_op {
    use super::*;
    #[test]
    fn test_parse() {
        assert_eq!(
            BinaryOp::parse(&[Token::Plus]),
            Ok((&[] as &[Token], BinaryOp::Add))
        );
    }

    #[test]
    fn test_parse_with_remainder() {
        assert_eq!(
            BinaryOp::parse(&[Token::Plus, Token::Number(42)]),
            Ok((&[Token::Number(42)] as &[Token], BinaryOp::Add))
        );
    }
}

#[derive(Debug, PartialEq)]
pub(crate) struct LValue {
    ident: String,
    indices: Option<Vec<Expr>>,
}

impl LValue {
    pub(crate) fn parse(ts: &[Token]) -> Result<(&[Token], Self), String> {
        let (ts, ident) = ident(ts)?;
        let (ts,indices) = Self::parse_indices(ts)?;
        Ok((
            ts,
            Self {
                ident: ident.clone(),
                indices,
            },
        ))
    }

    pub(crate) fn parse_indices(ts:&[Token]) -> Result<(&[Token], Option<Vec<Expr>>), String> {
        if let Ok(ts) = expect(Token::LeftBr, ts) {
            let (ts, indices) = sequence_with_sep(|ts| Expr::parse(ts), Token::Comma, ts)?;
            let ts = expect(Token::RightBr, ts)?;
            Ok((ts, Some(indices)))
        } else {
            Ok((ts, None))
        }
    }
}

#[cfg(test)]
mod lvalue_tests {
    use super::*;

    #[test]
    fn test_no_indices() {
        assert_eq!(
            LValue::parse(&tokenize_to_vec("abc")),
            Ok((
                &[Token::Eod] as &[Token],
                LValue {
                    ident: "abc".to_string(),
                    indices: None
                }
            ))
        );
    }

    #[test]
    fn test_with_indices() {
        assert_eq!(
            LValue::parse(&tokenize_to_vec("abc[0, 1]")),
            Ok((
                &[Token::Eod] as &[Token],
                LValue {
                    ident: "abc".to_string(),
                    indices: Some(vec![Expr::Number(0), Expr::Number(1),])
                }
            ))
        );
    }
}

#[derive(Debug, PartialEq)]
pub(crate) enum Expr {
    Number(i64),
    LValue(LValue),
    Call {
        fn_name: String,
        args: Vec<Expr>,
    },
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    Binary {
        op: BinaryOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
}

impl Expr {
    // Pascal operators precedence table: https://www.freepascal.org/docs-html/ref/refch12.html
    // so:
    // Expr -> Sum (RelOp Sum)*.
    // Sum -> Term (SumOp Term)*.
    // Term -> Factor (MulOp Factor)*.
    // Factor -> UnaryOp? Number | Ident | Call | '(' Expr')'.
    // (for comparison, C has 15 precedence levels)
    pub(crate) fn parse(ts: &[Token]) -> Result<(&[Token], Self), String> {
        let (mut ts, mut expr) = Self::sum(ts)?;
        while ts[0] == Token::Equal
            || ts[0] == Token::Greater
            || ts[0] == Token::GreaterEqual
            || ts[0] == Token::Less
            || ts[0] == Token::LessEqual
            || ts[0] == Token::NotEqual
        {
            let (new_ts, op) = BinaryOp::parse(ts)?;
            let (new_ts, rhs) = Self::sum(new_ts)?;
            expr = Self::Binary {
                op,
                lhs: Box::new(expr),
                rhs: Box::new(rhs),
            };
            ts = new_ts;
        }
        Ok((ts, expr))
    }

    fn sum(ts: &[Token]) -> Result<(&[Token], Self), String> {
        let (mut ts, mut expr) = Self::term(ts)?;
        while ts[0] == Token::Plus || ts[0] == Token::Minus || ts[0] == Token::Or {
            let (new_ts, op) = BinaryOp::parse(ts)?;
            let (new_ts, rhs) = Self::term(new_ts)?;
            expr = Self::Binary {
                op,
                lhs: Box::new(expr),
                rhs: Box::new(rhs),
            };
            ts = new_ts;
        }
        Ok((ts, expr))
    }

    fn term(ts: &[Token]) -> Result<(&[Token], Self), String> {
        let (mut ts, mut expr) = Self::factor(ts)?;
        while ts[0] == Token::Star || ts[0] == Token::Slash || ts[0] == Token::And {
            let (new_ts, op) = BinaryOp::parse(ts)?;
            let (new_ts, rhs) = Self::factor(new_ts)?;
            expr = Self::Binary {
                op,
                lhs: Box::new(expr),
                rhs: Box::new(rhs),
            };
            ts = new_ts;
        }
        Ok((ts, expr))
    }
    fn factor(ts: &[Token]) -> Result<(&[Token], Self), String> {
        if let Ok((ts, op)) = UnaryOp::parse(ts) {
            // call itself recursively and wrap into a Unary.
            let (ts, expr) = Self::factor(ts)?;
            Ok((
                ts,
                Self::Unary {
                    op,
                    expr: Box::new(expr),
                },
            ))
        } else if let Ok(ts) = expect(Token::LeftPar, ts) {
            let (ts, expr) = Self::parse(ts)?;
            let ts = expect(Token::RightPar, ts)?;
            Ok((ts, expr))
        } else if let Ok((ts, ident)) = ident(ts) {
            if let Ok(ts) = expect(Token::LeftPar, ts) {
                let (ts, args) = Self::parse_args(ts)?;
                let ts = expect(Token::RightPar, ts)?;
                Ok((
                    ts,
                    Expr::Call {
                        fn_name: ident,
                        args,
                    },
                ))
            } else {
                let (ts, indices) = LValue::parse_indices(ts)?;
                Ok((ts, Expr::LValue(LValue{
                    ident, indices
                })))
            }
        } else {
            match &ts[0] {
                Token::Number(num) => Ok((&ts[1..], Self::Number(*num))),
                _ => Err("Unexpected stuff".to_string()),
            }
        }
    }

    fn parse_args(ts: &[Token]) -> Result<(&[Token], Vec<Expr>), String> {
        sequence_with_sep(|ts| Expr::parse(ts), Token::Comma, ts)
    }
}

#[cfg(test)]
mod expr_tests {
    use super::*;

    #[test]
    fn test_parse_number() {
        assert_eq!(
            Expr::parse(&tokenize_to_vec("42")),
            Ok((&[Token::Eod] as &[Token], Expr::Number(42)))
        );
    }
    #[test]
    fn test_parse_sum_of_two_numbers() {
        assert_eq!(
            Expr::parse(&tokenize_to_vec("42 + 42")),
            Ok((
                &[Token::Eod] as &[Token],
                Expr::Binary {
                    op: BinaryOp::Add,
                    lhs: Box::new(Expr::Number(42)),
                    rhs: Box::new(Expr::Number(42))
                }
            ))
        );
    }

    #[test]
    fn test_parse_product_of_two_numbers() {
        assert_eq!(
            Expr::parse(&tokenize_to_vec("42 * 42")),
            Ok((
                &[Token::Eod] as &[Token],
                Expr::Binary {
                    op: BinaryOp::Mul,
                    lhs: Box::new(Expr::Number(42)),
                    rhs: Box::new(Expr::Number(42))
                }
            ))
        );
    }

    #[test]
    fn test_parse_priorities() {
        assert_eq!(
            Expr::parse(&tokenize_to_vec("42 - 42 / 42")),
            Ok((
                &[Token::Eod] as &[Token],
                Expr::Binary {
                    op: BinaryOp::Sub,
                    lhs: Box::new(Expr::Number(42)),
                    rhs: Box::new(Expr::Binary {
                        op: BinaryOp::Div,
                        lhs: Box::new(Expr::Number(42)),
                        rhs: Box::new(Expr::Number(42))
                    })
                }
            ))
        );
    }

    #[test]
    fn test_parse_parentheses() {
        assert_eq!(
            Expr::parse(&tokenize_to_vec("(42 - 42) / 42")),
            Ok((
                &[Token::Eod] as &[Token],
                Expr::Binary {
                    op: BinaryOp::Div,
                    lhs: Box::new(Expr::Binary {
                        op: BinaryOp::Sub,
                        lhs: Box::new(Expr::Number(42)),
                        rhs: Box::new(Expr::Number(42))
                    }),
                    rhs: Box::new(Expr::Number(42)),
                }
            ))
        );
    }
    #[test]
    fn test_unary() {
        assert_eq!(
            Expr::parse(&tokenize_to_vec("42 + -42")),
            Ok((
                &[Token::Eod] as &[Token],
                Expr::Binary {
                    op: BinaryOp::Add,
                    lhs: Box::new(Expr::Number(42)),
                    rhs: Box::new(Expr::Unary {
                        op: UnaryOp::Neg,
                        expr: Box::new(Expr::Number(42)),
                    })
                },
            ))
        );
    }

    #[test]
    fn logical_operators() {
        assert_eq!(
            // Relational operators have the lowest precedence, thus the parentheses
            Expr::parse(&tokenize_to_vec("(a > 0) or (a <= 0) and not b")),
            Ok((
                &[Token::Eod] as &[Token],
                Expr::Binary {
                    op: BinaryOp::Or,
                    lhs: Box::new(Expr::Binary {
                        op: BinaryOp::Greater,
                        lhs: Box::new(Expr::LValue(LValue {
                            ident: "a".to_string(),
                            indices: None
                        })),
                        rhs: Box::new(Expr::Number(0)),
                    }),
                    rhs: Box::new(Expr::Binary {
                        op: BinaryOp::And,
                        lhs: Box::new(Expr::Binary {
                            op: BinaryOp::LessEqual,
                            lhs: Box::new(Expr::LValue(LValue {
                                ident: "a".to_string(),
                                indices: None
                            })),
                            rhs: Box::new(Expr::Number(0)),
                        }),
                        rhs: Box::new(Expr::Unary {
                            op: UnaryOp::Not,
                            expr: Box::new(Expr::LValue(LValue {
                                ident: "b".to_string(),
                                indices: None
                            })),
                        })
                    })
                }
            ))
        );
    }

    #[test]
    fn test_identifiers() {
        assert_eq!(
            Expr::parse(&tokenize_to_vec("abc")),
            Ok((
                &[Token::Eod] as &[Token],
                Expr::LValue(LValue {
                    ident: "abc".to_string(),
                    indices: None
                })
            ))
        );
    }

    #[test]
    fn test_call_without_args() {
        assert_eq!(
            Expr::parse(&tokenize_to_vec("abc()")),
            Ok((
                &[Token::Eod] as &[Token],
                Expr::Call {
                    fn_name: "abc".to_string(),
                    args: Vec::new()
                }
            ))
        );
    }

    #[test]
    fn test_call_with_args() {
        assert_eq!(
            Expr::parse(&tokenize_to_vec("abc(1,2,3)")),
            Ok((
                &[Token::Eod] as &[Token],
                Expr::Call {
                    fn_name: "abc".to_string(),
                    args: vec![Expr::Number(1), Expr::Number(2), Expr::Number(3)]
                }
            ))
        );
    }

    #[test]
    fn test_array_access() {
        assert_eq!(
            Expr::parse(&tokenize_to_vec("abc[1,2,3]")),
            Ok((
                &[Token::Eod] as &[Token],
                Expr::LValue(LValue {
                    ident: "abc".to_string(),
                    indices: Some(vec![Expr::Number(1), Expr::Number(2), Expr::Number(3)])
                })
            ))
        );
    }
}
