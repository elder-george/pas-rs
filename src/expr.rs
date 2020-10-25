use crate::parse::{expect, sequence_with_sep};
use crate::token::{tokenize_to_vec, Token};

#[derive(Debug, PartialEq)]
pub(crate) enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Equal,
    GreaterEqual,
    Greater,
    LessEqual,
    Less,
}

impl BinaryOp {
    pub(crate) fn parse(ts: &[Token]) -> Result<(&[Token], Self), String> {
        let op = match ts[0] {
            Token::Plus => Ok(Self::Add),
            Token::Minus => Ok(Self::Sub),
            Token::Star => Ok(Self::Mul),
            Token::Slash => Ok(Self::Div),
            Token::Equal => Ok(Self::Equal),
            Token::Greater => Ok(Self::Greater),
            Token::GreaterEqual => Ok(Self::GreaterEqual),
            Token::Less => Ok(Self::Less),
            Token::LessEqual => Ok(Self::LessEqual),
            _ => Err(format!("expected a binary operator but got {:?}", ts[0])),
        }?;
        Ok((&ts[1..], op))
    }
}

#[derive(Debug, PartialEq)]
pub(crate) enum UnaryOp {
    Neg,
}

impl UnaryOp {
    pub(crate) fn parse(ts: &[Token]) -> Result<(&[Token], Self), String> {
        let op = match ts[0] {
            Token::Minus => Ok(Self::Neg),
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
pub(crate) struct Number(pub(crate) i64);

impl Number {
    pub(crate) fn parse(ts: &[Token]) -> Result<(&[Token], Self), String> {
        match ts[0] {
            Token::Number(n) => Ok((&ts[1..], Self(n))),
            _ => Err(format!("expected number, got {:?}", ts[0])),
        }
    }
}

#[cfg(test)]
mod number_tests {
    use super::*;

    #[test]
    fn test_parse_number() {
        assert_eq!(
            Number::parse(&[Token::Number(42)]),
            Ok((&[] as &[Token], Number(42)))
        );
    }
}

#[derive(Debug, PartialEq)]
pub(crate) enum Expr {
    Number(Number),
    Ident(String),
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
    pub(crate) fn parse(ts: &[Token]) -> Result<(&[Token], Self), String> {
        let (mut ts, mut expr) = Self::term(ts)?;
        while ts[0] == Token::Plus || ts[0] == Token::Minus {
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
        while ts[0] == Token::Star || ts[0] == Token::Slash {
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
        } else {
            match &ts[0] {
                Token::Number(_) => {
                    let (ts, num) = Number::parse(ts)?;
                    Ok((ts, Self::Number(num)))
                }
                Token::LeftPar => {
                    let ts = expect(Token::LeftPar, ts)?;
                    let (ts, expr) = Self::parse(ts)?;
                    let ts = expect(Token::RightPar, ts)?;
                    Ok((ts, expr))
                }
                Token::Ident(s) => {
                    if let Ok(ts) = expect(Token::LeftPar, &ts[1..]) {
                        let (ts, args) = Self::parse_args(ts)?;
                        let ts = expect(Token::RightPar, ts)?;
                        Ok((
                            ts,
                            Expr::Call {
                                fn_name: s.to_string(),
                                args,
                            },
                        ))
                    } else {
                        Ok((&ts[1..], Expr::Ident(s.to_string())))
                    }
                }
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
            Ok((&[Token::Eod] as &[Token], Expr::Number(Number(42))))
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
                    lhs: Box::new(Expr::Number(Number(42))),
                    rhs: Box::new(Expr::Number(Number(42)))
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
                    lhs: Box::new(Expr::Number(Number(42))),
                    rhs: Box::new(Expr::Number(Number(42)))
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
                    lhs: Box::new(Expr::Number(Number(42))),
                    rhs: Box::new(Expr::Binary {
                        op: BinaryOp::Div,
                        lhs: Box::new(Expr::Number(Number(42))),
                        rhs: Box::new(Expr::Number(Number(42)))
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
                        lhs: Box::new(Expr::Number(Number(42))),
                        rhs: Box::new(Expr::Number(Number(42)))
                    }),
                    rhs: Box::new(Expr::Number(Number(42))),
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
                    lhs: Box::new(Expr::Number(Number(42))),
                    rhs: Box::new(Expr::Unary {
                        op: UnaryOp::Neg,
                        expr: Box::new(Expr::Number(Number(42))),
                    })
                },
            ))
        );
    }

    #[test]
    fn test_identifiers() {
        assert_eq!(
            Expr::parse(&tokenize_to_vec("abc")),
            Ok((&[Token::Eod] as &[Token], Expr::Ident("abc".to_string())))
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
                    args: vec![
                        Expr::Number(Number(1)),
                        Expr::Number(Number(2)),
                        Expr::Number(Number(3))
                    ]
                }
            ))
        );
    }
}
