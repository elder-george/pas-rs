use crate::expr::{Expr, LValue};
use crate::parse::{expect, sequence_with_sep};
use crate::token::Token;

#[derive(Debug, PartialEq)]
pub(crate) enum Stmt {
    Assign {
        lhs: LValue,
        rhs: Expr,
    },
    If {
        cond: Expr,
        on_then: Box<Stmt>,
        on_else: Option<Box<Stmt>>,
    },
    While {
        cond: Expr,
        body: Box<Stmt>,
    },
    Block {
        stmts: Vec<Stmt>,
    },
    Call {
        fn_name: String,
        args: Vec<Expr>,
    },
}

impl Stmt {
    pub(crate) fn parse(ts: &[Token]) -> Result<(&[Token], Self), String> {
        if let Ok(ts) = expect(Token::If, ts) {
            let (ts, cond) = Expr::parse(ts)?;
            let ts = expect(Token::Then, ts)?;
            let (mut ts, on_then) = Stmt::parse(ts)?;
            let mut on_else = None;
            if let Ok(new_ts) = expect(Token::Else, ts) {
                let (new_ts, stmt) = Stmt::parse(new_ts)?;
                on_else = Some(stmt);
                ts = new_ts;
            }
            Ok((
                ts,
                Self::If {
                    cond,
                    on_then: Box::new(on_then),
                    on_else: on_else.map(Box::new),
                },
            ))
        } else if let Ok(ts) = expect(Token::Begin, ts) {
            let (ts, stmts) = sequence_with_sep(Stmt::parse, Token::Semicolon, ts)?;
            let ts = expect(Token::End, ts)?;
            Ok((ts, Self::Block { stmts }))
        } else if let Ok((ts, Expr::Call { fn_name, args })) = Expr::parse(ts) {
            // this feels like a cheat, TBH
            Ok((ts, Stmt::Call { fn_name, args }))
        } else if let Ok((ts, lhs)) = LValue::parse(ts) {
            // Note that `Assign` should go after `Call`
            let ts = expect(Token::Assign, ts)?;
            let (ts, rhs) = Expr::parse(ts)?;
            Ok((ts, Self::Assign { lhs, rhs }))
        } else if let Ok(ts) = expect(Token::While, ts) {
            let (ts, cond) = Expr::parse(ts)?;
            let ts = expect(Token::Do, ts)?;
            let (ts, body) = Stmt::parse(ts)?;
            Ok((
                ts,
                Self::While {
                    cond,
                    body: Box::new(body),
                },
            ))
        } else {
            Err(format!("Expected statement but got {:?}", &ts[0]))
        }
    }
}

#[cfg(test)]
mod stmt_tests {
    use super::*;
    use crate::expr::BinaryOp;
    use crate::token::tokenize_to_vec;

    #[test]
    fn block() {
        assert_parses_into!(
            Stmt::parse(&tokenize_to_vec("begin end")),
            Stmt::Block { stmts: Vec::new() }
        );
        assert_parses_into!(
            Stmt::parse(&tokenize_to_vec("begin a:=42 end")),
            Stmt::Block {
                stmts: vec![Stmt::Assign {
                    lhs: LValue {
                        ident: "a".to_string(),
                        indices: None
                    },
                    rhs: Expr::Number(42)
                }]
            }
        );
        assert_parses_into!(
            Stmt::parse(&tokenize_to_vec("begin a:=42; write(a) end")),
            Stmt::Block {
                stmts: vec![
                    Stmt::Assign {
                        lhs: LValue {
                            ident: "a".to_string(),
                            indices: None
                        },
                        rhs: Expr::Number(42)
                    },
                    Stmt::Call {
                        fn_name: "write".to_string(),
                        args: vec![Expr::LValue(LValue {
                            ident: "a".to_string(),
                            indices: None
                        })]
                    }
                ]
            }
        );
    }
    #[test]
    fn assignment() {
        assert_parses_into!(
            Stmt::parse(&tokenize_to_vec("a := 42")),
            Stmt::Assign {
                lhs: LValue {
                    ident: "a".to_string(),
                    indices: None,
                },
                rhs: Expr::Number(42),
            }
        );
        assert_parses_into!(
            Stmt::parse(&tokenize_to_vec("a[0] := 2 + 3")),
            Stmt::Assign {
                lhs: LValue {
                    ident: "a".to_string(),
                    indices: Some(vec![Expr::Number(0)]),
                },
                rhs: Expr::Binary {
                    op: BinaryOp::Add,
                    lhs: Box::new(Expr::Number(2)),
                    rhs: Box::new(Expr::Number(3)),
                },
            }
        );
    }

    #[test]
    fn call() {
        assert_parses_into!(
            Stmt::parse(&tokenize_to_vec("writeln()")),
            Stmt::Call {
                fn_name: "writeln".to_string(),
                args: Vec::new()
            }
        );
        assert_parses_into!(
            Stmt::parse(&tokenize_to_vec("writeln(a + 1, b)")),
            Stmt::Call {
                fn_name: "writeln".to_string(),
                args: vec![
                    Expr::Binary {
                        op: BinaryOp::Add,
                        lhs: Box::new(Expr::LValue(LValue {
                            ident: "a".to_string(),
                            indices: None
                        })),
                        rhs: Box::new(Expr::Number(1)),
                    },
                    Expr::LValue(LValue {
                        ident: "b".to_string(),
                        indices: None
                    })
                ]
            }
        );
    }

    #[test]
    fn if_then() {
        assert_parses_into!(
            Stmt::parse(&tokenize_to_vec("if a then begin end")),
            Stmt::If {
                cond: Expr::LValue(LValue {
                    ident: "a".to_string(),
                    indices: None,
                }),
                on_then: Box::new(Stmt::Block { stmts: Vec::new() }),
                on_else: None,
            }
        );
    }

    #[test]
    fn if_then_else() {
        assert_parses_into!(
            Stmt::parse(&tokenize_to_vec("if a then begin end else begin end")),
            Stmt::If {
                cond: Expr::LValue(LValue {
                    ident: "a".to_string(),
                    indices: None,
                }),
                on_then: Box::new(Stmt::Block { stmts: Vec::new() }),
                on_else: Some(Box::new(Stmt::Block { stmts: Vec::new() })),
            }
        );
    }

    #[test]
    fn nested_ifs() {
        assert_parses_into!(
            Stmt::parse(&tokenize_to_vec(
                "if a then if b then begin end else begin end"
            )),
            Stmt::If {
                cond: Expr::LValue(LValue {
                    ident: "a".to_string(),
                    indices: None,
                }),
                on_then: Box::new(Stmt::If {
                    cond: Expr::LValue(LValue {
                        ident: "b".to_string(),
                        indices: None
                    }),
                    on_then: Box::new(Stmt::Block { stmts: Vec::new() }),
                    on_else: Some(Box::new(Stmt::Block { stmts: Vec::new() }))
                }),
                on_else: None,
            }
        );
    }

    #[test]
    fn while_loop() {
        assert_parses_into!(
            Stmt::parse(&tokenize_to_vec("while foo do bar()")),
            Stmt::While {
                cond: Expr::LValue(LValue {
                    ident: "foo".to_string(),
                    indices: None
                }),
                body: Box::new(Stmt::Call {
                    fn_name: "bar".to_string(),
                    args: Vec::new()
                })
            }
        );
    }
}
