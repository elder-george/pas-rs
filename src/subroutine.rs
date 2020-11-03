use crate::parse::{expect, ident, sequence_with_sep};
use crate::stmt::Stmt;
use crate::token::Token;
use crate::types::Type;

#[derive(Debug, PartialEq)]
pub(crate) struct Var(pub(crate) String, pub(crate) Type);

impl Var {
    pub(crate) fn parse(ts: &[Token]) -> Result<(&[Token], Self), String> {
        let (ts, ident) = ident(ts)?;
        let ts = expect(&Token::Colon, ts)?;
        let (ts, param_type) = Type::parse(ts)?;
        Ok((ts, Var(ident, param_type)))
    }
}

#[derive(Debug, PartialEq)]
enum Subroutine {
    Proc {
        name: String,
        params: Vec<Var>,
        vars: Vec<Var>,
        body: Stmt,
    },
    Func {
        name: String,
        params: Vec<Var>,
        return_type: Type,
        vars: Vec<Var>,
        body: Stmt,
    },
}

impl Subroutine {
    pub(crate) fn parse(ts: &[Token]) -> Result<(&[Token], Self), String> {
        if let Ok(ts) = expect(&Token::Procedure, ts) {
            let (ts, name) = ident(ts)?;
            let (ts, params) = Self::parse_param_list(ts)?;
            let ts = expect(&Token::Semicolon, ts)?; // never understood why this is here.
            let (ts, vars) = Self::parse_vars(ts)?;
            let (ts, body) = Self::parse_body(ts)?;
            Ok((
                ts,
                Self::Proc {
                    name,
                    params,
                    vars,
                    body,
                },
            ))
        } else if let Ok(ts) = expect(&Token::Function, ts) {
            let (ts, name) = ident(ts)?;
            let (ts, params) = Self::parse_param_list(ts)?;
            let ts = expect(&Token::Colon, ts)?;
            let (ts, return_type) = Type::parse(ts)?;
            let ts = expect(&Token::Semicolon, ts)?; // never understood why this is here.
            let (ts, vars) = Self::parse_vars(ts)?;
            let (ts, body) = Self::parse_body(ts)?;
            Ok((
                ts,
                Self::Func {
                    name,
                    params,
                    return_type,
                    vars,
                    body,
                },
            ))
        } else {
            Err(format!("Expected subroutine, got {:?}", ts[0]))
        }
    }

    fn parse_param_list(ts: &[Token]) -> Result<(&[Token], Vec<Var>), String> {
        let ts = expect(&Token::LeftPar, ts)?;
        // Actual Pascal parameter definition is more complex, but let's simplify it
        let (ts, params) = sequence_with_sep(Var::parse, &Token::Comma, ts)?;
        let ts = expect(&Token::RightPar, ts)?;
        Ok((ts, params))
    }

    fn parse_vars(ts: &[Token]) -> Result<(&[Token], Vec<Var>), String> {
        if let Ok(ts) = expect(&Token::Var, ts) {
            let (ts, vars) = sequence_with_sep(Var::parse, &Token::Semicolon, ts)?;
            Ok((ts, vars))
        } else {
            Ok((ts, Vec::new()))
        }
    }

    fn parse_body(ts: &[Token]) -> Result<(&[Token], Stmt), String> {
        let (ts, body) = Stmt::parse(ts)?;
        if let Stmt::Block { stmts: _ } = body {
            Ok((ts, body))
        } else {
            Err("Expected block".to_string())
        }
    }
}

#[cfg(test)]
mod subroutine_tests {
    use super::*;
    use crate::expr::{BinaryOp, Expr, LValue};
    use crate::token::tokenize_to_vec;

    #[test]
    fn procedure() {
        assert_parses_into!(
            Subroutine::parse(&tokenize_to_vec(
                "procedure foo();
                var bar: baz;
                begin
                end
                "
            )),
            Subroutine::Proc {
                name: "foo".to_string(),
                params: Vec::new(),
                vars: vec![Var(
                    "bar".to_string(),
                    Type::Simple {
                        name: "baz".to_string()
                    }
                )],
                body: Stmt::Block { stmts: Vec::new() }
            }
        );
    }

    #[test]
    fn function() {
        assert_parses_into!(
            Subroutine::parse(&tokenize_to_vec(
                "function foo(bar: baz, quux:quuux): result;
                var abc: array[1..10] of string;
                begin
                    foo := bar + quux
                end"
            )),
            Subroutine::Func {
                name: "foo".to_string(),
                params: vec![
                    Var(
                        "bar".to_string(),
                        Type::Simple {
                            name: "baz".to_string()
                        }
                    ),
                    Var(
                        "quux".to_string(),
                        Type::Simple {
                            name: "quuux".to_string()
                        }
                    )
                ],
                return_type: Type::Simple {
                    name: "result".to_string()
                },
                vars: vec![Var(
                    "abc".to_string(),
                    Type::Array {
                        elem_type: Box::new(Type::Simple {
                            name: "string".to_string()
                        }),
                        indices: vec![Type::Range { start: 1, end: 10 }]
                    }
                )],
                body: Stmt::Block {
                    stmts: vec![Stmt::Assign {
                        lhs: LValue {
                            ident: "foo".to_string(),
                            indices: None,
                        },
                        rhs: Expr::Binary {
                            op: BinaryOp::Add,
                            lhs: Box::new(Expr::LValue(LValue {
                                ident: "bar".to_string(),
                                indices: None
                            })),
                            rhs: Box::new(Expr::LValue(LValue {
                                ident: "quux".to_string(),
                                indices: None
                            }))
                        }
                    }]
                }
            }
        )
    }
}
