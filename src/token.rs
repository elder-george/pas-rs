use logos::Logos;

#[derive(Debug, PartialEq, Clone, Logos)]
pub(crate) enum Token {
    #[token("begin")]
    Begin,
    #[token("end")]
    End,
    #[token("if")]
    If,
    #[token("then")]
    Then,
    #[token("else")]
    Else,
    #[regex("[a-zA-Z][a-zA-Z0-9]*", |lex| lex.slice().to_string())]
    Ident(String),
    #[regex(r"[0-9]+", |lex| lex.slice().parse())]
    Number(i64),
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token(":=")]
    Assign,
    #[token("=")]
    Equal,
    #[token("<>")]
    NotEqual,
    #[token(">")]
    Greater,
    #[token(">=")]
    GreaterEqual,
    #[token("<")]
    Less,
    #[token("<=")]
    LessEqual,
    #[token("and")]
    And,
    #[token("or")]
    Or,
    #[token("not")]
    Not,            
    #[token("(")]
    LeftPar,
    #[token(")")]
    RightPar,
    #[token(",")]
    Comma,
    #[token(";")]
    Semicolon,
    Eod, // fake node to terminate token stream
    #[error]
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Error,
}

pub(crate) fn tokenize_to_vec(s: &str) -> Vec<Token> {
    let mut lex = Token::lexer(s);
    let mut tokens = Vec::new();
    while let Some(token) = lex.next() {
        tokens.push(token);
    }
    tokens.push(Token::Eod);
    tokens
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_expression() {
        let tokens = tokenize_to_vec("42 + -95");
        assert_eq!(
            vec![
                Token::Number(42),
                Token::Plus,
                Token::Minus,
                Token::Number(95),
                Token::Eod,
            ],
            tokens
        );
    }

    #[test]
    fn test_if_stmt() {
        let tokens = tokenize_to_vec(
            "if a + b >= 0 then
                a := 42
            else begin
                b := 42
            end",
        );
        assert_eq!(
            vec![
                Token::If,
                Token::Ident("a".to_string()),
                Token::Plus,
                Token::Ident("b".to_string()),
                Token::GreaterEqual,
                Token::Number(0),
                Token::Then,
                Token::Ident("a".to_string()),
                Token::Assign,
                Token::Number(42),
                Token::Else,
                Token::Begin,
                Token::Ident("b".to_string()),
                Token::Assign,
                Token::Number(42),
                Token::End,
                Token::Eod,
            ],
            tokens
        );
    }
}
