use crate::token::Token;

pub(crate) fn expect(tk: Token, tokens: &[Token]) -> Result<&[Token], String> {
    if tokens[0] == tk {
        Ok(&tokens[1..])
    } else {
        Err(format!("Expected {:?}", tk))
    }
}

pub(crate) fn sequence_with_sep<'a, 'b, T>(
    parser: impl Fn(&'b [Token]) -> Result<(&'b [Token], T), String>,
    sep: Token,
    mut ts: &'b [Token],
) -> Result<(&'b [Token], Vec<T>), String> {
    let mut items = Vec::new();

    while let Ok((new_s, item)) = parser(ts) {
        items.push(item);
        match expect(sep.clone(), new_s) {
            Ok(new_s) => {
                ts = new_s;
            }
            Err(_) => return Ok((new_s, items)),
        }
    }
    Ok((ts, items))
}

pub(crate) fn ident(ts: &[Token]) -> Result<(&[Token], String), String> {
    if let Token::Ident(ident) = &ts[0] {
        Ok((&ts[1..], ident.clone()))
    } else {
        Err(format!("Expected identifier, got {:?}", ts[0]))
    }
}
