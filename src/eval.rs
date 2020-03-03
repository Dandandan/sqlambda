use super::parser::Expr;

impl<'a> Expr<'a> {
    pub fn eval(&'a self, e: &mut std::collections::HashMap<String, &'a Expr<'a>>) -> &'a Expr<'a> {
        match self {
            Expr::Equation(_x) => {
                unimplemented!();
            }
            Expr::Comment(_x) => {
                unimplemented!();
            }
            Expr::LetIn(l) => {
                let r = l.expr1.expr.eval(e);
                e.insert(l.name.to_string(), r);
                l.expr2.as_ref().expr.eval(e)
            }
            Expr::Ref(r) => e.get(*r).unwrap(),

            x @ Expr::Literal(_) => x,
        }
    }
}

#[cfg(test)]
use super::parser::Literal;
#[test]
fn test_eval() {
    assert_eq!(
        Expr::Literal(Literal::Int64(1)),
        Expr::Literal(Literal::Int64(1))
    );
}
