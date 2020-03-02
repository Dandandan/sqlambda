use super::parser::Expr;

impl<'a> Expr<'a> {
    pub fn eval(&self) -> &Expr<'a> {
        match self {
            Expr::Equation(_x) => {
                unimplemented!();
            }
            Expr::Comment(_x) => {
                unimplemented!();
            }
            x @ Expr::Literal(_) => x,
        }
    }
}

#[cfg(test)]
use super::parser::Literal;
#[test]
fn test_eval() {
    assert_eq!(
        Expr::Literal(Literal::Long(1)),
        Expr::Literal(Literal::Long(1))
    );
}
