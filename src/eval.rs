use super::parser::{Expr, Literal};
extern crate im;
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Value {
    Unit,
    Float(f64),
    Int64(i64),
    Int32(i32),
}

impl<'a> Expr<'_> {
    pub fn eval(&self, e: &im::HashMap<String, Value>) -> Result<Value, String> {
        match self {
            Expr::Equation(_x) => {
                unimplemented!();
            }
            Expr::Comment(_x) => {
                unimplemented!();
            }
            Expr::LetIn(l) => {
                let r = l.expr1.expr.eval(e)?;
                let m = e.update(l.name.to_string(), r);
                let res = l.expr2.as_ref().expr.eval(&m)?;

                Ok(res)
            }
            Expr::Ref(r) => {
                let x = e.get(*r).ok_or("Ref not found")?;
                Ok(*x)
            }

            Expr::Literal(Literal::Float(f)) => Ok(Value::Float(*f)),
            Expr::Literal(Literal::Int32(f)) => Ok(Value::Int32(*f)),
            Expr::Literal(Literal::Int64(f)) => Ok(Value::Int64(*f)),
        }
    }
}

#[cfg(test)]
#[test]
fn test_eval() {
    assert_eq!(
        Expr::Literal(Literal::Int64(1)),
        Expr::Literal(Literal::Int64(1))
    );
}
