use super::parser::{Expr, Literal};
extern crate im;
#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Unit,
    Float(f64),
    Int64(i64),
    Int32(i32),
    DataSet(Vec<String>, Vec<Vec<Value>>),
}

impl<'a> Literal {
    pub fn to_value(&self) -> Value {
        match self {
            Literal::Float(f) => Value::Float(*f),
            Literal::Int32(f) => Value::Int32(*f),
            Literal::Int64(f) => Value::Int64(*f),
        }
    }
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
                let x = e.get(*r).ok_or(format!("Ref {} not found", r))?;
                Ok(x.clone())
            }
            Expr::DataSet(header, values) => Ok(Value::DataSet(
                header.clone(),
                values
                    .into_iter()
                    .map(|y| y.into_iter().map(|x| x.eval(e).unwrap()).collect())
                    .collect(),
            )),

            Expr::Literal(l) => Ok(l.to_value()),
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
