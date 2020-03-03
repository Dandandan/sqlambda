use super::parser::{Expr, Literal};

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
pub enum Type {
    Int64,
    Int32,
    Float,
}

impl<'a> Expr<'a> {
    pub fn get_type(
        &self,
        // TODO use type environment
        env: &'a mut std::collections::HashMap<String, Type>,
    ) -> Result<Type, String> {
        match self {
            Expr::Literal(l) => Ok(l.get_type()),
            Expr::Ref(x) => {
                let err = format!("Could not find reference {}", x);
                env.get(*x).map(|x| *x).ok_or(err)
            }
            Expr::LetIn(x) => {
                let ty1 = x.expr1.expr.get_type(env)?;
                env.insert(x.name.to_string(), ty1);
                x.expr2.expr.get_type(env)
            }
            _ => Err("unsupported".to_string()),
        }
    }
}

impl Literal {
    fn get_type(&self) -> Type {
        match self {
            Literal::Int64(_) => Type::Int64,
            Literal::Int32(_) => Type::Int32,
            Literal::Float(_) => Type::Float,
        }
    }
}

#[test]
fn test_type() {
    assert_eq!(Literal::Float(1.0).get_type(), Type::Float);
    assert_eq!(Literal::Int64(1).get_type(), Type::Int64);
}
