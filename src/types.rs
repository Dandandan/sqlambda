use super::parser::{Expr, Literal};

#[derive(Eq, PartialEq, Debug, Clone)]
pub enum Type {
    Int64,
    Int32,
    Float,
    Dataset(Vec<String>, Vec<Type>),
}

fn get_item_types_inner(
    items: &[Vec<Expr>],
    index: usize,
    env: &im::HashMap<String, Type>,
) -> Type {
    let values = items.iter().map(|y| y.get(index).unwrap().clone());

    values
        .map(|x| x.get_type(env).unwrap())
        .collect::<Vec<Type>>()
        .get(0)
        .unwrap()
        .clone()
}

fn get_item_types(items: &[Vec<Expr>], env: &im::HashMap<String, Type>) -> Vec<Type> {
    items
        .iter()
        .enumerate()
        .map(|(i, _)| get_item_types_inner(items, i, env))
        .collect()
}

impl<'a> Expr<'_> {
    pub fn get_type(&self, env: &im::HashMap<String, Type>) -> Result<Type, String> {
        match self {
            Expr::Literal(l) => Ok(l.get_type()),
            Expr::Ref(x) => {
                let err = format!("Could not find reference {}", x);
                env.get(*x).cloned().ok_or(err)
            }
            Expr::LetIn(x) => {
                let ty1 = x.expr1.expr.get_type(env)?;
                let type_env1 = env.update(x.name.to_string(), ty1);
                x.expr2.expr.get_type(&type_env1)
            }
            Expr::DataSet(names, items) => {
                Ok(Type::Dataset(names.clone(), get_item_types(items, env)))
            }
            _ => Err("not implemented".to_string()),
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
