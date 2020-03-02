use super::parser::{Expr, Literal};

#[derive(Eq, PartialEq, Debug)]
pub enum Type {
    Long,
    Float,
}

impl<'a> Expr<'a> {
    pub fn get_type(&self) -> Option<Type> {
        match self {
            Expr::Literal(l) => Some(l.get_type()),
            _ => None,
        }
    }
}

impl Literal {
    fn get_type(&self) -> Type {
        match self {
            Literal::Long(_) => Type::Long,
            Literal::Float(_) => Type::Float,
        }
    }
}

#[test]
fn test_type() {
    assert_eq!(Literal::Float(1.0).get_type(), Type::Float);
    assert_eq!(Literal::Long(1).get_type(), Type::Long);
}
