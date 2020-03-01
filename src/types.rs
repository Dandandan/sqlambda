use super::parser::Literal;
#[derive(Eq, PartialEq, Debug)]
enum Type {
    Long,
    Float,
}

impl Literal {
    fn get_type(self) -> Type {
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
