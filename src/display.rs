use super::eval::Value;
use super::parser::{Expr, Literal};
use std::fmt;

impl<'a> fmt::Display for Expr<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Literal(l) => write!(f, "{}", l),
            _ => write!(f, ""),
        }
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::Int64(i) => write!(f, "{}", i),
            Literal::Int32(i) => write!(f, "{}", i),
            Literal::Float(i) => write!(f, "{}", i),
        }
    }
}
impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Int64(i) => write!(f, "{}", i),
            Value::Int32(i) => write!(f, "{}", i),
            Value::Float(i) => write!(f, "{}", i),
            Value::DataSet(i, values) => write!(f, "{:?} {:?}", i, values),
            Value::Unit => write!(f, "()"),
        }
    }
}
#[test]
fn test_display_long() {
    assert_eq!(format!("{}", Expr::Literal(Literal::Int64(1))), "1")
}
#[test]
fn test_display_float() {
    assert_eq!(format!("{}", Expr::Literal(Literal::Float(1.1))), "1.1")
}
