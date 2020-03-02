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
            Literal::Long(i) => write!(f, "{}", i),
            Literal::Float(i) => write!(f, "{}", i),
        }
    }
}

#[test]
fn test_display_long() {
    assert_eq!(format!("{}", Expr::Literal(Literal::Long(1))), "1")
}
#[test]
fn test_display_float() {
    assert_eq!(format!("{}", Expr::Literal(Literal::Float(1.1))), "1.1")
}
