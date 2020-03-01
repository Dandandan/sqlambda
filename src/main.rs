use std::fs::File;
use std::io;
use std::io::prelude::*;

extern crate nom;
extern crate nom_locate;

use nom_locate::{position, LocatedSpan};

type Span<'a> = LocatedSpan<&'a str>;

use nom::{
    bytes::complete::{tag, take_while1},
    IResult,
};

fn decl_parser(i: Span) -> IResult<Span, Span> {
    take_while1(|c: char| c.is_alphabetic())(i)
}

fn lit_parser(i: Span) -> IResult<Span, Span> {
    take_while1(|c: char| c.is_ascii_digit())(i)
}

fn parser(i: Span) -> IResult<Span, AnnotatedExpr> {
    let (_, pos) = position(i)?;

    let (i, name) = decl_parser(i)?;

    let (i, _d) = tag("=")(i)?;
    let (_, pos2) = position(i)?;

    let (i, expr) = lit_parser(i)?;
    let parsed_integer = expr.fragment().parse().unwrap();
    Ok((
        i,
        AnnotatedExpr {
            span: pos,
            expr: Expr::Equation(Equation {
                name: &name.fragment(),
                expr: Box::new(AnnotatedExpr {
                    span: pos2,
                    expr: Expr::Literal(Literal::Long(parsed_integer)),
                }),
            }),
        },
    ))
}
#[derive(Debug, Eq, PartialEq)]
struct AnnotatedExpr<'a> {
    expr: Expr<'a>,
    span: Span<'a>,
}
#[derive(Debug, Eq, PartialEq)]
enum Expr<'a> {
    /// Binds an expression to a name
    Equation(Equation<'a>),

    /// Literal values
    Literal(Literal),
}

#[derive(Debug, Eq, PartialEq)]
struct Equation<'a> {
    name: &'a str,
    expr: Box<AnnotatedExpr<'a>>,
}

#[derive(Debug, Eq, PartialEq)]
enum Literal {
    Long(i64),
}
fn main() -> io::Result<()> {
    let mut file = File::open("example.sql")?;

    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    let f = parser(Span::new(&contents));

    println!("Result: {:?}!", f);

    Ok(())
}

#[test]
fn test_parser() {
    let res = parser(Span::new("x=1"));
    assert!(res.is_ok());
    let res = res.unwrap();
    assert_eq!(res.1.span.location_offset(), 0);
    assert_eq!(res.1.span.get_utf8_column(), 1);

    match res.1.expr {
        Expr::Equation(eq) => {
            assert_eq!(eq.name, "x");
            assert_eq!(eq.expr.span.location_offset(), 2);
            assert_eq!(eq.expr.span.get_utf8_column(), 3);
        }
        _ => panic!("Did not expect something else than Equation"),
    }

    //assert_eq!(x, "abc");
}
