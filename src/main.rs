use std::fs::File;
use std::io;
use std::io::prelude::*;

extern crate nom;
extern crate nom_locate;

use nom_locate::{position, LocatedSpan};

type Span<'a> = LocatedSpan<&'a str>;
use nom::character::complete::{alpha1, digit1};

use nom::{branch::alt, bytes::complete::tag, IResult};

fn identifier(i: Span) -> IResult<Span, Span> {
    alpha1(i)
}

fn digit(i: Span) -> IResult<Span, Literal> {
    let (i, span) = digit1(i)?;

    Ok((i, Literal::Long(span.fragment().parse().unwrap())))
}

fn boolean(i: Span) -> IResult<Span, Literal> {
    let (i, span) = alt((tag("false"), tag("true")))(i)?;

    let bool_val = match span.fragment() {
        &"true" => Literal::Boolean(true),
        &"false" => Literal::Boolean(false),
        _ => unreachable!(),
    };

    Ok((i, bool_val))
}

fn literal(i: Span) -> IResult<Span, Literal> {
    alt((digit, boolean))(i)
}

fn parser(i: Span) -> IResult<Span, AnnotatedExpr> {
    let (_, pos) = position(i)?;

    let (i, name) = identifier(i)?;

    let (i, _d) = tag("=")(i)?;
    let (_, pos2) = position(i)?;

    let (i, literal) = literal(i)?;
    Ok((
        i,
        AnnotatedExpr {
            span: pos,
            expr: Expr::Equation(Equation {
                name: &name.fragment(),
                expr: Box::new(AnnotatedExpr {
                    span: pos2,
                    expr: Expr::Literal(literal),
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
    Boolean(bool),
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
}

#[test]
fn test_bool_true() {
    let res = literal(Span::new("true"));

    assert!(res.is_ok());

    assert_eq!(res.unwrap().1, Literal::Boolean(true))
}
#[test]
fn test_bool_false() {
    let res = literal(Span::new("false"));

    assert!(res.is_ok());

    assert_eq!(res.unwrap().1, Literal::Boolean(false))
}
