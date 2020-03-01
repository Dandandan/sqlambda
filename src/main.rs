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

fn float(i: Span) -> IResult<Span, Literal> {
    let (i, span1) = digit1(i)?;

    let (i, _) = tag(".")(i)?;

    let (i, span2) = digit1(i)?;

    // TODO, create from slice
    Ok((
        i,
        Literal::Float(
            format!("{}.{}", span1.fragment(), span2.fragment())
                .parse()
                .unwrap(),
        ),
    ))
}

fn literal(i: Span) -> IResult<Span, Literal> {
    alt((float, digit))(i)
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
#[derive(Debug, PartialEq)]
struct AnnotatedExpr<'a> {
    expr: Expr<'a>,
    span: Span<'a>,
}
#[derive(Debug, PartialEq)]
enum Expr<'a> {
    /// Binds an expression to a name
    Equation(Equation<'a>),

    /// Literal values
    Literal(Literal),
}

#[derive(Debug, PartialEq)]
struct Equation<'a> {
    name: &'a str,
    expr: Box<AnnotatedExpr<'a>>,
}

#[derive(Debug, PartialEq)]
enum Literal {
    Long(i64),
    Float(f64),
}
fn main() -> io::Result<()> {
    let mut file = File::open("example.sqla")?;

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
    let res = literal(Span::new("2.0"));

    assert!(res.is_ok());

    assert_eq!(res.unwrap().1, Literal::Float(2.0))
}
