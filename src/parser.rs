extern crate nom;
extern crate nom_locate;

use nom_locate::{position, LocatedSpan};

pub type Span<'a> = LocatedSpan<&'a str>;
use nom::character::complete::{alpha1, digit0, digit1, multispace0};

use nom::{branch::alt, bytes::complete::tag, IResult};

fn identifier(i: Span) -> IResult<Span, Span> {
    alpha1(i)
}

fn digit(i: Span) -> IResult<Span, Expr> {
    let (i, span) = digit1(i)?;
    // TODO: show error if parsing fails
    Ok((
        i,
        Expr::Literal(Literal::Long(span.fragment().parse().unwrap())),
    ))
}

fn float(input: Span) -> IResult<Span, Expr> {
    let (_, start_pos) = position(input)?;

    let (i, _) = digit1(input)?;

    let (i, _) = tag(".")(i)?;

    let (i, _) = digit0(i)?;

    let (_, end_pos) = position(i)?;

    Ok((
        i,
        Expr::Literal(Literal::Float(
            // TODO: Show error if parsing fails
            input.fragment()[0..end_pos.location_offset() - start_pos.location_offset()]
                .parse()
                .unwrap(),
        )),
    ))
}

fn expression(i: Span) -> IResult<Span, Expr> {
    let (i, _) = multispace0(i)?;
    alt((float, digit))(i)
}

pub fn parse_module(i: Span) -> IResult<Span, AnnotatedExpr> {
    let (_, pos) = position(i)?;

    let (i, name) = identifier(i)?;

    let (i, _d) = tag("=")(i)?;
    let (_, pos2) = position(i)?;

    let (i, expr) = expression(i)?;
    let (i, _) = multispace0(i)?;

    Ok((
        i,
        AnnotatedExpr {
            span: pos,
            expr: Expr::Equation(Equation {
                name: &name.fragment(),
                expr: Box::new(AnnotatedExpr {
                    span: pos2,
                    expr: expr,
                }),
            }),
        },
    ))
}
#[derive(Debug, PartialEq)]
pub struct AnnotatedExpr<'a> {
    expr: Expr<'a>,
    /// Span has extra information about where expression
    /// is located in source file
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
pub enum Literal {
    Long(i64),
    Float(f64),
}

#[test]
fn test_parse_module() {
    let res = parse_module(Span::new("x=1"));
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
fn test_parse_float() {
    let res = expression(Span::new("2.0"));

    assert!(res.is_ok());

    assert_eq!(res.unwrap().1, Expr::Literal(Literal::Float(2.0)))
}

#[test]
fn test_parse_float_no_decimal() {
    let res = expression(Span::new("2."));

    assert!(res.is_ok());

    assert_eq!(res.unwrap().1, Expr::Literal(Literal::Float(2.)))
}

#[test]
fn test_parse_float_whitespace() {
    let res = expression(Span::new(" 2. "));

    assert!(res.is_ok());

    assert_eq!(res.unwrap().1, Expr::Literal(Literal::Float(2.)))
}
