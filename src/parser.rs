extern crate nom;
extern crate nom_locate;

use nom_locate::{position, LocatedSpan};

pub type Span<'a> = LocatedSpan<&'a str>;
use nom::character::complete::{alpha1, char, digit0, digit1, space0, space1};

use nom::{
    branch::alt,
    bytes::complete::{tag, take_until},
    character::complete::line_ending,
    combinator::opt,
    multi::{many0, separated_list},
    sequence::pair,
    IResult,
};

fn table_row(input: Span) -> IResult<Span, Vec<Expr>> {
    let (i, _) = space0(input)?;
    separated_list(pair(char(','), space0), expression)(i)
}

fn table_literal(input: Span) -> IResult<Span, Expr> {
    // { a, b, ...
    //   1, 2, ...
    // }
    let (i, _) = char('{')(input)?;

    let (i, _) = space0(i)?;

    let (i, header) = separated_list(pair(char(','), space0), identifier)(i)?;

    let (i, _) = space0(i)?;

    let (i, _) = opt(line_ending)(i)?;
    let (i, s) = opt(separated_list(pair(line_ending, space0), table_row))(i)?;

    let rows = s.unwrap_or_else(|| vec![]);

    let (i, _) = tag("}")(i)?;

    Ok((
        i,
        Expr::DataSet(header.iter().map(|x| (*x.fragment())).collect(), rows),
    ))
}

fn let_in_expr(input: Span) -> IResult<Span, Expr> {
    // let <id> = <e1> in <e2>
    let (i, _) = tag("let")(input)?;

    let (i, _) = space1(i)?;

    let (i, name) = identifier(i)?;

    let (i, _) = space0(i)?;

    let (i, _) = char('=')(i)?;

    let (i, _) = space0(i)?;

    let (pos1, expr1) = expression(i)?;

    let (i, _) = space0(pos1)?;

    let (i, _) = tag("in")(i)?;

    let (i, _) = space1(i)?;

    let (pos2, expr2) = expression(i)?;

    Ok((
        pos2,
        Expr::LetIn(LetIn {
            expr1: Box::new(AnnotatedExpr {
                span: pos1,
                expr: expr1,
            }),
            expr2: Box::new(AnnotatedExpr {
                span: pos2,
                expr: expr2,
            }),
            name: &name.fragment(),
        }),
    ))
}

fn comment(input: Span) -> IResult<Span, Expr> {
    let (i, _) = tag("--")(input)?;

    let (_, start_pos) = position(i)?;

    let (i, _) = take_until("\n")(i)?;
    let (_, end_pos) = position(i)?;

    Ok((
        i,
        Expr::Comment(
            &input.fragment()[2..2 + end_pos.location_offset() - start_pos.location_offset()],
        ),
    ))
}

fn identifier(i: Span) -> IResult<Span, Span> {
    let (i, id) = alpha1(i)?;
    // reserved key words
    let f = id.fragment();
    if f == &"let" || f == &"in" || f == &"=" || f == &"type" {
        return Err(nom::Err::Error((i, nom::error::ErrorKind::Verify)));
    }
    Ok((i, id))
}

fn digit<T: std::str::FromStr>(i: Span, f: fn(T) -> Literal) -> IResult<Span, Expr> {
    let (i, span) = digit1(i)?;
    let integer = span.fragment().parse();
    let x = match integer {
        Ok(f) => f,
        Err(_) => return Err(nom::Err::Error((i, nom::error::ErrorKind::Digit))),
    };

    Ok((i, Expr::Literal(f(x))))
}

fn int64(i: Span) -> IResult<Span, Expr> {
    digit(i, Literal::Int64)
}

fn float(input: Span) -> IResult<Span, Expr> {
    let (_, start_pos) = position(input)?;

    let (i, _) = digit1(input)?;

    let (i, _) = char('.')(i)?;

    let (i, _) = digit0(i)?;

    let (_, end_pos) = position(i)?;

    let float =
        input.fragment()[0..end_pos.location_offset() - start_pos.location_offset()].parse();
    let x = match float {
        Ok(f) => f,
        Err(_) => return Err(nom::Err::Error((i, nom::error::ErrorKind::Float))),
    };

    Ok((i, Expr::Literal(Literal::Float(x))))
}

fn reference(i: Span) -> IResult<Span, Expr> {
    let (i, name) = identifier(i)?;
    Ok((i, Expr::Ref(&name.fragment())))
}

fn lambda(i: Span) -> IResult<Span, Expr> {
    let (_, pos) = position(i)?;

    let (i, _) = char('\\')(i)?;

    let (i, name) = identifier(i)?;

    let (i, _) = space0(i)?;

    let (i, _) = tag("->")(i)?;

    let (i, _) = space0(i)?;

    let (i, expr) = expression(i)?;
    Ok((
        i,
        Expr::Lambda(name.fragment(), Box::new(AnnotatedExpr { span: pos, expr })),
    ))
}

pub fn one_expression(i: Span) -> IResult<Span, Expr> {
    let (i, _) = space0(i)?;

    alt((
        let_in_expr,
        table_literal,
        lambda,
        reference,
        float,
        int64,
        comment,
    ))(i)
}

pub fn expression(i: Span) -> IResult<Span, Expr> {
    let (i, expr) = one_expression(i)?;

    let (i2, expr2) = opt(one_expression)(i)?;

    match expr2 {
        None => Ok((i, expr)),
        Some(expr2) => Ok((i2, Expr::App(Box::new(expr), Box::new(expr2)))),
    }
}

pub fn parse_equation(i: Span) -> IResult<Span, Decl> {
    let (i, _) = many0(char('\n'))(i)?;

    let (_, pos) = position(i)?;

    let (i, name) = identifier(i)?;

    let (i, _) = space0(i)?;

    let (i, _d) = char('=')(i)?;

    let (_, pos2) = position(i)?;

    let (i, expr) = expression(i)?;

    Ok((
        i,
        Decl::Equation(Equation {
            name: &name.fragment(),
            span: pos,
            expr: Box::new(AnnotatedExpr { span: pos2, expr }),
        }),
    ))
}

pub fn parse_type_def(i: Span) -> IResult<Span, Decl> {
    let (i, _) = tag("type")(i)?;
    let (i, _) = space0(i)?;
    let (_, pos) = position(i)?;

    let (i, name) = identifier(i)?;

    let (i, _) = space0(i)?;

    let (i, _) = char('=')(i)?;

    let (i, alts) = separated_list(char('|'), identifier)(i)?;
    Ok((
        i,
        Decl::TypeDef(TypeDef {
            name: &name.fragment(),
            alts: alts.iter().map(|x| x.fragment()).cloned().collect(),
            span: pos,
        }),
    ))
}
#[derive(Debug, PartialEq, Clone)]
pub struct AnnotatedExpr<'a> {
    pub expr: Expr<'a>,
    /// Span has extra information about where expression
    /// is located in source file
    span: Span<'a>,
}

pub fn parse_module(i: Span) -> IResult<Span, Vec<Decl>> {
    many0(alt((parse_equation, parse_type_def)))(i)
}

#[derive(Debug, PartialEq, Clone)]
pub enum Decl<'a> {
    /// Binds an expression to a name
    Equation(Equation<'a>),

    TypeDef(TypeDef<'a>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Equation<'a> {
    pub name: &'a str,
    pub expr: Box<AnnotatedExpr<'a>>,
    span: Span<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct TypeDef<'a> {
    pub name: &'a str,
    pub alts: Vec<&'a str>,
    span: Span<'a>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr<'a> {
    /// Normal comment
    Comment(&'a str),
    /// Literal values
    Literal(Literal),
    // Variable references
    Ref(&'a str),
    // Let in expression
    LetIn(LetIn<'a>),
    // DataSet expressions
    DataSet(Vec<&'a str>, Vec<Vec<Expr<'a>>>),
    // Lambda
    Lambda(&'a str, Box<AnnotatedExpr<'a>>),
    // Application
    App(Box<Expr<'a>>, Box<Expr<'a>>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct LetIn<'a> {
    pub name: &'a str,
    pub expr1: Box<AnnotatedExpr<'a>>,
    pub expr2: Box<AnnotatedExpr<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Int64(i64),
    Int32(i32),
    Float(f64),
}

#[test]
fn test_parse_equation() {
    let res = parse_equation(Span::new("x=1"));
    assert!(res.is_ok());
    if let (_, Decl::Equation(eq)) = res.unwrap() {
        assert_eq!(eq.span.location_offset(), 0);
        assert_eq!(eq.span.get_utf8_column(), 1);

        assert_eq!(eq.name, "x");
        assert_eq!(eq.expr.span.location_offset(), 2);
        assert_eq!(eq.expr.span.get_utf8_column(), 3);
    }
}

#[test]
fn test_parse_float() {
    let res = expression(Span::new("2.0"));

    assert!(res.is_ok());

    assert_eq!(res.unwrap().1, Expr::Literal(Literal::Float(2.0)))
}

#[test]
fn test_parse_digit_long() {
    let res = expression(Span::new("999999999999999999999999"));

    assert!(res.is_err());
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

#[test]
fn test_lambda() {
    let res = expression(Span::new(r"\x -> y"));

    if let Expr::Lambda(name, expr) = res.unwrap().1 {
        assert_eq!(name, "x");
        if let Expr::Ref(name) = expr.expr {
            assert_eq!(name, "y");
        } else {
            panic!("Did not expect non-ref")
        }
    } else {
        panic!("Did not expect non-lambda")
    }
}

#[test]
fn test_parse_comment() {
    let res = expression(Span::new("--comment\n"));

    assert!(res.is_ok());

    assert_eq!(res.unwrap().1, Expr::Comment("comment"))
}

#[test]
fn test_app() {
    let res = expression(Span::new("f 1"));

    assert!(res.is_ok());

    assert_eq!(
        res.unwrap().1,
        Expr::App(
            Box::new(Expr::Ref("f")),
            Box::new(Expr::Literal(Literal::Int64(1)))
        )
    )
}

#[test]
fn test_multiple_decls() {
    let res = parse_module(Span::new("x=1\ny=2"));

    assert!(res.is_ok());

    let (_, vs) = res.unwrap();
    if let Decl::Equation(eq) = vs.get(0).unwrap() {
        assert_eq!(eq.name, "x");
        assert_eq!(eq.expr.expr, Expr::Literal(Literal::Int64(1)));
    }

    if let Decl::Equation(eq) = vs.get(1).unwrap() {
        assert_eq!(eq.name, "y");
        assert_eq!(eq.expr.expr, Expr::Literal(Literal::Int64(2)));
    }
}

#[test]
fn test_table_literal() {
    let res = parse_module(Span::new("x={a, b\n1, 2}"));

    assert!(res.is_ok());
    let (_, vs) = res.unwrap();
    if let Decl::Equation(eq) = vs.get(0).unwrap() {
        assert_eq!(eq.name, "x");
        assert_eq!(
            eq.expr.expr,
            Expr::DataSet(
                vec!["a", "b"],
                vec![vec![
                    Expr::Literal(Literal::Int64(1)),
                    Expr::Literal(Literal::Int64(2))
                ]]
            )
        );
    }
}

#[test]
fn test_module() {
    let res = parse_module(Span::new(r"e={}\n\nid=\x -> x"));

    assert!(res.is_ok());
}

#[test]
fn test_type_res() {
    let res = parse_module(Span::new(r"type X=a|b"));

    assert!(res.is_ok());
    println!("{:?}", res);
    assert!(
        matches!(res.unwrap().1.as_slice(), [Decl::TypeDef(TypeDef{name: "X", alts, .. })] if *alts == ["a", "b"])
    );
}
