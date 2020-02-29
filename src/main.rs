use std::fs::File;
use std::io;
use std::io::prelude::*;
extern crate nom;
use nom::{
    bytes::complete::{tag, take_while1},
    IResult,
};

fn decl_parser(i: &str) -> IResult<&str, &str> {
    take_while1(|c: char| c.is_alphabetic())(i)
}

fn lit_parser(i: &str) -> IResult<&str, &str> {
    take_while1(|c: char| c.is_digit(10))(i)
}

fn parser(i: &str) -> IResult<&str, Expr> {
    let (i, name) = decl_parser(i)?;
    let (i, _d) = tag("=")(i)?;
    let (i, x) = lit_parser(i)?;

    Ok((
        i,
        Expr::Decl(
            name.to_string(),
            Box::new(Expr::Literal(Literal::Integer(0))),
        ),
    ))
}

#[derive(Debug)]
enum Expr {
    Decl(String, Box<Expr>),
    Literal(Literal),
}
#[derive(Debug)]
enum Literal {
    Integer(i32),
}
fn main() -> io::Result<()> {
    let mut file = File::open("example.d")?;

    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    let f = parser(&contents);

    println!("Hello, {:?}!", f);

    Ok(())
}
