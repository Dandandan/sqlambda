use std::fs::File;
use std::io;
use std::io::prelude::*;
mod eval;
mod parser;
mod types;
use std::io::stdin;

fn main() -> io::Result<()> {
    let mut file = File::open("example.sqla")?;

    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    let f = parser::parse_module(parser::Span::new(&contents));

    println!("Result: {:?}!", f);

    loop {
        let mut s = String::new();

        stdin()
            .read_line(&mut s)
            .expect("Did not enter a correct string");
        let exp = parser::expression(parser::Span::new(&s));

        match exp {
            Ok((_i, exp)) => {
                println!("{:?}", exp.eval());
            }
            Err(x) => {
                println!("Error: {:?}", x);
            }
        }
    }
}
