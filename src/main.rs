use std::fs::File;
use std::io;
use std::io::prelude::*;
mod display;
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
    let type_env = &im::HashMap::new();
    let env = &im::HashMap::new();
    loop {
        let mut s = String::new();

        stdin()
            .read_line(&mut s)
            .expect("Did not enter a correct string");

        match parser::expression(parser::Span::new(&s)) {
            Ok((_i, exp)) => {
                let ty = exp.get_type(type_env);
                match ty {
                    Ok(ty) => {
                        let res = exp.eval(env);
                        if let Ok(x) = res {
                            println!("{:} : {:?}", x, ty);
                        }
                    }
                    Err(err) => {
                        println!("Error: {}", err);
                    }
                }
            }
            Err(x) => {
                println!("Error: {:?}", x);
            }
        }
    }
}
