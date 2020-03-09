use std::fs::File;
use std::io;
use std::io::prelude::*;
mod display;
mod eval;
mod parser;
mod types;
use eval::Value;
use parser::{AnnotatedExpr, Expr, Span};
use std::io::stdin;
use types::Type;

pub fn exec(s: &str, type_env: &im::HashMap<String, Type>, env: &im::HashMap<String, Value>) {
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
                    println!("TypeError: {}", err);
                }
            }
        }
        Err(x) => {
            println!("ParseError: {:?}", x);
        }
    }
}

fn load_module<B>(
    module: Result<(Span<'_>, Vec<AnnotatedExpr>), B>,
) -> (im::HashMap<String, Type>, im::HashMap<String, Value>) {
    let mut type_env = im::HashMap::new();
    let mut env: im::HashMap<String, Value> = im::HashMap::new();
    if let Ok((_, b)) = module {
        for expr in b {
            if let AnnotatedExpr {
                expr: Expr::Equation(eq),
                ..
            } = expr
            {
                if let Ok(ty) = eq.expr.expr.get_type(&type_env) {
                    type_env = type_env.update(eq.name.to_string(), ty);
                    env = env.update(eq.name.to_string(), eq.expr.expr.eval(&env).unwrap());
                }
            }
        }
    }

    (type_env, env)
}

fn main() -> io::Result<()> {
    let mut file = File::open("base.sqla")?;

    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

    let module = parser::parse_module(parser::Span::new(&contents));

    println!("Result: {:?}!", module);
    let (type_env, env) = load_module(module);
    loop {
        let mut s = String::new();

        stdin()
            .read_line(&mut s)
            .expect("Did not enter a correct string");
        exec(&s, &type_env, &env);
    }
}
