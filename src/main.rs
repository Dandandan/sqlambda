use std::io;
mod display;
mod eval;
mod parser;
mod types;
use eval::Value;
use parser::{Decl, Equation, Span, TypeDef};
use std::io::stdin;
use types::{Scheme, Type};

pub fn exec(s: &str, type_env: &im::HashMap<String, Scheme>, env: &im::HashMap<String, Value>) {
    match parser::expression(parser::Span::new(&s)) {
        Ok((_i, exp)) => {
            let ty = exp.get_type(type_env);
            match ty {
                Ok((_, ty)) => {
                    let res = exp.to_run_expr().eval(env);
                    println!("{:} : {:}", res, ty);
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
    module: Result<(Span<'_>, Vec<Decl>), B>,
) -> (im::HashMap<String, Scheme>, im::HashMap<String, Value>) {
    let mut type_env = im::HashMap::new();
    let mut env: im::HashMap<String, Value> = im::HashMap::new();

    if let Ok((_, b)) = module {
        for decl in b {
            match decl {
                Decl::Equation(Equation { expr, name, .. }) => {
                    let type_res = expr.expr.get_type(&type_env);
                    if let Ok(ty) = type_res {
                        type_env.insert(name.to_string(), (vec![], ty.1));
                        env = env.update(name.to_string(), expr.expr.to_run_expr().eval(&env));
                    } else {
                        println!("Err {:?}", type_res);
                    }
                }
                Decl::TypeDef(TypeDef { name, alts, .. }) => {
                    for x in alts {
                        type_env
                            .insert((*x).to_string(), (vec![], Type::TyCon((*name).to_string())));
                        env = env.update((*x).to_string(), Value::Constant((*x).to_string()));
                    }
                }
            }
        }
    }

    (type_env, env)
}

fn main() -> io::Result<()> {
    let file = include_str!("base.sqla");

    let module = parser::parse_module(parser::Span::new(&file));

    let (type_env, env) = load_module(module);
    println!("Ok, modules loaded");

    loop {
        let mut s = String::new();

        stdin()
            .read_line(&mut s)
            .expect("Did not enter a correct string");
        exec(&s, &type_env, &env);
    }
}
