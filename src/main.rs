mod display;
mod eval;
mod parser;
mod types;
use eval::Value;
use parser::{Decl, Equation, Span, TypeDef};
use std::io::stdin;
use tokio_postgres::{Error, NoTls};
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
                        type_env.insert(name.to_string(), (im::HashSet::new(), ty.1));
                        env = env.update(name.to_string(), expr.expr.to_run_expr().eval(&env));
                    } else {
                        println!("Err {:?}", type_res);
                    }
                }
                Decl::TypeDef(TypeDef { name, alts, .. }) => {
                    for x in alts {
                        type_env.insert(
                            (*x).to_string(),
                            (im::HashSet::new(), Type::TyCon((*name).to_string())),
                        );
                        env = env.update((*x).to_string(), Value::Constant((*x).to_string()));
                    }
                }
            }
        }
    }

    (type_env, env)
}

#[tokio::main]
async fn main() -> Result<(), Error> {
    let file = include_str!("base.sqla");

    let module = parser::parse_module(parser::Span::new(&file));

    let (type_env, env) = load_module(module);
    println!("Ok, modules loaded");

    let (client, connection) = tokio_postgres::connect(
        "postgresql://postgres:postgres@localhost:5432/postgres",
        NoTls,
    )
    .await?;

    // The connection object performs the actual communication with the database,
    // so spawn it off to run on its own.
    tokio::spawn(async move {
        if let Err(e) = connection.await {
            eprintln!("connection error: {}", e);
        }
    });

    // Now we can execute a simple statement that just returns its parameter.

    // And then check that we got back the same string we sent over.
    let y = client
        .query("CREATE TABLE IF NOT EXISTS t AS SELECT 1 s", &[])
        .await?;
    println!("{:?}", y);
    let rows = client.query("SELECT s FROM t", &[]).await?;
    let value: i32 = rows[0].get(0);
    println!("{}", value);

    loop {
        let mut s = String::new();

        stdin()
            .read_line(&mut s)
            .expect("Did not enter a correct string");
        exec(&s, &type_env, &env);
    }
}
