mod display;
mod eval;
mod parser;
mod types;
use eval::Value;
use parser::{Decl, Equation, Model, Span, Table, TypeDef};
use postgres::{Client, Error, NoTls};
use std::io::stdin;
use types::{Scheme, Type};

pub fn exec(
    s: &str,
    type_env: &im::HashMap<String, Scheme>,
    env: &im::HashMap<String, Value>,
    client: &mut Postgres,
) {
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
    client: &mut Postgres,
) -> (
    im::HashMap<String, Scheme>,
    im::HashMap<String, Value>,
    Vec<(String, Value)>,
) {
    let mut type_env = im::HashMap::new();
    let mut env: im::HashMap<String, Value> = im::HashMap::new();
    let mut models = vec![];

    // Todo: load equatios
    if let Ok((_, b)) = module {
        for decl in b {
            match decl {
                // TODO, separate loading and type checking
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

                Decl::Table(Table { name, fields, .. }) => {
                    // TODO handle types
                    type_env.insert(
                        (*name).to_string(),
                        (
                            im::HashSet::new(),
                            Type::ExternalDataset(
                                fields
                                    .iter()
                                    .map(|x| (x.to_string(), Type::Int64))
                                    .collect(),
                            ),
                        ),
                    );
                    // for now table reference name = table name
                    env = env.update((*name).to_string(), Value::Table((*name).to_string()));
                }

                Decl::Model(Model { name, expr, .. }) => {
                    // TODO make available for other models
                    println!("{}", name);
                    models.push(((*name).to_string(), expr.expr.to_run_expr().eval(&env)))
                }
            }
        }
    }

    (type_env, env, models)
}

fn run_models(models: Vec<(String, Value)>, client: &mut Postgres) {
    println!("Found {} models, running...", models.len());

    for (name, query) in models {
        let a = client.exec(&format!("DROP TABLE {}", name));
        let r = client.exec(&format!("CREATE TABLE {} AS SELECT s from t", name));
        println!("{:?}", r);
    }
}

pub struct Postgres {
    client: postgres::Client,
}

impl Postgres {
    fn connect(
        username: &str,
        password: &str,
        host: &str,
        port: &str,
        database: &str,
    ) -> Result<Self, Error> {
        let client = Client::connect(
            &format!(
                "postgresql://{}:{}@{}:{}/{}",
                username, password, host, port, database
            ),
            NoTls,
        )?;
        Ok(Postgres { client })
    }

    fn exec<'a>(&mut self, sql: &str) -> Result<Vec<postgres::Row>, Error> {
        let x = self.client.query(sql, &[])?;

        Ok(x)
    }
}

fn main() -> Result<(), Error> {
    let file = include_str!("base.sqla");

    let module = parser::parse_module(parser::Span::new(&file));

    println!("Ok, modules loaded");

    // TODO make configurable, integrate
    let mut p = Postgres::connect("postgres", "postgres", "localhost", "5432", "postgres")?;
    let _f = p.exec("CREATE TABLE IF NOT EXISTS t AS SELECT 1 s");
    let rows = p.exec("SELECT s FROM t")?;
    let value: i32 = rows[0].get(0);
    println!("{}", value);
    let (type_env, env, models) = load_module(module, &mut p);

    println!("models {:?}", models);
    run_models(models, &mut p);
    loop {
        let mut s = String::new();

        stdin()
            .read_line(&mut s)
            .expect("Did not enter a correct string");
        exec(&s, &type_env, &env, &mut p);
    }
}
