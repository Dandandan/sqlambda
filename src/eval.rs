use super::parser::{Expr, Literal};
use super::Postgres;
extern crate im;

/// Runtime expression, optimized for compactness and use in interpreter
/// Now uses variables, but can be optimized to using indexes, offsets, etc.
///
#[derive(Debug, PartialEq, Clone)]
pub enum RunExpr {
    Ref(String),
    LetIn(String, Box<RunExpr>, Box<RunExpr>),
    Value(Literal),
    DataSet(im::HashMap<String, Vec<RunExpr>>),
    Lambda(String, Box<RunExpr>),
    App(Box<RunExpr>, Box<RunExpr>),
    Match(Box<RunExpr>, Vec<(String, RunExpr)>),
    Projection(Vec<String>, Box<RunExpr>),
    Table(String),
}

impl<'a> Expr<'a> {
    pub fn to_run_expr(&self) -> RunExpr {
        match self {
            Expr::LetIn(x) => RunExpr::LetIn(
                x.name.to_string(),
                Box::new(x.expr1.expr.to_run_expr()),
                Box::new(x.expr2.expr.to_run_expr()),
            ),
            Expr::Literal(l) => RunExpr::Value(l.to_owned()),
            Expr::Ref(v) => RunExpr::Ref((*v).to_string()),

            Expr::DataSet(items) => RunExpr::DataSet(
                items
                    .iter()
                    .map(|(x, y)| (x.to_string(), y.iter().map(|e| e.to_run_expr()).collect()))
                    .collect(),
            ),
            Expr::Lambda(name, y) => {
                RunExpr::Lambda((*name).to_string(), Box::new(y.expr.to_run_expr()))
            }
            Expr::App(e1, e2) => {
                RunExpr::App(Box::new((*e1).to_run_expr()), Box::new((*e2).to_run_expr()))
            }
            Expr::Match(e1, values) => RunExpr::Match(
                Box::new((*e1).to_run_expr()),
                values
                    .iter()
                    .map(|(x, y)| (x.name.to_string(), y.to_run_expr()))
                    .collect(),
            ),
            Expr::Projection(p, v) => RunExpr::Projection(
                p.iter().map(|x| (*x).to_string()).collect(),
                Box::new((*v).to_run_expr()),
            ),
            _ => unimplemented!(),
        }
    }
}
#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Constant(String),
    Float(f64),
    Int64(i64),
    Int32(i32),
    DataSet(std::collections::HashMap<String, Vec<Value>>),
    Table(String),
    FnClosure(String, Box<RunExpr>, im::HashMap<String, Value>),
}

impl<'a> Literal {
    pub fn to_value(&self) -> Value {
        match self {
            Literal::Float(f) => Value::Float(*f),
            Literal::Int32(f) => Value::Int32(*f),
            Literal::Int64(f) => Value::Int64(*f),
        }
    }
}
impl RunExpr {
    pub fn eval(&self, e: &im::HashMap<String, Value>, client: &mut Postgres) -> Value {
        match self {
            RunExpr::LetIn(name, expr1, expr2) => {
                let r = expr1.eval(e, client);
                let m = e.update(name.to_string(), r);
                expr2.eval(&m, client)
            }
            RunExpr::Ref(r) => e.get(r).unwrap().clone(),
            RunExpr::Value(l) => l.to_value(),
            RunExpr::DataSet(items) => Value::DataSet(
                items
                    .iter()
                    .map(|(x, y)| (x.to_string(), y.iter().map(|z| z.eval(e, client)).collect()))
                    .collect(),
            ),
            RunExpr::Lambda(name, expr) => {
                Value::FnClosure(name.to_string(), expr.clone(), e.clone())
            }
            RunExpr::Match(expr, exprs) => {
                let v1 = expr.eval(e, client);
                // TODO: optimize
                for arm in exprs {
                    if Value::Constant(arm.0.to_string()) == v1 {
                        return arm.1.eval(e, client);
                    }
                }
                panic!("Non-exhaustive pattern match");
            }
            RunExpr::App(e1, arg) => {
                let x = e1.eval(e, client);
                match x {
                    Value::FnClosure(x, body, clo) => {
                        let argv = arg.eval(e, client);
                        let nenv = clo.update(x, argv);
                        body.eval(&nenv, client)
                    }
                    _ => {
                        panic!("Expected fn-clusure here");
                    }
                }
            }
            RunExpr::Projection(names, expr) => {
                // TODO don't evaluate whole dataset
                let d = expr.eval(e, client);
                if let Value::DataSet(items) = d {
                    Value::DataSet(
                        items
                            .iter()
                            .filter(|(i, _x)| names.contains(*i))
                            .map(|(i, x)| (i.to_string(), x.clone()))
                            .collect(),
                    )
                } else {
                    panic!(format!("Unexpected argument in projection {:?}", d))
                }
            }
            RunExpr::Table(name) => {
                let str = &format!("SELECT s FROM {}", name);
                let rows = client.exec(str);
                unimplemented!()
            }
        }
    }
}

#[cfg(test)]
use super::parser::{expression, Span};

// #[test]
// fn test_eval() {
//     assert_eq!(
//         RunExpr::Value(Literal::Int64(1)).eval(&im::HashMap::new()),
//         Value::Int64(1)
//     );
// }
// #[test]
// fn test_eval_let_lam_app() {
//     let (_, expr) = expression(Span::new(r"let id = \x -> x in id 1")).unwrap();
//     let res = expr.to_run_expr().eval(&im::HashMap::new());
//     assert_eq!(res, Value::Int64(1));
// }

// #[test]
// fn test_eval_let_lam_app_fst() {
//     let (_, expr) = expression(Span::new(r"let fst = \x -> \y -> x in fst 1 2")).unwrap();
//     let res = expr.to_run_expr().eval(&im::HashMap::new());
//     assert_eq!(res, Value::Int64(1));
// }

// #[test]
// fn test_eval_let_lam_app_snd() {
//     let (_, expr) = expression(Span::new(r"let snd = \x -> \y -> y in snd 1 2")).unwrap();
//     let res = expr.to_run_expr().eval(&im::HashMap::new());
//     assert_eq!(res, Value::Int64(2));
// }

// #[test]
// fn test_eval_match() {
//     let (_, expr) = expression(Span::new(r"let snd = \x -> \y -> y in snd 1 2")).unwrap();
//     let res = expr.to_run_expr().eval(&im::HashMap::new());
//     assert_eq!(res, Value::Int64(2));
// }
