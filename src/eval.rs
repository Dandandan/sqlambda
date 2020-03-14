use super::parser::{Expr, Literal};
extern crate im;

/// Runtime expression, optimized for compactness and use in interpreter
/// Now uses variables, but can be optimized to using indexes, offsets, etc.
///
#[derive(Debug, PartialEq, Clone)]
pub enum RunExpr {
    Ref(String),
    LetIn(String, Box<RunExpr>, Box<RunExpr>),
    Value(Literal),
    DataSet(Vec<String>, Vec<Vec<RunExpr>>),
    Lambda(String, Box<RunExpr>),
    App(Box<RunExpr>, Box<RunExpr>),
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

            Expr::DataSet(header, values) => RunExpr::DataSet(
                header.iter().map(|x| (*x).to_string()).collect(),
                values
                    .iter()
                    .map(|v| v.iter().map(|x| x.to_run_expr()).collect())
                    .collect(),
            ),
            Expr::Lambda(name, y) => {
                RunExpr::Lambda((*name).to_string(), Box::new(y.expr.to_run_expr()))
            }
            Expr::App(e1, e2) => {
                RunExpr::App(Box::new((*e1).to_run_expr()), Box::new((*e2).to_run_expr()))
            }
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
    DataSet(Vec<String>, Vec<Vec<Value>>),
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
    pub fn eval(&self, e: &im::HashMap<String, Value>) -> Value {
        match self {
            RunExpr::LetIn(name, expr1, expr2) => {
                let r = expr1.eval(e);
                let m = e.update(name.to_string(), r);
                expr2.eval(&m)
            }
            RunExpr::Ref(r) => e.get(r).unwrap().clone(),
            RunExpr::Value(l) => l.to_value(),
            RunExpr::DataSet(l, m) => Value::DataSet(
                l.clone(),
                m.iter()
                    .map(|y| y.iter().map(|x| x.eval(e)).collect())
                    .collect(),
            ),
            RunExpr::Lambda(name, expr) => {
                Value::FnClosure(name.to_string(), expr.clone(), e.clone())
            }
            RunExpr::App(e1, arg) => {
                let x = e1.eval(e);
                match x {
                    Value::FnClosure(x, body, clo) => {
                        let argv = arg.eval(e);
                        let nenv = clo.update(x, argv);
                        body.eval(&nenv)
                    }
                    _ => {
                        panic!("Expected fn-clusure here");
                    }
                }
            }
        }
    }
}

#[cfg(test)]
use super::parser::{expression, Span};

#[test]
fn test_eval() {
    assert_eq!(
        RunExpr::Value(Literal::Int64(1)).eval(&im::HashMap::new()),
        Value::Int64(1)
    );
}
#[test]
fn test_eval_let_lam_app() {
    let (_, expr) = expression(Span::new(r"let id = \x -> x in id 1")).unwrap();
    let res = expr.to_run_expr().eval(&im::HashMap::new());
    assert_eq!(res, Value::Int64(1));
}

#[test]
fn test_eval_let_lam_app_fst() {
    let (_, expr) = expression(Span::new(r"let fst = \x -> \y -> x in fst 1 2")).unwrap();
    let res = expr.to_run_expr().eval(&im::HashMap::new());
    assert_eq!(res, Value::Int64(1));
}

#[test]
fn test_eval_let_lam_app_snd() {
    let (_, expr) = expression(Span::new(r"let snd = \x -> \y -> y in snd 1 2")).unwrap();
    let res = expr.to_run_expr().eval(&im::HashMap::new());
    assert_eq!(res, Value::Int64(2));
}
