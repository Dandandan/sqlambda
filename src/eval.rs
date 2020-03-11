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
    FnClosure(String, Box<RunExpr>),
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
            Expr::Ref(v) => RunExpr::Ref(v.to_string()),

            Expr::DataSet(header, values) => RunExpr::DataSet(
                header.iter().map(|x| x.to_string()).collect(),
                values
                    .iter()
                    .map(|v| v.iter().map(|x| x.to_run_expr()).collect())
                    .collect(),
            ),
            Expr::Lambda(name, y) => {
                RunExpr::FnClosure(name.to_string(), Box::new(y.expr.to_run_expr()))
            }
            _ => unimplemented!(),
        }
    }
}
#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Unit,
    Float(f64),
    Int64(i64),
    Int32(i32),
    DataSet(Vec<String>, Vec<Vec<Value>>),
    FnClosure(String, Box<RunExpr>),
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
            RunExpr::FnClosure(name, expr) => Value::FnClosure(name.to_string(), expr.clone()),
        }
    }
}

#[cfg(test)]
#[test]
fn test_eval() {
    assert_eq!(
        RunExpr::Value(Literal::Int64(1)).eval(&im::HashMap::new()),
        Value::Int64(1)
    );
}
