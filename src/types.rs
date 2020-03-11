use super::parser::{expression, Expr, Literal, Span};

#[derive(Eq, PartialEq, Debug, Clone)]
pub enum Type {
    Int64,
    Int32,
    Float,
    Dataset(Vec<String>, Vec<Type>),
    /// T -> U
    TyArr(Box<Type>, Box<Type>),
    /// Type variable
    TyVar(String),
}

fn get_item_types_inner(
    items: &[Vec<Expr>],
    index: usize,
    env: &im::HashMap<String, Type>,
) -> Type {
    let values = items.iter().map(|y| y.get(index).unwrap().clone());

    values
        .map(|x| x.get_type(env).unwrap().1)
        // TODO collect substitions
        .collect::<Vec<Type>>()
        .get(0)
        .unwrap()
        .clone()
}

fn get_item_types(items: &[Vec<Expr>], env: &im::HashMap<String, Type>) -> Vec<Type> {
    items
        .iter()
        .enumerate()
        .map(|(i, _)| get_item_types_inner(items, i, env))
        .collect()
}

type TypeRes = (im::HashMap<String, Type>, Type);

type Scheme = (Vec<String>, Type);

fn apply_sub_type(subs: &im::HashMap<String, Type>, ty: &Type) -> Type {
    match ty {
        Type::TyVar(name) => subs.get(name).unwrap_or(&ty.clone()).clone(),
        _ => ty.clone(),
    }
}

fn apply_sub_env(
    subs: &im::HashMap<String, Type>,
    env: &im::HashMap<String, Type>,
) -> im::HashMap<String, Type> {
    let mut h = im::HashMap::new();
    for (key, value) in env.into_iter() {
        h = h.update(key.to_string(), apply_sub_type(subs, value));
    }
    h
}

fn generalize(env: im::HashMap<String, Type>, ty: Type) -> Scheme {
    unimplemented!()
}

// Type inference using http://dev.stephendiehl.com/fun/006_hindley_milner.html#substitution
impl<'a> Expr<'_> {
    pub fn get_type(&self, env: &im::HashMap<String, Type>) -> Result<TypeRes, String> {
        match self {
            Expr::Literal(l) => Ok((im::HashMap::new(), l.get_type())),
            Expr::Ref(x) => {
                let err = format!("Could not find reference {}", x);
                let ty = env.get(*x).cloned().ok_or(err)?;
                Ok((im::HashMap::new(), ty))
            }
            Expr::LetIn(x) => {
                // TODO implement
                let (sub, ty) = x.expr1.expr.get_type(env)?;
                let env = apply_sub_env(&sub, env);
                //let n_ty = generalize(sub, ty.clone());
                let type_env1 = env.update(x.name.to_string(), ty);
                x.expr2.expr.get_type(&type_env1)
            }
            Expr::DataSet(names, items) => Ok((
                im::HashMap::new(),
                Type::Dataset(
                    names.iter().map(|x| x.to_string()).collect(),
                    get_item_types(items, env),
                ),
            )),
            Expr::Lambda(name, expr) => {
                let type_var = Type::TyVar("x".to_string()); //fresh();
                let env1 = env.update(name.to_string(), type_var.clone());
                let (sub, t1) = expr.expr.get_type(&env1)?;
                let substituted = apply_sub_type(&sub, &type_var);
                Ok((sub, Type::TyArr(Box::new(substituted), Box::new(t1))))
            }
            x => Err(format!("not implemented {:?}", x)),
        }
    }
}

impl Literal {
    fn get_type(&self) -> Type {
        match self {
            Literal::Int64(_) => Type::Int64,
            Literal::Int32(_) => Type::Int32,
            Literal::Float(_) => Type::Float,
        }
    }
}

#[test]
fn test_type() {
    assert_eq!(Literal::Float(1.0).get_type(), Type::Float);
    assert_eq!(Literal::Int64(1).get_type(), Type::Int64);
}
#[test]
fn test_type_let() {
    let (_, expr) = expression(Span::new("let x = 1 in x")).unwrap();
    assert_eq!(expr.get_type(&im::HashMap::new()).unwrap().1, Type::Int64);
}

#[test]
fn test_type_lam() {
    let (_, expr) = expression(Span::new("\\x -> x")).unwrap();
    let ty = expr.get_type(&im::HashMap::new()).unwrap().1;
    match ty {
        Type::TyArr(x, y) => assert_eq!(x, y),
        _ => panic!("Did not expect non-tyarr result"),
    }
}
