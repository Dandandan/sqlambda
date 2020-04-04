use super::parser::{Expr, Literal, Pattern};

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
    // Data type
    TyCon(String),
}

fn get_item_types_inner(
    items: &[Vec<Expr>],
    index: usize,
    env: &im::HashMap<String, Scheme>,
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

static COUNTER: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(1);
fn get_id() -> usize {
    COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed)
}

fn get_item_types(items: &[Vec<Expr>], env: &im::HashMap<String, Scheme>) -> Vec<Type> {
    items
        .iter()
        .enumerate()
        .map(|(i, _)| get_item_types_inner(items, i, env))
        .collect()
}

type TypeRes<'a> = (im::HashMap<String, Type>, Type);

pub type Scheme = (im::HashSet<String>, Type);

type Subs<'a> = &'a im::HashMap<String, Type>;

fn apply_sub_type(subs: Subs, ty: &Type) -> Type {
    match ty {
        Type::TyVar(name) => subs.get(name).unwrap_or_else(|| &ty).clone(),
        Type::TyArr(t1, t2) => Type::TyArr(
            Box::new(apply_sub_type(subs, t1)),
            Box::new(apply_sub_type(subs, t2)),
        ),
        _ => ty.clone(),
    }
}

fn apply_sub_scheme(subs: Subs, scheme: Scheme) -> Scheme {
    let mut subs1 = subs.clone();
    for key in scheme.0.clone().into_iter() {
        subs1 = subs1.alter(|_x| Option::None, key);
    }
    let ty = apply_sub_type(&subs1, &scheme.1);
    (scheme.0, ty)
}
fn apply_sub_env(
    subs: &im::HashMap<String, Type>,
    env: &im::HashMap<String, Scheme>,
) -> im::HashMap<String, Scheme> {
    let mut h = im::HashMap::new();
    for (key, value) in env.into_iter() {
        h = h.update(key.to_string(), apply_sub_scheme(subs, value.clone()));
    }
    h
}

fn compose(subs: Subs, subs2: Subs) -> im::HashMap<String, Type> {
    let mut h = im::HashMap::new();
    for (key, value) in subs.into_iter() {
        h = h.update(key.to_string(), apply_sub_type(subs, &value.clone()));
    }
    h.union(subs2.clone())
}

fn ftv_ty(ty: &Type) -> im::HashSet<String> {
    match ty {
        Type::TyVar(a) => [a].iter().cloned().cloned().collect(),
        Type::TyArr(ty1, ty2) => {
            let x = ftv_ty(ty1);
            let y = ftv_ty(ty2);
            x.union(y)
        }

        _ => im::HashSet::new(),
    }
}

fn ftv_env(env: &im::HashMap<String, Scheme>) -> im::HashSet<String> {
    let ftvs = env.values().map(|x| ftv_ty(&x.1));
    let mut j = im::HashSet::new();
    for y in ftvs {
        for z in y {
            j.insert(z);
        }
    }
    j
}

fn generalize(env: &im::HashMap<String, Scheme>, ty: &Type) -> Scheme {
    let xs = ftv_ty(ty);
    let ys = ftv_env(env);
    let a = xs.difference(ys);
    (a, ty.clone())
}

fn unify(ty1: &Type, ty2: &Type) -> Result<im::HashMap<String, Type>, String> {
    match (ty1, ty2) {
        (Type::TyArr(l, r), Type::TyArr(l1, r1)) => {
            let s1 = unify(l, l1)?;
            let s2 = unify(&apply_sub_type(&s1, &r), &apply_sub_type(&s1, &r1))?;

            Ok(compose(&s2, &s1))
        }
        (Type::TyVar(a), t) => bind(&a, &t),
        (t, Type::TyVar(a)) => bind(&a, &t),
        (t1, t2) => {
            if t1 == t2 {
                Ok(im::HashMap::new())
            } else {
                Err("UnificationFail".to_string())
            }
        }
    }
}

fn bind(var: &str, ty: &Type) -> Result<im::HashMap<String, Type>, String> {
    if let Type::TyVar(x) = ty {
        if var == x {
            return Ok(im::HashMap::new());
        }
    }
    if ftv_ty(ty).contains(var) {
        return Err("Infinite Type".to_string());
    }

    Ok(im::HashMap::new().update(var.to_string(), ty.clone()))
}

fn type_pat(
    env: &im::HashMap<String, Scheme>,
    case_type: &Type,
    pattern: &Pattern,
) -> Result<im::HashMap<String, Type>, String> {
    // todo vars / wildcards, etc
    let (_s, ty) = env.get(pattern.name).unwrap();

    unify(case_type, ty)
}

// Type inference using http://dev.stephendiehl.com/fun/006_hindley_milner.html#substitution
impl<'a> Expr<'_> {
    pub fn get_type(&self, env: &im::HashMap<String, Scheme>) -> Result<TypeRes, String> {
        match self {
            Expr::Literal(l) => Ok((im::HashMap::new(), l.get_type())),
            Expr::Ref(x) => {
                let err = format!("Could not find reference {}", x);
                let ty = env.get(*x).cloned().ok_or(err)?;
                Ok((im::HashMap::new(), ty.1))
            }
            Expr::LetIn(x) => {
                let (s1, t1) = x.expr1.expr.get_type(env)?;
                let env1 = apply_sub_env(&s1, env);
                let t2 = generalize(&env1, &t1);
                let extended_ty = env.update(x.name.to_string(), t2);
                let (s2, t2) = x.expr2.expr.get_type(&extended_ty)?;
                Ok((compose(&s1, &s2), t2))
            }
            Expr::DataSet(names, items) => Ok((
                im::HashMap::new(),
                Type::Dataset(
                    names.iter().map(|x| (*x).to_string()).collect(),
                    get_item_types(items, env),
                ),
            )),
            Expr::Lambda(name, expr) => {
                let type_var = Type::TyVar(get_id().to_string()); //fresh();
                let env1 = env.update((*name).to_string(), (im::HashSet::new(), type_var.clone()));
                let (sub, t1) = expr.expr.get_type(&env1)?;
                let substituted = apply_sub_type(&sub, &type_var);
                Ok((sub, Type::TyArr(Box::new(substituted), Box::new(t1))))
            }
            Expr::App(expr1, expr2) => {
                let tv = Type::TyVar(get_id().to_string());

                let (s1, t1) = expr1.get_type(env)?;
                let (s2, t2) = expr2.get_type(&apply_sub_env(&s1, env))?;
                let s3 = unify(
                    &apply_sub_type(&s2, &t1),
                    &Type::TyArr(Box::new(t2), Box::new(tv.clone())),
                )?;
                Ok((compose(&compose(&s3, &s2), &s1), apply_sub_type(&s3, &tv)))
            }
            Expr::Match(expr, exprs) => {
                let (mut subs, case_type) = expr.get_type(env)?;
                let mut branch_type = Type::TyVar(get_id().to_string());

                for (p, branch) in exprs {
                    // TODO check, test
                    let pat_sub = type_pat(env, &case_type, p)?;
                    subs = compose(&subs, &pat_sub);

                    let (s, n_branch_type) = branch.get_type(env)?;
                    subs = compose(&subs, &s);
                    let cur_branch_type = apply_sub_type(&subs, &n_branch_type);

                    let s2 = unify(&branch_type, &cur_branch_type)?;
                    subs = compose(&subs, &s2);
                    branch_type = apply_sub_type(&subs, &branch_type);
                }

                Ok((subs, branch_type))
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
#[cfg(test)]
use super::parser::{expression, Span};

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
    let (_, expr) = expression(Span::new(r"\x -> x")).unwrap();
    let ty = expr.get_type(&im::HashMap::new()).unwrap().1;
    match ty {
        Type::TyArr(x, y) => assert_eq!(x, y),
        _ => panic!("Did not expect non-tyarr result"),
    }
}

#[test]
fn test_type_lam_app() {
    let (_, expr) = expression(Span::new(r"let id = \x -> x in id 1")).unwrap();
    let ty = expr.get_type(&im::HashMap::new()).unwrap().1;
    assert_eq!(ty, Type::Int64);
}
