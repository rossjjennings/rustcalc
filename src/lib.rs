use std::rc::Rc;
use std::collections::VecDeque;

pub enum Expr {
    Literal(f64),
    Unary(Rc<UnaryOp>, Box<Expr>),
    Binary(Rc<BinaryOp>, Box<Expr>, Box<Expr>),
}

pub struct UnaryOp {
    prec: u32,
    transform: Box<dyn Fn(f64) -> f64>,
}

impl UnaryOp {
    pub fn new(prec: u32, transform: Box<dyn Fn(f64) -> f64>) -> UnaryOp {
        UnaryOp { prec, transform }
    }
}

pub struct BinaryOp {
    prec: u32,
    right_assoc: bool,
    transform: Box<dyn Fn(f64, f64) -> f64>,
}

impl BinaryOp {
    pub fn new(prec: u32, right_assoc: bool,
           transform: Box<dyn Fn(f64, f64) -> f64>) -> BinaryOp {
        BinaryOp { prec, right_assoc, transform }
    }
}

impl Expr {
    pub fn eval(&self) -> f64 {
        match self {
            Expr::Literal(x) => *x,
            Expr::Unary(op, e) => (op.transform)(e.eval()),
            Expr::Binary(op, e1, e2) => (op.transform)(e1.eval(), e2.eval()),
        }
    }
}

pub enum ParseError {
    Expected(tok: Option<char>, found: Option<char>),
}

pub fn parse(input: &str) -> Result<Expr, String> {
    let mut queue : VecDeque<char> = VecDeque::new();
    for chr in input.chars() {
        queue.push_back(chr);
    }
    
    let expr = parse_expr(&mut queue, 0)?;
    expect(&mut queue, None)?;
    Ok(expr)
}

fn expect(queue: &mut VecDeque<char>, tok: Option<char>)
    -> Result<(), String> {
    if let Some(&chr) = queue.front() {
        match tok {
            Some(val) => {
                if chr == val {
                    queue.pop_front();
                    Ok(())
                } else {
                    Err(format!("Expected '{}' but found '{}'.", val, chr))
                }
            },
            None => Err(format!("Expected end of input but found '{}'.", chr))
        }
    } else {
        match tok {
            None => Ok(()),
            Some(val) => Err(format!("Expected '{}' but input ended.", val)),
        }
    }
}

fn parse_expr(queue: &mut VecDeque<char>, prec: u32)
    -> Result<Expr, String> {
    Ok(Expr::Literal(0.0))
}

//fn parse_atom(queue: &mut VecDeque<char>) -> Result<Expr, &'static str> {}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn construct_expr() {
        let e = Expr::Literal(4.0);
        let u = UnaryOp::new(1, Box::new(|x| -x));
        let e = Expr::Unary(Rc::new(u), Box::new(e));
        let e2 = Expr::Literal(2.0);
        let b = BinaryOp::new(1, false, Box::new(|x, y| x + y));
        let e = Expr::Binary(Rc::new(b), Box::new(e), Box::new(e2));
        
        assert_eq!(e.eval(), -2.0);
    }
    
    #[test]
    fn add_twice() {
        let x = Expr::Literal(1.0);
        let y = Expr::Literal(2.0);
        let z = Expr::Literal(3.0);
        let plus = Rc::new(BinaryOp::new(1, false, Box::new(|x, y| x + y)));
        let e = Expr::Binary(Rc::clone(&plus), Box::new(y), Box::new(z));
        let e = Expr::Binary(Rc::clone(&plus), Box::new(x), Box::new(e));
        
        assert_eq!(e.eval(), 6.0);
    }
    
    #[test]
    fn parse_invalid() {
        let result = parse("something");
        assert!(result.is_err());
    }
}
