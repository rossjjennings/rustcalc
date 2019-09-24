use std::fmt;
use std::num;
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
    assoc: Assoc,
    transform: Box<dyn Fn(f64, f64) -> f64>,
}

pub enum Assoc {
    Right,
    Left,
}

impl BinaryOp {
    pub fn new(prec: u32, assoc: Assoc,
           transform: Box<dyn Fn(f64, f64) -> f64>) -> BinaryOp {
        BinaryOp { prec, assoc, transform }
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

#[derive(Debug)]
pub enum ParseError {
    Expected(Option<char>, Option<char>),
    InvalidAtom,
    InvalidFloat,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
        | ParseError::Expected(Some(tok), Some(found)) => {
            write!(f, "Expected '{}' but fount '{}'.", tok, found)}
        | ParseError::Expected(Some(tok), None) => {
            write!(f, "Expected '{}' but input ended.", tok)}
        | ParseError::Expected(None, Some(found)) => {
            write!(f, "Expected end of input but found '{}'.", found)}
        | ParseError::Expected(None, None) => {
            write!(f, "Expected end of input but found end of input. Eek!")}
        | ParseError::InvalidAtom => {
            write!(f, "Invalid atomic expression.")}
        | ParseError::InvalidFloat => {
            write!(f, "Invalid floating-point number.")}
        }
    }
}

impl From<num::ParseFloatError> for ParseError {
    fn from(_e: num::ParseFloatError) -> ParseError {
        ParseError::InvalidFloat
    }
}

pub fn parse(input: &str) -> Result<Expr, ParseError> {
    let mut queue : VecDeque<char> = VecDeque::new();
    for chr in input.chars() {
        queue.push_back(chr);
    }
    
    let expr = parse_expr(&mut queue, 0)?;
    expect(&queue, None)?;
    Ok(expr)
}

fn expect(queue: &VecDeque<char>, tok: Option<char>)
    -> Result<(), ParseError> {
    let front = queue.front().map(|&chr| chr);
    if tok == front { Ok(()) } else { Err(ParseError::Expected(tok, front)) }
}

fn parse_expr(queue: &mut VecDeque<char>, prec: u32)
    -> Result<Expr, ParseError> {
    let mut lhs = parse_atom(queue)?;
    while let Some(b) = next_binary(&queue) {
        if b.prec < prec { break; }
        queue.pop_front();
        let new_prec = match b.assoc {
            Assoc::Right => b.prec,
            Assoc::Left => b.prec + 1,
        };
        let rhs = parse_expr(queue, new_prec)?;
        lhs = Expr::Binary(Rc::clone(&b), Box::new(lhs), Box::new(rhs));
    }
    Ok(lhs)
}

fn next_binary(queue: &VecDeque<char>) -> Option<Rc<BinaryOp>> {
    let b = if let Some(&chr) = queue.front() {
        match chr {
        | '+' => Some(BinaryOp::new(1, Assoc::Left, Box::new(|x, y| x + y))),
        | '-' => Some(BinaryOp::new(1, Assoc::Left, Box::new(|x, y| x - y))),
        | '*' => Some(BinaryOp::new(2, Assoc::Left, Box::new(|x, y| x * y))),
        | '/' => Some(BinaryOp::new(2, Assoc::Left, Box::new(|x, y| x / y))),
        | '^' => Some(BinaryOp::new(3, Assoc::Right, Box::new(|x, y| x.powf(y)))),
        | _ => None,
        }
    } else { None };
    if let Some(op) = b { Some(Rc::new(op)) } else { None }
}

fn parse_atom(queue: &mut VecDeque<char>) -> Result<Expr, ParseError> {
    if let Some(u) = next_unary(&queue) {
        queue.pop_front();
        let arg = parse_expr(queue, u.prec)?;
        Ok(Expr::Unary(Rc::clone(&u), Box::new(arg)))
    } else if queue.front().map(|&chr| chr) == Some('(') {
        queue.pop_front();
        let arg = parse_expr(queue, 0)?;
        expect(&queue, Some(')'))?;
        queue.pop_front();
        Ok(arg)
    } else if let Some(x) = consume_literal(queue)? {
        Ok(Expr::Literal(x))
    } else {
        Err(ParseError::InvalidAtom) 
    }
}

fn next_unary(queue: &VecDeque<char>) -> Option<Rc<UnaryOp>> {
    if let Some(&chr) = queue.front() {
        match chr {
        | '-' => Some(Rc::new(UnaryOp::new(1, Box::new(|x| -x)))),
        | _ => None,
        }
    } else { None }
}

fn consume_literal(queue: &mut VecDeque<char>)
    -> Result<Option<f64>, num::ParseFloatError> {
    let mut numchars: Vec<char> = (b'0'..b'9').map(char::from).collect();
    numchars.push('.');
    numchars.push('e');
    numchars.push('E');
    let mut literal = String::new();
    
    while let Some(chr) = queue.front() {
        let mut is_numeric = false;
        for d in numchars.iter() {
            is_numeric |= chr == d; 
        }
        if !is_numeric { break; }
        
        literal.push(queue.pop_front().unwrap());
    }
    
    if literal.len() == 0 { return Ok(None); }
    let x: f64 = literal.parse()?;
    Ok(Some(x))
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn construct_expr() {
        let e = Expr::Literal(4.0);
        let u = UnaryOp::new(1, Box::new(|x| -x));
        let e = Expr::Unary(Rc::new(u), Box::new(e));
        let e2 = Expr::Literal(2.0);
        let b = BinaryOp::new(1, Assoc::Left, Box::new(|x, y| x + y));
        let e = Expr::Binary(Rc::new(b), Box::new(e), Box::new(e2));
        
        assert_eq!(e.eval(), -2.0);
    }
    
    #[test]
    fn add_twice() {
        let x = Expr::Literal(1.0);
        let y = Expr::Literal(2.0);
        let z = Expr::Literal(3.0);
        let plus = Rc::new(BinaryOp::new(1, Assoc::Left, Box::new(|x, y| x + y)));
        let e = Expr::Binary(Rc::clone(&plus), Box::new(y), Box::new(z));
        let e = Expr::Binary(Rc::clone(&plus), Box::new(x), Box::new(e));
        
        assert_eq!(e.eval(), 6.0);
    }
    
    #[test]
    fn parse_invalid() {
        let result = parse("something");
        assert!(result.is_err());
    }
    
    #[test]
    fn parse_valid() {
        let result = parse("2+2");
        assert!(!result.is_err());
    }
    
    #[test]
    fn evaluate_expr() {
        let expr = parse("2*3-0.5*2^2-0.25*2^2^2").unwrap();
        assert_eq!(expr.eval(), 0.0);
    }
}
