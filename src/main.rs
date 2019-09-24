use std::env;

fn main() {
    let args : Vec<String> = env::args().collect();
    let x = rustcalc::parse(&args[1]);
    match x {
    | Ok(expr) => println!("{}", expr.eval()), 
    | Err(e) => println!("Error parsing input: {}", e),
    }
}
