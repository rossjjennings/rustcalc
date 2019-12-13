use std::env;

fn main() {
    let args : Vec<String> = env::args().collect();
    
    let arg = args.get(1);
    if arg == None {
        println!("Please enter an expression!");
        return;
    }
    let arg = arg.unwrap();
    
    let x = rustcalc::parse(arg);
    match x {
    | Ok(expr) => {
        let mut buffer = ryu::Buffer::new();
        println!("{}", buffer.format(expr.eval()));
      }, 
    | Err(e) => println!("Error parsing input: {}", e),
    }
}
