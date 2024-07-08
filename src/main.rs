use std::fs;
use std::env;
use jsonp::scanner::Scanner;
//use jsonp::parser::Parser;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("usage: {} [filename]", args[0]);
        return;
    }

    let data = match fs::read_to_string(&args[1]) {
        Ok(data) => data,
        Err(err) => panic!("{:?}", err),
    };

    let tokens = Scanner::new(&data).scan().unwrap();
    for token in tokens {
        println!("{:?}", token);
    }
    
    //let expr = Parser::new(&data).unwrap().parse().unwrap();
    //println!("{:?}", expr);
}
