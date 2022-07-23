mod tokenizer;
mod parser;
mod evaluator;

fn main() {
    let tokens = tokenizer::tokenize(&mut tokenizer::Input::new());
    println!("TOKENS:\n{:?}\n", tokens);
    let expressions = parser::parse(&mut tokens.iter().cloned().peekable());
    
    println!("EXPRESSIONS:");
    for expr in expressions.iter() {
        println!("{expr}");
    }

    println!("\nEVALUATION:");
    let mut scope = evaluator::Scope::new();
    for expr in expressions {
        match evaluator::evaluate_expr(expr, &mut scope) {
            Some(value) => println!("{}", value),
            _ => {},
        }
    }
}
