use super::expression::Expression;

#[derive(Debug)]
pub enum Statement {
    Block(Vec<Statement>),
    Conditional(Expression, Box<Statement>, Option<Box<Statement>>),
    LocalVar(),
}

impl Statement {
    pub fn pretty_print(&self, depth: usize) {
        for i in 0..depth {
            if i == depth - 1 {
                print!("├─ ");
            } else {
                print!("│  ");
            }
        }

        match &self {
            &Statement::Block(statements) => {
                println!("block");
                for s in statements.iter() {
                    s.pretty_print(depth + 1);
                }
            }
            &Statement::LocalVar(..) => println!("local"),
            &Statement::Conditional(..) => println!("conditioanl"),
        }
    }
}
