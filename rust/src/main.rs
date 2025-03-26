use rustyline::Editor;
use rustyline::history::DefaultHistory;

#[derive(Debug, Clone, PartialEq)]
enum TokenType {
    Number,
    Plus,
    Minus,
    Multiply,
    Divide,
    Power,
    LeftParen,
    RightParen,
    EOF,
}

#[derive(Debug, Clone)]
struct Token {
    token_type: TokenType,
    value: f64,
}

struct Lexer {
    input: String,
    pos: usize,
}

impl Lexer {
    fn new(input: String) -> Self {
        Lexer { input, pos: 0 }
    }

    fn peek(&self) -> Option<char> {
        self.input.chars().nth(self.pos)
    }

    fn advance(&mut self) {
        self.pos += 1;
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek() {
            if !c.is_whitespace() {
                break;
            }
            self.advance();
        }
    }

    fn read_number(&mut self) -> Token {
        let start = self.pos;
        while let Some(c) = self.peek() {
            if !c.is_digit(10) && c != '.' {
                break;
            }
            self.advance();
        }
        let number = self.input[start..self.pos].parse::<f64>().unwrap();
        Token {
            token_type: TokenType::Number,
            value: number,
        }
    }

    fn get_next_token(&mut self) -> Token {
        self.skip_whitespace();

        if let Some(c) = self.peek() {
            self.advance();
            match c {
                '+' => Token {
                    token_type: TokenType::Plus,
                    value: 0.0,
                },
                '-' => Token {
                    token_type: TokenType::Minus,
                    value: 0.0,
                },
                '*' => Token {
                    token_type: TokenType::Multiply,
                    value: 0.0,
                },
                '/' => Token {
                    token_type: TokenType::Divide,
                    value: 0.0,
                },
                '^' => Token {
                    token_type: TokenType::Power,
                    value: 0.0,
                },
                '(' => Token {
                    token_type: TokenType::LeftParen,
                    value: 0.0,
                },
                ')' => Token {
                    token_type: TokenType::RightParen,
                    value: 0.0,
                },
                c if c.is_digit(10) => {
                    self.pos -= 1;
                    self.read_number()
                }
                _ => panic!("Invalid character: {}", c),
            }
        } else {
            Token {
                token_type: TokenType::EOF,
                value: 0.0,
            }
        }
    }

    fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        loop {
            let token = self.get_next_token();
            tokens.push(token.clone());
            if token.token_type == TokenType::EOF {
                break;
            }
        }
        tokens
    }
}

struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, current: 0 }
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn advance(&mut self) {
        self.current += 1;
    }

    fn expression(&mut self) -> f64 {
        let mut left = self.term();

        while let Some(token) = self.tokens.get(self.current) {
            match token.token_type {
                TokenType::Plus | TokenType::Minus => {
                    let op = token.token_type.clone();
                    self.advance();
                    let right = self.term();
                    match op {
                        TokenType::Plus => left += right,
                        TokenType::Minus => left -= right,
                        _ => unreachable!(),
                    }
                }
                _ => break,
            }
        }

        left
    }

    fn term(&mut self) -> f64 {
        let mut left = self.factor();

        while let Some(token) = self.tokens.get(self.current) {
            match token.token_type {
                TokenType::Multiply | TokenType::Divide | TokenType::Power => {
                    let op = token.token_type.clone();
                    self.advance();
                    let right = self.factor();
                    match op {
                        TokenType::Multiply => left *= right,
                        TokenType::Divide => {
                            if right == 0.0 {
                                panic!("Division by zero error!");
                            }
                            left /= right;
                        }
                        TokenType::Power => left = left.powf(right),
                        _ => unreachable!(),
                    }
                }
                _ => break,
            }
        }

        left
    }

    fn factor(&mut self) -> f64 {
        let token_type = self.peek().token_type.clone();
        let value = self.peek().value;
        self.advance();

        match token_type {
            TokenType::Number => value,
            TokenType::LeftParen => {
                let result = self.expression();
                if self.peek().token_type != TokenType::RightParen {
                    panic!("Expected closing parenthesis");
                }
                self.advance();
                result
            }
            _ => panic!("Unexpected token"),
        }
    }
}

fn evaluate_expression(input: &str) -> Result<f64, String> {
    let mut lexer = Lexer::new(input.to_string());
    let tokens = lexer.tokenize();
    let mut parser = Parser::new(tokens);

    match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| parser.expression())) {
        Ok(result) => Ok(result),
        Err(_) => Err("Invalid expression".to_string()),
    }
}

fn main() {
    let mut rl = Editor::<(), DefaultHistory>::new().expect("Failed to create line editor");
    println!("Input:");

    loop {
        match rl.readline("") {
            Ok(line) => {
                if line.trim().is_empty() {
                    break;
                }

                let _ = rl.add_history_entry(line.as_str());

                match evaluate_expression(&line) {
                    Ok(result) => println!("Output: {}", result),
                    Err(e) => println!("Error: {}", e),
                }
            }
            Err(_) => break,
        }
    }
} 