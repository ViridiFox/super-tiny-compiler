use std::{
    cell::{RefCell, RefMut},
    iter::Peekable,
    rc::Rc,
};

type RcCell<T> = Rc<RefCell<T>>;
macro_rules! shared_mut {
    ($e:expr) => {
        ::std::rc::Rc::new(::std::cell::RefCell::new($e))
    };
}

macro_rules! unwrap_rc {
    ($e:expr) => {
        ::std::cell::RefCell::into_inner(::std::rc::Rc::into_inner($e).unwrap())
    };
}

#[derive(Debug, PartialEq)]
pub enum Token {
    ParenLeft,
    ParenRight,
    Name(String),
    Number(u64),
    String(String),
}

pub fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut input = input.chars().peekable();

    'tokenize: while let Some(&(mut c)) = input.peek() {
        match c {
            '(' => {
                tokens.push(Token::ParenLeft);
                input.next();
            }
            ')' => {
                tokens.push(Token::ParenRight);
                input.next();
            }
            c if c.is_whitespace() => {
                input.next();
            }
            '0'..='9' => {
                let mut value = String::new();

                while matches!(c, '0'..='9') {
                    value.push(c);
                    input.next();
                    if let Some(&c_next) = input.peek() {
                        c = c_next;
                    } else {
                        tokens.push(Token::Number(value.parse().unwrap()));
                        break 'tokenize;
                    }
                }

                tokens.push(Token::Number(value.parse().unwrap()));
            }
            '"' => {
                let mut value = String::new();

                // skip starting quote
                input.next();
                let Some(&(mut c)) = input.peek() else {
                    panic!("empty string at the end of input");
                };

                while c != '"' {
                    value.push(c);
                    input.next();
                    if let Some(&c_next) = input.peek() {
                        c = c_next;
                    } else {
                        tokens.push(Token::String(value));
                        break 'tokenize;
                    }
                }

                tokens.push(Token::String(value));
                input.next();
            }
            'a'..='z' => {
                let mut value = String::new();

                while matches!(c, 'a'..='z') {
                    value.push(c);
                    input.next();
                    if let Some(&c_next) = input.peek() {
                        c = c_next;
                    } else {
                        tokens.push(Token::Name(value));
                        break 'tokenize;
                    }
                }

                tokens.push(Token::Name(value));
            }
            _ => {
                panic!("Unknown character '{c}'");
            }
        }
    }

    tokens
}

#[derive(Debug, PartialEq, Clone)]
pub enum ParsedNode {
    Program(Vec<ParsedNode>),
    CallExpression {
        name: String,
        params: Vec<ParsedNode>,
    },
    NumberLiteral(u64),
    StringLiteral(String),
}

pub fn parse(tokens: Vec<Token>) -> ParsedNode {
    fn walk(tokens: &mut Peekable<impl Iterator<Item = Token>>) -> ParsedNode {
        let token = tokens.next().unwrap();

        match token {
            Token::Number(num) => ParsedNode::NumberLiteral(num),
            Token::String(string) => ParsedNode::StringLiteral(string),
            Token::ParenLeft => {
                let token = tokens.next();
                let Some(Token::Name(name)) = token else {
                    panic!("expected name, found '{token:?}'");
                };
                let mut params = Vec::new();

                while tokens.peek() != Some(&Token::ParenRight) {
                    params.push(walk(tokens));
                }
                tokens.next();

                ParsedNode::CallExpression { name, params }
            }
            token => panic!("unexpected token '{token:?}'"),
        }
    }

    let mut tokens = tokens.into_iter().peekable();
    let mut body = Vec::new();

    while tokens.peek().is_some() {
        body.push(walk(&mut tokens));
    }

    ParsedNode::Program(body)
}

trait Visitor {
    fn enter_program(&mut self, _node: &ParsedNode) {}
    fn exit_program(&mut self, _node: &ParsedNode) {}

    fn enter_call_expression(&mut self, _node: &ParsedNode) {}
    fn exit_call_expression(&mut self, _node: &ParsedNode) {}

    fn visit_number_literal(&mut self, _node: &ParsedNode) {}
    fn visit_string_literal(&mut self, _node: &ParsedNode) {}
}

fn traverse(ast: ParsedNode, visitor: &mut impl Visitor) {
    fn traverse_array(array: Vec<ParsedNode>, visitor: &mut impl Visitor) {
        array
            .into_iter()
            .for_each(|child| traverse_node(child, visitor));
    }

    fn traverse_node(node: ParsedNode, visitor: &mut impl Visitor) {
        match node.clone() {
            ParsedNode::Program(body) => {
                visitor.enter_program(&node);
                traverse_array(body, visitor);
                visitor.exit_program(&node);
            }
            ParsedNode::CallExpression { params, .. } => {
                visitor.enter_call_expression(&node);
                traverse_array(params, visitor);
                visitor.exit_call_expression(&node);
            }
            ParsedNode::NumberLiteral(_) => {
                visitor.visit_number_literal(&node);
            }
            ParsedNode::StringLiteral(_) => {
                visitor.visit_string_literal(&node);
            }
        }
    }

    traverse_node(ast, visitor);
}

#[derive(Debug, PartialEq)]
pub enum Node {
    Program(Vec<RcCell<Node>>),
    ExpressionStatement(RcCell<Node>),
    CallExpression {
        callee: RcCell<Node>,
        arguments: Vec<RcCell<Node>>,
    },
    Identifier(String),
    StringLiteral(String),
    NumberLiteral(u64),
}

struct Transformer {
    ast: RcCell<Node>,
    context_stack: Vec<RcCell<Node>>,
}

impl Transformer {
    fn new() -> Self {
        let ast = Rc::new(RefCell::new(Node::Program(Vec::new())));
        let context_stack = vec![Rc::clone(&ast)];
        Transformer { ast, context_stack }
    }

    fn push_in_context(&mut self, node: RcCell<Node>) {
        RefMut::map(
            RefCell::borrow_mut(
                &self
                    .context_stack
                    .last()
                    .expect("context to exist for pushing"),
            ),
            |context| match context {
                Node::Program(nodes) => nodes,
                Node::CallExpression { arguments, .. } => arguments,
                node => panic!("can't push node to '{node:?}'"),
            },
        )
        .push(node);
    }

    fn finish(self) -> Node {
        drop(self.context_stack);

        Rc::into_inner(self.ast).unwrap().into_inner()
    }
}

impl Visitor for Transformer {
    fn visit_string_literal(&mut self, node: &ParsedNode) {
        if let ParsedNode::StringLiteral(string) = node {
            self.push_in_context(shared_mut!(Node::StringLiteral(string.clone())));
        }
    }

    fn visit_number_literal(&mut self, node: &ParsedNode) {
        if let ParsedNode::NumberLiteral(number) = node {
            self.push_in_context(shared_mut!(Node::NumberLiteral(*number)));
        }
    }

    fn enter_call_expression(&mut self, node: &ParsedNode) {
        if let ParsedNode::CallExpression { name, .. } = node {
            let node = shared_mut!(Node::CallExpression {
                callee: shared_mut!(Node::Identifier(name.clone())),
                arguments: Vec::new(),
            });

            let mut needs_expression = false;

            RefMut::map(
                RefCell::borrow_mut(self.context_stack.last().unwrap()),
                |context| {
                    if let Node::CallExpression { .. } = context {
                        needs_expression = false;
                    } else {
                        needs_expression = true;
                    }
                    context
                },
            );

            if needs_expression {
                self.push_in_context(shared_mut!(Node::ExpressionStatement(Rc::clone(&node))));
            } else {
                self.push_in_context(Rc::clone(&node));
            }

            self.context_stack.push(node);
        }
    }

    fn exit_call_expression(&mut self, _node: &ParsedNode) {
        self.context_stack.pop();
    }
}

pub fn transform(ast: ParsedNode) -> Node {
    let mut transformer = Transformer::new();

    traverse(ast, &mut transformer);

    transformer.finish()
}

pub fn codegen(ast: Node) -> String {
    match ast {
        Node::Program(body) => body
            .into_iter()
            .map(|node| codegen(unwrap_rc!(node)))
            .collect::<Vec<String>>()
            .join("\n"),
        Node::ExpressionStatement(expression) => codegen(unwrap_rc!(expression)) + ";",
        Node::CallExpression { callee, arguments } => {
            codegen(unwrap_rc!(callee))
                + "("
                + &arguments
                    .into_iter()
                    .map(|node| codegen(unwrap_rc!(node)))
                    .collect::<Vec<String>>()
                    .join(", ")
                + ")"
        }
        Node::Identifier(name) => name,
        Node::NumberLiteral(number) => number.to_string(),
        Node::StringLiteral(string) => "\"".to_string() + &string + "\"",
    }
}

pub fn compile(input: &str) -> String {
    let tokens = tokenize(input);
    let ast = parse(tokens);
    let new_ast = transform(ast);
    return codegen(new_ast);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let input = "(add 2 (subtract 4 2))";
        let output = "add(2, subtract(4, 2));";

        let tokens = vec![
            Token::ParenLeft,
            Token::Name("add".to_string()),
            Token::Number(2),
            Token::ParenLeft,
            Token::Name("subtract".to_string()),
            Token::Number(4),
            Token::Number(2),
            Token::ParenRight,
            Token::ParenRight,
        ];

        let ast = ParsedNode::Program(vec![ParsedNode::CallExpression {
            name: "add".to_string(),
            params: vec![
                ParsedNode::NumberLiteral(2),
                ParsedNode::CallExpression {
                    name: "subtract".to_string(),
                    params: vec![ParsedNode::NumberLiteral(4), ParsedNode::NumberLiteral(2)],
                },
            ],
        }]);

        let new_ast = Node::Program(vec![shared_mut!(Node::ExpressionStatement(shared_mut!(
            Node::CallExpression {
                callee: shared_mut!(Node::Identifier("add".to_string())),
                arguments: vec![
                    shared_mut!(Node::NumberLiteral(2)),
                    shared_mut!(Node::CallExpression {
                        callee: shared_mut!(Node::Identifier("subtract".to_string())),
                        arguments: vec![
                            shared_mut!(Node::NumberLiteral(4)),
                            shared_mut!(Node::NumberLiteral(2))
                        ],
                    }),
                ],
            }
        )))]);

        assert_eq!(tokenize(input), tokens);
        assert_eq!(parse(tokens), ast);
        assert_eq!(transform(ast), new_ast);
        assert_eq!(codegen(new_ast), output);
        assert_eq!(compile(input), output);
    }
}
