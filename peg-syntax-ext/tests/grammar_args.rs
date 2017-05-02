#![feature(plugin)]
#![plugin(peg_syntax_ext)]

use std::collections::HashMap;

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub struct Identifier(usize);

pub struct Interner {
    forward: HashMap<String, Identifier>,
    backward: Vec<String>
}

impl Interner {
    fn new() -> Interner {
        Interner { forward: HashMap::new(), backward: Vec::new() }
    }

    fn intern(&mut self, s: &str) -> Identifier {
        if let Some(&id) = self.forward.get(s) { id } else {
            let id = Identifier(self.backward.len());
            self.backward.push(s.to_owned());
            self.forward.insert(s.to_owned(), id);
            id
        }
    }
}

#[derive(Eq, PartialEq, Debug)]
pub struct Spanned<T> {
    file_id: usize,
    start: usize,
    end: usize,
    node: T,
}

impl<T> Spanned<T> {
    fn with_node<U>(&self, node: U) -> Spanned<U> {
        Spanned { file_id: self.file_id, start: self.start, end: self.end, node }
    }
}

#[derive(Eq, PartialEq, Debug)]
pub enum Node {
    Variable(Identifier),
    Add(Box<Spanned<Node>>, Box<Spanned<Node>>),
    Mul(Box<Spanned<Node>>, Box<Spanned<Node>>),
}

peg! parser(r#"
use super::{Interner, Identifier, Spanned, Node};

#![arguments(file_id: usize, interner: &mut Interner)]

pub outer_rule -> Vec<Spanned<Identifier>> = inner_rule**","

inner_rule -> Spanned<Identifier> = spanned<identifier>

spanned<X>
    = start:#position node:X end:#position
    { Spanned { file_id, start, end, node} }

identifier -> Identifier
    = s:$([a-z]+) { interner.intern(s) }

variable -> Spanned<Node>
    = v:spanned<identifier>
    { v.with_node(Node::Variable(v.node)) }

pub arith_ast -> Spanned<Node> = #infix<variable> {
	#L x op:spanned<"+"> y { op.with_node(Node::Add(Box::new(x), Box::new(y))) }
	#L x op:spanned<"*"> y { op.with_node(Node::Mul(Box::new(x), Box::new(y))) }
}

"#);

#[test]
fn test_grammar_args() {
    let mut interner = Interner::new();
    let result = parser::outer_rule("abcd,xyz,foobar,xyz", 23, &mut interner).unwrap();

    assert_eq!(result, vec![
        Spanned { file_id: 23, start: 0,  end: 4,  node: Identifier(0) },
        Spanned { file_id: 23, start: 5,  end: 8,  node: Identifier(1) },
        Spanned { file_id: 23, start: 9,  end: 15, node: Identifier(2) },
        Spanned { file_id: 23, start: 16, end: 19, node: Identifier(1) },
    ]);
}

#[test]
fn test_grammar_args_infix() {
    let mut interner = Interner::new();
    let result = parser::arith_ast("baz*foo+baz", 55, &mut interner).unwrap();
    assert_eq!(result, Spanned { file_id: 55, start: 7, end: 8, node:
        Node::Add(
            Box::new(Spanned { file_id: 55, start: 3, end: 4, node:
                Node::Mul(
                    Box::new(Spanned { file_id: 55, start: 0, end: 3, node: Node::Variable(Identifier(0))}),
                    Box::new(Spanned { file_id: 55, start: 4, end: 7, node: Node::Variable(Identifier(1))})
                )
            }),
            Box::new(Spanned { file_id: 55, start: 8, end: 11, node: Node::Variable(Identifier(0))})
        )
    })
}
