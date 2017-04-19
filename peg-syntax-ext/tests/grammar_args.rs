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

peg! parser(r#"
use super::{Interner, Identifier, Spanned};

#![arguments(file_id: usize, interner: &mut Interner)]

pub outer_rule -> Vec<Spanned<Identifier>> = inner_rule**","

inner_rule -> Spanned<Identifier> = spanned<identifier>

spanned<X>
    = start:#position node:X end:#position
    { Spanned { file_id, start, end, node} }

identifier -> Identifier
    = s:$([a-z]+) { interner.intern(s) }

"#);

#[test]
fn test_errors() {
    let mut interner = Interner::new();
    let result = parser::outer_rule("abcd,xyz,foobar,xyz", 23, &mut interner).unwrap();

    assert_eq!(result, vec![
        Spanned { file_id: 23, start: 0,  end: 4,  node: Identifier(0) },
        Spanned { file_id: 23, start: 5,  end: 8,  node: Identifier(1) },
        Spanned { file_id: 23, start: 9,  end: 15, node: Identifier(2) },
        Spanned { file_id: 23, start: 16, end: 19, node: Identifier(1) },
    ]);

    println!("Ok");
}
