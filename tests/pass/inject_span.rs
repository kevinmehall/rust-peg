use std::ops::Range;

#[derive(Debug, PartialEq)]
struct Node {
    span: Range<usize>,
    kind: NodeKind,
}

#[derive(Debug, PartialEq)]
enum NodeKind {
    Number(i64),
    Add(Box<Node>, Box<Node>),
    Sub(Box<Node>, Box<Node>),
    Mul(Box<Node>, Box<Node>),
    Div(Box<Node>, Box<Node>),
    Factorial(Box<Node>),
    Neg(Box<Node>),
    Group(Box<Node>),
    Var(String),
}

peg::parser!( grammar lang() for str {
    inject span(_input, lpos, rpos) -> Range<usize> { lpos..rpos }

    rule number() -> Node
        = n:$(['0'..='9']+) {? match n.parse() {
            Ok(n) => Ok(Node { span, kind: NodeKind::Number(n) }),
            Err(_) => Err("number too large"),
        }}

    rule var() -> Node
        = v:$(['a'..='z']+) { Node { span, kind: NodeKind::Var(v.to_string()) } }

    pub rule expr() -> Node = precedence!{
        x:(@) "+" y:@ { Node { span, kind: NodeKind::Add(Box::new(x), Box::new(y)) } }
        x:(@) "-" y:@ { Node { span, kind: NodeKind::Sub(Box::new(x), Box::new(y)) } }
              "-" v:@ { Node { span, kind: NodeKind::Neg(Box::new(v)) } }
        --
        x:(@) "*" y:@ { Node { span, kind: NodeKind::Mul(Box::new(x), Box::new(y)) } }
        x:(@) "/" y:@ { Node { span, kind: NodeKind::Div(Box::new(x), Box::new(y)) } }
        --
        v:@   "!"     { Node { span, kind: NodeKind::Factorial(Box::new(v)) } }
        --
        "(" v:expr() ")" { Node { span, kind: NodeKind::Group(Box::new(v)) } }
        v:var() { v }
        n:number() { n }
    }
});

#[test]
fn main() {
    assert_eq!(
        lang::expr("3+3*(-33+v!)"),
        Ok(Node {
            span: 0..12,
            kind: NodeKind::Add(
                Box::new(Node {
                    span: 0..1,
                    kind: NodeKind::Number(3),
                }),
                Box::new(Node {
                    span: 2..12,
                    kind: NodeKind::Mul(
                        Box::new(Node {
                            span: 2..3,
                            kind: NodeKind::Number(3),
                        }),
                        Box::new(Node {
                            span: 4..12,
                            kind: NodeKind::Group(Box::new(Node {
                                span: 5..11,
                                kind: NodeKind::Add(
                                    Box::new(Node {
                                        span: 5..8,
                                        kind: NodeKind::Neg(Box::new(Node {
                                            span: 6..8,
                                            kind: NodeKind::Number(33),
                                        })),
                                    }),
                                    Box::new(Node {
                                        span: 9..11,
                                        kind: NodeKind::Factorial(Box::new(Node {
                                            span: 9..10,
                                            kind: NodeKind::Var("v".to_string()),
                                        })),
                                    }),
                                ),
                            })),
                        }),
                    )
                }),
            ),
        })
    );
}

peg::parser!( grammar inject2(offset: usize) for str {
    inject span(_input, lpos, rpos) -> Range<usize> { (offset + lpos)..(offset + rpos) }
    inject text(input, lpos, rpos) -> &'input str { &input[lpos..rpos] }

    pub rule test() -> (Range<usize>, String)
        = "abc" { (span, text.to_string()) }
});

#[test]
fn inject2() {
    assert_eq!(inject2::test("abc", 10), Ok((10..13, "abc".into())));
}
