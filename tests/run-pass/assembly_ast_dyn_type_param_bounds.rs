extern crate peg;
use peg::parser;

// C++ in Rust
pub trait Operation<'a>: std::fmt::Debug + PartialEq + Eq + Clone{}
pub trait Operand<'a>: std::fmt::Debug + PartialEq + Eq + Clone{}
pub trait Location<'a>: Operand<'a> {}
impl<'a, T: ?Sized + Location<'a>> Operand<'a> for T {}

#[derive(Debug, PartialEq, Eq, Clone)]
struct Program<'a: 'b, 'b>(Vec<Box<dyn Operation<'a> + 'b>>);

#[derive(Debug, PartialEq, Eq, Clone)]
struct Add<'a: 'b, 'b> {
    result: Box<dyn Location<'a> + 'b>,
    lhs: Box<dyn Operand<'a> + 'b>,
    rhs: Box<dyn Operand<'a> + 'b>,
}
impl<'a: 'b, 'b> Operation<'a> for Add<'a, 'b> {}

#[derive(Debug, PartialEq, Eq, Clone)]
struct Sub<'a: 'b, 'b> {
    result: Box<dyn Location<'a> + 'b>,
    lhs: Box<dyn Operand<'a> + 'b>,
    rhs: Box<dyn Operand<'a> + 'b>,
}
impl<'a: 'b, 'b> Operation<'a> for Sub<'a, 'b> {}

#[derive(Debug, PartialEq, Eq, Clone)]
struct Register<'a>(&'a str);
impl<'a> Location<'a> for Register<'a> {}

#[derive(Debug, PartialEq, Eq, Clone)]
struct Global<'a>(&'a str);
impl<'a> Location<'a> for Global<'a> {}

#[derive(Debug, PartialEq, Eq, Clone)]
struct Literal(i32);
impl<'a> Operand<'a> for Literal {}

parser!{
grammar assembly<'b>() for str {
    pub rule program() -> Program<'input, 'b>
        = op:operation() ** "\n" { Program(op) }

    rule _ = [' ']*

    rule operation() -> Box<dyn Operation<'input> + 'b>
        = add() / sub()

    rule add() -> Box<Add<'input, 'b>>
        = result:location() _ "=" _ "add" _ lhs:operand() _ rhs:operand() { Box::new(Add{ result, lhs, rhs }) }

    rule sub() -> Box<Sub<'input, 'b>>
        = result:location() _ "=" _ "sub" _ lhs:operand() _ rhs:operand() { Box::new(Sub{ result, lhs, rhs }) }

    rule location() -> Box<dyn Location<'input> + 'b>
        = register() / global()

    rule register() -> Box<Register<'input>>
        = "%" _ id:identifier() { Box::new(Register(id)) }

    rule global() -> Box<Global<'input>>
        = "@" _ id:identifier() { Box::new(Global(id)) }

    rule identifier() -> &'input str
        = $(['a'..='z' | 'A'..='Z']+)

    rule operand() -> Box<dyn Operand<'input> + 'b>
        = location() / literal()
    
    rule literal() -> Box<Literal>
        = n:$(['0'..='9']+) {?
            let n = n.parse::<i32>().map_err(|_| "invalid int literal")?;
            Ok(ast::Constant::Integer(n))
        }

}}

fn main() {
    let Program(ops) = assembly::program("%apple = add 1 @g
@b = add 2 %a
%c = sub 82 @b
@dog = sub @b 12").unwrap();
    let iter = ops.into_iter();
    assert_eq!(iter.next(), Some(Add{
        result: Box::new(Register("apple")),
        lhs: Box::new(Literal(1)),
        rhs: Box::new(Global("g"))
    }));
    assert_eq!(iter.next(), Some(Add{
        result: Box::new(Global("b")),
        lhs: Box::new(Literal(2)),
        rhs: Box::new(Register("a"))
    }));
    assert_eq!(iter.next(), Some(Sub{
        result: Box::new(Register("c")),
        lhs: Box::new(Literal(82)),
        rhs: Box::new(Global("b"))
    }));
    assert_eq!(iter.next(), Some(Sub{
        result: Box::new("dog"),
        lhs: Box::new(Global("b")),
        rhs: Box::new(Literal(12))
    }));
    assert_eq!(iter.next(), None);
}
