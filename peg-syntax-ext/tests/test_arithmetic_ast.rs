#![feature(proc_macro_gen)]
extern crate peg_syntax_ext;
use peg_syntax_ext::peg_file;
use arithmetic::expression;

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Expression {
	Number(i64),
	Sum(Box<Expression>, Box<Expression>),
	Product(Box<Expression>, Box<Expression>),
}

peg_file!(arithmetic("arithmetic_ast.rustpeg"));

#[test]
fn main() {
	assert_eq!(expression("1+1"), Ok(Expression::Sum(
		Box::new(Expression::Number(1)),
		Box::new(Expression::Number(1)))
	));
	assert_eq!(expression("5*5"), Ok(Expression::Product(
		Box::new(Expression::Number(5)),
		Box::new(Expression::Number(5)))
	));
	assert_eq!(expression("2+3*4"), Ok(Expression::Sum(
		Box::new(Expression::Number(2)),
		Box::new(Expression::Product(
			Box::new(Expression::Number(3)),
			Box::new(Expression::Number(4))
		)),
	)));
	assert!(expression("(22+)+1").is_err());
	assert!(expression("1++1").is_err());
	assert!(expression("3)+1").is_err());
}
