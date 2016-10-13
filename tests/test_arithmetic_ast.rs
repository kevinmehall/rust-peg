#![feature(plugin)]
#![plugin(peg_syntax_ext)]
use arithmetic::expression;

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Expression {
	Number(i64),
	Sum(Box<Expression>, Box<Expression>),
	Product(Box<Expression>, Box<Expression>),
}

peg! arithmetic(r#"
use super::Expression;

#[pub]
expression -> Expression
	= sum

sum -> Expression
	= l:product "+" r:product { Expression::Sum(Box::new(l), Box::new(r)) }
	/ product

product -> Expression
	= l:atom "*" r:atom { Expression::Product(Box::new(l), Box::new(r)) }
	/ atom

atom -> Expression
	= number
	/ "(" v:sum ")" { v }

number -> Expression
	= n:$([0-9]+) { Expression::Number(n.parse().unwrap()) }
"#);

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
