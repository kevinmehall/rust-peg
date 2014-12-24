#![feature(phase)]

#[phase(plugin)]
extern crate peg_syntax_ext;

use arithmetic::expression;

peg! arithmetic(r#"
#[pub]
expression -> int
	= sum

sum -> int
	= l:product "+" r:product { l+r }
	/ product

product -> int
	= l:atom "*" r:atom { l*r }
	/ atom

atom -> int
	= number
	/ "(" v:sum ")" { v }

number -> int
	= [0-9]+ { from_str::<int>(match_str).unwrap() }
"#);

fn main() {
	assert_eq!(expression("1+1"), Ok(2));
	assert_eq!(expression("5*5"), Ok(25));
	assert_eq!(expression("222+3333"), Ok(3555));
	assert_eq!(expression("2+3*4"), Ok(14));
	assert_eq!(expression("(2+2)*3"), Ok(12));
	assert!(expression("(22+)+1").is_err());
	assert!(expression("1++1").is_err());
	assert!(expression("3)+1").is_err());
	println!("Ok");
}
