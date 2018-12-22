use crate::{
	grammar::peg::peg_grammar,
	analysis::{Diagnostic, check},
	tokens::FlatTokenStream,
};

fn diagnostics(src: &str) -> Vec<Diagnostic> {
	let tokens = src.parse().expect("failed to tokenize");
	let ast = peg_grammar(&FlatTokenStream::new(tokens)).expect("failed to parse");

	let mut diagnostics = Vec::new();
	check(&ast, &mut |d| diagnostics.push(d));

	diagnostics
}

#[test]
fn test_finds_left_recursion() {
	let d = diagnostics(r#"
		grammar g() for str {
			rule foo
				= "foo" foo
				/ bar

			rule bar
				= "bar" bar
				/ foo
		}
	"#);

	assert_eq!(d.len(), 2);
	assert_eq!(d[0].msg(), "left recursive rules create an infinite loop: foo -> bar -> foo");
	assert_eq!(d[1].msg(), "left recursive rules create an infinite loop: bar -> foo -> bar");	
}

#[test]
fn test_finds_direct_left_recursion() {
	let d = diagnostics(r#"
		grammar g() for str {
			rule foo = foo
		}
	"#);

	assert_eq!(d[0].msg(), "left recursive rules create an infinite loop: foo -> foo");
	assert_eq!(d.len(), 1);
}