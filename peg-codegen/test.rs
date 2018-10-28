use ::{ grammar, translate, PegCompiler };

fn compile(src: &str) -> PegCompiler {
	let mut compiler = PegCompiler::new();
	let file = compiler.codemap.add_file("input.test".into(), src.into());

	let ast_items = grammar::items(&file.source(), file.span).unwrap();
	translate::Grammar::from_ast(&mut compiler, ast_items).unwrap();

    compiler
}

#[test]
fn test_finds_left_recursion() {
	let compiler = compile(r#"
foo
	= "foo" foo
	/ bar

bar
	= "bar" bar
	/ foo
"#);

	assert_eq!(compiler.diagnostics[0].message, "Found illegal left recursion for rule foo: foo -> bar -> foo");
	assert_eq!(compiler.diagnostics[1].message, "Found illegal left recursion for rule bar: bar -> foo -> bar");
	assert_eq!(compiler.diagnostics.len(), 2);
}

#[test]
fn test_finds_direct_left_recursion() {
	let compiler = compile(r#"
foo
	= foo
"#);

	assert_eq!(compiler.diagnostics[0].message, "Found illegal left recursion for rule foo: foo -> foo");
	assert_eq!(compiler.diagnostics.len(), 1);
}