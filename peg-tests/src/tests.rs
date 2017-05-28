use std::collections::HashMap;

const FOO: i32 = 42;

mod test_grammar {
    include!(concat!(env!("OUT_DIR"), "/test_grammar.rs"));
}

use self::test_grammar::*;

#[test]
fn test_neg_assert() {
	assert!(consonants("qwrty").is_ok());
	assert!(consonants("rust").is_err());
}

#[test]
fn test_pos_assert() {
    assert_eq!(lookahead_result("abcd"), Ok("abc"));
    assert!(lookahead_result("abc").is_err());
}

#[test]
fn test_eof() {
	assert_eq!(expect_nothing("t"), Ok(()));
	match expect_nothing("tt") {
		Err(e) => println!("{}", e),
		Ok(_) => panic!("should not happen")
	};
}

#[test]
fn test_optional() {
	assert_eq!(options("abc"), Ok(None));
	assert_eq!(options("abcdef"), Ok(Some(())));
	assert!(options("def").is_err());
}

#[test]
fn test_list() {
	assert_eq!(list("5"), Ok(vec![5]));
	assert_eq!(list("1,2,3,4"), Ok(vec![1,2,3,4]));
}

#[test]
fn test_repeat() {
	assert!(repeat_n("123").is_err());
	assert_eq!(repeat_n("1234"), Ok(vec![1,2,3,4]));
	assert!(repeat_n("12345").is_err());

	assert!(repeat_min("").is_err());
	assert!(repeat_min("1").is_err());
	assert_eq!(repeat_min("12"), Ok(vec![1,2]));
	assert_eq!(repeat_min("123"), Ok(vec![1,2,3]));

	assert_eq!(repeat_max(""), Ok(vec![]));
	assert_eq!(repeat_max("1"), Ok(vec![1]));
	assert_eq!(repeat_max("12"), Ok(vec![1,2]));
	assert!(repeat_max("123").is_err());

	assert!(repeat_min_max("").is_err());
	assert!(repeat_min_max("1").is_err());
	assert_eq!(repeat_min_max("12"), Ok(vec![1,2]));
	assert_eq!(repeat_min_max("123"), Ok(vec![1,2,3]));
	assert!(repeat_min_max("1234").is_err());

	assert!(repeat_sep_3("1,2").is_err());
	assert!(repeat_sep_3("1,2,3,4").is_err());
	assert_eq!(repeat_sep_3("1,2,3"), Ok(vec![1,2,3]));

	assert_eq!(repeat_variable("1a3abc222"), Ok(vec!["a", "abc", "22"]));
}

#[test]
// before we were testing string matches using .slice(), which
// threw an ugly panic!() when we compared unequal character
// boundaries.. this popped up while parsing unicode
fn test_boundaries() {
	assert!(boundaries("f↙↙↙↙").is_err());
	assert!(case_insensitive("f↙↙↙↙").is_err());
}

#[test]
fn test_borrowed() {
	assert_eq!(borrowed("abcd"), Ok("abcd"));
}

#[test]
fn test_lifetime_parameter() {
	assert_eq!(&*lifetime_parameter("abcd").unwrap(), "abcd");
	assert_eq!(&*lifetime_parameter("COW").unwrap(), "cow");
}

#[test]
fn test_block() {
	assert_eq!(block("foo"), Ok("foo"));
}

#[test]
fn test_keyval() {
    let mut expected = HashMap::new();
    expected.insert(1, 3);
    expected.insert(2, 4);
    assert_eq!(keyvals("1:3\n2:4"), Ok(expected));
}

#[test]
fn test_case_insensitive() {
	assert_eq!(case_insensitive("foo").unwrap(), "foo");
	assert_eq!(case_insensitive("FoO").unwrap(), "FoO");
	assert_eq!(case_insensitive("fOo").unwrap(), "fOo");
	assert_eq!(case_insensitive("FOO").unwrap(), "FOO");
	assert!(case_insensitive("boo").is_err());
	assert!(case_insensitive(" foo").is_err());
	assert!(case_insensitive("foo ").is_err());
}

#[test]
fn test_position() {
	assert_eq!(position("aaaabbb").unwrap(), (0, 4, 7));
}

#[test]
fn test_templates() {
    assert_eq!(parens("(asdf)").unwrap(), "asdf");
    assert_eq!(double_parens("((asdf))").unwrap(), "asdf");
}

#[test]
fn test_renamed_imports() {
	assert_eq!(renamed_imports("").unwrap(), (42, 42));
}

#[test]
fn test_neg_lookahead_err() {
	let err = neg_lookahead_err("ac").err().unwrap();
	assert_eq!(err.expected.len(), 1, "expected set includes: {:?}", err.expected);
	assert_eq!(err.offset, 1);
}

#[test]
fn test_infix_arith() {
	assert_eq!(infix_arith("3+3*3+3"), Ok(15));
	assert_eq!(infix_arith("2+2^2^2^2/2+2"), Ok(32772));
	assert_eq!(infix_arith("1024/2/2/2+1"), Ok(129));
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum InfixAst {
    Ident(String),
    Add(Box<InfixAst>, Box<InfixAst>),
    Op(String, Box<InfixAst>, Box<InfixAst>)
}

#[test]
fn test_infix_ast(){
	assert_eq!(infix_ast("a + b `x` c").unwrap(),
		InfixAst::Add(
			Box::new(InfixAst::Ident("a".to_owned())),
			Box::new(InfixAst::Op("x".to_owned(),
				Box::new(InfixAst::Ident("b".to_owned())),
				Box::new(InfixAst::Ident("c".to_owned()))
			))
		)
	)
}

#[test]
fn test_error_pos() {
    let err = error_pos("aab\n").unwrap_err();
    assert_eq!(err.line, 1);
    assert_eq!(err.column, 3);
    assert_eq!(err.offset, 2);
    assert_eq!(err.expected, ["\r", "\n", "a"].iter().map(|x| *x).collect());

    let err = error_pos("aa\naaaa\nbaaa\n").unwrap_err();
    assert_eq!(err.line, 3);
    assert_eq!(err.column, 1);

    let err = error_pos("aa\naaaa\naaab\naa").unwrap_err();
    assert_eq!(err.line, 3);
    assert_eq!(err.column, 4);

    let err = error_pos("aa\r\naaaa\r\naaab\r\naa").unwrap_err();
    assert_eq!(err.line, 3);
    assert_eq!(err.column, 4);
}
