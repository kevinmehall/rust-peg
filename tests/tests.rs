extern crate peg;
use std::collections::HashMap;

const FOO: i32 = 42;

peg::peg!(test_grammar r#"
use std::collections::HashMap;
use std::borrow::{ToOwned, Cow};

pub consonants
	= (!['a'|'e'|'i'|'o'|'u']['a'..='z'])+

pub options -> Option<()>
	= "abc" v:"def"? {v}

number -> i64
	= n:$(['0'..='9']+) { n.parse().unwrap() }

pub list -> Vec<i64>
	= number ** ","

digit -> i64
	= n:$(['0'..='9']) {n.parse().unwrap() }

pub repeat_n -> Vec<i64>
	= digit*<4>

pub repeat_min -> Vec<i64>
	= digit*<2,>

pub repeat_max -> Vec<i64>
	= digit*<,2>

pub repeat_min_max -> Vec<i64>
	= digit*<2,3>

pub repeat_sep_3 -> Vec<i64>
	= digit **<3> ","

pub repeat_variable -> Vec<&'input str>
	= (count:digit s:$(['a'..='z'|'0'..='9']*<{count as usize}>) {s})*

pub boundaries -> String
	= n:$("foo") { n.to_string() }

pub borrowed -> &'input str
	= $(['a'..='z']+)

pub lifetime_parameter -> Cow<'input, str>
	= x:$(['a'..='z']+) { x.into() }
	/ "COW"  { "cow".to_owned().into() }

pub block -> &'input str
	= x:$(['a'..='z']+) {
		let result = x;
		result
	}

pub keyvals -> HashMap<i64, i64>
    = kvs:keyval ++ "\n" {
        let mut rv = HashMap::new();
        for &(k, v) in kvs.iter() {
           rv.insert(k, v);
        };
        rv
    }

keyval -> (i64, i64)
    = k:number ":" + v:number { (k, v) }

pub expect_nothing -> ()
	= ['a'..='z']

pub position -> (usize, usize, usize)
 = start:#position ['a']* middle:#position ['b']* end:#position { (start, middle, end) }

pub option_unused_result = "a"? / "b"

pub lookahead_result -> &'input str
  = v:&($(['a'..='c']*)) "abcd" { v }

parenthesized<foo> = "(" s:foo ")" { s }
pub parens -> &'input str = parenthesized<$(['a'..='z']*)>

double_parenthesized<x> = parenthesized<parenthesized<x>>
pub double_parens -> &'input str = double_parenthesized<$(['a'..='z']*)>

use super::FOO as F1;
use super::{FOO as F2};
pub renamed_imports -> (i32, i32) = { (F1, F2) }

pub neg_lookahead_err = !(['a']['b']) ['a']['x']

atom -> i64
	= "(" v:infix_arith ")" { v }
	/ number

pub(crate) infix_arith -> i64 = #infix<atom> {
	#L x:@ "+" y:@ { x + y }
	   x:@ "-" y:@ { x - y }
	       "-" v:@ { - v }
	#L x:@ "*" y:@ { x * y }
	   x:@ "/" y:@ { x / y }
	#R x:@ "^" y:@ { x.pow(y as u32) }
	   v:@ "!"     { (1..v+1).product() }
}

use super::InfixAst;

ident -> &'input str = $(['a'..='z']+)
haskell_op -> String = "`" i:ident "`" [' '|'\n']* { i.to_owned() }
infix_atom -> InfixAst = i:ident [' '|'\n']* { InfixAst::Ident(i.to_owned()) }
plus = "+" [' '|'\n']*

pub infix_ast -> InfixAst = #infix<infix_atom> {
	#L x:@ plus y:@ { InfixAst::Add(Box::new(x), Box::new(y)) }
	#L x:@ op:haskell_op y:@ { InfixAst::Op(op, Box::new(x), Box::new(y)) }
}

issue152 -> i32 // a
    = "5" { 5 //b
}

pub error_pos = ("a" / "\n" / "\r")*
"#);

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
	assert_eq!(infix_arith("1024/(1+1)/2/2+1"), Ok(129));
	assert_eq!(infix_arith("-1-2*-2"), Ok(3));
	assert_eq!(infix_arith("1+3!+1"), Ok(8));
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
