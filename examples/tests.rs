#![feature(globs)]
use test_grammar::*;
use std::collections::HashMap;
mod test_grammar;

#[test]
fn test_neg_assert() {
	assert!(consonants("qwrty").is_ok());
	assert!(consonants("rust").is_err());
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
}

#[test]
// before we were testing string matches using .slice(), which
// threw an ugly fail!() when we compared unequal character
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
