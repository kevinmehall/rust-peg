use test_grammar::{consonants, options, list, boundaries, borrowed, block};
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
fn test_block() {
	assert_eq!(block("foo"), Ok("foo"));
}
