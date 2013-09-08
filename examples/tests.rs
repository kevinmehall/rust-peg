use test_grammar::*;
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
	assert_eq!(list("5,"), Ok(~[5]));
	assert_eq!(list("1,2,3,4,"), Ok(~[1,2,3,4]));
}