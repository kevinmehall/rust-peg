use test_grammar::{consonants, options, list};
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
	assert_eq!(list("5,"), Ok(vec![5]));
	assert_eq!(list("1,2,3,4,"), Ok(vec![1,2,3,4]));
}