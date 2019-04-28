extern crate peg;

peg::parser!( grammar lookahead() for str {
    pub rule consonants()
	    = (!['a'|'e'|'i'|'o'|'u']['a'..='z'])+

    pub rule neg_lookahead_err() = !(['a']['b']) ['a']['x']

    pub rule lookahead_result() -> &'input str
        = v:&($(['a'..='c']*)) "abcd" { v }
});

#[test]
fn test_neg_assert() {
	assert!(lookahead::consonants("qwrty").is_ok());
	assert!(lookahead::consonants("rust").is_err());
}

#[test]
fn test_neg_lookahead_err() {
	let err = lookahead::neg_lookahead_err("ac").err().unwrap();
	assert_eq!(err.expected.tokens().count(), 1, "expected set includes: {}", err.expected);
	assert_eq!(err.location.offset, 1);
}

#[test]
fn test_pos_assert() {
    assert_eq!(lookahead::lookahead_result("abcd"), Ok("abc"));
    assert!(lookahead::lookahead_result("abc").is_err());
}
