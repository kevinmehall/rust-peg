#![feature(plugin, collections, str_char)]
#![plugin(peg_syntax_ext)]

peg! parse(r#"

alist -> &'input str
    = "a"+ {match_str}

#[pub]
test -> &'input str
    = a0:alist "b" ~a0 {match_str}

tag -> &'input str
    = [a-z]+ {match_str}

#[pub]
xml -> ()
    = "<" name:tag ">" inner:xml* "</" ~name ">" {}

"#);

#[test]
fn test() {
    // matches `"a"+ "b" "a"+` if there is an equal number of "a"s on both sides
    assert_eq!(parse::test("aba"), Ok("aba"));
    assert_eq!(parse::test("aabaa"), Ok("aabaa"));
    assert!(parse::test("b").is_err());
    assert!(parse::test("ab").is_err());
    assert!(parse::test("aaabaa").is_err());

    // matches correctly closed and balanced XML tags (note that no whitespace or non-tag data is
    // supported)
    assert!(parse::xml("<a></a>").is_ok());
    assert!(parse::xml("<a><b></b></a>").is_ok());
    assert!(parse::xml("<html><head><title></title></head><body></body></html>").is_ok());

    assert!(parse::xml("<a><b></a></b>").is_err());
    assert!(parse::xml("<a><b></b></b>").is_err());
    assert!(parse::xml("<a><b></b></a></c>").is_err());
    assert!(parse::xml("<c><a><b></b></a>").is_err());
}
