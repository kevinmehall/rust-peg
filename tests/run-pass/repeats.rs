extern crate peg;
#[macro_use] extern crate assert_matches;

peg::parser!( grammar repeats() for str {
    rule number() -> i64
        = n:$(['0'..='9']+) { n.parse().unwrap() }

    pub rule list() -> Vec<i64>
        = n:(number() ** ",") { n.to_vec() }

    rule digit() -> i64
        = n:$(['0'..='9']) {n.parse().unwrap() }

    pub rule repeat_n() -> Vec<i64>
        = n:(digit()*<4>) { n.to_vec() }

    pub rule repeat_min() -> Vec<i64>
        = n:(digit()*<2,>) { n.to_vec() }

    pub rule repeat_max() -> Vec<i64>
        = n:(digit()*<,2>) { n.to_vec() }

    pub rule repeat_min_max() -> Vec<i64>
        = n:(digit()*<2,3>) { n.to_vec() }

    pub rule repeat_sep_3() -> Vec<i64>
        = n:(digit() **<3> ",") { n.to_vec() }

    pub rule repeat_variable() -> Vec<&'input str>
        = n:(count:digit() s:$(['a'..='z'|'0'..='9']*<{count as usize}>) {s})* { n.to_vec() }
});

use repeats::*;

fn main() {
    assert_matches!(list("5").as_deref(), Ok([5]));
    assert_matches!(list("1,2,3,4").as_deref(), Ok([1,2,3,4]));

    assert!(repeat_n("123").is_err());
    assert_matches!(repeat_n("1234").as_deref(), Ok([1,2,3,4]));
    assert!(repeat_n("12345").is_err());

    assert!(repeat_min("").is_err());
    assert!(repeat_min("1").is_err());
    assert_matches!(repeat_min("12").as_deref(), Ok([1,2]));
    assert_matches!(repeat_min("123").as_deref(), Ok([1,2,3]));

    assert_matches!(repeat_max("").as_deref(), Ok([]));
    assert_matches!(repeat_max("1").as_deref(), Ok([1]));
    assert_matches!(repeat_max("12").as_deref(), Ok([1,2]));
    assert!(repeat_max("123").is_err());

    assert!(repeat_min_max("").is_err());
    assert!(repeat_min_max("1").is_err());
    assert_matches!(repeat_min_max("12").as_deref(), Ok([1,2]));
    assert_matches!(repeat_min_max("123").as_deref(), Ok([1,2,3]));
    assert!(repeat_min_max("1234").is_err());

    assert!(repeat_sep_3("1,2").is_err());
    assert!(repeat_sep_3("1,2,3,4").is_err());
    assert_matches!(repeat_sep_3("1,2,3").as_deref(), Ok([1,2,3]));

    assert_matches!(repeat_variable("1a3abc222").as_deref(), Ok(["a", "abc", "22"]));
}