extern crate peg;

peg::parser!( grammar repeats() for str {
    rule number() -> i64
        = n:$(['0'..='9']+) { n.parse().unwrap() }

    pub rule list() -> Vec<i64>
        = number() ** ","

    rule digit() -> i64
        = n:$(['0'..='9']) {n.parse().unwrap() }

    pub rule repeat_n() -> Vec<i64>
        = digit()*<4>

    pub rule repeat_min() -> Vec<i64>
        = digit()*<2,>

    pub rule repeat_max() -> Vec<i64>
        = digit()*<,2>

    pub rule repeat_min_max() -> Vec<i64>
        = digit()*<2,3>

    pub rule repeat_sep_3() -> Vec<i64>
        = digit() **<3> ","

    pub rule repeat_variable() -> Vec<&'input str>
        = (count:digit() s:$(['a'..='z'|'0'..='9']*<{count as usize}>) {s})*
});

use repeats::*;

fn main() {
    assert_eq!(list("5").into_result(), Ok(vec![5]));
    assert_eq!(list("1,2,3,4").into_result(), Ok(vec![1,2,3,4]));

    assert!(repeat_n("123").into_result().is_err());
    assert_eq!(repeat_n("1234").into_result(), Ok(vec![1,2,3,4]));
    assert!(repeat_n("12345").into_result().is_err());

    assert!(repeat_min("").into_result().is_err());
    assert!(repeat_min("1").into_result().is_err());
    assert_eq!(repeat_min("12").into_result(), Ok(vec![1,2]));
    assert_eq!(repeat_min("123").into_result(), Ok(vec![1,2,3]));

    assert_eq!(repeat_max("").into_result(), Ok(vec![]));
    assert_eq!(repeat_max("1").into_result(), Ok(vec![1]));
    assert_eq!(repeat_max("12").into_result(), Ok(vec![1,2]));
    assert!(repeat_max("123").into_result().is_err());

    assert!(repeat_min_max("").into_result().is_err());
    assert!(repeat_min_max("1").into_result().is_err());
    assert_eq!(repeat_min_max("12").into_result(), Ok(vec![1,2]));
    assert_eq!(repeat_min_max("123").into_result(), Ok(vec![1,2,3]));
    assert!(repeat_min_max("1234").into_result().is_err());

    assert!(repeat_sep_3("1,2").into_result().is_err());
    assert!(repeat_sep_3("1,2,3,4").into_result().is_err());
    assert_eq!(repeat_sep_3("1,2,3").into_result(), Ok(vec![1,2,3]));

    assert_eq!(repeat_variable("1a3abc222").into_result(), Ok(vec!["a", "abc", "22"]));
}