fn prepend(head: i64, mut tail: Vec<i64>) -> Vec<i64> {
    tail.insert(0, head);
    tail
}

fn append(mut init: Vec<i64>, last: i64) -> Vec<i64> {
    init.push(last);
    init
}

peg::parser! {
grammar list() for str {
    stack_limit 2;

    pub rule of_one_or_two() -> Vec<i64>
        = head:number() "," tail:of_one_or_two() { prepend(head, tail) }
        / single:number() { vec![single] }

    rule number() -> i64
        = n:$(['0'..='9']+) { n.parse().unwrap() }

    #[cache_left_rec]
    pub rule leftrec() -> Vec<i64>
        = init:leftrec() "," last:number() { append(init, last) }
        / single: number() { vec![single] }
}
}

fn main() {
    assert_eq!(list::of_one_or_two("1"), Ok(vec![1]));
    assert_eq!(list::of_one_or_two("1,2"), Ok(vec![1, 2]));
    let err = list::of_one_or_two("1,2,3");
    assert!(err.is_err());
    assert_eq!(
        err.unwrap_err().expected.tokens().collect::<Vec<_>>(),
        vec!["STACK OVERFLOW"]
    );

    assert_eq!(list::leftrec("1,2"), Ok(vec![1, 2]));
    let err = list::leftrec("1,2,3");
    assert!(err.is_err());
    assert_eq!(
        err.unwrap_err().expected.tokens().collect::<Vec<_>>(),
        vec!["STACK OVERFLOW"]
    );
}
