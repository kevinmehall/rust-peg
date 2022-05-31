//! Example: How to use errors and parser recovery for a simple expression language.

extern crate peg;
use arithmetic::expression;
use std::fmt::{Debug, Display};

peg::parser!( grammar arithmetic() for str {
    pub rule expression() -> i64
        =
          // The whole expression should just be a sum. If there's at least one
          // character left over, report it and consume it.
          _ v:sum() error_if!{ [_]+ | "unexpected content at end" }? { v }
          // If we reach this alternative, we've not found anything at all
          // (except possibly whitespace), so report that.
        / error!{"empty expression" _} { 0 }

    rule sum() -> i64
        = l:product() "+" _ r:sum() { l+r }
        / product()

    rule product() -> i64
        =
          // If we see some common symbols, give a special error message explaining
          // why they don't work. Then just treat them as if they were addition.
          l:atom() ("*" / error_if!{ ['/' | '-' | '^'] | "unsupported operation" }) _ r:product() { l*r }
        / atom()

    rule atom() -> i64
        = number()
          // A parenthesised expression must end with `)`. If not, assume
          // something has gone wrong with the expression and skip everything
          // until we find that `)` (or EOF). This is likely to avoid a lot
          // of spurious error messages - usually the `)` hasn't been forgotten,
          // it's some error earlier.
        / "(" _ v:sum() error_unless!{ ")" _ | "missing close paren" skip_to_rparen() } { v }
          // Some other character that wasn't expected. Report it.
        / error_if!{[_] | "expected atom" _} { 0 }

    rule number() -> i64
        = n:$(['0'..='9']+) _ { n.parse().unwrap() }

    rule _ = quiet!{ [' ' | '\r' | '\n' | '\t']* }

    /// Helper rule for recovery: skip to the next `)`, if any, skipping properly
    /// over any balanced `(`...`)`.
    rule skip_to_rparen()
        = ("(" skip_to_rparen() / [^ '(' | ')'])* ")"? _

});

/// Check the errors and (recovered) value of the given parse result.
fn assert_recovered<T: Eq + PartialEq + Display + Debug, L: Eq + PartialEq + Display + Debug>(
    result: peg::ParseResults<T, L>,
    errors: Vec<&str>,
    value: T,
) {
    let pretty_result = result.to_string();
    let actual_errors: Vec<String> = result.errors.iter().map(|e| e.to_string()).collect();
    let expected_errors: Vec<String> = errors.iter().map(|e| e.to_string()).collect();
    let recovered_result = result.recover_result();
    assert_eq!(
        recovered_result,
        Ok(value),
        "Should have recovered and got a result\nparsed: {}",
        pretty_result
    );
    assert_eq!(
        actual_errors, expected_errors,
        "Unexpected list of recovered errors\nparsed: {}",
        pretty_result
    );
}

fn main() {
    assert_eq!(expression("1 + 1").into_result(), Ok(2));
    assert_eq!(expression("1 + 2 + 3").into_result(), Ok(6));
    assert_eq!(expression("5 * 5").into_result(), Ok(25));
    assert_eq!(expression("222 + 3333").into_result(), Ok(3555));
    assert_eq!(expression("2 + 3 * 4").into_result(), Ok(14));
    assert_eq!(expression("2 * 3 + 4").into_result(), Ok(10));
    assert_eq!(expression("(2 + 2) * 3").into_result(), Ok(12));
    assert!(expression("(22 + ) + 1").into_result().is_err());
    assert!(expression("1 + + 1").into_result().is_err());
    assert!(expression("3) + 1").into_result().is_err());

    // Demonstrate that we spot the error, give a sensible message,
    // and continue to give a result anyway.
    //   offset: 1:          1         2         3         4
    //              1234567890123456789012345678901234567890
    assert_recovered(
        expression("1 + 1 1"),
        vec!["error at 1:7: unexpected content at end"],
        2,
    );
    assert_recovered(
        expression("(1 + 1 1)"),
        vec!["error at 1:8: missing close paren"],
        2,
    );
    assert_recovered(
        expression("12 * (5 + 5 a) + 1"),
        vec!["error at 1:13: missing close paren"],
        121,
    );
    assert_recovered(
        expression("12 * (5 + 5 + a) + 1"),
        vec!["error at 1:15: expected atom"],
        121,
    );
    assert_recovered(
        expression("12 * (5 + 5 + atom) + 1"),
        vec![
            "error at 1:15: expected atom",
            "error at 1:16: missing close paren",
        ],
        121,
    );
    // skips over nested parentheses
    assert_recovered(
        expression("12 * (5 + 5 a (thingy + wotsit) b) + 1"),
        vec!["error at 1:13: missing close paren"],
        121,
    );
    assert_recovered(
        expression("12 * (5 + 5 (thingy + wotsit) b) + 1"),
        vec!["error at 1:13: missing close paren"],
        121,
    );
    // copes with unterminated parentheses
    assert_recovered(
        expression("12 * (5 + 5 (thingy + wotsit) + 1"),
        vec!["error at 1:13: missing close paren"],
        120,
    );
    assert_recovered(
        expression("42 / 6"),
        vec!["error at 1:4: unsupported operation"],
        252,
    );
    assert_recovered(
        expression(".1 + 10"),
        vec![
            "error at 1:1: expected atom",
            "error at 1:2: unexpected content at end",
        ],
        0,
    );
    assert_recovered(
        expression("+1"),
        vec![
            "error at 1:1: expected atom",
            "error at 1:2: unexpected content at end",
        ],
        0,
    );
    assert_recovered(expression(""), vec!["error at 1:1: empty expression"], 0);
    assert_recovered(expression(" "), vec!["error at 1:1: empty expression"], 0);
}
