extern crate peg;

// The sample grammar from the paper https://arxiv.org/abs/1806.11150
peg::parser!(grammar test_grammar() for str {
    rule _ = quiet!{ [' ' | '\r' | '\n' | '\t']* }
    rule name() = ['a'..='z' | 'A'..='Z' | '_'] ['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*
    rule number() = ['0'..='9']+
    pub rule prog()
        = "public" _ "class" _ name() _ "{" _ "public" _ "static" _ "void" _ "main" _ "(" _ "String" _ "[" _ "]" _ name() _ ")" _ block_stmt() _ "}" _
    rule block_stmt() = "{" _ (stmt() _)* _ error_unless!{ "}" | "missing end of block" skip_to_rcur() }
    rule skip_to_rcur() = ("{" skip_to_rcur() / [^ '{' | '}'])* "}"
    rule stmt() = if_stmt() / while_stmt() / print_stmt() / dec_stmt() / assign_stmt() / block_stmt()
    rule if_stmt() = "if" _ "(" _ exp() _ ")" _ stmt() _ ("else" _ stmt() _)?
    rule while_stmt() = "while" _ "(" _ exp() _ ")" _ stmt()
    rule dec_stmt() = "int" _ name() _ ( "=" _ exp() _)? ";"
    rule assign_stmt() = name() _ "=" _ exp() _ error_unless!{ ";" | "missing semicolon in assignment" }
    rule print_stmt() = "System.out.println" _ "(" _ exp() _ ")" _ ";" / "provoke_error" error!{"provoked error" [^';']* ";"}
    rule exp() = rel_exp() _ ("==" _ rel_exp() _)*
    rule rel_exp() = add_exp() _ ("<" _ add_exp() _)*
    rule add_exp() = mul_exp() _ (['+' | '-'] _ mul_exp() _)*
    rule mul_exp() = atom_exp() _ (['*' | '/'] _ atom_exp() _)*
    rule atom_exp() = "(" _ exp() _ ")" / number() / name()
});

use std::fmt::Debug;
use self::test_grammar::*;
use peg::{ParseResults, ParseResult};
use peg::error::{ParseError, ParseErr};

fn expect_failed<T: Debug, L: Debug>(results: ParseResults<T, L>) -> (ParseError<L>, Vec<ParseErr<L>>) {
    match results {
        ParseResults { result: ParseResult::Failed(fail), errors } => (fail, errors),
        r => panic!("Unexpected {:?}", r),
    }
}

fn expect_error<T: Debug, L: Debug>(results: ParseResults<T, L>) -> (ParseErr<L>, Vec<ParseErr<L>>) {
    match results {
        ParseResults { result: ParseResult::Error(error), errors } => (error, errors),
        r => panic!("Unexpected {:?}", r),
    }
}

fn main() {
    let input = concat!(
        "public class Example {\n",
        "  public static void main(String[] args) {\n",
        "    int n = 5;\n",
        "    int f = 1;\n",
        "    while (0 < n) {\n",
        "      f = f * n;\n",
        "      n = n - 1\n",
        "    };\n",
        "    System.out.println(f);\n",
        "  }\n",
        "}\n"
    );

    // Parses successfully, but recovers from two errors.
    let r = prog(input);
    assert_eq!(r.clone().unwrap(), ());
    assert_eq!(r.errors.len(), 2);
    assert_eq!(r.errors[0].location.line, 8);
    assert_eq!(r.errors[0].location.column, 5);
    assert_eq!(r.errors[0].error, "missing semicolon in assignment");
    assert_eq!(r.errors[1].location.line, 8);
    assert_eq!(r.errors[1].location.column, 6);
    assert_eq!(r.errors[1].error, "missing end of block");

    // Alter input slightly to fail on first line.
    // No rule to recover from this, so the parse fails.
    let input2 = input.replace("public", "private");
    let (fail, errors) = expect_failed(prog(&input2));
    assert_eq!(fail.location.line, 1);
    assert_eq!(fail.location.column, 1);
    assert_eq!(format!("{}", fail.expected), r#""public""#);
    assert_eq!(errors, vec![]);

    // Alter input slightly to strip last two closing braces.
    // Recovers from missing semicolon, but then when the block is not closed
    // the recovery rule cannot find a '}' to close the block,
    // so it returns an error.
    let input3 = input.replace("}\n}\n", "");
    let (error, errors) = expect_error(prog(&input3));
    assert_eq!(error.location.line, 8);
    assert_eq!(error.location.column, 6);
    assert_eq!(error.error, "missing end of block");
    assert_eq!(errors.len(), 1);
    assert_eq!(errors[0].error, "missing semicolon in assignment");

    // Provoke an error using the error! expression.
    // Parses successfully, but recovers from three errors.
    let input4 = input.replace("int f = 1", "provoke_error with a bunch of stuff that goes here");
    let r = prog(&input4);
    assert_eq!(r.clone().unwrap(), ());
    assert_eq!(r.errors.len(), 3);
    assert_eq!(r.errors[0].location.line, 4);
    assert_eq!(r.errors[0].location.column, 18);
    assert_eq!(r.errors[0].error, "provoked error");
    assert_eq!(r.errors[1].location.line, 8);
    assert_eq!(r.errors[1].location.column, 5);
    assert_eq!(r.errors[1].error, "missing semicolon in assignment");
    assert_eq!(r.errors[2].location.line, 8);
    assert_eq!(r.errors[2].location.column, 6);
    assert_eq!(r.errors[2].error, "missing end of block");
}
