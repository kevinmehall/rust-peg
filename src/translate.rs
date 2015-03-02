use std::borrow::ToOwned;
use rustast;
use rustast::DUMMY_SP;
use rustast::AstBuilder;
pub use self::RustUse::*;
pub use self::Expr::*;

pub struct Grammar {
	pub imports: Vec<RustUse>,
	pub rules: Vec<Rule>,
}

#[derive(Clone)]
pub enum RustUse {
	RustUseSimple(String),
	RustUseGlob(String),
	RustUseList(String, Vec<String>),
}

pub struct Rule {
	pub name: String,
	pub expr: Box<Expr>,
	pub ret_type: String,
	pub exported: bool,
}

pub struct CharSetCase {
	pub start: char,
	pub end: char
}

pub struct TaggedExpr {
	pub name: Option<String>,
	pub expr: Box<Expr>,
}

pub enum Expr {
	AnyCharExpr,
	LiteralExpr(String,bool),
	CharSetExpr(bool, Vec<CharSetCase>),
	RuleExpr(String),
	SequenceExpr(Vec<Expr>),
	ChoiceExpr(Vec<Expr>),
	OptionalExpr(Box<Expr>),
	Repeat(Box<Expr>, /*min*/ usize, /*max*/ Option<usize>, /*sep*/ Option<Box<Expr>>),
	PosAssertExpr(Box<Expr>),
	NegAssertExpr(Box<Expr>),
	ActionExpr(Vec<TaggedExpr>, String),
}

pub fn compile_grammar(ctxt: &rustast::ExtCtxt, grammar: &Grammar) -> rustast::P<rustast::Mod> {
    let mut imports = grammar.imports.clone();
    imports.push(RustUseList("self::RuleResult".to_string(),
                             vec!("Matched".to_string(), "Failed".to_string())));
    let mut items = translate_view_items(ctxt, imports.as_slice());

    items.push_all(
        header_items(ctxt).into_iter()
	    .chain(grammar.rules.iter().map(|rule|{
	        compile_rule(ctxt, rule)
	    }))
	    .chain(grammar.rules.iter().filter(|rule| rule.exported).map(|rule| {
	        compile_rule_export(ctxt, rule)
	    }))
	    .collect::<Vec<_>>().as_slice());

    rustast::module(items)
}

pub fn translate_view_items(ctxt: &rustast::ExtCtxt, imports: &[RustUse]) -> Vec<rustast::P<rustast::Item>> {
	imports.iter().map(| i |{
		match *i {
			RustUseSimple(ref p) => ctxt.item_use_simple(DUMMY_SP, rustast::ast::Inherited, rustast::parse_path(p.as_slice())),
			RustUseGlob(ref p) => ctxt.item_use_glob(DUMMY_SP, rustast::ast::Inherited, rustast::parse_path_vec(p.as_slice())),
			RustUseList(ref p, ref v) => ctxt.item_use_list(DUMMY_SP, rustast::ast::Inherited, rustast::parse_path_vec(p.as_slice()),
				v.iter().map(|s| rustast::str_to_ident(s.as_slice())).collect::<Vec<_>>().as_slice()
			),
		}
	}).collect::<Vec<_>>()
}

pub fn header_items(ctxt: &rustast::ExtCtxt) -> Vec<rustast::P<rustast::Item>> {
	let mut items = Vec::new();

	items.push(quote_item!(ctxt,
		enum RuleResult<T> {
			Matched(usize, T),
			Failed,
		}
	).unwrap());

	items.push(quote_item!(ctxt,
		struct ParseState {
			max_err_pos: usize,
			expected: ::std::collections::HashSet<&'static str>,
		}
	).unwrap());

	items.push(quote_item!(ctxt,
		#[derive(PartialEq, Eq, Debug)]
		pub struct ParseError {
			pub line: usize,
			pub column: usize,
			pub offset: usize,
			pub expected: ::std::collections::HashSet<&'static str>,
		}
	).unwrap());
	
	items.push(quote_item!(ctxt,
		pub type ParseResult<T> = Result<T, ParseError>;
	).unwrap());

	items.push(quote_item!(ctxt,
		impl ::std::fmt::Display for ParseError {
			fn fmt(&self, fmt: &mut ::std::fmt::Formatter) -> ::std::result::Result<(), ::std::fmt::Error> {
				try!(write!(fmt, "error at {}:{}: expected ", self.line, self.column));
				if self.expected.len() == 1 {
					try!(write!(fmt, "`{}`", self.expected.iter().next().unwrap().escape_default()));
				} else {
					let mut iter = self.expected.iter();

					try!(write!(fmt, "one of `{}`", iter.next().unwrap().escape_default()));
					for elem in iter {
						try!(write!(fmt, ", `{}`", elem.escape_default()));
					}
				}

				Ok(())
			}
		}
	).unwrap());

	items.push(quote_item!(ctxt,
		impl ParseState {
			fn new() -> ParseState {
				ParseState{ max_err_pos: 0, expected: ::std::collections::HashSet::new() }
			}
			fn mark_failure(&mut self, pos: usize, expected: &'static str) -> RuleResult<()> {
				if pos > self.max_err_pos {
					self.max_err_pos = pos;
					self.expected.clear();
				}

				if pos == self.max_err_pos {
					self.expected.insert(expected);
				}

				Failed
			}
		}
	).unwrap());

	items.push(quote_item!(ctxt,
		fn slice_eq(input: &str, state: &mut ParseState, pos: usize, m: &'static str) -> RuleResult<()> {
			#![inline]
			#![allow(dead_code)]

	    let l = m.len();
	    if input.len() >= pos + l && &input.as_bytes()[pos..pos+l] == m.as_bytes() {
	      Matched(pos+l, ())
	    } else {
	      state.mark_failure(pos, m)
	    }
		}
	).unwrap());

	items.push(quote_item!(ctxt,
		fn slice_eq_case_insensitive(input: &str, state: &mut ParseState, pos: usize, m: &'static str) -> RuleResult<()> {
			#![inline]
			#![allow(dead_code)]

			let mut used = 0usize;
			let mut input_iter = input[pos..].chars();

			for m_char in m.chars() {
				let m_char_upper = m_char.to_uppercase();
				used += m_char_upper.len_utf8();

				let input_char_result = input_iter.next();

				if input_char_result.is_none()
					|| input_char_result.unwrap().to_uppercase() != m_char_upper {
					return state.mark_failure(pos, m);
				}
			}

			Matched(pos+used, ())
		}

	).unwrap());

	items.push(quote_item!(ctxt,
		fn any_char(input: &str, state: &mut ParseState, pos: usize) -> RuleResult<()> {
			#![inline]
			#![allow(dead_code)]

			if input.len() > pos {
				Matched(input.char_range_at(pos).next, ())
			} else {
				state.mark_failure(pos, "<character>")
			}
		}
	).unwrap());


	items.push(quote_item!(ctxt,
		fn pos_to_line(input: &str, pos: usize) -> (usize, usize) {
			let mut remaining = pos;
			let mut lineno: usize = 1;
			for line in input.lines() {
				let line_length = line.len() + 1;
				if remaining < line_length {
					return (lineno, remaining + 1);
				}
				remaining -= line_length;
				lineno += 1;
			}
			return (lineno, remaining + 1);
		}
	).unwrap());

	items
}


fn compile_rule(ctxt: &rustast::ExtCtxt, rule: &Rule) -> rustast::P<rustast::Item> {
	let name = rustast::str_to_ident(format!("parse_{}", rule.name).as_slice());
	let ret = rustast::parse_type(rule.ret_type.as_slice());
	let body = compile_expr(ctxt, &*rule.expr, rule.ret_type.as_slice() != "()");
	(quote_item!(ctxt,
		fn $name<'input>(input: &'input str, state: &mut ParseState, pos: usize) -> RuleResult<$ret> {
			$body
		}
	)).unwrap()
}

fn compile_rule_export(ctxt: &rustast::ExtCtxt, rule: &Rule) -> rustast::P<rustast::Item> {
	let name = rustast::str_to_ident(rule.name.as_slice());
	let ret = rustast::parse_type(rule.ret_type.as_slice());
	let parse_fn = rustast::str_to_ident(format!("parse_{}", rule.name).as_slice());
	(quote_item!(ctxt,
		pub fn $name<'input>(input: &'input str) -> ParseResult<$ret> {
			let mut state = ParseState::new();
			match $parse_fn(input, &mut state, 0) {
				Matched(pos, value) => {
					if pos == input.len() {
						return Ok(value)
					}
				}
				_ => {}
			}
			let (line, col) = pos_to_line(input, state.max_err_pos);

			Err(ParseError {
				line: line,
				column: col,
				offset: state.max_err_pos,
				expected: state.expected,
			})
		}
	)).unwrap()
}

fn compile_match_and_then(ctxt: &rustast::ExtCtxt, e: &Expr, value_name: Option<&str>, then: rustast::P<rustast::Expr>) -> rustast::P<rustast::Expr> {
	let seq_res = compile_expr(ctxt, e, value_name.is_some());
	let name_pat = match value_name {
		Some(name) => rustast::str_to_ident(name),
		None => rustast::str_to_ident("_")
	};

	quote_expr!(ctxt, {
		let seq_res = $seq_res;
		match seq_res {
			Matched(pos, $name_pat) => { $then }
			Failed => Failed,
		}
	})
}

fn cond_swap<T>(swap: bool, tup: (T, T)) -> (T, T) {
	let (a, b) = tup;
	if swap {
		(b, a)
	} else {
		(a, b)
	}
}

fn format_char_set(cases: &[CharSetCase]) -> String {
	let mut r = "[".to_owned();

	for &CharSetCase{start, end} in cases.iter() {
		r.push(start);
		if start != end {
			r.push('-');
			r.push(end);
		}
	}
	r.push(']');
	r
}

#[allow(unused_imports)] // quote_tokens! imports things
fn compile_expr(ctxt: &rustast::ExtCtxt, e: &Expr, result_used: bool) -> rustast::P<rustast::Expr> {
	match *e {
		AnyCharExpr => {
			quote_expr!(ctxt, any_char(input, state, pos))
		}

		LiteralExpr(ref s, ref case_insensitive) => {
			let sl = s.as_slice();
			if *case_insensitive {
				quote_expr!(ctxt, slice_eq_case_insensitive(input, state, pos, $sl))
			}
			else {
				quote_expr!(ctxt, slice_eq(input, state, pos, $sl))
			}
		}

		CharSetExpr(invert, ref cases) => {
			let expected_set = format_char_set(cases.as_slice());
			let expected_str = expected_set.as_slice();

			let (in_set, not_in_set) = cond_swap(invert, (
				quote_expr!(ctxt, Matched(next, ())),
				quote_expr!(ctxt, state.mark_failure(pos, $expected_str)),
			));

			let m = ctxt.expr_match(DUMMY_SP, quote_expr!(ctxt, ch), vec!(
				ctxt.arm(DUMMY_SP, cases.iter().map(|case| {
					if case.start == case.end {
						ctxt.pat_lit(DUMMY_SP, ctxt.expr_lit(DUMMY_SP, rustast::ast::LitChar(case.start)))
					} else {
						ctxt.pat(DUMMY_SP, rustast::ast::PatRange(
							ctxt.expr_lit(DUMMY_SP, rustast::ast::LitChar(case.start)),
							ctxt.expr_lit(DUMMY_SP, rustast::ast::LitChar(case.end))
						))
					}
				}).collect::<Vec<_>>(), in_set),
				ctxt.arm(DUMMY_SP, vec!(ctxt.pat_wild(DUMMY_SP)), not_in_set)
			));

			quote_expr!(ctxt, if input.len() > pos {
				let ::std::str::CharRange {ch, next} = input.char_range_at(pos);
				$m
			} else {
				state.mark_failure(pos, $expected_str)
			})
		}

		RuleExpr(ref rule_name) => {
			let func = rustast::str_to_ident(format!("parse_{}", *rule_name).as_slice());
			quote_expr!(ctxt, $func(input, state, pos))
		}

		SequenceExpr(ref exprs) => {
			fn write_seq(ctxt: &rustast::ExtCtxt, exprs: &[Expr]) -> rustast::P<rustast::Expr> {
				if exprs.len() == 1 {
					compile_expr(ctxt, &exprs[0], false)
				} else {
					compile_match_and_then(ctxt, &exprs[0], None, write_seq(ctxt, exprs.tail()))
				}
			}

			if exprs.len() > 0 {
				write_seq(ctxt, exprs.as_slice())
			} else {
				quote_expr!(ctxt, Matched(pos, ()))
			}
		}

		ChoiceExpr(ref exprs) => {
			fn write_choice(ctxt: &rustast::ExtCtxt, exprs: &[Expr], result_used: bool) -> rustast::P<rustast::Expr> {
				if exprs.len() == 1 {
					compile_expr(ctxt, &exprs[0], result_used)
				} else {
					let choice_res = compile_expr(ctxt, &exprs[0], result_used);
					let next = write_choice(ctxt, exprs.tail(), result_used);

					quote_expr!(ctxt, {
						let choice_res = $choice_res;
						match choice_res {
							Matched(pos, value) => Matched(pos, value),
							Failed => $next
						}
					})
				}
			}

			if exprs.len() > 0 {
				write_choice(ctxt, exprs.as_slice(), result_used)
			} else {
				quote_expr!(ctxt, Matched(pos, ()))
			}
		}

		OptionalExpr(box ref e) => {
			let optional_res = compile_expr(ctxt, e, result_used);
			quote_expr!(ctxt, match $optional_res {
				Matched(newpos, value) => { Matched(newpos, Some(value)) },
				Failed => { Matched(pos, None) },
			})
		}

		Repeat(box ref e, min, max, ref sep) => {
			let inner = compile_expr(ctxt, e, result_used);

			let match_sep = match *sep {
				Some(box ref sep) => {
					let sep_inner = compile_expr(ctxt, sep, false);
					quote_tokens!(ctxt,
						let pos = if repeat_value.len() > 0 {
							let sep_res = $sep_inner;
							match sep_res {
								Matched(newpos, _) => { newpos },
								Failed => break,
							}
						} else { pos };
					)
				}
				None => vec!()
			};

			let result = if result_used {
				quote_expr!(ctxt, repeat_value)
			} else {
				quote_expr!(ctxt, ())
			};

			let (repeat_vec, repeat_step) =
			if result_used || min > 0 || max.is_some() || sep.is_some() {
				(quote_tokens!(ctxt, let mut repeat_value = vec!();),
				 quote_tokens!(ctxt, repeat_value.push(value);))
			} else {
				(vec!(), vec!())
			};

			let max_check = match max {
				None => vec!(),
				Some(max) => quote_tokens!(ctxt,
					if repeat_value.len() >= $max { break }
				)
			};

			let result_check = if min > 0 {
				quote_expr!(ctxt,
					if repeat_value.len() >= $min {
						Matched(repeat_pos, $result)
					} else {
						Failed
					}
				)
			} else {
				quote_expr!(ctxt, Matched(repeat_pos, $result))
			};

			quote_expr!(ctxt, {
				let mut repeat_pos = pos;
				$repeat_vec

				loop {
					let pos = repeat_pos;

					$match_sep
					$max_check

					let step_res = $inner;
					match step_res {
						Matched(newpos, value) => {
							repeat_pos = newpos;
							$repeat_step
						},
						Failed => {
							break;
						}
					}
				}

				$result_check
			})
		}

		PosAssertExpr(box ref e) => {
			let assert_res = compile_expr(ctxt, e, false);
			quote_expr!(ctxt, {
				let assert_res = $assert_res;
				match assert_res {
					Matched(..) => Matched(pos, ()),
					Failed => Failed,
				}
			})
		}

		NegAssertExpr(box ref e) => {
			let assert_res = compile_expr(ctxt, e, false);
			quote_expr!(ctxt, {
				let assert_res = $assert_res;
				match assert_res {
					Failed => Matched(pos, ()),
					Matched(..) => Failed,
				}
			})
		}

		ActionExpr(ref exprs, ref code) => {
			fn write_seq(ctxt: &rustast::ExtCtxt, exprs: &[TaggedExpr], code: &str) -> rustast::P<rustast::Expr> {
				match exprs.first() {
					Some(ref first) => {
						let name = first.name.as_ref().map(|s| s.as_slice());
						compile_match_and_then(ctxt, &*first.expr, name,
							write_seq(ctxt, exprs.tail(), code)
						)
					}
					None => {
						let code_block = rustast::parse_block(code);
						quote_expr!(ctxt, {
							let match_str = &input[start_pos..pos];
							Matched(pos, $code_block)
						})
					}
				}
			}

			let body = write_seq(ctxt, exprs.as_slice(), code.as_slice());

			quote_expr!(ctxt, {
				let start_pos = pos;
				$body
			})
		}
	}
}
