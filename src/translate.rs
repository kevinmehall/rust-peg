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

impl Grammar {
	fn find_rule(&self, name: &str) -> Option<&Rule> {
		self.rules.iter().find(|rule| rule.name == name)
	}
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
	pub cached: bool,
}

#[derive(Clone)]
pub struct CharSetCase {
	pub start: char,
	pub end: char
}

#[derive(Clone)]
pub struct TaggedExpr {
	pub name: Option<String>,
	pub expr: Box<Expr>,
}

#[derive(Clone)]
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
	ActionExpr(Vec<TaggedExpr>, /*action*/ String, /*cond*/ bool),
	MatchStrExpr(Box<Expr>),
	PositionExpr,
}

pub fn compile_grammar(ctxt: &rustast::ExtCtxt, grammar: &Grammar) -> rustast::P<rustast::Mod> {
    let mut imports = grammar.imports.clone();
    imports.push(RustUseList("self::RuleResult".to_string(),
                             vec!("Matched".to_string(), "Failed".to_string())));
    let mut items = translate_view_items(ctxt, &imports);
	items.append(&mut header_items(ctxt));
	items.append(&mut make_parse_state(ctxt, &grammar.rules));
	items.extend(grammar.rules.iter().map(|rule| {
		compile_rule(ctxt, grammar, rule)
	}));
	items.extend(grammar.rules.iter().filter(|rule| rule.exported).map(|rule| {
		compile_rule_export(ctxt, rule)
	}));

    rustast::module(items)
}

pub fn translate_view_items(ctxt: &rustast::ExtCtxt, imports: &[RustUse]) -> Vec<rustast::P<rustast::Item>> {
	imports.iter().map(| i |{
		match *i {
			RustUseSimple(ref p) => ctxt.item_use_simple(DUMMY_SP, rustast::ast::Visibility::Inherited, rustast::parse_path(ctxt, &p)),
			RustUseGlob(ref p) => ctxt.item_use_glob(DUMMY_SP, rustast::ast::Visibility::Inherited, rustast::parse_path_vec(&p)),
			RustUseList(ref p, ref v) => ctxt.item_use_list(DUMMY_SP, rustast::ast::Visibility::Inherited, rustast::parse_path_vec(&p),
				&v.iter().map(|s| rustast::str_to_ident(&s)).collect::<Vec<_>>()
			),
		}
	}).collect::<Vec<_>>()
}

fn make_parse_state(ctxt: &rustast::ExtCtxt, rules: &[Rule]) -> Vec<rustast::P<rustast::Item>> {
	let mut items = Vec::new();

	let mut cache_fields: Vec<rustast::TokenTree> = Vec::new();
	let mut cache_init: Vec<rustast::TokenTree> = Vec::new();
	for rule in rules {
		if rule.cached {
			let name = rustast::str_to_ident(&format!("{}_cache", rule.name));
			let map_type = rustast::parse_type(ctxt,
				&format!("::std::collections::HashMap<usize, RuleResult<{}>>", rule.ret_type));

			cache_fields.append(&mut quote_tokens!(ctxt, $name: $map_type,));
			cache_init.append(&mut quote_tokens!(ctxt, $name: ::std::collections::HashMap::new(),));
		}
	}

	items.push(quote_item!(ctxt,
		struct ParseState<'input> {
			max_err_pos: usize,
			expected: ::std::collections::HashSet<&'static str>,
			_phantom: ::std::marker::PhantomData<&'input ()>,
			$cache_fields
		}
	).unwrap());

	items.push(quote_item!(ctxt,
		impl<'input> ParseState<'input> {
			fn new() -> ParseState<'input> {
				ParseState {
					max_err_pos: 0,
					expected: ::std::collections::HashSet::new(),
					_phantom: ::std::marker::PhantomData,
					$cache_init
				}
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

	items
}

pub fn header_items(ctxt: &rustast::ExtCtxt) -> Vec<rustast::P<rustast::Item>> {
	let mut items = Vec::new();

	items.push(quote_item!(ctxt,
		fn escape_default(s: &str) -> String {
			s.chars().flat_map(|c| c.escape_default()).collect()
		}
	).unwrap());

	items.push(quote_item!(ctxt,
		fn char_range_at(s: &str, pos: usize) -> (char, usize) {
			let c = &s[pos..].chars().next().unwrap();
			let next_pos = pos + c.len_utf8();
			(*c, next_pos)
		}
	).unwrap());

	items.push(quote_item!(ctxt,
		#[derive(Clone)]
		enum RuleResult<T> {
			Matched(usize, T),
			Failed,
		}
	).unwrap());

	items.push(quote_item!(ctxt,
		#[derive(PartialEq, Eq, Debug, Clone)]
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
				if self.expected.len() == 0 {
					try!(write!(fmt, "EOF"));
				} else if self.expected.len() == 1 {
					try!(write!(fmt, "`{}`", escape_default(self.expected.iter().next().unwrap())));
				} else {
					let mut iter = self.expected.iter();

					try!(write!(fmt, "one of `{}`", escape_default(iter.next().unwrap())));
					for elem in iter {
						try!(write!(fmt, ", `{}`", escape_default(elem)));
					}
				}

				Ok(())
			}
		}
	).unwrap());

	items.push(quote_item!(ctxt,
		impl ::std::error::Error for ParseError {
			fn description(&self) -> &str {
				"parse error"
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
			let mut input_iter = input[pos..].chars().flat_map(|x| x.to_uppercase());

			for m_char_upper in m.chars().flat_map(|x| x.to_uppercase()) {
				used += m_char_upper.len_utf8();

				let input_char_result = input_iter.next();

				if input_char_result.is_none() || input_char_result.unwrap() != m_char_upper {
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
				let (_, next) = char_range_at(input, pos);
				Matched(next, ())
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


fn compile_rule(ctxt: &rustast::ExtCtxt, grammar: &Grammar, rule: &Rule) -> rustast::P<rustast::Item> {
	let ref rule_name = rule.name;
	let name = rustast::str_to_ident(&format!("parse_{}", rule.name));
	let ret = rustast::parse_type(ctxt, &rule.ret_type);
	let body = compile_expr(ctxt, grammar, &*rule.expr, (&rule.ret_type as &str) != "()");
	let wrapped_body = if cfg!(feature = "trace") {
		quote_expr!(ctxt, {
			let (line, col) = pos_to_line(input, pos);
			println!("[PEG_TRACE] Attempting to match rule {} at {}:{} (pos {})", $rule_name, line, col, pos);
			let mut __peg_closure = || {
				$body
			};
			let __peg_result = __peg_closure();
			match __peg_result {
				Matched(_, _) => println!("[PEG_TRACE] Matched rule {} at {}:{} (pos {})", $rule_name, line, col, pos),
				Failed => println!("[PEG_TRACE] Failed to match rule {} at {}:{} (pos {})", $rule_name, line, col, pos)
			}
			__peg_result
		})
	} else { body };

	if rule.cached {
		let cache_field = rustast::str_to_ident(&format!("{}_cache", rule.name));

		quote_item!(ctxt,
			fn $name<'input>(input: &'input str, state: &mut ParseState<'input>, pos: usize) -> RuleResult<$ret> {
				let rule_result = $wrapped_body;
				state.$cache_field.insert(pos, rule_result.clone());

				rule_result
			}
		).unwrap()
	} else {
		quote_item!(ctxt,
			fn $name<'input>(input: &'input str, state: &mut ParseState<'input>, pos: usize) -> RuleResult<$ret> {
				$wrapped_body
			}
		).unwrap()
	}
}

fn compile_rule_export(ctxt: &rustast::ExtCtxt, rule: &Rule) -> rustast::P<rustast::Item> {
	let name = rustast::str_to_ident(&rule.name);
	let ret = rustast::parse_type(ctxt, &rule.ret_type);
	let parse_fn = rustast::str_to_ident(&format!("parse_{}", rule.name));
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

fn compile_match_and_then(ctxt: &rustast::ExtCtxt, grammar: &Grammar, e: &Expr, value_name: Option<&str>, then: rustast::P<rustast::Expr>) -> rustast::P<rustast::Expr> {
	let seq_res = compile_expr(ctxt, grammar, e, value_name.is_some());
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

fn format_char_set(invert: bool, cases: &[CharSetCase]) -> String {
	let mut r = "[".to_owned();

    if invert {
        r.push('^');
    }

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
fn compile_expr(ctxt: &rustast::ExtCtxt, grammar: &Grammar, e: &Expr, result_used: bool) -> rustast::P<rustast::Expr> {
	match *e {
		AnyCharExpr => {
			quote_expr!(ctxt, any_char(input, state, pos))
		}

		LiteralExpr(ref s, ref case_insensitive) => {
			let sl: &str = &s;
			if *case_insensitive {
				quote_expr!(ctxt, slice_eq_case_insensitive(input, state, pos, $sl))
			}
			else {
				quote_expr!(ctxt, slice_eq(input, state, pos, $sl))
			}
		}

		CharSetExpr(invert, ref cases) => {
			let expected_set = format_char_set(invert, &cases);
			let expected_str: &str = &expected_set;

			let (in_set, not_in_set) = cond_swap(invert, (
				quote_expr!(ctxt, Matched(next, ())),
				quote_expr!(ctxt, state.mark_failure(pos, $expected_str)),
			));

			let m = ctxt.expr_match(DUMMY_SP, quote_expr!(ctxt, ch), vec!(
				ctxt.arm(DUMMY_SP, cases.iter().map(|case| {
					if case.start == case.end {
						ctxt.pat_lit(DUMMY_SP, ctxt.expr_lit(DUMMY_SP, rustast::ast::LitKind::Char(case.start)))
					} else {
						ctxt.pat(DUMMY_SP, rustast::ast::PatKind::Range(
							ctxt.expr_lit(DUMMY_SP, rustast::ast::LitKind::Char(case.start)),
							ctxt.expr_lit(DUMMY_SP, rustast::ast::LitKind::Char(case.end))
						))
					}
				}).collect::<Vec<_>>(), in_set),
				ctxt.arm(DUMMY_SP, vec!(ctxt.pat_wild(DUMMY_SP)), not_in_set)
			));

			quote_expr!(ctxt, if input.len() > pos {
				let (ch, next) = char_range_at(input, pos);
				$m
			} else {
				state.mark_failure(pos, $expected_str)
			})
		}

		RuleExpr(ref rule_name) => {
			let func = rustast::str_to_ident(&format!("parse_{}", *rule_name));
			let rule = grammar.find_rule(rule_name);
			match rule {
				Some(rule) if rule.cached => {
					let cache_field = rustast::str_to_ident(&format!("{}_cache", *rule_name));

					if cfg!(feature = "trace") {
						quote_expr!(ctxt, {
							state.$cache_field.get(&pos).map(|entry| {
								let (line, col) = pos_to_line(input, pos);
								match entry {
									&Matched(..) => println!("[PEG_TRACE] Cached match of rule {} at {}:{} (pos {})", $rule_name, line, col, pos),
									&Failed => println!("[PEG_TRACE] Cached fail of rule {} at {}:{} (pos {})", $rule_name, line, col, pos),
								};

								entry.clone()
							}).unwrap_or_else(|| $func(input, state, pos))
						})
					} else {
						quote_expr!(ctxt, {
							state.$cache_field.get(&pos).map(|entry| entry.clone()).unwrap_or_else(|| $func(input, state, pos))
						})
					}
				},
				_ => {
					quote_expr!(ctxt, $func(input, state, pos))
				}
			}
		}

		SequenceExpr(ref exprs) => {
			fn write_seq(ctxt: &rustast::ExtCtxt, grammar: &Grammar, exprs: &[Expr]) -> rustast::P<rustast::Expr> {
				if exprs.len() == 1 {
					compile_expr(ctxt, grammar, &exprs[0], false)
				} else {
					compile_match_and_then(ctxt, grammar, &exprs[0], None, write_seq(ctxt, grammar, &exprs[1..]))
				}
			}

			if exprs.len() > 0 {
				write_seq(ctxt, grammar, &exprs)
			} else {
				quote_expr!(ctxt, Matched(pos, ()))
			}
		}

		ChoiceExpr(ref exprs) => {
			fn write_choice(ctxt: &rustast::ExtCtxt, grammar: &Grammar, exprs: &[Expr], result_used: bool) -> rustast::P<rustast::Expr> {
				if exprs.len() == 1 {
					compile_expr(ctxt, grammar, &exprs[0], result_used)
				} else {
					let choice_res = compile_expr(ctxt, grammar, &exprs[0], result_used);
					let next = write_choice(ctxt, grammar, &exprs[1..], result_used);

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
				write_choice(ctxt, grammar, &exprs, result_used)
			} else {
				quote_expr!(ctxt, Matched(pos, ()))
			}
		}

		OptionalExpr(box ref e) => {
			let optional_res = compile_expr(ctxt, grammar, e, result_used);
			quote_expr!(ctxt, match $optional_res {
				Matched(newpos, value) => { Matched(newpos, Some(value)) },
				Failed => { Matched(pos, None) },
			})
		}

		Repeat(box ref e, min, max, ref sep) => {
			let inner = compile_expr(ctxt, grammar, e, result_used);

			let match_sep = match *sep {
				Some(box ref sep) => {
					let sep_inner = compile_expr(ctxt, grammar, sep, false);
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
			let assert_res = compile_expr(ctxt, grammar, e, false);
			quote_expr!(ctxt, {
				let assert_res = $assert_res;
				match assert_res {
					Matched(..) => Matched(pos, ()),
					Failed => Failed,
				}
			})
		}

		NegAssertExpr(box ref e) => {
			let assert_res = compile_expr(ctxt, grammar, e, false);
			quote_expr!(ctxt, {
				let assert_res = $assert_res;
				match assert_res {
					Failed => Matched(pos, ()),
					Matched(..) => Failed,
				}
			})
		}

		ActionExpr(ref exprs, ref code, is_cond) => {
			fn write_seq(ctxt: &rustast::ExtCtxt, grammar: &Grammar, exprs: &[TaggedExpr], code: &str, is_cond: bool) -> rustast::P<rustast::Expr> {
				match exprs.first() {
					Some(ref first) => {
						let name = first.name.as_ref().map(|s| &s[..]);
						compile_match_and_then(ctxt, grammar, &*first.expr, name,
							write_seq(ctxt, grammar, &exprs[1..], code, is_cond)
						)
					}
					None => {
						let code_block = rustast::parse_block(ctxt, code);

						if is_cond {
							quote_expr!(ctxt, {
								let match_str = &input[start_pos..pos];

								match $code_block {
									Ok(res) => Matched(pos, res),
									Err(expected) => {
										state.mark_failure(pos, expected);
										Failed
									},
								}
							})
						} else {
							quote_expr!(ctxt, {
								let match_str = &input[start_pos..pos];
								Matched(pos, $code_block)
							})
						}
					}
				}
			}

			let body = write_seq(ctxt, grammar, &exprs, &code, is_cond);
			quote_expr!(ctxt, {
				let start_pos = pos;
				$body
			})
		}
		MatchStrExpr(ref expr) => {
			let inner = compile_expr(ctxt, grammar, expr, false);
			quote_expr!(ctxt, {
				let str_start = pos;
				match $inner {
					Matched(newpos, _) => { Matched(newpos, &input[str_start..newpos]) },
					Failed => Failed,
				}
			})
		}
		PositionExpr => {
			quote_expr!(ctxt, Matched(pos, pos))
		}
	}
}

#[test]
fn test_format_char_set() {
    assert_eq!(format_char_set(false,
                               &[CharSetCase{start: 'a', end: 'z'},
                                 CharSetCase{start: '0', end: '9'}]),
               "[a-z0-9]");
    assert_eq!(format_char_set(true, &[CharSetCase{start: 'A', end: 'Z'}]),
               "[^A-Z]");
}
