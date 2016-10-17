use std::borrow::ToOwned;
use quote::{Tokens, ToTokens};
pub use self::RustUse::*;
pub use self::Expr::*;

fn raw(s: &str) -> Tokens {
	let mut t = Tokens::new();
	t.append(s);
	t
}

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

impl ToTokens for RustUse {
	fn to_tokens(&self, tok: &mut Tokens) {
		match *self {
			RustUseSimple(ref s) => {
				tok.append(&s[..]);
			}
			RustUseGlob(ref s) => {
				tok.append(&s[..]);
				tok.append("::");
				tok.append("*");
			}
			RustUseList(ref s, ref n) => {
				tok.append(&s[..]);
				tok.append("::");
				tok.append("{");
				for t in n {
					tok.append(&t[..]);
					tok.append(",");
				}
				tok.append("}");
			}
		}

	}
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

static HELPERS: &'static str = stringify! {
	fn escape_default(s: &str) -> String {
		s.chars().flat_map(|c| c.escape_default()).collect()
	}

	fn char_range_at(s: &str, pos: usize) -> (char, usize) {
		let c = &s[pos..].chars().next().unwrap();
		let next_pos = pos + c.len_utf8();
		(*c, next_pos)
	}

	#[derive(Clone)]
	enum RuleResult<T> {
		Matched(usize, T),
		Failed,
	}

	#[derive(PartialEq, Eq, Debug, Clone)]
	pub struct ParseError {
		pub line: usize,
		pub column: usize,
		pub offset: usize,
		pub expected: ::std::collections::HashSet<&'static str>,
	}

	pub type ParseResult<T> = Result<T, ParseError>;
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

	impl ::std::error::Error for ParseError {
		fn description(&self) -> &str {
			"parse error"
		}
	}

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

	impl<'input> ParseState<'input> {
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
};

pub fn compile_grammar(grammar: &Grammar) -> Tokens {
	let mut items = vec![make_parse_state(&grammar.rules)];

	items.extend(grammar.rules.iter().map(|rule| {
		compile_rule(grammar, rule)
	}));
	items.extend(grammar.rules.iter().filter(|rule| rule.exported).map(|rule| {
		compile_rule_export(rule)
	}));

	let view_items = &grammar.imports;
	let helpers = raw(HELPERS);

	quote! {
		use self::RuleResult::{Matched, Failed};
		#(use #view_items;)*

		#helpers

		#(#items)*
	}
}

fn make_parse_state(rules: &[Rule]) -> Tokens {
	let mut cache_fields_def: Vec<Tokens> = Vec::new();
	let mut cache_fields: Vec<Tokens> = Vec::new();
	for rule in rules {
		if rule.cached {
			let name = raw(&format!("{}_cache", rule.name));
			let ret_ty = raw(&rule.ret_type);
			cache_fields_def.push(quote!{ #name: ::std::collections::HashMap<usize, RuleResult<#ret_ty>> });
			cache_fields.push(name);
		}
	}

	quote! {
		struct ParseState<'input> {
			max_err_pos: usize,
			expected: ::std::collections::HashSet<&'static str>,
			_phantom: ::std::marker::PhantomData<&'input ()>,
			#(#cache_fields_def),*
		}

		impl<'input> ParseState<'input> {
			fn new() -> ParseState<'input> {
				ParseState {
					max_err_pos: 0,
					expected: ::std::collections::HashSet::new(),
					_phantom: ::std::marker::PhantomData,
					#(#cache_fields: ::std::collections::HashMap::new()),*
				}
			}
		}
	}
}


fn compile_rule(grammar: &Grammar, rule: &Rule) -> Tokens {
	let ref rule_name = rule.name;
	let name = raw(&format!("parse_{}", rule.name));
	let ret_ty = raw(&rule.ret_type);
	let body = compile_expr(grammar, &*rule.expr, (&rule.ret_type as &str) != "()");

	let wrapped_body = if cfg!(feature = "trace") {
		quote!{{
			let (line, col) = pos_to_line(input, pos);
			println!("[PEG_TRACE] Attempting to match rule {} at {}:{} (pos {})", #rule_name, line, col, pos);
			let mut __peg_closure = || {
				#body
			};
			let __peg_result = __peg_closure();
			match __peg_result {
				Matched(_, _) => println!("[PEG_TRACE] Matched rule {} at {}:{} (pos {})", #rule_name, line, col, pos),
				Failed => println!("[PEG_TRACE] Failed to match rule {} at {}:{} (pos {})", #rule_name, line, col, pos)
			}
			__peg_result
		}}
	} else { body };

	let nl = raw("\n\n"); // make output slightly more readable

	if rule.cached {
		let cache_field = raw(&format!("{}_cache", rule.name));

		quote! { #nl
			fn #name<'input>(input: &'input str, state: &mut ParseState<'input>, pos: usize) -> RuleResult<#ret_ty> {
				let rule_result = #wrapped_body;
				state.#cache_field.insert(pos, rule_result.clone());
				rule_result
			}
		}
	} else {
		quote! { #nl
			fn #name<'input>(input: &'input str, state: &mut ParseState<'input>, pos: usize) -> RuleResult<#ret_ty> {
				#wrapped_body
			}
		}
	}
}

fn compile_rule_export(rule: &Rule) -> Tokens {
	let name = raw(&rule.name);
	let ret_ty = raw(&rule.ret_type);
	let parse_fn = raw(&format!("parse_{}", rule.name));
	quote! {
		pub fn #name<'input>(input: &'input str) -> ParseResult<#ret_ty> {
			let mut state = ParseState::new();
			match #parse_fn(input, &mut state, 0) {
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
	}
}

fn compile_match_and_then(grammar: &Grammar, e: &Expr, value_name: Option<&str>, then: Tokens) -> Tokens {
	let seq_res = compile_expr(grammar, e, value_name.is_some());
	let name_pat = match value_name {
		Some(name) => raw(name),
		None => raw("_")
	};

	quote! {{
		let seq_res = #seq_res;
		match seq_res {
			Matched(pos, #name_pat) => { #then }
			Failed => Failed,
		}
	}}
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

fn compile_expr(grammar: &Grammar, e: &Expr, result_used: bool) -> Tokens {
	match *e {
		AnyCharExpr => {
			quote!{ any_char(input, state, pos) }
		}

		LiteralExpr(ref s, case_insensitive) => {
			if case_insensitive {
				quote!{ slice_eq_case_insensitive(input, state, pos, #s) }
			} else {
				quote!{ slice_eq(input, state, pos, #s) }
			}
		}

		CharSetExpr(invert, ref cases) => {
			let expected_set = format_char_set(invert, &cases);

			let (in_set, not_in_set) = cond_swap(invert, (
				quote!{ Matched(next, ()) },
				quote!{ state.mark_failure(pos, #expected_set) },
			));

			let conds: Vec<_> = cases.iter().map(|case| {
				let start = case.start;
				let end = case.end;
				if start == end {
					quote!{ #start }
				} else {
					quote!{ #start...#end }
				}
			}).collect();

			quote!{
				if input.len() > pos {
					let (ch, next) = char_range_at(input, pos);
					match ch {
						#(#conds)|* => #in_set,
						_ => #not_in_set,
					}
				} else {
					state.mark_failure(pos, #expected_set)
				}
			}
		}

		RuleExpr(ref rule_name) => {
			let func = raw(&format!("parse_{}", rule_name));
			let rule = grammar.find_rule(rule_name);
			match rule {
				Some(rule) if rule.cached => {
					let cache_field = raw(&format!("{}_cache", *rule_name));

					if cfg!(feature = "trace") {
						quote! {
							state.#cache_field.get(&pos).map(|entry| {
								let (line, col) = pos_to_line(input, pos);
								match entry {
									&Matched(..) => println!("[PEG_TRACE] Cached match of rule {} at {}:{} (pos {})", #rule_name, line, col, pos),
									&Failed => println!("[PEG_TRACE] Cached fail of rule {} at {}:{} (pos {})", #rule_name, line, col, pos),
								};

								entry.clone()
							}).unwrap_or_else(|| #func(input, state, pos))
						}
					} else {
						quote! {
							state.#cache_field.get(&pos).map(|entry| entry.clone()).unwrap_or_else(|| #func(input, state, pos))
						}
					}
				},
				_ => {
					quote!{ #func(input, state, pos) }
				}
			}
		}

		SequenceExpr(ref exprs) => {
			fn write_seq(grammar: &Grammar, exprs: &[Expr]) -> Tokens {
				if exprs.len() == 1 {
					compile_expr(grammar, &exprs[0], false)
				} else {
					compile_match_and_then(grammar, &exprs[0], None, write_seq(grammar, &exprs[1..]))
				}
			}

			if exprs.len() > 0 {
				write_seq(grammar, &exprs)
			} else {
				quote!{ Matched(pos, ()) }
			}
		}

		ChoiceExpr(ref exprs) => {
			fn write_choice(grammar: &Grammar, exprs: &[Expr], result_used: bool) -> Tokens  {
				if exprs.len() == 1 {
					compile_expr(grammar, &exprs[0], result_used)
				} else {
					let choice_res = compile_expr(grammar, &exprs[0], result_used);
					let next = write_choice(grammar, &exprs[1..], result_used);

					quote! {{
						let choice_res = #choice_res;
						match choice_res {
							Matched(pos, value) => Matched(pos, value),
							Failed => #next
						}
					}}
				}
			}

			if exprs.len() > 0 {
				write_choice(grammar, &exprs, result_used)
			} else {
				quote!{ Matched(pos, ()) }
			}
		}

		OptionalExpr(box ref e) => {
			let optional_res = compile_expr(grammar, e, result_used);
			quote!{
				match #optional_res {
					Matched(newpos, value) => { Matched(newpos, Some(value)) },
					Failed => { Matched(pos, None) },
				}
			}
		}

		Repeat(box ref e, min, max, ref sep) => {
			let inner = compile_expr(grammar, e, result_used);

			let match_sep = sep.as_ref().map(|sep| {
				let sep_inner = compile_expr(grammar, &*sep, false);
				quote! {
					let pos = if repeat_value.len() > 0 {
						let sep_res = #sep_inner;
						match sep_res {
							Matched(newpos, _) => { newpos },
							Failed => break,
						}
					} else { pos };
				}
			});

			let result = if result_used {
				quote!( repeat_value )
			} else {
				quote!( () )
			};

			let (repeat_vec, repeat_step) =
			if result_used || min > 0 || max.is_some() || sep.is_some() {
				(Some(quote! { let mut repeat_value = vec!(); }),
				 Some(quote! { repeat_value.push(value); }))
			} else {
				(None, None)
			};

			let max_check = max.map(|max| { quote! { if repeat_value.len() >= #max { break } }});

			let result_check = if min > 0 {
				quote!{
					if repeat_value.len() >= #min {
						Matched(repeat_pos, #result)
					} else {
						Failed
					}
				}
			} else {
				quote!{ Matched(repeat_pos, #result) }
			};

			quote!{{
				let mut repeat_pos = pos;
				#repeat_vec

				loop {
					let pos = repeat_pos;

					#match_sep
					#max_check

					let step_res = #inner;
					match step_res {
						Matched(newpos, value) => {
							repeat_pos = newpos;
							#repeat_step
						},
						Failed => {
							break;
						}
					}
				}

				#result_check
			}}
		}

		PosAssertExpr(box ref e) => {
			let assert_res = compile_expr(grammar, e, false);
			quote! {{
				let assert_res = #assert_res;
				match assert_res {
					Matched(..) => Matched(pos, ()),
					Failed => Failed,
				}
			}}
		}

		NegAssertExpr(box ref e) => {
			let assert_res = compile_expr(grammar, e, false);
			quote! {{
				let assert_res = #assert_res;
				match assert_res {
					Failed => Matched(pos, ()),
					Matched(..) => Failed,
				}
			}}
		}

		ActionExpr(ref exprs, ref code, is_cond) => {
			fn write_seq(grammar: &Grammar, exprs: &[TaggedExpr], code: &str, is_cond: bool) -> Tokens {
				match exprs.first() {
					Some(ref first) => {
						let name = first.name.as_ref().map(|s| &s[..]);
						compile_match_and_then(grammar, &*first.expr, name,
							write_seq(grammar, &exprs[1..], code, is_cond)
						)
					}
					None => {
						let code_block = raw(code);

						if is_cond {
							quote!{
								match #code_block {
									Ok(res) => Matched(pos, res),
									Err(expected) => {
										state.mark_failure(pos, expected);
										Failed
									},
								}
							}
						} else {
							quote!{ Matched(pos, #code_block) }
						}
					}
				}
			}

			write_seq(grammar, &exprs, &code, is_cond)
		}
		MatchStrExpr(ref expr) => {
			let inner = compile_expr(grammar, expr, false);
			quote! {{
				let str_start = pos;
				match #inner {
					Matched(newpos, _) => { Matched(newpos, &input[str_start..newpos]) },
					Failed => Failed,
				}
			}}
		}
		PositionExpr => {
			quote! { Matched(pos, pos) }
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
