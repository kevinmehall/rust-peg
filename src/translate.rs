use std::borrow::ToOwned;
use std::collections::HashMap;
use quote::Tokens;
pub use self::Expr::*;

pub struct Error {
	message: String,
}

impl From<Error> for String {
	fn from(e: Error) -> String {
		e.message
	}
}

fn raw(s: &str) -> Tokens {
	let mut t = Tokens::new();
	t.append(s);
	t
}

pub struct Grammar {
	pub imports: Vec<String>,
	pub rules: Vec<Rule>,
	pub templates: HashMap<String, Template>,
}

impl Grammar {
	pub fn from_items(items: Vec<Item>) -> Grammar {
		let mut imports = Vec::new();
		let mut rules = Vec::new();
		let mut templates = HashMap::new();
		for item in items {
			match item {
				Item::Use(u) => { imports.push(u); }
				Item::Rule(rule) => { rules.push(rule); }
				Item::Template(template) => { templates.insert(template.name.clone(), template); }
			}
		}
		Grammar{ imports:imports, rules:rules, templates:templates }
	}

	fn find_rule(&self, name: &str) -> Option<&Rule> {
		self.rules.iter().find(|rule| rule.name == name)
	}
}

pub enum Item {
	Use(String),
	Rule(Rule),
	Template(Template),
}

pub struct Rule {
	pub name: String,
	pub expr: Box<Expr>,
	pub ret_type: String,
	pub exported: bool,
	pub cached: bool,
}

pub struct Template {
	pub name: String,
	pub params: Vec<String>,
	pub expr: Box<Expr>,
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
	Repeat(Box<Expr>, BoundedRepeat, /*sep*/ Option<Box<Expr>>),
	PosAssertExpr(Box<Expr>),
	NegAssertExpr(Box<Expr>),
	ActionExpr(Vec<TaggedExpr>, /*action*/ String, /*cond*/ bool),
	MatchStrExpr(Box<Expr>),
	PositionExpr,
	TemplateInvoke(String, Vec<Expr>),
	QuietExpr(Box<Expr>),
	FailExpr(String),
}

#[derive(Clone)]
pub enum BoundedRepeat {
	None,
	Plus,
	Exact(String),
	Both(Option<String>, Option<String>),
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
			if self.suppress_fail == 0 {
				if pos > self.max_err_pos {
					self.max_err_pos = pos;
					self.expected.clear();
				}

				if pos == self.max_err_pos {
					self.expected.insert(expected);
				}
			}

			Failed
		}
	}
};

pub fn compile_grammar(grammar: &Grammar) -> Result<Tokens, Error> {
	let mut items = vec![make_parse_state(&grammar.rules)];

	for rule in &grammar.rules {
		items.push(compile_rule(grammar, rule)?)
	}
	items.extend(grammar.rules.iter().filter(|rule| rule.exported).map(|rule| {
		compile_rule_export(rule)
	}));

	let view_items: Vec<_> = grammar.imports.iter().map(|x| raw(x)).collect();
	let helpers = raw(HELPERS);

	Ok(quote! {
		use self::RuleResult::{Matched, Failed};
		#(#view_items)*

		#helpers

		#(#items)*
	})
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
			suppress_fail: usize,
			expected: ::std::collections::HashSet<&'static str>,
			_phantom: ::std::marker::PhantomData<&'input ()>,
			#(#cache_fields_def),*
		}

		impl<'input> ParseState<'input> {
			fn new() -> ParseState<'input> {
				ParseState {
					max_err_pos: 0,
					suppress_fail: 0,
					expected: ::std::collections::HashSet::new(),
					_phantom: ::std::marker::PhantomData,
					#(#cache_fields: ::std::collections::HashMap::new()),*
				}
			}
		}
	}
}


fn compile_rule(grammar: &Grammar, rule: &Rule) -> Result<Tokens, Error> {
	let ref rule_name = rule.name;
	let name = raw(&format!("parse_{}", rule.name));
	let ret_ty = raw(&rule.ret_type);
	let context = Context {
		grammar: grammar,
		result_used: (&rule.ret_type as &str) != "()",
		lexical: &LexicalContext { defs: HashMap::new() },
	};

	let body = compile_expr(context, &*rule.expr)?;

	let wrapped_body = if cfg!(feature = "trace") {
		quote!{{
			let (line, col) = pos_to_line(__input, __pos);
			println!("[PEG_TRACE] Attempting to match rule {} at {}:{} (pos {})", #rule_name, line, col, __pos);
			let mut __peg_closure = || {
				#body
			};
			let __peg_result = __peg_closure();
			match __peg_result {
				Matched(_, _) => println!("[PEG_TRACE] Matched rule {} at {}:{} (pos {})", #rule_name, line, col, __pos),
				Failed => println!("[PEG_TRACE] Failed to match rule {} at {}:{} (pos {})", #rule_name, line, col, __pos)
			}
			__peg_result
		}}
	} else { body };

	let nl = raw("\n\n"); // make output slightly more readable

	Ok(if rule.cached {
		let cache_field = raw(&format!("{}_cache", rule.name));

		quote! { #nl
			fn #name<'input>(__input: &'input str, __state: &mut ParseState<'input>, __pos: usize) -> RuleResult<#ret_ty> {
				#![allow(non_snake_case, unused)]
				let rule_result = #wrapped_body;
				__state.#cache_field.insert(__pos, rule_result.clone());
				rule_result
			}
		}
	} else {
		quote! { #nl
			fn #name<'input>(__input: &'input str, __state: &mut ParseState<'input>, __pos: usize) -> RuleResult<#ret_ty> {
				#![allow(non_snake_case, unused)]
				#wrapped_body
			}
		}
	})
}

fn compile_rule_export(rule: &Rule) -> Tokens {
	let name = raw(&rule.name);
	let ret_ty = raw(&rule.ret_type);
	let parse_fn = raw(&format!("parse_{}", rule.name));
	quote! {
		pub fn #name<'input>(input: &'input str) -> ParseResult<#ret_ty> {
			#![allow(non_snake_case, unused)]
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

fn compile_match_and_then(cx: Context, e: &Expr, value_name: Option<&str>, then: Tokens) -> Result<Tokens, Error> {
	let seq_res = compile_expr(Context{ result_used: value_name.is_some(), ..cx }, e)?;
	let name_pat = match value_name {
		Some(name) => raw(name),
		None => raw("_")
	};

	Ok(quote! {{
		let seq_res = #seq_res;
		match seq_res {
			Matched(__pos, #name_pat) => { #then }
			Failed => Failed,
		}
	}})
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

struct LexicalContext<'a> {
	defs: HashMap<&'a str, (&'a Expr, &'a LexicalContext<'a>)>
}

#[derive(Copy, Clone)]
struct Context<'a> {
	grammar: &'a Grammar,
	result_used: bool,
	lexical: &'a LexicalContext<'a>,
}

impl<'a> Context<'a> {
	fn result_used(self, result_used: bool) -> Context<'a> {
		Context { result_used: result_used, ..self }
	}
}


fn compile_expr(cx: Context, e: &Expr) -> Result<Tokens, Error> {
	Ok(match *e {
		AnyCharExpr => {
			quote!{ any_char(__input, __state, __pos) }
		}

		LiteralExpr(ref s, case_insensitive) => {
			if case_insensitive {
				quote!{ slice_eq_case_insensitive(__input, __state, __pos, #s) }
			} else {
				quote!{ slice_eq(__input, __state, __pos, #s) }
			}
		}

		CharSetExpr(invert, ref cases) => {
			let expected_set = format_char_set(invert, &cases);

			let (in_set, not_in_set) = cond_swap(invert, (
				quote!{ Matched(__next, ()) },
				quote!{ __state.mark_failure(__pos, #expected_set) },
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

			let in_set_arm = if conds.len() > 0 {
				quote!( #(#conds)|* => #in_set, )
			} else {
				quote!()
			};

			quote!{
				if __input.len() > __pos {
					let (__ch, __next) = char_range_at(__input, __pos);
					match __ch {
						#in_set_arm
						_ => #not_in_set,
					}
				} else {
					__state.mark_failure(__pos, #expected_set)
				}
			}
		}

		RuleExpr(ref rule_name) => {
			if let Some(&(template_arg, lexical_context)) = cx.lexical.defs.get(&rule_name[..]) {
				compile_expr(Context{ lexical: lexical_context, ..cx }, template_arg)?
			} else if let Some(rule) = cx.grammar.find_rule(rule_name) {
				let func = raw(&format!("parse_{}", rule_name));
				if rule.cached {
					let cache_field = raw(&format!("{}_cache", *rule_name));

					if cfg!(feature = "trace") {
						quote! {
							__state.#cache_field.get(&__pos).map(|entry| {
								let (line, col) = pos_to_line(__input, __pos);
								match entry {
									&Matched(..) => println!("[PEG_TRACE] Cached match of rule {} at {}:{} (pos {})", #rule_name, line, col, pos),
									&Failed => println!("[PEG_TRACE] Cached fail of rule {} at {}:{} (pos {})", #rule_name, line, col, pos),
								};

								entry.clone()
							}).unwrap_or_else(|| #func(__input, __state, __pos))
						}
					} else {
						quote! {
							__state.#cache_field.get(&__pos).map(|entry| entry.clone()).unwrap_or_else(|| #func(__input, __state, __pos))
						}
					}
				} else {
					quote!{ #func(__input, __state, __pos) }
				}
			} else {
				Err(Error{ message: format!("No rule named `{}`", rule_name) })?
			}
		}

		TemplateInvoke(ref name, ref params) => {
			let template = cx.grammar.templates.get(&name[..])
				.ok_or_else(|| Error { message: format!("No template named `{}``", name) })?;

			if template.params.len() != params.len() {
				Err(Error { message: format!("Expected {} arguments to `{}`, found {}", template.params.len(), template.name, params.len())})?
			}

			let defs = template.params.iter().zip(params.iter())
				.map(|(name, expr)| (&name[..], (expr, cx.lexical))).collect();
			compile_expr(Context{ lexical: &LexicalContext { defs: defs }, ..cx }, &template.expr)?
		}

		SequenceExpr(ref exprs) => {
			fn write_seq(cx: Context, exprs: &[Expr]) -> Result<Tokens, Error> {
				if exprs.len() == 1 {
					compile_expr(cx.result_used(false), &exprs[0])
				} else {
					compile_match_and_then(cx, &exprs[0], None, write_seq(cx, &exprs[1..])?)
				}
			}

			if exprs.len() > 0 {
				write_seq(cx, &exprs)?
			} else {
				quote!{ Matched(__pos, ()) }
			}
		}

		ChoiceExpr(ref exprs) => {
			fn write_choice(cx: Context, exprs: &[Expr]) -> Result<Tokens, Error>  {
				if exprs.len() == 1 {
					compile_expr(cx, &exprs[0])
				} else {
					let choice_res = compile_expr(cx, &exprs[0])?;
					let next = write_choice(cx, &exprs[1..])?;

					Ok(quote! {{
						let choice_res = #choice_res;
						match choice_res {
							Matched(__pos, __value) => Matched(__pos, __value),
							Failed => #next
						}
					}})
				}
			}

			if exprs.len() > 0 {
				write_choice(cx, &exprs)?
			} else {
				quote!{ Matched(__pos, ()) }
			}
		}

		OptionalExpr(ref e) => {
			let optional_res = compile_expr(cx, e)?;

			if cx.result_used {
				quote!{
					match #optional_res {
						Matched(newpos, value) => { Matched(newpos, Some(value)) },
						Failed => { Matched(__pos, None) },
					}
				}
			} else {
				quote!{
					match #optional_res {
						Matched(__newpos, _) => { Matched(__newpos, ()) },
						Failed => { Matched(__pos, ()) },
					}
				}
			}
		}

		Repeat(ref e, ref bounds, ref sep) => {
			let inner = compile_expr(cx, e)?;

			let (min, max) = match *bounds {
				BoundedRepeat::None => (None, None),
				BoundedRepeat::Plus => (Some(quote!(1)), None),
				BoundedRepeat::Exact(ref code) => (Some(raw(code)), Some(raw(code))),
				BoundedRepeat::Both(ref min, ref max) => (min.as_ref().map(|x| raw(x)), max.as_ref().map(|x| raw(x)))
			};

			let match_sep = if let &Some(ref sep) = sep {
				let sep_inner = compile_expr(cx.result_used(false), &*sep)?;
				quote! {
					let __pos = if repeat_value.len() > 0 {
						let sep_res = #sep_inner;
						match sep_res {
							Matched(newpos, _) => { newpos },
							Failed => break,
						}
					} else { __pos };
				}
			} else { quote!() };

			let result = if cx.result_used {
				quote!( repeat_value )
			} else {
				quote!( () )
			};

			let (repeat_vec, repeat_step) =
			if cx.result_used || min.is_some() || max.is_some() || sep.is_some() {
				(Some(quote! { let mut repeat_value = vec!(); }),
				 Some(quote! { repeat_value.push(value); }))
			} else {
				(None, None)
			};

			let max_check = max.map(|max| { quote! { if repeat_value.len() >= #max { break } }});

			let result_check = if let Some(min) = min {
				quote!{
					if repeat_value.len() >= #min {
						Matched(__repeat_pos, #result)
					} else {
						Failed
					}
				}
			} else {
				quote!{ Matched(__repeat_pos, #result) }
			};

			quote!{{
				let mut __repeat_pos = __pos;
				#repeat_vec

				loop {
					let __pos = __repeat_pos;

					#match_sep
					#max_check

					let step_res = #inner;
					match step_res {
						Matched(newpos, value) => {
							__repeat_pos = newpos;
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

		PosAssertExpr(ref e) => {
			let assert_res = compile_expr(cx, e)?;
			quote! {{
				let __assert_res = #assert_res;
				match __assert_res {
					Matched(_, __value) => Matched(__pos, __value),
					Failed => Failed,
				}
			}}
		}

		NegAssertExpr(ref e) => {
			let assert_res = compile_expr(cx.result_used(false), e)?;
			quote! {{
				let __assert_res = #assert_res;
				match __assert_res {
					Failed => Matched(__pos, ()),
					Matched(..) => Failed,
				}
			}}
		}

		ActionExpr(ref exprs, ref code, is_cond) => {
			fn write_seq(cx: Context, exprs: &[TaggedExpr], code: &str, is_cond: bool) -> Result<Tokens, Error> {
				match exprs.first() {
					Some(ref first) => {
						let name = first.name.as_ref().map(|s| &s[..]);
						compile_match_and_then(cx, &*first.expr, name,
							write_seq(cx, &exprs[1..], code, is_cond)?
						)
					}
					None => {
						let code_block = raw(code);

						Ok(if is_cond {
							quote!{
								match #code_block {
									Ok(res) => Matched(__pos, res),
									Err(expected) => {
										__state.mark_failure(__pos, expected);
										Failed
									},
								}
							}
						} else {
							quote!{ Matched(__pos, #code_block) }
						})
					}
				}
			}

			write_seq(cx, &exprs, &code, is_cond)?
		}
		MatchStrExpr(ref expr) => {
			let inner = compile_expr(cx.result_used(false), expr)?;
			quote! {{
				let str_start = __pos;
				match #inner {
					Matched(newpos, _) => { Matched(newpos, &__input[str_start..newpos]) },
					Failed => Failed,
				}
			}}
		}
		PositionExpr => {
			quote! { Matched(__pos, __pos) }
		}
		QuietExpr(ref expr) => {
			let inner = compile_expr(cx, expr)?;
			quote! {{
				__state.suppress_fail += 1;
				let res = #inner;
				__state.suppress_fail -= 1;
				res
			}}
		}
		FailExpr(ref expected) => {
			quote!{{ __state.mark_failure(__pos, #expected); Failed }}
		}
	})
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
