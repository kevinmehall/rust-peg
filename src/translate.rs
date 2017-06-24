use std::borrow::ToOwned;
use std::collections::HashMap;
use quote::Tokens;
pub use self::Expr::*;

#[derive(Debug)]
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
	pub args: Vec<(String, String)>,
}

impl Grammar {
	pub fn from_ast(items: Vec<Item>) -> Result<Grammar, Error> {
		let mut imports = Vec::new();
		let mut rules = Vec::new();
		let mut templates = HashMap::new();

		let mut grammar_args = None;

		for item in items {
			match item {
				Item::Use(u) => { imports.push(u); }
				Item::Rule(rule) => { rules.push(rule); }
				Item::Template(template) => { templates.insert(template.name.clone(), template); }
				Item::GrammarArgs(args) => {
					if grammar_args.is_none() {
						grammar_args = Some(args);
					} else {
						Err(Error{ message: format!("Multiple #![grammar_args] attributes") })?
					}
				}
			}
		}
		Ok(Grammar{ imports:imports, rules:rules, templates:templates, args: grammar_args.unwrap_or(vec![]) })
	}

	fn find_rule(&self, name: &str) -> Option<&Rule> {
		self.rules.iter().find(|rule| rule.name == name)
	}

	fn extra_args_def(&self) -> Tokens {
		let args: Vec<Tokens> = self.args.iter().map(|&(ref name, ref tp)| {
			let name = raw(name);
			let tp = raw(tp);
			quote!(, #name: #tp)
		}).collect();
		quote!(#(#args)*)
	}

	fn extra_args_call(&self) -> Tokens {
		let args: Vec<Tokens> = self.args.iter().map(|&(ref name, _)| {
			let name = raw(name);
			quote!(, #name)
		}).collect();
		quote!(#(#args)*)
	}

	fn check_name(&self, name: &str) -> Result<(), Error> {
		if self.args.iter().any(|x| x.0 == name) {
			Err(Error{ message: format!("Capture variable `{}` shadows grammar argument", name) })?
		}

		Ok(())
	}
}

pub enum Item {
	Use(String),
	Rule(Rule),
	Template(Template),
	GrammarArgs(Vec<(String, String)>)
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
	InfixExpr{ atom: Box<Expr>, levels: Vec<InfixLevel> }
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum InfixAssoc { Left, Right }

#[derive(Clone)]
pub struct InfixLevel {
	pub assoc: InfixAssoc,
	pub operators: Vec<InfixOperator>,
}

#[derive(Clone)]
pub struct InfixOperator {
	pub operator: Box<Expr>,
	pub l_arg: String,
	pub op_arg: Option<String>,
	pub r_arg: String,
	pub action: String,
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
		let before = &input[..pos];
		let line = before.as_bytes().iter().filter(|&&c| c == b'\n').count() + 1;
		let col = before.chars().rev().take_while(|&c| c != '\n').count() + 1;
		(line, col)
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
		compile_rule_export(grammar, rule)
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
	let name = raw(&format!("__parse_{}", rule.name));
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
	let extra_args_def = grammar.extra_args_def();

	Ok(if rule.cached {
		let cache_field = raw(&format!("{}_cache", rule.name));

		let cache_trace = if cfg!(feature = "trace") {
			quote!{
				let (line, col) = pos_to_line(__input, __pos);
                match entry {
                    &Matched(..) => println!("[PEG_TRACE] Cached match of rule {} at {}:{} (pos {})", #rule_name, line, col, __pos),
                    &Failed => println!("[PEG_TRACE] Cached fail of rule {} at {}:{} (pos {})", #rule_name, line, col, __pos),
                };
			}
		} else {
			quote!()
		};

		quote! { #nl
			fn #name<'input>(__input: &'input str, __state: &mut ParseState<'input>, __pos: usize #extra_args_def) -> RuleResult<#ret_ty> {
				#![allow(non_snake_case, unused)]
				if let Some(entry) = __state.#cache_field.get(&__pos) {
					#cache_trace
					return entry.clone();
				}
				let __rule_result = #wrapped_body;
				__state.#cache_field.insert(__pos, __rule_result.clone());
				__rule_result
			}
		}
	} else {
		quote! { #nl
			fn #name<'input>(__input: &'input str, __state: &mut ParseState<'input>, __pos: usize #extra_args_def) -> RuleResult<#ret_ty> {
				#![allow(non_snake_case, unused)]
				#wrapped_body
			}
		}
	})
}

fn compile_rule_export(grammar: &Grammar, rule: &Rule) -> Tokens {
	let name = raw(&rule.name);
	let ret_ty = raw(&rule.ret_type);
	let parse_fn = raw(&format!("__parse_{}", rule.name));
	let nl = raw("\n\n"); // make output slightly more readable
	let extra_args_def = grammar.extra_args_def();
	let extra_args_call = grammar.extra_args_call();

	quote! {
		#nl
		pub fn #name<'input>(__input: &'input str #extra_args_def) -> ParseResult<#ret_ty> {
			#![allow(non_snake_case, unused)]
			let mut __state = ParseState::new();
			match #parse_fn(__input, &mut __state, 0 #extra_args_call) {
				Matched(__pos, __value) => {
					if __pos == __input.len() {
						return Ok(__value)
					}
				}
				_ => {}
			}
			let (__line, __col) = pos_to_line(__input, __state.max_err_pos);

			Err(ParseError {
				line: __line,
				column: __col,
				offset: __state.max_err_pos,
				expected: __state.expected,
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
		let __seq_res = #seq_res;
		match __seq_res {
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
				let func = raw(&format!("__parse_{}", rule_name));
				let extra_args_call = cx.grammar.extra_args_call();
				if cx.result_used && rule.ret_type == "()" {
					return Err(Error{ message: format!("Tried to bind result of rule {}, which returns () - perhaps you forgot a return type?", rule_name)})?
				}
				if cx.result_used || rule.ret_type == "()" {
					quote!{ #func(__input, __state, __pos #extra_args_call) }
				} else {
					quote!{
						match #func(__input, __state, __pos #extra_args_call) {
							Matched(pos, _) => Matched(pos, ()),
							Failed => Failed,
						}
					}
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
						let __choice_res = #choice_res;
						match __choice_res {
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
						Matched(__newpos, __value) => { Matched(__newpos, Some(__value)) },
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
					let __pos = if __repeat_value.len() > 0 {
						let __sep_res = #sep_inner;
						match __sep_res {
							Matched(__newpos, _) => { __newpos },
							Failed => break,
						}
					} else { __pos };
				}
			} else { quote!() };

			let result = if cx.result_used {
				quote!( __repeat_value )
			} else {
				quote!( () )
			};

			let (repeat_vec, repeat_step) =
			if cx.result_used || min.is_some() || max.is_some() || sep.is_some() {
				(Some(quote! { let mut __repeat_value = vec!(); }),
				 Some(quote! { __repeat_value.push(__value); }))
			} else {
				(None, None)
			};

			let max_check = max.map(|max| { quote! { if __repeat_value.len() >= #max { break } }});

			let result_check = if let Some(min) = min {
				quote!{
					if __repeat_value.len() >= #min {
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

					let __step_res = #inner;
					match __step_res {
						Matched(__newpos, __value) => {
							__repeat_pos = __newpos;
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
				__state.suppress_fail += 1;
				let __assert_res = #assert_res;
				__state.suppress_fail -= 1;
				match __assert_res {
					Matched(_, __value) => Matched(__pos, __value),
					Failed => Failed,
				}
			}}
		}

		NegAssertExpr(ref e) => {
			let assert_res = compile_expr(cx.result_used(false), e)?;
			quote! {{
				__state.suppress_fail += 1;
				let __assert_res = #assert_res;
				__state.suppress_fail -= 1;
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

						if let Some(name) = name {
							cx.grammar.check_name(name)?;
						}

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
					Matched(__newpos, _) => { Matched(__newpos, &__input[str_start..__newpos]) },
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

		InfixExpr{ ref atom, ref levels } => {
			let match_atom = compile_expr(cx, atom)?;
			let ty = if let RuleExpr(ref atom_rule_name) = **atom {
				if let Some(rule) = cx.grammar.find_rule(atom_rule_name) {
					raw(&rule.ret_type[..])
				} else {
					Err(Error{ message: format!("No rule named `{}`", atom_rule_name) })?
				}
			} else {
				Err(Error{ message: format!("#infix atom must be a rule, not an arbitrary expression (so its return type can be inspected)") })?
			};

			let mut level_code = Vec::new();
			let extra_args_def = cx.grammar.extra_args_def();
			let extra_args_call = cx.grammar.extra_args_call();

			for (prec, level) in levels.iter().enumerate() {
				let prec = prec as i32;
				let new_prec = match level.assoc {
					InfixAssoc::Left => prec + 1,
					InfixAssoc::Right => prec
				};

				let mut rules = Vec::new();
				for op in &level.operators {
					let match_rule = compile_expr(cx.result_used(op.op_arg.is_some()), &*op.operator)?;
					let action = raw(&op.action[..]);

					let l_arg = raw(&op.l_arg[..]);
					let op_arg = op.op_arg.as_ref().map(|x| raw(&x[..])).unwrap_or_else(|| quote!(_));
					let r_arg = raw(&op.r_arg[..]);

					rules.push(quote!{
						if let Matched(__pos, #op_arg) = #match_rule {
							if let Matched(__pos, #r_arg) = __infix_parse(#new_prec, __input, __state, __pos #extra_args_call) {
								let #l_arg = __infix_result;
								__infix_result = #action;
								__repeat_pos = __pos;
								continue;
							}
						}
					});
				}

				level_code.push(quote!{
					if #prec >= __min_prec {
						#(#rules)*
					}
				});
			}

			let (enter, leave) = if cfg!(feature = "trace") {
				(quote!{println!("[PEG_TRACE] Entering level {}", __min_prec);},
				 quote!{println!("[PEG_TRACE] Leaving level {}", __min_prec);})
			} else {
				(quote!(), quote!())
			};

			quote!{{
				fn __infix_parse<'input>(__min_prec:i32, __input: &'input str, __state: &mut ParseState<'input>, __pos: usize #extra_args_def) -> RuleResult<#ty> {
					if let Matched(__pos, mut __infix_result) = #match_atom {
						#enter
						let mut __repeat_pos = __pos;
						loop {
							let __pos = __repeat_pos;
							#(#level_code)*
							break;
						}
						#leave
						Matched(__repeat_pos, __infix_result)
					} else {
						Failed
					}
				}
				__infix_parse(0, __input, __state, __pos #extra_args_call)
			}}
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
