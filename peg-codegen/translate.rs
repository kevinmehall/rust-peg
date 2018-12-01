use std::borrow::ToOwned;
use std::collections::{ HashMap, HashSet };
use proc_macro2::{Ident, Span, TokenStream};
pub use self::Expr::*;
use codemap::{ Spanned, Span as CodemapSpan };
use PegCompiler;

fn raw(s: &str) -> TokenStream {
	s.parse().expect("Lexical error")
}

pub(crate) struct Grammar {
	pub imports: Vec<String>,
	pub rules: Vec<Rule>,
	pub templates: HashMap<String, Template>,
	pub args: Vec<(String, String)>,
	pub input_type: String,
}

impl Grammar {
	pub fn from_ast(compiler: &mut PegCompiler, items: Vec<Spanned<Item>>) -> Result<Grammar, ()> {
		let mut imports = Vec::new();
		let mut rules: Vec<Rule> = Vec::new();
		let mut templates = HashMap::new();

		let mut grammar_args = None;
		let mut input_type = None;

		for item in items {
			match item.node {
				Item::Use(u) => { imports.push(u); }
				Item::Rule(rule) => {
					if rules.iter().any(|existing_rule| existing_rule.name == rule.name) {
						compiler.span_error(
							format!("Multiple rules named `{}`", rule.name),
							item.span,
							Some("duplicate declaration".to_owned())
						)
					}

					rules.push(rule);
				}
				Item::Template(template) => {
					if let Some(prev) = templates.insert(template.name.clone(), template) {
						compiler.span_error(
							format!("Multiple templates named `{}`", prev.name),
							item.span,
							Some("duplicate declaration".to_owned())
						)
					}
				}
				Item::GrammarArgs(args) => {
					if grammar_args.is_none() {
						grammar_args = Some(args);
					} else {
						compiler.span_error(
							"Grammar contains multiple #![grammar_args] attributes".to_owned(),
							item.span,
							Some("duplicate declaration".to_owned())
						)
					}
				}
				Item::InputType(args) => {
					if grammar_args.is_none() {
						input_type = Some(args);
					} else {
						compiler.span_error(
							"Grammar contains multiple `type Input = ...` declarations".to_owned(),
							item.span,
							Some("duplicate declaration".to_owned())
						)
					}
				}
			}
		}

		for recursion in Grammar::check_for_left_recursion(&rules) {
			compiler.span_error(
				format!("Found illegal left recursion for rule {}: {}", recursion.rule_name, recursion.format_path()),
				recursion.span,
				None,
			)
		}

		Ok(Grammar{
			imports, rules, templates,
			args: grammar_args.unwrap_or(vec![]),
			input_type: input_type.unwrap_or("str".to_owned()),
		})
	}

	fn find_rule(&self, name: &str) -> Option<&Rule> {
		self.rules.iter().find(|rule| rule.name == name)
	}

	fn extra_args_def(&self) -> TokenStream {
		let args: Vec<TokenStream> = self.args.iter().map(|&(ref name, ref tp)| {
			let name = Ident::new(name, Span::call_site());
			let tp = raw(tp);
			quote!(, #name: #tp)
		}).collect();
		quote!(#(#args)*)
	}

	fn extra_args_call(&self) -> TokenStream {
		let args: Vec<TokenStream> = self.args.iter().map(|&(ref name, _)| {
			let name = Ident::new(name, Span::call_site());
			quote!(, #name)
		}).collect();
		quote!(#(#args)*)
	}

	fn check_for_left_recursion(rules: &Vec<Rule>) -> Vec<LeftRecursion> {
		let mut recursions = Vec::new();

		let rule_map: HashMap<&str, &Rule> = rules.iter().map(|rule| (&rule.name[..], rule)).collect();
		for rule in rules.iter() {
			LeftRecursionChecker::new(&rule.name, &rule_map).check(rule, &mut recursions)
		}

		recursions
	}
}

struct LeftRecursionChecker<'a> {
	rule_map: &'a HashMap<&'a str, &'a Rule>,
	rule_name: &'a str,
	rule_stack: Vec<&'a str>,
	visited_rules: HashSet<&'a str>,
}

impl <'a> LeftRecursionChecker<'a> {
	fn new(rule_name: &'a str, rule_map: &'a HashMap<&'a str, &'a Rule>) -> LeftRecursionChecker<'a> {
		let mut rule_stack = Vec::new();
		rule_stack.push(rule_name);
		LeftRecursionChecker {
			rule_map,
			rule_name,
			rule_stack,
			visited_rules: HashSet::new(),
		}
    }

	fn check(&mut self, rule: &'a Rule, recursions: &mut Vec<LeftRecursion>) {
		let left_rules = rule.expr.left_rules(&rule.expr.span);

		for left_rule in left_rules {
			if self.rule_name == left_rule.node {
				self.rule_stack.push(left_rule.node);
				recursions.push(LeftRecursion{
					rule_name: self.rule_name.into(),
					span: left_rule.span,
					path: self.rule_stack.iter().map(|x| (*x).into()).collect(),
				});
				self.rule_stack.pop();
			} else if let Some(rule) = self.rule_map.get(left_rule.node) {
				if self.visited_rules.insert(&rule.name) {
					self.rule_stack.push(&rule.name);
					self.check(rule, recursions);
					self.rule_stack.pop();
				}
            }
		}
	}
}

struct LeftRecursion {
	rule_name: String,
	path: Vec<String>,
	span: CodemapSpan,
}

impl LeftRecursion {
	fn format_path(&self) -> String {
		self.path.join(" -> ")
	}
}

pub enum Item {
	Use(String),
	Rule(Rule),
	Template(Template),
	GrammarArgs(Vec<(String, String)>),
	InputType(String),
}

pub struct Rule {
	pub name: String,
	pub expr: Spanned<Expr>,
	pub ret_type: String,
	pub visibility: Option<String>,
	pub cached: bool,
}

pub struct Template {
	pub name: String,
	pub params: Vec<String>,
	pub expr: Box<Spanned<Expr>>,
}

#[derive(Clone, PartialEq)]
pub struct CharSetCase {
	pub start: char,
	pub end: char
}

#[derive(Clone, PartialEq)]
pub struct TaggedExpr {
	pub name: Option<Spanned<String>>,
	pub expr: Box<Spanned<Expr>>,
}

#[derive(Clone, PartialEq)]
pub enum Expr {
	AnyCharExpr,
	LiteralExpr(String),
	PatternExpr(String),
	RuleExpr(String),
	SequenceExpr(Vec<Spanned<Expr>>),
	ChoiceExpr(Vec<Spanned<Expr>>),
	OptionalExpr(Box<Spanned<Expr>>),
	Repeat(Box<Spanned<Expr>>, BoundedRepeat, /*sep*/ Option<Box<Spanned<Expr>>>),
	PosAssertExpr(Box<Spanned<Expr>>),
	NegAssertExpr(Box<Spanned<Expr>>),
	ActionExpr(Vec<TaggedExpr>, /*action*/ String, /*cond*/ bool),
	MatchStrExpr(Box<Spanned<Expr>>),
	PositionExpr,
	TemplateInvoke(String, Vec<Spanned<Expr>>),
	QuietExpr(Box<Spanned<Expr>>),
	FailExpr(String),
	InfixExpr{ atom: Box<Spanned<Expr>>, levels: Vec<InfixLevel> },
	MarkerExpr,
}

impl Expr {
	fn left_rules(&self, span: &CodemapSpan) -> HashSet<Spanned<&str>> {
		let mut rules = HashSet::new();
		self.left_rules_recurse(&mut rules, span);
		rules
	}

	fn left_rules_recurse<'a>(&'a self, rules: &mut HashSet<Spanned<&'a str>>, span: &CodemapSpan) {
		match *self {
            RuleExpr(ref rule) => {
				rules.insert(Spanned{ span: *span, node: rule });
			},
            SequenceExpr(ref exprs)
			| TemplateInvoke(_, ref exprs) => {
                if let Some(expr) = exprs.first() {
                    exprs[0].left_rules_recurse(rules, &expr.span);
                }
            }
            ChoiceExpr(ref choices) => {
                for choice in choices {
                    choice.left_rules_recurse(rules, &choice.span);
                }
            }
            OptionalExpr(ref expr)
            | Repeat(ref expr, _, _)
			| MatchStrExpr(ref expr)
			| QuietExpr(ref expr)
            | PosAssertExpr(ref expr)
            | NegAssertExpr(ref expr) => expr.left_rules_recurse(rules, &expr.span),
			InfixExpr{ ref atom, .. } => atom.left_rules_recurse(rules, &atom.span),
            ActionExpr(ref tagged_actions, _, _) => {
                if let Some(action) = tagged_actions.first() {
                    action.expr.left_rules_recurse(rules, &action.expr.span);
                }
            }
            _ => {}
		}
	}
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum InfixAssoc { Left, Right }

#[derive(Clone, PartialEq)]
pub struct InfixLevel {
	pub assoc: InfixAssoc,
	pub operators: Vec<Spanned<InfixOperator>>,
}

#[derive(Clone, PartialEq)]
pub struct InfixOperator {
	pub elements: Vec<TaggedExpr>,
	pub action: String,
}

#[derive(Clone, PartialEq)]
pub enum BoundedRepeat {
	None,
	Plus,
	Exact(String),
	Both(Option<String>, Option<String>),
}

pub(crate) fn compile_grammar(compiler: &mut PegCompiler, grammar: &Grammar) -> Result<TokenStream, ()> {
	let mut items = vec![make_parse_state(&grammar.rules)];

	for rule in &grammar.rules {
		items.push(compile_rule(compiler, grammar, rule))
	}
	items.extend(grammar.rules.iter().filter(|rule| rule.visibility.is_some()).map(|rule| {
		compile_rule_export(grammar, rule)
	}));

	let view_items: Vec<_> = grammar.imports.iter().map(|x| raw(x)).collect();
	let input_type = raw(&grammar.input_type);

	Ok(quote! {
		use ::peg::RuleResult::{Matched, Failed};
		#(#view_items)*

		type Input = #input_type;
		type Position<'input> = <Input as ::peg::Parse<'input>>::Position;
		type PositionRepr<'input> = <Input as ::peg::Parse<'input>>::PositionRepr;

		#(#items)*
	})
}

fn make_parse_state(rules: &[Rule]) -> TokenStream {
	let mut cache_fields_def: Vec<TokenStream> = Vec::new();
	let mut cache_fields: Vec<Ident> = Vec::new();
	for rule in rules {
		if rule.cached {
			let name = Ident::new(&format!("{}_cache", rule.name), Span::call_site());
			let ret_ty = raw(&rule.ret_type);
			cache_fields_def.push(quote!{ #name: ::std::collections::HashMap<usize, ::peg::RuleResult<usize, #ret_ty>> });
			cache_fields.push(name);
		}
	}

	quote! {
		struct ParseState<'input> {
			_phantom: ::std::marker::PhantomData<&'input ()>,
			#(#cache_fields_def),*
		}

		impl<'input> ParseState<'input> {
			fn new() -> ParseState<'input> {
				ParseState {
					_phantom: ::std::marker::PhantomData,
					#(#cache_fields: ::std::collections::HashMap::new()),*
				}
			}
		}
	}
}


fn compile_rule(compiler: &mut PegCompiler, grammar: &Grammar, rule: &Rule) -> TokenStream {
	let ref rule_name = rule.name;
	let name = Ident::new(&format!("__parse_{}", rule.name), Span::call_site());
	let ret_ty = raw(&rule.ret_type);
	let context = Context {
		grammar: grammar,
		result_used: (&rule.ret_type as &str) != "()",
		lexical: &LexicalContext { defs: HashMap::new() },
	};

	let body = compile_expr(compiler, context, &rule.expr);

	let wrapped_body = if cfg!(feature = "trace") {
		quote!{{
			let loc = ::peg::Parse::position_repr(__input, pos);
			println!("[PEG_TRACE] Attempting to match rule {} at {}", #rule_name, loc);
			let mut __peg_closure = || {
				#body
			};
			let __peg_result = __peg_closure();
			match __peg_result {
				Matched(_, _) => println!("[PEG_TRACE] Matched rule {} at {}:{}", #rule_name, loc),
				Failed => println!("[PEG_TRACE] Failed to match rule {} at {}:{}", #rule_name, loc)
			}
			__peg_result
		}}
	} else { body };

	let nl = raw("\n\n"); // make output slightly more readable
	let extra_args_def = grammar.extra_args_def();

	if rule.cached {
		let cache_field = Ident::new(&format!("{}_cache", rule.name), Span::call_site());

		let cache_trace = if cfg!(feature = "trace") {
			quote!{
				let loc = ::peg::Parse::position_repr(__input, pos);
                match entry {
                    &Matched(..) => println!("[PEG_TRACE] Cached match of rule {} at {}", #rule_name, loc),
                    &Failed => println!("[PEG_TRACE] Cached fail of rule {} at {}", #rule_name, loc),
                };
			}
		} else {
			quote!()
		};

		quote! { #nl
			fn #name<'input>(__input: &'input Input, __state: &mut ParseState<'input>, __err_state: &mut ::peg::error::ErrorState<Position<'input>>, __pos: Position<'input> #extra_args_def) -> ::peg::RuleResult<usize, #ret_ty> {
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
			fn #name<'input>(__input: &'input Input, __state: &mut ParseState<'input>, __err_state: &mut ::peg::error::ErrorState<Position<'input>>, __pos: Position<'input> #extra_args_def) -> ::peg::RuleResult<usize, #ret_ty> {
				#![allow(non_snake_case, unused)]
				#wrapped_body
			}
		}
	}
}

fn compile_rule_export(grammar: &Grammar, rule: &Rule) -> TokenStream {
	let name = Ident::new(&rule.name, Span::call_site());
	let ret_ty = raw(&rule.ret_type);
	let visibility = rule.visibility.as_ref().map(|s| raw(&s));
	let parse_fn = Ident::new(&format!("__parse_{}", rule.name), Span::call_site());
	let nl = raw("\n\n"); // make output slightly more readable
	let extra_args_def = grammar.extra_args_def();
	let extra_args_call = grammar.extra_args_call();

	quote! {
		#nl
		#visibility fn #name<'input>(__input: &'input Input #extra_args_def) -> Result<#ret_ty, ::peg::error::ParseError<PositionRepr<'input>>> {
			#![allow(non_snake_case, unused)]

			let mut __err_state = ::peg::error::ErrorState::new(::peg::Parse::start(__input));
			let mut __state = ParseState::new();
			match #parse_fn(__input, &mut __state, &mut __err_state, ::peg::Parse::start(__input) #extra_args_call) {
				Matched(__pos, __value) => {
					if __pos == __input.len() {
						return Ok(__value)
					}
				}
				_ => ()
			}

			__state = ParseState::new();
			__err_state.reparse_for_error();

			#parse_fn(__input, &mut __state, &mut __err_state, ::peg::Parse::start(__input) #extra_args_call);

			let loc = ::peg::Parse::position_repr(__input, __err_state.max_err_pos);;

			Err(::peg::error::ParseError {
				location: loc,
				expected: __err_state.expected,
			})
		}
	}
}

fn compile_match_and_then(compiler: &mut PegCompiler, cx: Context, e: &Spanned<Expr>, value_name: Option<&str>, then: TokenStream) -> TokenStream {
	let seq_res = compile_expr(compiler, Context{ result_used: value_name.is_some(), ..cx }, e);
	let name_pat = Ident::new(value_name.unwrap_or("_"), Span::call_site());

	quote! {{
		let __seq_res = #seq_res;
		match __seq_res {
			Matched(__pos, #name_pat) => { #then }
			Failed => Failed,
		}
	}}
}

fn labeled_seq(compiler: &mut PegCompiler, cx: Context, exprs: &[TaggedExpr], inner: TokenStream) -> TokenStream {
	match exprs.first() {
		Some(ref first) => {
			if let Some(name) = first.name.as_ref() {
				if cx.grammar.args.iter().any(|x| x.0 == name.node) {
					compiler.span_error(
						format!("Capture variable `{}` shadows grammar argument", name.node),
						name.span,
						Some("prohibited name".to_owned())
					)
				}

				if name.node.starts_with("__") {
					compiler.span_error(
						format!("Capture variable `{}` has reserved name", name.node),
						name.span,
						Some("prohibited name".to_owned())
					)
				}
			}

			let name = first.name.as_ref().map(|s| &s[..]);
			let seq = labeled_seq(compiler, cx, &exprs[1..], inner);
			compile_match_and_then(compiler, cx, &*first.expr, name, seq)
		}
		None => inner
	}
}

fn cond_swap<T>(swap: bool, tup: (T, T)) -> (T, T) {
	let (a, b) = tup;
	if swap {
		(b, a)
	} else {
		(a, b)
	}
}

struct LexicalContext<'a> {
	defs: HashMap<&'a str, (&'a Spanned<Expr>, &'a LexicalContext<'a>)>
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


fn compile_expr(compiler: &mut PegCompiler, cx: Context, e: &Spanned<Expr>) -> TokenStream {
	match e.node {
		AnyCharExpr => {
			quote!{ ::peg::ParseElem::parse_elem(__input, __pos) }
		}

		LiteralExpr(ref s) => {
			quote!{ match ::peg::ParseLiteral::parse_string_literal(__input, __pos, #s) {
				Matched(__pos, __val) => Matched(__pos, __val),
				Failed => __err_state.mark_failure(__pos, #s)
			}}
		}

		PatternExpr(ref pattern) => {
			let invert = false;
			let expected_set = pattern;

			let (in_set, not_in_set) = cond_swap(invert, (
				quote!{ Matched(__next, ()) },
				quote!{ __err_state.mark_failure(__pos, #expected_set) },
			));

			let pat = raw(pattern);
			let in_set_arm = quote!( #pat => #in_set, );

			quote!{
				match ::peg::ParseElem::parse_elem(__input, __pos) {
					Matched(__next, __ch) => match __ch {
						#in_set_arm
						_ => #not_in_set,
					}
					Failed => __err_state.mark_failure(__pos, #expected_set)
				}					
			}
		}

		RuleExpr(ref rule_name) => {
			if let Some(&(template_arg, lexical_context)) = cx.lexical.defs.get(&rule_name[..]) {
				compile_expr(compiler, Context{ lexical: lexical_context, ..cx }, template_arg)
			} else if let Some(rule) = cx.grammar.find_rule(rule_name) {
				let func = Ident::new(&format!("__parse_{}", rule_name), Span::call_site());
				let extra_args_call = cx.grammar.extra_args_call();

				if cx.result_used && rule.ret_type == "()" {
					compiler.span_warning(
						format!("Tried to bind result of rule `{}`, which returns () - perhaps you forgot a return type?", rule_name),
						e.span,
						Some("does not return a value".to_owned())
					);
				}

				if cx.result_used || rule.ret_type == "()" {
					quote!{ #func(__input, __state, __err_state, __pos #extra_args_call) }
				} else {
					quote!{
						match #func(__input, __state, __err_state, __pos #extra_args_call) {
							Matched(pos, _) => Matched(pos, ()),
							Failed => Failed,
						}
					}
				}
			} else {
				compiler.span_error(
					format!("No rule named `{}`", rule_name),
					e.span,
					Some("rule not found".to_owned())
				);
				quote!()
			}
		}

		TemplateInvoke(ref name, ref params) => {
			let template = match cx.grammar.templates.get(&name[..]) {
				Some(x) => x,
				None => {
					compiler.span_error(
						format!("No template named `{}`", name),
						e.span,
						Some("template not found".to_owned())
					);
					return quote!()
				}
			};

			if template.params.len() != params.len() {
				compiler.span_error(
					format!("Expected {} arguments to `{}`, found {}", template.params.len(), template.name, params.len()),
					e.span,
					Some("wrong number of arguments".to_owned())
				);
				return quote!()
			}

			let defs = template.params.iter().zip(params.iter())
				.map(|(name, expr)| (&name[..], (expr, cx.lexical))).collect();
			compile_expr(compiler, Context{ lexical: &LexicalContext { defs: defs }, ..cx }, &template.expr)
		}

		SequenceExpr(ref exprs) => {
			fn write_seq(compiler: &mut PegCompiler, cx: Context, exprs: &[Spanned<Expr>]) -> TokenStream {
				if exprs.len() == 1 {
					compile_expr(compiler, cx.result_used(false), &exprs[0])
				} else {
					let seq = write_seq(compiler, cx, &exprs[1..]);
					compile_match_and_then(compiler, cx, &exprs[0], None, seq)
				}
			}

			if exprs.is_empty() {
				quote!{ Matched(__pos, ()) }
			} else {
				write_seq(compiler, cx, &exprs)
			}
		}

		ChoiceExpr(ref exprs) => {
			fn write_choice(compiler: &mut PegCompiler, cx: Context, exprs: &[Spanned<Expr>]) -> TokenStream  {
				if exprs.len() == 1 {
					compile_expr(compiler, cx, &exprs[0])
				} else {
					let choice_res = compile_expr(compiler, cx, &exprs[0]);
					let next = write_choice(compiler, cx, &exprs[1..]);

					quote! {{
						let __choice_res = #choice_res;
						match __choice_res {
							Matched(__pos, __value) => Matched(__pos, __value),
							Failed => #next
						}
					}}
				}
			}

			if exprs.is_empty() {
				quote!{ Matched(__pos, ()) }
			} else {
				write_choice(compiler, cx, &exprs)
			}
		}

		OptionalExpr(ref e) => {
			let optional_res = compile_expr(compiler, cx, e);

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
			let inner = compile_expr(compiler, cx, e);

			let (min, max) = match *bounds {
				BoundedRepeat::None => (None, None),
				BoundedRepeat::Plus => (Some(quote!(1)), None),
				BoundedRepeat::Exact(ref code) => (Some(raw(code)), Some(raw(code))),
				BoundedRepeat::Both(ref min, ref max) => (min.as_ref().map(|x| raw(x)), max.as_ref().map(|x| raw(x)))
			};

			let match_sep = if let &Some(ref sep) = sep {
				let sep_inner = compile_expr(compiler, cx.result_used(false), &*sep);
				quote! {
					let __pos = if __repeat_value.is_empty() { __pos } else {
						let __sep_res = #sep_inner;
						match __sep_res {
							Matched(__newpos, _) => { __newpos },
							Failed => break,
						}
					};
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
			let assert_res = compile_expr(compiler, cx, e);
			quote! {{
				__err_state.suppress_fail += 1;
				let __assert_res = #assert_res;
				__err_state.suppress_fail -= 1;
				match __assert_res {
					Matched(_, __value) => Matched(__pos, __value),
					Failed => Failed,
				}
			}}
		}

		NegAssertExpr(ref e) => {
			let assert_res = compile_expr(compiler, cx.result_used(false), e);
			quote! {{
				__err_state.suppress_fail += 1;
				let __assert_res = #assert_res;
				__err_state.suppress_fail -= 1;
				match __assert_res {
					Failed => Matched(__pos, ()),
					Matched(..) => Failed,
				}
			}}
		}

		ActionExpr(ref exprs, ref code, is_cond) => {
			labeled_seq(compiler, cx, &exprs, {
				let code_block = raw(code);

				if is_cond {
					quote!{
						match #code_block {
							Ok(res) => Matched(__pos, res),
							Err(expected) => {
								__err_state.mark_failure(__pos, expected);
								Failed
							},
						}
					}
				} else {
					quote!{ Matched(__pos, #code_block) }
				}
			})
		}
		MatchStrExpr(ref expr) => {
			let inner = compile_expr(compiler, cx.result_used(false), expr);
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
			let inner = compile_expr(compiler, cx, expr);
			quote! {{
				__err_state.suppress_fail += 1;
				let res = #inner;
				__err_state.suppress_fail -= 1;
				res
			}}
		}
		FailExpr(ref expected) => {
			quote!{{ __err_state.mark_failure(__pos, #expected); Failed }}
		}

		InfixExpr{ ref atom, ref levels } => {
			let match_atom = compile_expr(compiler, cx, atom);
			let ty = if let RuleExpr(ref atom_rule_name) = atom.node {
				if let Some(rule) = cx.grammar.find_rule(atom_rule_name) {
					raw(&rule.ret_type[..])
				} else {
					// Error emitted by compile_expr
					return quote!();
				}
			} else {
				compiler.span_error(
					format!("#infix atom must be a rule, not an arbitrary expression (so its return type can be inspected)"),
					atom.span,
					Some("not allowed here".to_owned())
				);
				return quote!();
			};

			let mut pre_rules = Vec::new();
			let mut level_code = Vec::new();
			let extra_args_def = cx.grammar.extra_args_def();
			let extra_args_call = cx.grammar.extra_args_call();

			for (prec, level) in levels.iter().enumerate() {
				let prec = prec as i32;
				let new_prec = match level.assoc {
					InfixAssoc::Left => prec + 1,
					InfixAssoc::Right => prec
				};

				let mut post_rules = Vec::new();

				for op in &level.operators {
					if op.elements.len() < 2 {
						compiler.span_error(
							format!("#infix rule must contain at least two elements"),
							op.span,
							Some("incomplete rule".to_owned())
						);
						return quote!();
					}

					let left_arg = &op.elements[0];
					let l_arg = left_arg.name.as_ref().map(|n| raw(n)).unwrap_or_else(|| quote!(_));

					let right_arg = &op.elements[op.elements.len() - 1];
					let r_arg = right_arg.name.as_ref().map(|n| raw(n)).unwrap_or_else(|| quote!(_));

					let action = raw(&op.action[..]);

					match (&left_arg.expr.node, &right_arg.expr.node) {
						(&MarkerExpr, &MarkerExpr) => { //infix
							post_rules.push(
								labeled_seq(compiler, cx, &op.elements[1..op.elements.len()-1], {
									quote!{
										if let Matched(__pos, #r_arg) = __infix_parse(#new_prec, __input, __state, __err_state, __pos #extra_args_call) {
											let #l_arg = __infix_result;
											__infix_result = #action;
											Matched(__pos, ())
										} else { Failed }
									}
								})
							);
						}
						(&MarkerExpr, _) => { // postfix
							post_rules.push(
								labeled_seq(compiler, cx, &op.elements[1..op.elements.len()], {
									quote!{
										let #l_arg = __infix_result;
										__infix_result = #action;
										Matched(__pos, ())
									}
								})
							);
						}
						(_, &MarkerExpr) => { // prefix
							pre_rules.push(
								labeled_seq(compiler, cx, &op.elements[..op.elements.len()-1], {
									quote!{
										if let Matched(__pos, #r_arg) = __infix_parse(#new_prec, __input, __state, __err_state, __pos #extra_args_call) {
											Matched(__pos, {#action})
										} else { Failed }
									}
								})
							);
						}
						_ => {
							compiler.span_error(
								format!("#infix rule must be prefix, postfix, or infix"),
								op.span,
								Some("needs to begin and/or end with `@`".to_owned())
							);
							return quote!();
						}
					};
				}

				if !post_rules.is_empty() {
					level_code.push(quote!{
						if #prec >= __min_prec {
							#(
								if let Matched(__pos, ()) = #post_rules {
									__repeat_pos = __pos;
									continue;
								}
							)*
						}
					});
				}
			}

			let (enter, leave) = if cfg!(feature = "trace") {
				(quote!{println!("[PEG_TRACE] Entering level {}", __min_prec);},
				 quote!{println!("[PEG_TRACE] Leaving level {}", __min_prec);})
			} else {
				(quote!(), quote!())
			};

			quote!{{
				fn __infix_parse<'input>(__min_prec:i32, __input: &'input Input, __state: &mut ParseState<'input>, __err_state: &mut ::peg::error::ErrorState<Position<'input>>, __pos: Position<'input> #extra_args_def) -> ::peg::RuleResult<usize, #ty> {
					let __initial = (|| {
						#(
							if let Matched(__pos, __v) = #pre_rules {
								return Matched(__pos, __v);
							}
						)*

						#match_atom
					})();

					if let Matched(__pos, mut __infix_result) = __initial {
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
				__infix_parse(0, __input, __state, __err_state, __pos #extra_args_call)
			}}
		}
		MarkerExpr => {
			compiler.span_error(
				format!("`@` is only allowed in #infix"),
				e.span,
				Some("not allowed here".to_owned())
			);
			return quote!();
		}
	}
}

