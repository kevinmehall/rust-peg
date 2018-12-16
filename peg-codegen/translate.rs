use std::collections::HashMap;
use proc_macro2::{Ident, Span, TokenStream};
pub use self::Expr::*;
use crate::ast::*;

fn extra_args_def(grammar: &Grammar) -> TokenStream {
    let args: Vec<TokenStream> = grammar.args.iter().map(|&(ref name, ref tp)| {
        quote!(, #name: #tp)
    }).collect();
    quote!(#(#args)*)
}

fn extra_args_call(grammar: &Grammar) -> TokenStream {
    let args: Vec<TokenStream> = grammar.args.iter().map(|&(ref name, _)| {
        quote!(, #name)
    }).collect();
    quote!(#(#args)*)
}

struct Context<'a> {
    grammar: &'a Grammar,
    extra_args_call: TokenStream,
    extra_args_def: TokenStream,
}

struct LexicalContext<'a> {
	defs: HashMap<String, (&'a Expr, &'a LexicalContext<'a>)>
}

impl<'a> LexicalContext<'a> {
    fn new() -> LexicalContext<'a> { LexicalContext { defs: HashMap::new() }}
}

pub(crate) fn compile_grammar(grammar: &Grammar) -> TokenStream {
    let name = &grammar.name;
	let mut items = vec![make_parse_state(&grammar)];

    let context = &Context {
        grammar: grammar,
        extra_args_call: extra_args_call(grammar),
        extra_args_def: extra_args_def(grammar),
    };

	for item in &grammar.items {
        match item {
            Item::Use(tt) => items.push(tt.clone()),
            Item::Rule(rule) => {
                if rule.visibility.is_some() {
                    items.push(compile_rule_export(context, rule));
                }

                items.push(compile_rule(context, rule))
            }
            Item::Template(_) => {}
        }
		
	}

	let input_type = &grammar.input_type;

	quote! {
        mod #name {
            use ::peg::RuleResult::{Matched, Failed};

            type Input = #input_type;
            type Position<'input> = <Input as ::peg::Parse<'input>>::Position;
            type PositionRepr<'input> = <Input as ::peg::Parse<'input>>::PositionRepr;

            #(#items)*
        }
	}
}

fn make_parse_state(grammar: &Grammar) -> TokenStream {
	let mut cache_fields_def: Vec<TokenStream> = Vec::new();
	let mut cache_fields: Vec<Ident> = Vec::new();
	for rule in grammar.iter_rules() {
		if rule.cached {
			let name = Ident::new(&format!("{}_cache", rule.name), Span::call_site());
			let ret_ty = rule.ret_type.clone().unwrap_or_else(|| quote!(()));
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

fn compile_rule(context: &Context, rule: &Rule) -> TokenStream {
	let ref rule_name = rule.name;
	let name = Ident::new(&format!("__parse_{}", rule.name), Span::call_site());
	let ret_ty = rule.ret_type.clone().unwrap_or_else(|| quote!(()));
    let result_used =  rule.ret_type.is_some();

	let body = compile_expr(context, &rule.expr, &LexicalContext::new(), result_used);

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

	let extra_args_def = &context.extra_args_def;

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

		quote! {
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
		quote! {
			fn #name<'input>(__input: &'input Input, __state: &mut ParseState<'input>, __err_state: &mut ::peg::error::ErrorState<Position<'input>>, __pos: Position<'input> #extra_args_def) -> ::peg::RuleResult<usize, #ret_ty> {
				#![allow(non_snake_case, unused)]
				#wrapped_body
			}
		}
	}
}

fn compile_rule_export(context: &Context, rule: &Rule) -> TokenStream {
	let name = &rule.name;
	let ret_ty = rule.ret_type.clone().unwrap_or_else(|| quote!(()));
	let visibility = &rule.visibility;
	let parse_fn = Ident::new(&format!("__parse_{}", rule.name.to_string()), name.span());

	let extra_args_def = &context.extra_args_def;
	let extra_args_call = &context.extra_args_call;

	quote! {
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

fn compile_match_and_then(context: &Context, e: &Expr, lx: &LexicalContext, value_name: Option<&Ident>, then: TokenStream) -> TokenStream {
	let seq_res = compile_expr(context, e, lx, value_name.is_some());
	let name_pat = match value_name {
        Some(x) => quote!(#x),
        None => quote!(_)
    };

	quote! {{
		let __seq_res = #seq_res;
		match __seq_res {
			Matched(__pos, #name_pat) => { #then }
			Failed => Failed,
		}
	}}
}

fn labeled_seq(context: &Context, exprs: &[TaggedExpr], lx: &LexicalContext, inner: TokenStream) -> TokenStream {
	match exprs.first() {
		Some(ref first) => {
			let name = first.name.as_ref();
			let seq = labeled_seq(context, &exprs[1..], lx, inner);
			compile_match_and_then(context, &first.expr, lx, name, seq)
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

fn compile_expr(context: &Context, e: &Expr, lx: &LexicalContext, result_used: bool) -> TokenStream {
	match e {
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
            let pat_str = pattern.to_string();

			let (in_set, not_in_set) = cond_swap(invert, (
				quote!{ Matched(__next, ()) },
				quote!{ __err_state.mark_failure(__pos, #pat_str) },
			));

			let in_set_arm = quote!( #pattern => #in_set, );

			quote!{
				match ::peg::ParseElem::parse_elem(__input, __pos) {
					Matched(__next, __ch) => match __ch {
						#in_set_arm
						_ => #not_in_set,
					}
					Failed => __err_state.mark_failure(__pos, #pat_str)
				}					
			}
		}

		RuleExpr(ref rule_name) => {
			let rule_name_str = rule_name.to_string();
		    if let Some(&(template_arg, lexical_context)) = lx.defs.get(&rule_name_str[..]) {
				// Rule is actually an argument to an enclosing template
				return compile_expr(context, template_arg, lexical_context, result_used)
			}

			let func = Ident::new(&format!("__parse_{}", rule_name), rule_name.span());
			let extra_args_call = &context.extra_args_call;

			if result_used {
				quote!{ #func(__input, __state, __err_state, __pos #extra_args_call) }
			} else {
				quote!{
					match #func(__input, __state, __err_state, __pos #extra_args_call){
						Matched(pos, _) => Matched(pos, ()),
						Failed => Failed,
					}
				}
			}
		}

		MethodExpr(ref method, ref args) => {
			quote!{ __input.#method(__pos, #args) }
		}

		TemplateInvoke(ref name, ref params) => {
			let template_name = name.to_string();
			let template = match context.grammar.find_template(&template_name[..]) {
				Some(x) => x,
				None => {
					let msg = format!("No template named `{}`", name);
					return quote!(compile_error!(#msg));
				}
			};

			if template.params.len() != params.len() {
				let msg = format!("Expected {} arguments to `{}`, found {}", template.params.len(), template.name, params.len());
				return quote!(compile_error!(#msg));
			}

			let defs = template.params.iter().zip(params.iter())
				.map(|(name, expr)| (name.to_string(), (expr, lx))).collect();

			compile_expr(context, &template.expr, &LexicalContext { defs }, result_used)
		}

		ChoiceExpr(ref exprs) => {
			fn write_choice(context: &Context, exprs: &[Expr], lx: &LexicalContext, result_used: bool) -> TokenStream  {
				if exprs.len() == 1 {
					compile_expr(context, &exprs[0], lx, result_used)
				} else {
					let choice_res = compile_expr(context, &exprs[0], lx, result_used);
					let next = write_choice(context, &exprs[1..], lx, result_used);

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
				write_choice(context, &exprs, lx, result_used)
			}
		}

		OptionalExpr(ref e) => {
			let optional_res = compile_expr(context, e, lx, result_used);

			if result_used {
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
			let inner = compile_expr(context, e, lx, result_used);

			let (min, max) = match *bounds {
				BoundedRepeat::None => (None, None),
				BoundedRepeat::Plus => (Some(quote!(1)), None),
				BoundedRepeat::Exact(ref code) => (Some(code.clone()), Some(code.clone())),
				BoundedRepeat::Both(ref min, ref max) => (min.clone(), max.clone())
			};

			let match_sep = if let Some(sep) = sep {
				let sep_inner = compile_expr(context, sep, lx, false);
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

			let result = if result_used {
				quote!( __repeat_value )
			} else {
				quote!( () )
			};

			let (repeat_vec, repeat_step) =
			if result_used || min.is_some() || max.is_some() || sep.is_some() {
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
			let assert_res = compile_expr(context, e, lx, result_used);
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
			let assert_res = compile_expr(context, e, lx, false);
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
			labeled_seq(context, &exprs, lx, {
				if *is_cond {
					quote!{
						match { #code } {
							Ok(res) => Matched(__pos, res),
							Err(expected) => {
								__err_state.mark_failure(__pos, expected);
								Failed
							},
						}
					}
				} else {
					quote!{ Matched(__pos, { #code }) }
				}
			})
		}
		MatchStrExpr(ref expr) => {
			let inner = compile_expr(context, expr, lx, false);
			quote! {{
				let str_start = __pos;
				match #inner {
					Matched(__newpos, _) => { Matched(__newpos, ::peg::ParseSlice::parse_slice(__input, str_start, __newpos)) },
					Failed => Failed,
				}
			}}
		}
		PositionExpr => {
			quote! { Matched(__pos, __pos) }
		}
		QuietExpr(ref expr) => {
			let inner = compile_expr(context, expr, lx, result_used);
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
			let match_atom = compile_expr(context, atom, lx, result_used);
			let ty = if let RuleExpr(atom_rule_name) = &**atom {
                let atom_rule_name_str = atom_rule_name.to_string();
				context.grammar.iter_rules()
                    .find(|r| r.name == atom_rule_name_str)
                    .and_then(|r| r.ret_type.clone())
					.clone().unwrap_or_else(|| quote!(()))
			} else {
				return quote!(compile_error!("#infix atom must be a rule, not an arbitrary expression (so its return type can be inspected)"));
			};

			let mut pre_rules = Vec::new();
			let mut level_code = Vec::new();
			let extra_args_def = &context.extra_args_def;
			let extra_args_call = &context.extra_args_call;

			for (prec, level) in levels.iter().enumerate() {
				let prec = prec as i32;
				let new_prec = match level.assoc {
					InfixAssoc::Left => prec + 1,
					InfixAssoc::Right => prec
				};

				let mut post_rules = Vec::new();

				for op in &level.operators {
					if op.elements.len() < 2 {
						return quote!(compile_error!("incomplete rule".to_owned()));
					}

					let left_arg = &op.elements[0];
					let l_arg = left_arg.name.as_ref().map_or_else(|| quote!(_), |n| quote!(#n));

					let right_arg = &op.elements[op.elements.len() - 1];
					let r_arg = right_arg.name.as_ref().map_or_else(|| quote!(_), |n| quote!(#n));

					let action = &op.action;

					match (&left_arg.expr, &right_arg.expr) {
						(&MarkerExpr, &MarkerExpr) => { //infix
							post_rules.push(
								labeled_seq(context, &op.elements[1..op.elements.len()-1], lx, {
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
								labeled_seq(context, &op.elements[1..op.elements.len()], lx, {
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
								labeled_seq(context, &op.elements[..op.elements.len()-1], lx, {
									quote!{
										if let Matched(__pos, #r_arg) = __infix_parse(#new_prec, __input, __state, __err_state, __pos #extra_args_call) {
											Matched(__pos, {#action})
										} else { Failed }
									}
								})
							);
						}
						_ => {
							return quote!(compile_error!("#infix rule must be prefix, postfix, or infix"));
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
			return quote!(compile_error!("`@` is only allowed in #infix"));
		}
	}
}

