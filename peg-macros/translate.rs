use proc_macro2::{Group, Ident, Literal, Span, TokenStream, TokenTree};
use std::collections::{HashMap, HashSet};

use quote::{format_ident, quote, quote_spanned};

pub use self::Expr::*;
use crate::analysis;
use crate::ast::*;

pub fn report_error(span: Span, msg: String) -> TokenStream {
    quote_spanned!(span=>compile_error!(#msg);)
}

pub fn report_error_expr(span: Span, msg: String) -> TokenStream {
    // panic!() to avoid "Mismatched types" error
    quote_spanned!(span=> { compile_error!(#msg); panic!() })
}

/// Test if the group begins with a specific marker character, and if so, return the remaining tokens.
fn group_check_prefix(group: &Group, prefix: char) -> Option<TokenStream> {
    let mut iter = group.stream().into_iter();
    match iter.next() {
        Some(TokenTree::Punct(p)) if p.as_char() == prefix => {
            Some(iter.collect())
        }
        _ => None
    }
}

fn extra_args_def(grammar: &Grammar) -> TokenStream {
    let args: Vec<TokenStream> = grammar
        .args
        .iter()
        .map(|&(ref name, ref tp)| quote!(, #name: #tp))
        .collect();
    quote!(#(#args)*)
}

fn extra_args_call(grammar: &Grammar) -> TokenStream {
    let args: Vec<TokenStream> = grammar
        .args
        .iter()
        .map(|&(ref name, _)| quote!(, #name))
        .collect();
    quote!(#(#args)*)
}

#[derive(Clone)]
struct Context<'a> {
    rules: &'a HashMap<String, &'a Rule>,
    rules_from_args: HashSet<String>,
    extra_ty_params: &'a Option<Vec<TokenStream>>,
    extra_args_call: TokenStream,
    extra_args_def: TokenStream,
}

pub(crate) fn compile_grammar(grammar: &Grammar) -> TokenStream {
    let name = &grammar.name;
    let mut items = vec![make_parse_state(&grammar)];

    let analysis = analysis::check(&grammar);

    let context = &Context {
        rules: &analysis.rules,
        rules_from_args: HashSet::new(),
        extra_ty_params: &grammar.ty_params,
        extra_args_call: extra_args_call(grammar),
        extra_args_def: extra_args_def(grammar),
    };

    let mut seen_rule_names = HashSet::new();

    for item in &grammar.items {
        match item {
            Item::Use(tt) => items.push(tt.clone()),
            Item::Rule(rule) => {
                if seen_rule_names.insert(rule.name.to_string()) {
                    if rule.cached && !(rule.params.is_empty() && rule.ty_params.is_none()) {
                        items.push(report_error(
                            rule.name.span(),
                            format!("rules with arguments cannot use #[cache]"),
                        ));
                    }

                    if rule.visibility.is_some() {
                        for param in &rule.params {
                            match &param.ty {
                                RuleParamTy::Rule(..) => items.push(report_error(
                                    param.name.span(),
                                    format!("parameters on `pub rule` must be Rust types"),
                                )),
                                _ => {}
                            }
                        }

                        items.push(compile_rule_export(context, rule));
                    } else if rule.no_eof {
                        items.push(report_error(
                            rule.name.span(),
                            format!("#[no_eof] is only meaningful for `pub rule`"),
                        ));
                    }

                    items.push(compile_rule(context, rule));
                } else {
                    items.push(report_error(
                        rule.name.span(),
                        format!("duplicate rule `{}`", rule.name),
                    ));
                }
            }
        }
    }

    let doc = &grammar.doc;
    let input_type = &grammar.input_type;
    let input_ty_params = ty_params_slice(&grammar.ty_params);
    let visibility = &grammar.visibility;

    let mut errors = Vec::new();

    for rec in &analysis.left_recursion {
        errors.push(report_error(rec.span, rec.msg()));
    }

    for rec in &analysis.loop_nullability {
        errors.push(report_error(rec.span, rec.msg()));
    }

    quote_spanned! { Span::mixed_site() =>
        #doc
        #visibility mod #name {
            #[allow(unused_imports)]
            use super::*;
            type Input<#(#input_ty_params),*> = #input_type;
            type PositionRepr<#(#input_ty_params),*> = <Input<#(#input_ty_params),*> as ::peg::Parse>::PositionRepr;

            #(#errors)*
            #(#items)*
        }
    }
}

fn make_parse_state(grammar: &Grammar) -> TokenStream {
    let span = Span::mixed_site();
    let mut cache_fields_def: Vec<TokenStream> = Vec::new();
    let mut cache_fields: Vec<Ident> = Vec::new();
    for rule in grammar.iter_rules() {
        if rule.cached {
            let name = format_ident!("{}_cache", rule.name);
            let ret_ty = rule.ret_type.clone().unwrap_or_else(|| quote!(()));
            cache_fields_def.push(
                quote_spanned! { span =>  #name: ::std::collections::HashMap<usize, ::peg::RuleResult<#ret_ty>> },
            );
            cache_fields.push(name);
        }
    }

    quote_spanned! { span =>
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

fn ty_params_slice(ty_params: &Option<Vec<TokenStream>>) -> &[TokenStream] {
    ty_params.as_ref().map(|x| &x[..]).unwrap_or(&[])
}

fn rule_params_list(context: &Context, rule: &Rule) -> Vec<TokenStream> {
    let extra_ty_params = ty_params_slice(context.extra_ty_params);
    let span = rule.span.resolved_at(Span::mixed_site());
    rule.params.iter().map(|param| {
        let name = &param.name;
        match &param.ty {
            RuleParamTy::Rust(ty) => quote_spanned!{ span => #name: #ty },
            RuleParamTy::Rule(ty) => quote_spanned!{ span =>
                #name: impl Fn(&'input Input<#(#extra_ty_params),*>, &mut ParseState<'input>, &mut ::peg::error::ErrorState, usize) -> ::peg::RuleResult<#ty>
            },
        }
    }).collect()
}

fn compile_rule(context: &Context, rule: &Rule) -> TokenStream {
    let span = rule.span.resolved_at(Span::mixed_site());
    let ref rule_name = rule.name;
    let name = format_ident!("__parse_{}", rule.name, span=span);
    let ret_ty = rule.ret_type.clone().unwrap_or_else(|| quote!(()));
    let result_used = rule.ret_type.is_some();
    let ty_params = ty_params_slice(&rule.ty_params);
    let extra_ty_params = ty_params_slice(context.extra_ty_params);

    let mut context = context.clone();
    context
        .rules_from_args
        .extend(rule.params.iter().map(|param| param.name.to_string()));

    let body = compile_expr(&context, &rule.expr, result_used);

    let wrapped_body = if cfg!(feature = "trace") {
        let str_rule_name = rule_name.to_string();
        quote_spanned! { span => {
            let loc = ::peg::Parse::position_repr(__input, __pos);
            println!("[PEG_TRACE] Attempting to match rule `{}` at {}", #str_rule_name, loc);
            let __peg_result: ::peg::RuleResult<#ret_ty> = {#body};
            match __peg_result {
                ::peg::RuleResult::Matched(epos, v) => {
                    let eloc = ::peg::Parse::position_repr(__input, epos);
                    println!("[PEG_TRACE] Matched rule `{}` at {} to {}", #str_rule_name, loc, eloc);
                    ::peg::RuleResult::Matched(epos, v)
                }
                ::peg::RuleResult::Failed => {
                    println!("[PEG_TRACE] Failed to match rule `{}` at {}", #str_rule_name, loc);
                    ::peg::RuleResult::Failed
                }
            }
        }}
    } else {
        body
    };

    let extra_args_def = &context.extra_args_def;

    let rule_params = rule_params_list(&context, rule);

    if rule.cached {
        let cache_field = format_ident!("{}_cache", rule.name);

        let cache_trace = if cfg!(feature = "trace") {
            let str_rule_name = rule.name.to_string();
            quote_spanned! { span =>
                let loc = ::peg::Parse::position_repr(__input, __pos);
                match &entry {
                    &::peg::RuleResult::Matched(..) => println!("[PEG_TRACE] Cached match of rule {} at {}", #str_rule_name, loc),
                    &Failed => println!("[PEG_TRACE] Cached fail of rule {} at {}", #str_rule_name, loc),
                };
            }
        } else {
            quote!()
        };

        quote_spanned! { span =>
            fn #name<'input #(, #extra_ty_params)* #(, #ty_params)*>(__input: &'input Input<#(#extra_ty_params),*>, __state: &mut ParseState<'input>, __err_state: &mut ::peg::error::ErrorState, __pos: usize #extra_args_def #(, #rule_params)*) -> ::peg::RuleResult<#ret_ty> {
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
        quote_spanned! { span =>
            fn #name<'input #(, #extra_ty_params)* #(, #ty_params)*>(__input: &'input Input<#(#extra_ty_params),*>, __state: &mut ParseState<'input>, __err_state: &mut ::peg::error::ErrorState, __pos: usize #extra_args_def #(, #rule_params)*) -> ::peg::RuleResult<#ret_ty> {
                #![allow(non_snake_case, unused)]
                #wrapped_body
            }
        }
    }
}

fn compile_rule_export(context: &Context, rule: &Rule) -> TokenStream {
    let span = rule.span.resolved_at(Span::mixed_site());
    let doc = &rule.doc;
    let name = &rule.name;
    let ret_ty = rule.ret_type.clone().unwrap_or_else(|| quote!(()));
    let visibility = &rule.visibility;
    let parse_fn = format_ident!("__parse_{}", rule.name.to_string(), span = name.span());
    let ty_params = ty_params_slice(&rule.ty_params);
    let extra_ty_params = ty_params_slice(context.extra_ty_params);
    let rule_params = rule_params_list(context, rule);
    let rule_params_call: Vec<TokenStream> = rule
        .params
        .iter()
        .map(|param| {
            let param_name = &param.name;
            quote!(#param_name)
        })
        .collect();

    let extra_args_def = &context.extra_args_def;
    let extra_args_call = &context.extra_args_call;
    let eof_check = if rule.no_eof {
        quote_spanned!{ span => true }
    } else {
        quote_spanned!{ span => ::peg::Parse::is_eof(__input, __pos) }
    };

    quote_spanned! { span =>
        #doc
        #visibility fn #name<'input #(, #extra_ty_params)* #(, #ty_params)*>(__input: &'input Input<#(#extra_ty_params),*> #extra_args_def #(, #rule_params)*) -> ::std::result::Result<#ret_ty, ::peg::error::ParseError<PositionRepr<#(#extra_ty_params),*>>> {
            #![allow(non_snake_case, unused)]

            let mut __err_state = ::peg::error::ErrorState::new(::peg::Parse::start(__input));
            let mut __state = ParseState::new();
            match #parse_fn(__input, &mut __state, &mut __err_state, ::peg::Parse::start(__input) #extra_args_call #(, #rule_params_call)*) {
                ::peg::RuleResult::Matched(__pos, __value) => {
                    if #eof_check {
                        return Ok(__value)
                    } else {
                        __err_state.mark_failure(__pos, "EOF");
                    }
                }
                _ => ()
            }

            __state = ParseState::new();
            __err_state.reparse_for_error();

            match #parse_fn(__input, &mut __state, &mut __err_state, ::peg::Parse::start(__input) #extra_args_call #(, #rule_params_call)*) {
                ::peg::RuleResult::Matched(__pos, __value) => {
                    if #eof_check {
                        panic!("Parser is nondeterministic: succeeded when reparsing for error position");
                    } else {
                        __err_state.mark_failure(__pos, "EOF");
                    }
                }
                _ => ()
            }

            Err(__err_state.into_parse_error(__input))
        }
    }
}

fn name_or_ignore(n: Option<&Ident>) -> TokenStream {
    match n {
        Some(n) => quote!(#n),
        None => quote!(_),
    }
}

fn ordered_choice(span: Span, mut rs: impl DoubleEndedIterator<Item = TokenStream>) -> TokenStream {
    rs.next_back().map(|last| rs.rfold(last, |fallback, preferred| {
        quote_spanned! { span => {
            let __choice_res = #preferred;
            match __choice_res {
                ::peg::RuleResult::Matched(__pos, __value) => ::peg::RuleResult::Matched(__pos, __value),
                ::peg::RuleResult::Failed => #fallback
            }
        }}
    })).expect("ordered choice must not be empty")
}

fn labeled_seq(context: &Context, exprs: &[TaggedExpr], inner: TokenStream) -> TokenStream {
    exprs.iter().rfold(inner, |then, expr| {
        compile_expr_continuation(context, &expr.expr, expr.name.as_ref(), then)
    })
}

fn compile_expr_continuation(context: &Context, e: &SpannedExpr, result_name: Option<&Ident>, continuation: TokenStream) -> TokenStream {
    let span = e.span.resolved_at(Span::mixed_site());

    let result_pat = name_or_ignore(result_name);
    match e.expr {
        LiteralExpr(ref s) => {
            compile_literal_expr(s, continuation)
        }

        PatternExpr(ref pattern) => {
            compile_pattern_expr(pattern, quote_spanned!{ span =>
                { let __pos = __next; let #result_pat = (); { #continuation } }
            })
        }

        _ => {
            let seq_res = compile_expr(context, e, result_name.is_some());
            quote_spanned! { span => {
                let __seq_res = #seq_res;
                match __seq_res {
                    ::peg::RuleResult::Matched(__pos, #result_pat) => { #continuation }
                    ::peg::RuleResult::Failed => ::peg::RuleResult::Failed,
                }
            }}
        }
    }
}

fn compile_literal_expr(s: &Literal, continuation: TokenStream) -> TokenStream {
    let span = s.span().resolved_at(Span::mixed_site());
    let escaped_str = s.to_string();
    quote_spanned! { span => 
            match ::peg::ParseLiteral::parse_string_literal(__input, __pos, #s) {
            ::peg::RuleResult::Matched(__pos, __val) => { #continuation }
            ::peg::RuleResult::Failed => { __err_state.mark_failure(__pos, #escaped_str); ::peg::RuleResult::Failed }
        }
    }
}

fn compile_pattern_expr(pattern_group: &Group, success_res: TokenStream) -> TokenStream {
    let span = pattern_group.span().resolved_at(Span::mixed_site());
    let pat_str = pattern_group.to_string();
    let failure_res = quote_spanned! { span => { __err_state.mark_failure(__pos, #pat_str); ::peg::RuleResult::Failed } };

    let (pattern, in_set, not_in_set) = if let Some(pattern) = group_check_prefix(pattern_group, '^') {
        (pattern, failure_res, success_res)
    } else {
        (pattern_group.stream(), success_res, failure_res)
    };

    quote_spanned! { span =>
        match ::peg::ParseElem::parse_elem(__input, __pos) {
            ::peg::RuleResult::Matched(__next, __ch) => match __ch {
                #pattern => #in_set,
                _ => #not_in_set,
            }
            ::peg::RuleResult::Failed => { __err_state.mark_failure(__pos, #pat_str); ::peg::RuleResult::Failed }
        }
    }
}

fn compile_expr(context: &Context, e: &SpannedExpr, result_used: bool) -> TokenStream {
    let span = e.span.resolved_at(Span::mixed_site());

    match e.expr {
        LiteralExpr(ref s) => {
            compile_literal_expr(s, quote_spanned! { span =>
                 ::peg::RuleResult::Matched(__pos, __val)
            })
        }

        PatternExpr(ref pattern_group) => {
            compile_pattern_expr(pattern_group, quote_spanned! { span =>
                ::peg::RuleResult::Matched(__next, ())
            })
        }

        RuleExpr(ref rule_name, ref rule_args)
            if context.rules_from_args.contains(&rule_name.to_string()) =>
        {
            if !rule_args.is_empty() {
                return report_error_expr(
                    rule_name.span(),
                    format!("rule closure does not accept arguments"),
                );
            }

            quote_spanned!{ span=> #rule_name(__input, __state, __err_state, __pos) }
        }

        RuleExpr(ref rule_name, ref rule_args) => {
            let rule_name_str = rule_name.to_string();

            let rule_def = if let Some(rule_def) = context.rules.get(&rule_name_str) {
                rule_def
            } else {
                return report_error_expr(
                    rule_name.span(),
                    format!("undefined rule `{}`", rule_name_str),
                );
            };

            if result_used && rule_def.ret_type.is_none() {
                let msg = format!(
                    "using result of rule `{}`, which does not return a value",
                    rule_name_str
                );
                return report_error_expr(rule_name.span(), msg);
            }

            if rule_def.params.len() != rule_args.len() {
                return report_error_expr(
                    rule_name.span(),
                    format!(
                        "this rule takes {} parameters but {} parameters were supplied",
                        rule_def.params.len(),
                        rule_args.len()
                    ),
                );
            }

            let func = format_ident!("__parse_{}", rule_name, span = rule_name.span());
            let extra_args_call = &context.extra_args_call;

            let rule_args_call: Vec<TokenStream> = rule_args
                .iter()
                .map(|arg| match arg {
                    RuleArg::Peg(e) => {
                        let expr = compile_expr(context, e, true);
                        quote_spanned! { span=> |__input, __state, __err_state, __pos| { #expr } }
                    }
                    RuleArg::Rust(e) => e.clone(),
                })
                .collect();

            if result_used {
                quote_spanned!{ span=> #func(__input, __state, __err_state, __pos #extra_args_call #(, #rule_args_call)*) }
            } else {
                quote_spanned!{ span=>
                    match #func(__input, __state, __err_state, __pos #extra_args_call #(, #rule_args_call)*){
                        ::peg::RuleResult::Matched(pos, _) => ::peg::RuleResult::Matched(pos, ()),
                        ::peg::RuleResult::Failed => ::peg::RuleResult::Failed,
                    }
                }
            }
        }

        MethodExpr(ref method, ref args) => {
            quote_spanned!{ span=> __input.#method(__pos, #args) }
        }

        ChoiceExpr(ref exprs) => ordered_choice(span,
            exprs
                .iter()
                .map(|expr| compile_expr(context, expr, result_used)),
        ),

        OptionalExpr(ref e) => {
            let optional_res = compile_expr(context, e, result_used);

            if result_used {
                quote_spanned!{ span=>
                    match #optional_res {
                        ::peg::RuleResult::Matched(__newpos, __value) => { ::peg::RuleResult::Matched(__newpos, Some(__value)) },
                        ::peg::RuleResult::Failed => { ::peg::RuleResult::Matched(__pos, None) },
                    }
                }
            } else {
                quote_spanned!{ span=>
                    match #optional_res {
                        ::peg::RuleResult::Matched(__newpos, _) => { ::peg::RuleResult::Matched(__newpos, ()) },
                        ::peg::RuleResult::Failed => { ::peg::RuleResult::Matched(__pos, ()) },
                    }
                }
            }
        }

        Repeat{ ref inner, ref bound, ref sep} => {
            let inner = compile_expr(context, inner, result_used);

            let (min, max) = match bound {
                BoundedRepeat::None => (None, None),
                BoundedRepeat::Plus => (Some(quote!(1)), None),
                BoundedRepeat::Exact(ref code) => (Some(code.clone()), Some(code.clone())),
                BoundedRepeat::Both(ref min, ref max) => (min.clone(), max.clone()),
            };

            let match_sep = if let Some(sep) = sep {
                let sep_inner = compile_expr(context, sep, false);
                quote_spanned!{ span=>
                    let __pos = if __repeat_value.is_empty() { __pos } else {
                        let __sep_res = #sep_inner;
                        match __sep_res {
                            ::peg::RuleResult::Matched(__newpos, _) => { __newpos },
                            ::peg::RuleResult::Failed => break,
                        }
                    };
                }
            } else {
                quote!()
            };

            let result = if result_used {
                quote_spanned!{ span=> __repeat_value }
            } else {
                quote!(())
            };

            let (repeat_vec, repeat_step) =
                if result_used || min.is_some() || max.is_some() || sep.is_some() {
                    (
                        Some(quote_spanned! { span => let mut __repeat_value = vec!(); }),
                        Some(quote_spanned! { span => __repeat_value.push(__value); }),
                    )
                } else {
                    (None, None)
                };

            let max_check = max.map(|max| {
                quote_spanned!{ span=> if __repeat_value.len() >= #max { break } }
            });

            let result_check = if let Some(min) = min {
                quote_spanned!{ span=>
                    if __repeat_value.len() >= #min {
                        ::peg::RuleResult::Matched(__repeat_pos, #result)
                    } else {
                        ::peg::RuleResult::Failed
                    }
                }
            } else {
                quote_spanned!{ span=> ::peg::RuleResult::Matched(__repeat_pos, #result) }
            };

            quote_spanned!{ span=> {
                let mut __repeat_pos = __pos;
                #repeat_vec

                loop {
                    let __pos = __repeat_pos;

                    #match_sep
                    #max_check

                    let __step_res = #inner;
                    match __step_res {
                        ::peg::RuleResult::Matched(__newpos, __value) => {
                            __repeat_pos = __newpos;
                            #repeat_step
                        },
                        ::peg::RuleResult::Failed => {
                            break;
                        }
                    }
                }

                #result_check
            }}
        }

        PosAssertExpr(ref e) => {
            let assert_res = compile_expr(context, e, result_used);
            quote_spanned!{ span=> {
                __err_state.suppress_fail += 1;
                let __assert_res = #assert_res;
                __err_state.suppress_fail -= 1;
                match __assert_res {
                    ::peg::RuleResult::Matched(_, __value) => ::peg::RuleResult::Matched(__pos, __value),
                    ::peg::RuleResult::Failed => ::peg::RuleResult::Failed,
                }
            }}
        }

        NegAssertExpr(ref e) => {
            let assert_res = compile_expr(context, e, false);
            quote_spanned!{ span=> {
                __err_state.suppress_fail += 1;
                let __assert_res = #assert_res;
                __err_state.suppress_fail -= 1;
                match __assert_res {
                    ::peg::RuleResult::Failed => ::peg::RuleResult::Matched(__pos, ()),
                    ::peg::RuleResult::Matched(..) => ::peg::RuleResult::Failed,
                }
            }}
        }

        ActionExpr(ref exprs, ref code) => labeled_seq(context, &exprs, {
            if let Some(code) = code {
                let code_span = code.span().resolved_at(Span::mixed_site());

                // Peek and see if the first token in the block is '?'. If so, it's a conditional block
                if let Some(body) = group_check_prefix(&code, '?') {
                    quote_spanned!{code_span =>
                        match (||{ #body })() {
                            Ok(res) => ::peg::RuleResult::Matched(__pos, res),
                            Err(expected) => {
                                __err_state.mark_failure(__pos, expected);
                                ::peg::RuleResult::Failed
                            },
                        }
                    }
                } else {
                    quote_spanned!{code_span => ::peg::RuleResult::Matched(__pos, (||#code)()) }
                }
            } else {
                quote_spanned!{ span => ::peg::RuleResult::Matched(__pos, ()) }
            }
        }),
        MatchStrExpr(ref expr) => {
            let inner = compile_expr(context, expr, false);
            quote_spanned! { span => {
                let str_start = __pos;
                match #inner {
                    ::peg::RuleResult::Matched(__newpos, _) => { ::peg::RuleResult::Matched(__newpos, ::peg::ParseSlice::parse_slice(__input, str_start, __newpos)) },
                    ::peg::RuleResult::Failed => ::peg::RuleResult::Failed,
                }
            }}
        }
        PositionExpr => {
            quote_spanned! { span => ::peg::RuleResult::Matched(__pos, __pos) }
        }
        QuietExpr(ref expr) => {
            let inner = compile_expr(context, expr, result_used);
            quote_spanned! { span => {
                __err_state.suppress_fail += 1;
                let res = #inner;
                __err_state.suppress_fail -= 1;
                res
            }}
        }
        FailExpr(ref expected) => {
            quote_spanned! { span => { __err_state.mark_failure(__pos, #expected); ::peg::RuleResult::Failed }}
        }

        PrecedenceExpr { ref levels } => {
            let mut pre_rules = Vec::new();
            let mut level_code = Vec::new();
            let mut span_capture: Option<(TokenStream, TokenStream, TokenStream, &Group)> = None;

            for (prec, level) in levels.iter().enumerate() {
                let prec = prec as i32;

                let mut post_rules = Vec::new();

                for op in &level.operators {
                    let op_span = op.span.resolved_at(Span::mixed_site());

                    if op.elements.len() < 1 {
                        return report_error(op_span, format!("incomplete rule"));
                    }

                    let left_arg = &op.elements[0];
                    let l_arg = name_or_ignore(left_arg.name.as_ref());

                    let right_arg = &op.elements[op.elements.len() - 1];
                    let r_arg = name_or_ignore(right_arg.name.as_ref());

                    let action = &op.action;
                    let action = quote_spanned!(op.action.span()=>(||#action)());

                    let action = if let Some((lpos_name, val_name, rpos_name, wrap_action)) =
                        &span_capture
                    {
                        let wrap_action_span = wrap_action.span().resolved_at(Span::mixed_site());
                        quote_spanned!(wrap_action_span => (|#lpos_name, #val_name, #rpos_name|#wrap_action)(__lpos, #action, __pos))
                    } else {
                        action
                    };

                    match (&left_arg.expr.expr, &right_arg.expr.expr) {
                        (&PositionExpr, &PositionExpr) if op.elements.len() == 3 => {
                            // wrapper rule to capture expression span
                            match &op.elements[1].expr.expr {
                                &MarkerExpr(..) => (),
                                _ => {
                                    return report_error(op_span, format!("span capture rule must be `l:position!() n:@ r:position!()"));
                                }
                            }

                            span_capture = Some((
                                name_or_ignore(op.elements[0].name.as_ref()),
                                name_or_ignore(op.elements[1].name.as_ref()),
                                name_or_ignore(op.elements[2].name.as_ref()),
                                &op.action,
                            ));
                        }
                        (&MarkerExpr(la), &MarkerExpr(ra)) if op.elements.len() >= 3 => {
                            //infix
                            let new_prec = match (la, ra) {
                                (true, false) => prec + 1, // left associative
                                (false, true) => prec,     // right associative
                                _ => return report_error(op_span, format!("precedence rules must use `@` and `(@)` to indicate associativity"))
                            };

                            post_rules.push(
                                labeled_seq(context, &op.elements[1..op.elements.len()-1], {
                                    quote_spanned!{ op_span =>
                                        if let ::peg::RuleResult::Matched(__pos, #r_arg) = __recurse(__pos, #new_prec, __state, __err_state) {
                                            let #l_arg = __infix_result;
                                            __infix_result = #action;
                                            ::peg::RuleResult::Matched(__pos, ())
                                        } else { ::peg::RuleResult::Failed }
                                    }
                                })
                            );
                        }
                        (&MarkerExpr(_), _) if op.elements.len() >= 2 => {
                            // postfix
                            post_rules.push(labeled_seq(
                                context,
                                &op.elements[1..op.elements.len()],
                                {
                                    quote_spanned! { op_span =>
                                        let #l_arg = __infix_result;
                                        __infix_result = #action;
                                        ::peg::RuleResult::Matched(__pos, ())
                                    }
                                },
                            ));
                        }
                        (_, &MarkerExpr(a)) if op.elements.len() >= 2 => {
                            // prefix
                            let new_prec = match a {
                                true => prec,
                                false => prec + 1,
                            };
                            pre_rules.push(
                                labeled_seq(context, &op.elements[..op.elements.len()-1], {
                                    quote_spanned!{ op_span =>
                                        if let ::peg::RuleResult::Matched(__pos, #r_arg) = __recurse(__pos, #new_prec, __state, __err_state) {
                                            ::peg::RuleResult::Matched(__pos, #action)
                                        } else { ::peg::RuleResult::Failed }
                                    }
                                })
                            );
                        }
                        _ => {
                            // atom
                            pre_rules.push(labeled_seq(context, &op.elements, {
                                quote_spanned! { op_span => ::peg::RuleResult::Matched(__pos, #action) }
                            }));
                        }
                    };
                }

                if !post_rules.is_empty() {
                    level_code.push(quote_spanned! { span =>
                        if #prec >= __min_prec {
                            #(
                                if let ::peg::RuleResult::Matched(__pos, ()) = #post_rules {
                                    return (__infix_result, ::peg::RuleResult::Matched(__pos, ()));
                                }
                            )*
                        }
                    });
                }
            }

            let (enter, leave) = if cfg!(feature = "trace") {
                (
                    quote! {println!("[PEG_TRACE] Entering level {}", min_prec);},
                    quote! {println!("[PEG_TRACE] Leaving level {}", min_prec);},
                )
            } else {
                (quote!(), quote!())
            };

            // The closures below must be defined within the function call to which they are passed
            // due to https://github.com/rust-lang/rust/issues/41078

            quote_spanned! { span => {
                fn __infix_parse<T, S>(
                    state: &mut S,
                    err_state: &mut ::peg::error::ErrorState,
                    min_prec: i32,
                    lpos: usize,
                    prefix_atom: &Fn(usize, &mut S, &mut ::peg::error::ErrorState, &Fn(usize, i32, &mut S, &mut ::peg::error::ErrorState) -> ::peg::RuleResult<T>) -> ::peg::RuleResult<T>,
                    level_code: &Fn(usize, usize, i32, T, &mut S, &mut ::peg::error::ErrorState, &Fn(usize, i32, &mut S, &mut ::peg::error::ErrorState) -> ::peg::RuleResult<T>) -> (T, ::peg::RuleResult<()>),
                ) -> ::peg::RuleResult<T> {
                    let initial = {
                        prefix_atom(lpos, state, err_state, &|pos, min_prec, state, err_state| {
                            __infix_parse(state, err_state, min_prec, pos, prefix_atom, level_code)
                        })
                    };

                    if let ::peg::RuleResult::Matched(pos, mut infix_result) = initial {
                        #enter
                        let mut repeat_pos = pos;
                        loop {
                            let (val, res) = level_code(
                                repeat_pos,
                                lpos,
                                min_prec,
                                infix_result,
                                state,
                                err_state,
                                &|pos, min_prec, state, err_state| {
                                    __infix_parse(state, err_state, min_prec, pos, prefix_atom, level_code)
                                }
                            );
                            infix_result = val;

                            if let ::peg::RuleResult::Matched(pos, ()) = res {
                                repeat_pos = pos;
                                continue;
                            }

                            break;
                        }
                        #leave
                        ::peg::RuleResult::Matched(repeat_pos, infix_result)
                    } else {
                        ::peg::RuleResult::Failed
                    }
                }

                __infix_parse(__state, __err_state, 0, __pos,
                    &|__pos, __state, __err_state, __recurse| {
                        let __lpos = __pos;
                        #(
                            if let ::peg::RuleResult::Matched(__pos, __v) = #pre_rules {
                                return ::peg::RuleResult::Matched(__pos, __v);
                            }
                        )*

                        ::peg::RuleResult::Failed
                    },
                    &|__pos, __lpos, __min_prec, mut __infix_result, __state, __err_state, __recurse| {
                        #(#level_code)*
                        (__infix_result, ::peg::RuleResult::Failed)
                    }
                )
            }}
        }
        MarkerExpr { .. } => {
            return report_error(span, format!("`@` is only allowed in `precedence!{{}}`"));
        }
    }
}
