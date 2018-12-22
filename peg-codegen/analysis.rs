use std::collections::HashMap;
use proc_macro2::{ Span, TokenStream };

use crate::ast::*;

#[derive(Debug)]
pub enum Diagnostic {
    DuplicateRule(String, Vec<Span>),
    UndefinedRule(String, Span),
    UsedUnitResult(String, Span),
    LeftRecursion(Vec<String>, Span),
}

impl Diagnostic {
    pub fn primary_span(&self) -> &Span {
        use self::Diagnostic::*;
        match self {
            DuplicateRule(_, spans) => spans.first().unwrap(),
            UndefinedRule(_, span) => span,
            UsedUnitResult(_, span) => span,
            LeftRecursion(_, span) => span,
        }
    }

    pub fn msg(&self) -> String {
        use self::Diagnostic::*;
        match self {
            DuplicateRule(name, _) => format!("duplicate rule `{}`", name),
            UndefinedRule(name, _) => format!("undefined rule `{}`", name),
            UsedUnitResult(name, _) => format!("using result of rule `{}`, which does not return a value", name),
            LeftRecursion(path, _) => format!("left recursive rules create an infinite loop: {}", path.join(" -> ")),
        }
    }

    pub fn to_compile_error(&self) -> TokenStream {
        let span = self.primary_span();
        let msg = self.msg();
        quote_spanned!(*span=> compile_error!(#msg);)
    }
}

pub fn check(grammar: &Grammar, emit_error: &mut FnMut(Diagnostic)) {
    let mut rules = HashMap::new();

    for rule in grammar.iter_rules() {
        if let Some(prev) = rules.insert(rule.name.to_string(), rule) {
            emit_error(Diagnostic::DuplicateRule(rule.name.to_string(), vec![prev.name.span(), rule.name.span()]))
        }
    }

   ExpressionVisitor::check(grammar, &rules, emit_error);
   RecursionVisitor::check(grammar, &rules, emit_error);
}

struct ExpressionVisitor<'a> {
    emit_error: &'a mut FnMut(Diagnostic),
    rules: &'a HashMap<String, &'a Rule>,
}

impl<'a> ExpressionVisitor<'a> {
    fn check(grammar: &'a Grammar, rules: &HashMap<String, &'a Rule>, emit_error: &'a mut FnMut(Diagnostic)) {
        let mut visitor = ExpressionVisitor { rules, emit_error };

        for rule in grammar.iter_rules() {
            visitor.walk_expr(&rule.expr, rule.ret_type.is_some());
        }
    }

    fn walk_expr(&mut self, this_expr: &Expr, result_used: bool) {
        use self::Expr::*;
        match *this_expr {
            RuleExpr(ref name_ident) => {
                let name = name_ident.to_string();

                if let Some(rule_def) = self.rules.get(&name) {
                    if result_used && rule_def.ret_type.is_none() {
                        (self.emit_error)(Diagnostic::UsedUnitResult(name, name_ident.span()));
                    }
                } else {
                    (self.emit_error)(Diagnostic::UndefinedRule(name, name_ident.span()));
                }
            }
            ActionExpr(ref elems, ..) => {
                for elem in elems {
                    self.walk_expr(&elem.expr, elem.name.is_some());
                }
            }
            TemplateInvoke(..) => {
                // unimplemented
            }
            ChoiceExpr(ref choices) => {
                for expr in choices {
                    self.walk_expr(expr, result_used);
                }
            }

            OptionalExpr(ref expr) | 
            Repeat(ref expr, _, _) |
            QuietExpr(ref expr) |
            PosAssertExpr(ref expr) => self.walk_expr(expr, result_used),

            MatchStrExpr(ref expr) | NegAssertExpr(ref expr) => self.walk_expr(expr, false),
            InfixExpr{ ref atom, .. } => self.walk_expr(atom, true),

            AnyCharExpr |
            LiteralExpr(_) |
            PatternExpr(_) |
            MethodExpr(_, _) |
            FailExpr(_) |
            MarkerExpr |
            PositionExpr => { }
        }
    }
}

struct RecursionVisitor<'a> {
    stack: Vec<String>,
    emit_error: &'a mut FnMut(Diagnostic),
    rules: &'a HashMap<String, &'a Rule>,
}

#[derive(Clone)]
struct RuleInfo {
    /// True if the rule is known to match without consuming any input.
    /// This is a conservative heuristic, if unknown, we return false to avoid reporting false-positives
    /// for left recursion.
    nullable: bool,
}


impl<'a> RecursionVisitor<'a> {
    fn check(grammar: &'a Grammar, rules: &HashMap<String, &'a Rule>, emit_error: &'a mut FnMut(Diagnostic)) {
        let mut visitor = RecursionVisitor {
            rules, emit_error,
            stack: Vec::new(),
        };

        for rule in grammar.iter_rules() {
            visitor.walk_rule(rule);
            debug_assert!(visitor.stack.is_empty());
        }
    }

    fn walk_rule(&mut self, rule: &'a Rule) -> RuleInfo {
        self.stack.push(rule.name.to_string());
        let res = self.walk_expr(&rule.expr);
        self.stack.pop().unwrap();
        res
    }

    fn walk_expr(&mut self, this_expr: &Expr) -> RuleInfo {
        use self::Expr::*;
        match *this_expr {
            RuleExpr(ref rule_ident) => {
                let name = rule_ident.to_string();

                if let Some(loop_start) = self.stack.iter().position(|caller_name| { caller_name == &name}) {
                    let mut recursive_loop = self.stack[loop_start..].to_vec();
                    recursive_loop.push(name);
                    (self.emit_error)(Diagnostic::LeftRecursion(recursive_loop, rule_ident.span()));
                    return RuleInfo { nullable: false };
                }

                self.walk_rule(self.rules.get(&name).expect("missing rule"))
            }
            TemplateInvoke(..) => {
                // unimplemented
                RuleInfo { nullable: false }
            }
            ActionExpr(ref elems, ..) => {
                for elem in elems {
                    if !self.walk_expr(&elem.expr).nullable {
                        return RuleInfo { nullable: false }
                    }
                }
                
                RuleInfo { nullable: true }
            }
            ChoiceExpr(ref choices) => {
                let mut nullable = false;

                for expr in choices {
                    nullable |= self.walk_expr(expr).nullable;
                }

                RuleInfo { nullable }
            }

            OptionalExpr(ref expr) |
            PosAssertExpr(ref expr) |
            NegAssertExpr(ref expr) => {
                self.walk_expr(expr);
                RuleInfo { nullable: true }
            }

            Repeat(ref expr, ref bounds, _) =>  {
                let nullable = match bounds {
                    BoundedRepeat::None => true,
                    _ => false,
                };

                let res = self.walk_expr(expr);
                RuleInfo { nullable: res.nullable | nullable }
            }

            MatchStrExpr(ref expr) |
            QuietExpr(ref expr) => self.walk_expr(expr),

            InfixExpr{ ref atom, .. } => self.walk_expr(atom),

            | AnyCharExpr
            | LiteralExpr(_)
            | PatternExpr(_)
            | MethodExpr(_, _)
            | FailExpr(_)
            | MarkerExpr => { RuleInfo { nullable: false } }
            PositionExpr => { RuleInfo { nullable: true} }
        }
    }
}
