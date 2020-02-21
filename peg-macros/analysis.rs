use proc_macro2::Span;
use std::collections::HashMap;

use crate::ast::*;

pub struct GrammarAnalysis<'a> {
    pub rules: HashMap<String, &'a Rule>,
    pub left_recursion: Vec<RecursionError>,
}

pub fn check<'a>(grammar: &'a Grammar) -> GrammarAnalysis<'a> {
    let mut rules = HashMap::new();

    for rule in grammar.iter_rules() {
        rules.entry(rule.name.to_string()).or_insert(rule);
    }

    let left_recursion = RecursionVisitor::check(grammar, &rules);

    GrammarAnalysis {
        rules,
        left_recursion,
    }
}

struct RecursionVisitor<'a> {
    stack: Vec<String>,
    rules: &'a HashMap<String, &'a Rule>,
    errors: Vec<RecursionError>,
}

pub struct RecursionError {
    pub span: Span,
    pub path: Vec<String>,
}

impl RecursionError {
    pub fn msg(&self) -> String {
        format!(
            "left recursive rules create an infinite loop: {}",
            self.path.join(" -> ")
        )
    }
}

#[derive(Clone)]
struct RuleInfo {
    /// True if the rule is known to match without consuming any input.
    /// This is a conservative heuristic, if unknown, we return false to avoid reporting false-positives
    /// for left recursion.
    nullable: bool,
}

impl<'a> RecursionVisitor<'a> {
    fn check(grammar: &'a Grammar, rules: &HashMap<String, &'a Rule>) -> Vec<RecursionError> {
        let mut visitor = RecursionVisitor {
            rules,
            errors: Vec::new(),
            stack: Vec::new(),
        };

        for rule in grammar.iter_rules() {
            visitor.walk_rule(rule);
            debug_assert!(visitor.stack.is_empty());
        }

        visitor.errors
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
            RuleExpr(ref rule_ident, _) => {
                let name = rule_ident.to_string();

                if let Some(loop_start) = self
                    .stack
                    .iter()
                    .position(|caller_name| caller_name == &name)
                {
                    let mut recursive_loop = self.stack[loop_start..].to_vec();
                    recursive_loop.push(name);
                    self.errors.push(RecursionError {
                        path: recursive_loop,
                        span: rule_ident.span(),
                    });
                    return RuleInfo { nullable: false };
                }

                if let Some(rule) = self.rules.get(&name) {
                    self.walk_rule(rule)
                } else {
                    // Missing rule would have already been reported
                    RuleInfo { nullable: false }
                }
            }
            ActionExpr(ref elems, ..) => {
                for elem in elems {
                    if !self.walk_expr(&elem.expr).nullable {
                        return RuleInfo { nullable: false };
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

            OptionalExpr(ref expr) | PosAssertExpr(ref expr) | NegAssertExpr(ref expr) => {
                self.walk_expr(expr);
                RuleInfo { nullable: true }
            }

            Repeat(ref expr, ref bounds, _) => {
                let nullable = match bounds {
                    BoundedRepeat::None => true,
                    _ => false,
                };

                let res = self.walk_expr(expr);
                RuleInfo {
                    nullable: res.nullable | nullable,
                }
            }

            MatchStrExpr(ref expr) | QuietExpr(ref expr) => self.walk_expr(expr),

            PrecedenceExpr { .. } => RuleInfo { nullable: false },

            LiteralExpr(_) | PatternExpr(_) | MethodExpr(_, _) | FailExpr(_) | MarkerExpr(_) => {
                RuleInfo { nullable: false }
            }
            PositionExpr => RuleInfo { nullable: true },
        }
    }
}
