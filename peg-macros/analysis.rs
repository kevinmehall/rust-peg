use proc_macro2::Span;
use std::collections::HashMap;

use crate::ast::*;

pub struct GrammarAnalysis<'a> {
    pub rules: HashMap<String, &'a Rule>,
    pub left_recursion: Vec<LeftRecursionError>,
}

pub fn check<'a>(grammar: &'a Grammar) -> GrammarAnalysis<'a> {
    let mut rules = HashMap::new();

    // Pick only the first for duplicate rules (the duplicate is reported when translating the rule)
    for rule in grammar.iter_rules() {
        rules.entry(rule.name.to_string()).or_insert(rule);
    }

    let (_, left_recursion) = LeftRecursionVisitor::check(grammar, &rules);

    GrammarAnalysis {
        rules,
        left_recursion,
    }
}

/// Check for infinite loops in the form of left recursion.
///
/// If a PEG expression recurses without first consuming input, it will
/// recurse until the stack overflows.
struct LeftRecursionVisitor<'a> {
    stack: Vec<String>,
    rules: &'a HashMap<String, &'a Rule>,
    errors: Vec<LeftRecursionError>,
}

pub struct LeftRecursionError {
    pub span: Span,
    pub path: Vec<String>,
}

impl LeftRecursionError {
    pub fn msg(&self) -> String {
        format!(
            "left recursive rules create an infinite loop: {}",
            self.path.join(" -> ")
        )
    }
}

impl<'a> LeftRecursionVisitor<'a> {
    fn check(grammar: &'a Grammar, rules: &HashMap<String, &'a Rule>) -> (HashMap<String, bool>, Vec<LeftRecursionError>) {
        let mut visitor = LeftRecursionVisitor {
            rules,
            errors: Vec::new(),
            stack: Vec::new(),
        };

        let mut rule_nullability: HashMap<String, bool> = HashMap::new();

        for rule in grammar.iter_rules() {
            let nullable = visitor.walk_rule(rule);
            debug_assert!(visitor.stack.is_empty());
            rule_nullability.entry(rule.name.to_string()).or_insert(nullable);
        }

        (rule_nullability, visitor.errors)
    }

    fn walk_rule(&mut self, rule: &'a Rule) -> bool {
        self.stack.push(rule.name.to_string());
        let res = self.walk_expr(&rule.expr);
        self.stack.pop().unwrap();
        res
    }

    /// Walk the prefix of an expression that can be reached without consuming input.
    /// 
    /// Returns true if the rule is known to match completely without consuming any input.
    /// This is a conservative heuristic, if unknown, we return false to avoid reporting false-positives
    /// for left recursion.
    fn walk_expr(&mut self, this_expr: &Expr) -> bool {
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
                    self.errors.push(LeftRecursionError {
                        path: recursive_loop,
                        span: rule_ident.span(),
                    });
                    return false;
                }

                if let Some(rule) = self.rules.get(&name) {
                    self.walk_rule(rule)
                } else {
                    // Missing rule would have already been reported
                   false
                }
            }

            ActionExpr(ref elems, ..) => {
                for elem in elems {
                    if !self.walk_expr(&elem.expr) {
                        return false;
                    }
                }

                true
            }

            ChoiceExpr(ref choices) => {
                let mut nullable = false;
                for expr in choices {
                    nullable |= self.walk_expr(expr);
                }
                nullable
            }

            OptionalExpr(ref expr) | PosAssertExpr(ref expr) | NegAssertExpr(ref expr) => {
                self.walk_expr(expr);
                true
            }

            Repeat(ref expr, ref bounds, _) => {
                let inner_nullable = self.walk_expr(expr);
                inner_nullable | !bounds.has_lower_bound()
            }

            MatchStrExpr(ref expr) | QuietExpr(ref expr) => self.walk_expr(expr),

            PrecedenceExpr { ref levels } => {
                let mut nullable = false;

                for level in levels {
                    for operator in &level.operators {
                        let mut operator_nullable = true;
                        for element in &operator.elements {
                            if !self.walk_expr(&element.expr) {
                                operator_nullable = false;
                                break;
                            }
                        }
                        nullable |= operator_nullable;
                    }
                }

               nullable
            }

            LiteralExpr(_) | PatternExpr(_) | MethodExpr(_, _) | FailExpr(_) | MarkerExpr(_) => false,

            PositionExpr => true,
        }
    }
}
