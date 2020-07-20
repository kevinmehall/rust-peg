use proc_macro2::{Ident, Literal, TokenStream};

#[derive(Debug)]
pub struct Grammar {
    pub doc: Option<TokenStream>,
    pub visibility: Option<TokenStream>,
    pub name: Ident,
    pub args: Vec<(Ident, TokenStream)>,
    pub items: Vec<Item>,
    pub input_type: TokenStream,
}

impl Grammar {
    pub fn iter_rules(&self) -> impl Iterator<Item = &Rule> {
        self.items.iter().filter_map(|item| match item {
            Item::Rule(r) => Some(r),
            _ => None,
        })
    }
}

#[derive(Debug)]
pub enum Item {
    Use(TokenStream),
    Rule(Rule),
}

#[derive(Debug)]
pub struct Rule {
    pub name: Ident,
    pub ty_params: Option<Vec<TokenStream>>,
    pub params: Vec<RuleParam>,
    pub expr: Expr,
    pub ret_type: Option<TokenStream>,
    pub doc: Option<TokenStream>,
    pub visibility: Option<TokenStream>,
    pub cached: bool,
}

#[derive(Debug)]
pub struct RuleParam {
    pub name: Ident,
    pub ty: RuleParamTy,
}

#[derive(Debug)]
pub enum RuleParamTy {
    Rust(TokenStream),
    Rule(TokenStream),
}

#[derive(Debug, Clone)]
pub struct TaggedExpr {
    pub name: Option<Ident>,
    pub expr: Expr,
}

#[derive(Debug, Clone)]
pub enum Expr {
    LiteralExpr(Literal),
    PatternExpr(TokenStream),
    RuleExpr(Ident, Vec<RuleArg>),
    MethodExpr(Ident, TokenStream),
    ChoiceExpr(Vec<Expr>),
    OptionalExpr(Box<Expr>),
    Repeat(Box<Expr>, BoundedRepeat, /*sep*/ Option<Box<Expr>>),
    PosAssertExpr(Box<Expr>),
    NegAssertExpr(Box<Expr>),
    ActionExpr(
        Vec<TaggedExpr>,
        /*action*/ Option<TokenStream>,
        /*cond*/ bool,
    ),
    MatchStrExpr(Box<Expr>),
    PositionExpr,
    QuietExpr(Box<Expr>),
    FailExpr(Literal),
    PrecedenceExpr {
        levels: Vec<PrecedenceLevel>,
    },
    MarkerExpr(bool),
}

#[derive(Debug, Clone)]
pub enum RuleArg {
    Rust(TokenStream),
    Peg(Expr),
}

#[derive(Debug, Clone)]
pub struct PrecedenceLevel {
    pub operators: Vec<PrecedenceOperator>,
}

#[derive(Debug, Clone)]
pub struct PrecedenceOperator {
    pub elements: Vec<TaggedExpr>,
    pub action: TokenStream,
}

#[derive(Debug, Clone)]
pub enum BoundedRepeat {
    None,
    Plus,
    Exact(TokenStream),
    Both(Option<TokenStream>, Option<TokenStream>),
}
