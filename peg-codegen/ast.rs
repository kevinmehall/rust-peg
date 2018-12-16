use proc_macro2::{ TokenStream, Ident, Literal };

#[derive(Debug)]
pub struct Grammar {
    pub name: Ident,
    pub args: Vec<(Ident, TokenStream)>,
    pub items: Vec<Item>,
    pub input_type: TokenStream,
}

impl Grammar {
    pub fn iter_rules(&self) -> impl Iterator<Item=&Rule> {
        self.items.iter().filter_map(|item| {
            match item {
                Item::Rule(r) => Some(r),
                _ => None
            }
        })
    }

    pub fn find_template(&self, name: &str) -> Option<&Template> {
        self.items.iter().find_map(|item| {
            match item {
                Item::Template(t) if t.name == name => Some(t),
                _ => None
            }
        })
    }
}

#[derive(Debug)]
pub enum Item {
    Use(TokenStream),
    Rule(Rule),
    Template(Template),
}

#[derive(Debug)]
pub struct Rule {
    pub name: Ident,
    pub expr: Expr,
    pub ret_type: Option<TokenStream>,
    pub visibility: Option<TokenStream>,
    pub cached: bool,
}

#[derive(Debug)]
pub struct Template {
    pub name: Ident,
    pub params: Vec<Ident>,
    pub expr: Expr,
}

#[derive(Debug, Clone)]
pub struct TaggedExpr {
    pub name: Option<Ident>,
    pub expr: Expr,
}

#[derive(Debug, Clone)]
pub enum Expr {
    AnyCharExpr,
    LiteralExpr(Literal),
    PatternExpr(TokenStream),
    RuleExpr(Ident),
    TemplateInvoke(Ident, Vec<Expr>),
    MethodExpr(Ident, TokenStream),
    ChoiceExpr(Vec<Expr>),
    OptionalExpr(Box<Expr>),
    Repeat(Box<Expr>, BoundedRepeat, /*sep*/ Option<Box<Expr>>),
    PosAssertExpr(Box<Expr>),
    NegAssertExpr(Box<Expr>),
    ActionExpr(Vec<TaggedExpr>, /*action*/ Option<TokenStream>, /*cond*/ bool),
    MatchStrExpr(Box<Expr>),
    PositionExpr,
    QuietExpr(Box<Expr>),
    FailExpr(Literal),
    InfixExpr{ atom: Box<Expr>, levels: Vec<InfixLevel> },
    MarkerExpr,
}


#[derive(Copy, Debug, Clone, PartialEq, Eq)]
pub enum InfixAssoc { Left, Right }

#[derive(Debug, Clone)]
pub struct InfixLevel {
    pub assoc: InfixAssoc,
    pub operators: Vec<InfixOperator>,
}

#[derive(Debug, Clone)]
pub struct InfixOperator {
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