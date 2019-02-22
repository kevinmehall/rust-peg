extern crate peg;

peg::parser!( grammar arithmetic() for str {
     use super::InfixAst;

    rule ident -> &'input str = $(['a'..='z']+)
    rule haskell_op -> String = "`" i:ident "`" [' '|'\n']* { i.to_owned() }
    rule infix_atom -> InfixAst = i:ident [' '|'\n']* { InfixAst::Ident(i.to_owned()) }
    rule plus = "+" [' '|'\n']*

    pub rule expression -> InfixAst = #infix<infix_atom> {
        #L x:@ plus y:@ { InfixAst::Add(Box::new(x), Box::new(y)) }
        #L x:@ op:haskell_op y:@ { InfixAst::Op(op, Box::new(x), Box::new(y)) }
    }
});

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum InfixAst {
    Ident(String),
    Add(Box<InfixAst>, Box<InfixAst>),
    Op(String, Box<InfixAst>, Box<InfixAst>)
}

#[test]
fn test_infix_ast(){
    assert_eq!(arithmetic::expression("a + b `x` c").unwrap(),
        InfixAst::Add(
            Box::new(InfixAst::Ident("a".to_owned())),
            Box::new(InfixAst::Op("x".to_owned(),
                Box::new(InfixAst::Ident("b".to_owned())),
                Box::new(InfixAst::Ident("c".to_owned()))
            ))
        )
    )
}