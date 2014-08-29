#![feature(phase)]

#[phase(plugin)]
extern crate peg_syntax_ext;

use parser::element;

enum Node {
  Text(String),
  Element(String, Vec<Node>)
}

peg! parser(r#"
#[pub]
element -> super::Node
  = s:start_tag c:content e:end_tag { super::Element(s, c) }

name -> String
  = [a-zA-Z0-9_]+ { match_str.to_string() }

start_tag -> String
  = "<" n:name ">" { n }

end_tag -> String
  = "</" n:name ">" { n }

content -> Vec<super::Node>
  = (element / text)*

text -> super::Node
  = (!start_tag !end_tag .)+ { super::Text(match_str.to_string()) }
"#)

fn main() {
  let result = element("<div><em>ABC</em></div>");
  assert!(result.is_ok());

  let node = result.unwrap();
  assert_eq!(
    match node {
      Text(_) => fail!(),
      Element(name, _) => name
    }, "div".to_string());

  let child = match node {
    Element(_, ref children) => children.index(&0),
    _ => fail!()
  };

  let text = match *child {
    Element(_, ref children) => children.index(&0),
    _ => fail!()
  };

  assert_eq!(
    match *text {
      Text(ref t) => t.clone(),
      _ => fail!()
    }, "ABC".to_string());
}
