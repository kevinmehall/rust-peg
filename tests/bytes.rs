extern crate peg;
use peg::peg;

peg!(byteparser r#"
type Input = [u8];

pub rule commands -> Vec<&'input[u8]> = command*

rule command -> &'input [u8] = ">" val:$([b' ' ... b'~']+) [0] { val }

"#);

#[test]
fn main() {
	assert_eq!(byteparser::commands(b">asdf\0>xyz\0"), Ok(vec![&b"asdf"[..], &b"xyz"[..]]));
}
