use std::cell::RefCell;
use std::io::Writer;
use std::cast::transmute_mut;

pub struct RustWriter {
	writer: ~Writer,
	indent: RefCell<uint>,
}

impl RustWriter {
	pub fn new<W: Writer+Send>(writer: W) -> RustWriter {
		RustWriter {
			writer: ~writer as ~Writer,
			indent: RefCell::new(0)
		}
	}

	#[inline]
	pub fn write(&self, s: &str){
		self.writer().write(s.as_bytes()).unwrap();
	}

	#[inline]
	pub fn writer<'a>(&'a self) -> &'a mut Writer {
		// This struct cannot be mutable without causing borrowck errors
		// when methods are passed closures that also reference the struct
		unsafe { &'a mut transmute_mut(self).writer as &'a mut Writer }
	}

	pub fn write_indent(&self) {
		for _ in range(0, *self.indent.borrow()) {
			self.write("    ");
		}
	}

	pub fn line(&self, line: &str) {
		self.write_indent();
		self.write(line);
		self.write("\n");
	}

	pub fn indented(&self, inner: || ) {
		*self.indent.borrow_mut() += 1;
		inner();
		*self.indent.borrow_mut() -= 1;
	}

	pub fn let_stmt(&self, varname: &str, value: &str) {
		self.write_indent();
		(write!(self.writer(), "let {} = {};\n", varname, value)).unwrap();
	}

	pub fn let_mut_stmt(&self, varname: &str, value: &str) {
		self.write_indent();
		(write!(self.writer(), "let mut {} = {};\n", varname, value)).unwrap();
	}

	pub fn let_block(&self, varname: &str, inner: || ){
		self.write_indent();
		(write!(self.writer(), "let {} = \\{\n", varname)).unwrap();
		self.indented(inner);
		self.write_indent();
		self.write("};\n")
	}

	pub fn block(&self, inner: || )  {
		self.write(" {\n");
		self.indented(inner);
		self.write_indent();
		self.write("}");
	}

	pub fn def_fn(&self, public: bool, name: &str, args: &str, retn: &str, inner: || ) {
		self.write_indent();
		if public { self.write("pub "); }
		(write!(self.writer(), "fn {}({}) -> {}", name, args, retn)).unwrap();
		self.block(inner);
		self.write("\n");
	}

	#[inline]
	pub fn cond_block(&self, kwd: &str, condition: &str, inner: || ) {
		self.write_indent();
		self.write(kwd);
		self.write(condition);
		self.block(inner);
		self.write("\n");
	}

	pub fn if_else(&self, condition: &str, if_inner: || , else_inner: || ) {
		self.write_indent();
		self.write("if ");
		self.write(condition);
		self.block(if_inner);
		self.write(" else");
		self.block(else_inner);
		self.write("\n");
	}

	pub fn loop_block(&self, inner: || ) {
		self.write_indent();
		self.write("loop");
		self.block(inner);
		self.write("\n");
	}

	pub fn match_block(&self, expr: &str, inner: || ) {
		self.cond_block("match ", expr, inner);
	}

	pub fn match_inline_case(&self, m: &str, e: &str) {
		self.write_indent();
		(write!(self.writer(), "{} => {},\n", m, e)).unwrap();
	}

	pub fn match_case(&self, m: &str, inner: || ) {
		self.write_indent();
		(write!(self.writer(), "{} => \\{\n", m)).unwrap();
		self.indented(inner);
		self.write_indent();
		self.write("}\n");
	}
}
