use std::cell::Cell;

pub struct RustWriter {
	writer: @Writer,
	indent: Cell<uint>,
}

impl RustWriter {
	pub fn new(writer: @Writer) -> RustWriter {
		RustWriter {
			writer: writer,
			indent: Cell::new(0)
		}
	}

	#[inline]
	pub fn write(&self, s: &str){
		self.writer.write_str(s);
	}

	pub fn write_indent(&self) {
		do self.indent.with_ref |&indent| {
			for _ in range(0, indent) {
				self.write("    ");
			}
		}
	}

	pub fn line(&self, line: &str) {
		self.write_indent();
		self.write(line);
		self.write("\n");
	}

	pub fn comment(&self, comment: &str) {
		self.write_indent();
		self.write("// ");
		self.write(comment);
		self.write("\n");

	}

	pub fn indented(&self, inner: &fn()) {
		do self.indent.with_mut_ref |i| {*i += 1;}
		inner();
		do self.indent.with_mut_ref |i| {*i -= 1;}
	}

	pub fn let_stmt(&self, varname: &str, value: &str) {
		self.write_indent();
		self.write("let ");
		self.write(varname);
		self.write(" = ");
		self.write(value);
		self.write(";\n");
	}

	pub fn let_mut_stmt(&self, varname: &str, value: &str) {
		self.write_indent();
		self.write("let mut ");
		self.write(varname);
		self.write(" = ");
		self.write(value);
		self.write(";\n");
	}

	pub fn let_block(&self, varname: &str, inner: &fn()){
		self.write_indent();
		self.write("let ");
		self.write(varname);
		self.write(" = {\n");
		self.indented(inner);
		self.write_indent();
		self.write("};\n")
	}

	pub fn block(&self, inner: &fn())  {
		self.write(" {\n");
		self.indented(inner);
		self.write_indent();
		self.write("}");
	}

	pub fn def_fn(&self, public: bool, name: &str, args: &str, retn: &str, inner: &fn()) {
		self.write_indent();
		if public { self.write("pub "); }
		self.write("fn ");
		self.write(name);
		self.write("(");
		self.write(args);
		self.write(") -> ");
		self.write(retn);
		self.block(inner);
		self.write("\n");
	}

	#[inline]
	pub fn cond_block(&self, kwd: &str, condition: &str, inner: &fn()) {
		self.write_indent();
		self.write(kwd);
		self.write(condition);
		self.block(inner);
		self.write("\n");
	}

	pub fn if_block(&self, condition: &str, inner: &fn()) {
		self.cond_block("if ", condition, inner);
	}

	pub fn if_else(&self, condition: &str, if_inner: &fn(), else_inner: &fn()) {
		self.write_indent();
		self.write("if ");
		self.write(condition);
		self.block(if_inner);
		self.write(" else");
		self.block(else_inner);
		self.write("\n");
	}

	pub fn loop_block(&self, inner: &fn()) {
		self.write_indent();
		self.write("loop");
		self.block(inner);
		self.write("\n");
	}

	pub fn while_block(&self, condition: &str, inner: &fn()) {
		self.cond_block("while ", condition, inner);
	}

	pub fn match_block(&self, expr: &str, inner: &fn()) {
		self.cond_block("match ", expr, inner);
	}

	pub fn match_inline_case(&self, m: &str, e: &str) {
		self.write_indent();
		self.write(m);
		self.write(" => ");
		self.write(e);
		self.write(",\n");
	}

	pub fn match_case(&self, m: &str, inner: &fn()) {
		self.write_indent();
		self.write(m);
		self.write(" => {\n");
		self.indented(inner);
		self.write_indent();
		self.write("}\n");
	}

	pub fn def_enum(&self, public: bool, name: &str, inner: &fn()) {
		self.write_indent();
		if public { self.write("pub "); }
		self.write("enum ");
		self.write(name);
		self.block(inner);
		self.write("\n");
	}

	pub fn enum_simple(&self, name: &str){
		self.write_indent();
		self.write(name);
		self.write(",\n");
	}

	pub fn enum_tuple(&self, name: &str, types: &[&str]) {
		self.write_indent();
		self.write(name);
		self.write("(");
		for &i in types.iter() {
			self.write(i);
			self.write(", "); // TODO: don't leave a trailing comma
		}
		self.write("),\n");
	}

	pub fn enum_struct(&self, name: &str, inner: &fn()) {
		self.write_indent();
		self.write(name);
		self.block(inner);
		self.write(",\n");
	}

	pub fn struct_field(&self, name: &str, typename: &str) {
		self.write_indent();
		self.write(name);
		self.write(": ");
		self.write(typename);
		self.write(",\n");
	}
}