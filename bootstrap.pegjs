{
  function repr(s) {
    return '"' + s.replace(/\\/g, '\\\\').replace(/"/g, '\\"') + '"'
  }

  function repr_char(c) {
    return "'" + c.replace(/\n/g, '\\n').replace(/\r/g, '\\r').replace(/\t/g, '\\t') + "'"
  }
}

grammar
  = __ header: action? rules:rule* {
      header = header ? ('Some(~'+repr(header)+')') : "None"
      return "~Grammar { initializer: " + header + ",\n rules: ~[\n" + rules.join(',\n') + "\n]\n }"
    }

rule
  = name:identifier returns: returntype equals expression:expression semicolon? {
      return "~Rule {\n\tname: ~\"" + name + "\",\nexpr: "+ expression + ', ret_type:~"' + returns + '"\n}'
    }

returntype 
  = returns tp:$(rust_type) { return tp.trim(); }
  / {return "()"}

rust_type
  = "()" __
  / "~" __ rust_type
  / "[" rust_type "]" __
  / identifier "<" rust_type ">" __
  / identifier

expression
  = choice

choice
  = head:sequence tail:(slash sequence)* {
      if (tail.length > 0) {
        var alternatives = [head].concat(tail.map(
            function(element) { return element[1]; }
        ));
        
        return "~ChoiceExpr(~[\n" + alternatives.join(",\n") + "\n])"
      } else {
        return head;
      }
    }

sequence
  = elements:labeled* code:action {
      return '~ActionExpr(~[\n' + elements.join(",\n") + '\n],~' + repr(code) +')';
    }
  / elements:prefixed* {
      if (elements.length !== 1) {
          return "~SequenceExpr(~[\n" + elements.join(",\n") + "\n])"
      } else {
          return elements[0];
      }
    }

labeled
  = label:identifier colon expression:prefixed {
      return 'TaggedExpr{ name: Some(~"' + label + '"), expr: '  + expression + '}';
    }
  / expr: prefixed {
      return 'TaggedExpr{ name: None, expr: '+ expr + '}';
  }

prefixed
  = dollar expression:suffixed {
      return expression;
    }
  / and code:action {
      return "/*Semantic and unsupported*/"
    }
  / and expression:suffixed {
      return "~PosAssertExpr(\n" + expression + "\n)"
    }
  / not code:action {
      return "/*Semantic not unsupported*/"
      
    }
  / not expression:suffixed {
      return "~NegAssertExpr(\n" + expression + ")\n"
    }
  / suffixed

suffixed
  = expression:primary question {
        return "~OptionalExpr(\n" + expression + ")\n"
    }
  / expression:primary star {
      return "~ZeroOrMore(\n" + expression + ")\n"
    }
  / expression:primary plus {
      return "~OneOrMore(\n" + expression + ")\n"
    }
  / primary

primary
  = name:identifier !(string? returntype equals) {
      return "~RuleExpr(~\""+name+"\")"
    }
  / literal
  / class
  / dot { return "~AnyCharExpr" }
  / lparen expression:expression rparen { return expression; }

/* "Lexical" elements */

action "action"
  = braced:braced __ { return braced.substr(1, braced.length - 2); }

braced
  = $("{" (braced / nonBraceCharacters)* "}")

nonBraceCharacters
  = nonBraceCharacter+

nonBraceCharacter
  = [^{}]

equals    = "=" __ { return "="; }
colon     = ":" __ { return ":"; }
semicolon = ";" __ { return ";"; }
slash     = "/" __ { return "/"; }
and       = "&" __ { return "&"; }
not       = "!" __ { return "!"; }
dollar    = "$" __ { return "$"; }
question  = "?" __ { return "?"; }
star      = "*" __ { return "*"; }
plus      = "+" __ { return "+"; }
lparen    = "(" __ { return "("; }
rparen    = ")" __ { return ")"; }
dot       = "." __ { return "."; }
returns   = "->" __ {return "->";}

/*
 * Modeled after ECMA-262, 5th ed., 7.6, but much simplified:
 *
 * * no Unicode escape sequences
 *
 * * "Unicode combining marks" and "Unicode connection punctuation" can't be
 *   part of the identifier
 *
 * * only [a-zA-Z] is considered a "Unicode letter"
 *
 * * only [0-9] is considered a "Unicode digit"
 *
 * The simplifications were made just to make the implementation little bit
 * easier, there is no "philosophical" reason behind them.
 *
 * Contrary to ECMA 262, the "$" character is not valid because it serves other
 * purpose in the grammar.
 */
identifier "identifier"
  = chars:$((letter / "_") (letter / digit / "_")*) __ { return chars; }

/*
 * Modeled after ECMA-262, 5th ed., 7.8.4. (syntax & semantics, rules only
 * vaguely).
 */
literal "literal"
  = value:(doubleQuotedString / singleQuotedString) flags:"i"? __ {
      return "~LiteralExpr(~"+repr(value)+")"
    }

string "string"
  = string:(doubleQuotedString / singleQuotedString) __ { return string; }

doubleQuotedString
  = '"' chars:doubleQuotedCharacter* '"' { return chars.join(""); }

doubleQuotedCharacter
  = simpleDoubleQuotedCharacter
  / simpleEscapeSequence
  / zeroEscapeSequence
  / hexEscapeSequence
  / unicodeEscapeSequence
  / eolEscapeSequence

simpleDoubleQuotedCharacter
  = !('"' / "\\" / eolChar) char_:. { return char_; }

singleQuotedString
  = "'" chars:singleQuotedCharacter* "'" { return chars.join(""); }

singleQuotedCharacter
  = simpleSingleQuotedCharacter
  / simpleEscapeSequence
  / zeroEscapeSequence
  / hexEscapeSequence
  / unicodeEscapeSequence
  / eolEscapeSequence

simpleSingleQuotedCharacter
  = !("'" / "\\" / eolChar) char_:. { return char_; }

class "character class"
  = "[" inverted:"^"? parts:(classCharacterRange / classCharacter)* "]" flags:"i"? __ {
      return "~CharSetExpr(" + (inverted?"true":"false") + ", ~[" + parts.join(",") + "])"
    }

classCharacterRange
  = begin:bracketDelimitedCharacter "-" end:bracketDelimitedCharacter {
      if (begin.charCodeAt(0) > end.charCodeAt(0)) {
        throw new this.SyntaxError(
          "Invalid character range: " + begin.rawText + "-" + end.rawText + "."
        );
      }

      return "CharSetCase{start:"+repr_char(begin)+", end: "+repr_char(end)+"}"
    }

classCharacter
  = char_:bracketDelimitedCharacter {
      return "CharSetCase{start:"+repr_char(char_)+", end: "+repr_char(char_)+"}"
    }

bracketDelimitedCharacter
  = simpleBracketDelimitedCharacter
  / simpleEscapeSequence
  / zeroEscapeSequence
  / hexEscapeSequence
  / unicodeEscapeSequence
  / eolEscapeSequence

simpleBracketDelimitedCharacter
  = !("]" / "\\" / eolChar) char_:. { return char_; }

simpleEscapeSequence
  = "\\" !(digit / "x" / "u" / eolChar) char_:. {
      return char_
        .replace("b", "\b")
        .replace("f", "\f")
        .replace("n", "\n")
        .replace("r", "\r")
        .replace("t", "\t")
        .replace("v", "\x0B"); // IE does not recognize "\v".
    }

zeroEscapeSequence
  = "\\0" !digit { return "\x00"; }

hexEscapeSequence
  = "\\x" digits:$(hexDigit hexDigit) {
      return String.fromCharCode(parseInt(digits, 16));
    }

unicodeEscapeSequence
  = "\\u" digits:$(hexDigit hexDigit hexDigit hexDigit) {
      return String.fromCharCode(parseInt(digits, 16));
    }

eolEscapeSequence
  = "\\" eol:eol { return eol; }

digit
  = [0-9]

hexDigit
  = [0-9a-fA-F]

letter
  = lowerCaseLetter
  / upperCaseLetter

lowerCaseLetter
  = [a-z]

upperCaseLetter
  = [A-Z]

__ = (whitespace / eol / comment)*

/* Modeled after ECMA-262, 5th ed., 7.4. */
comment "comment"
  = singleLineComment
  / multiLineComment

singleLineComment
  = "//" (!eolChar .)*

multiLineComment
  = "/*" (!"*/" .)* "*/"

/* Modeled after ECMA-262, 5th ed., 7.3. */
eol "end of line"
  = "\n"
  / "\r\n"
  / "\r"
  / "\u2028"
  / "\u2029"

eolChar
  = [\n\r\u2028\u2029]

/* Modeled after ECMA-262, 5th ed., 7.2. */
whitespace "whitespace"
  = [ \t\v\f\u00A0\uFEFF\u1680\u180E\u2000-\u200A\u202F\u205F\u3000]