{

  open TokenJson
(*  open String *)
(*  open Str *)
  exception LexicalError

}

let digit = ['0'-'9']
let integer = '-'? ['0'-'9'] ['0'-'9']*
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule lexer = parse
    | white    { lexer lexbuf }
    | newline  { lexer lexbuf }
    | integer      { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | float    { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
    | "true"   { TRUE }
    | "false"  { FALSE }
    | "null"   { NULL }
    | id       { ID (Lexing.lexeme lexbuf)}
    | '"'      { read_string (Buffer.create 17) lexbuf }
    | '{'      { LEFT_BRACE }
    | '}'      { RIGHT_BRACE }
    | '['      { LEFT_BRACK }
    | ']'      { RIGHT_BRACK }
    | ':'      { COLON }
    | ','      { COMMA }
    | eof      { EOF }
    | _ as texte { (print_string "Erreur lexicale : ");(print_char texte);(print_newline ()); NULL }

and read_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
{

}