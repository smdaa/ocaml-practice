type token =
  | NULL
  | TRUE
  | FALSE
  | STRING of string
  | INT of int
  | FLOAT of float
  | ID of string
  | LEFT_BRACK
  | RIGHT_BRACK
  | LEFT_BRACE
  | RIGHT_BRACE
  | COMMA
  | COLON
  | EOF

let printToken t =
  (print_endline
       (match t with
         | NULL -> "null"
         | TRUE -> "true"
         | FALSE -> "false"
         | STRING x -> "string : " ^ x
         | INT x -> "int : " ^ string_of_int x
         | FLOAT x -> "float : " ^ string_of_float x
         | ID x -> "id : " ^ x
         | LEFT_BRACK -> "bloc : ["
         | RIGHT_BRACK -> "bloc : ]"
         | LEFT_BRACE -> "bloc : {"
         | RIGHT_BRACE -> "bloc : }"
         | COMMA -> "separator : ,"
         | COLON -> "separator : :"
         | EOF -> "end of file"))
         