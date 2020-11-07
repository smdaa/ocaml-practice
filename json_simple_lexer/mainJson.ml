open TokenJson;;

if (Array.length Sys.argv > 1)
then
    let lexbuf = (Lexing.from_channel (open_in Sys.argv.(1))) in
    let token = ref (LexerJson.lexer lexbuf) in
    while ((!token) != EOF) do
        (printToken (!token));
        (token := (LexerJson.lexer lexbuf))
    done
else
    (print_endline "mainJava.exe fichier")

(*
dune build mainJson.exe
./_build/default/mainJson.exe ./tests/
*)