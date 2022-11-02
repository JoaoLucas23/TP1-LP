(* Plc Lexer *)

(* User declarations *)

open Tokens
type pos = int
type slvalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (slvalue, pos)token

(* A function to print a message error on the screen. *)
val error = fn x => TextIO.output(TextIO.stdOut, x ^ "\n")
val lineNumber = ref 0

fun stringToInt string = 
    case Int.fromString string of
    SOME i => i
    | NONE => raise Fail ("Erro: '"^ string ^"' não pode ser convertido para inteiro")

(* Get the current line being read. *)
fun getLineAsString() =
    let
        val lineNum = !lineNumber
    in
        Int.toString lineNum
    end

(* Define what to do with the key words *)
fun keyWord (word, left, right) = 
    case word of
    "var" => VAR (left, right)
    | "then" => ENTAO (left, right)
    | "else" => SE_NAO (left, right)
    | "match" => CORRESPONDE (left, right)
    | "with" => COM (left, right)
    | "end" => FIM (left, right)
    | "fn" => FN (left, right)
    | "rec" => RECURSAO (left, right)
    | "if" => SE (left, right)
    | "hd" => CABECA (left, right)
    | "tl" => CAUDA (left, right)
    | "ise" => VAZIO (left, right)
    | "_" => UNDERLINE (left, right)
    | "Nil" => NADA (left, right)
    | "Bool" => BOOLEANO (left, right)
    | "Int" => INTEIRO (left, right)
    | "true" => VERDADEIRO (left, right)
    | "false" => FALSO (left, right)
    | "fun" => FUNCAO (left, right)
    | "print" => IMPRIME (left, right)
    | _ => NOME (word, left, right)


(* Define what to do when the end of the file is reached. *)
fun eof () = Tokens.EOF(0,0)

(* Initialize the lexer. *)
fun init() = ()
%%
%header (functor PlcLexerFun(structure Tokens: PlcParser_TOKENS));
digit=[0-9];
whitespace=[\ \t];
identifier=[a-zA-Z_][a-zA-Z_0-9]*;
%s COMMENTARY;
%%

\n => (lineNumber := !lineNumber + 1; lex());
<INITIAL>"(*" => (YYBEGIN COMMENTARY; lex());
<COMMENTARY>"*)" => (YYBEGIN INITIAL; lex());
<COMMENTARY>. => (lex());
<INITIAL>{whitespace}+ => (lex());
<INITIAL>{digit}+ => (CONST_INT(stringToInt(yytext), yypos, yypos));
<INITIAL>{identifier} => (keyWord(yytext, yypos, yypos));
<INITIAL>"!" => (NEGACAO(yypos, yypos));
<INITIAL>"&&" => (E(yypos, yypos));
<INITIAL>"+" => (SOMA(yypos, yypos));
<INITIAL>"-" => (SUBTRACAO(yypos, yypos));
<INITIAL>"*" => (MULTIPLICACAO(yypos, yypos));
<INITIAL>"/" => (DIVISAO(yypos, yypos));
<INITIAL>"=" => (IGUAL(yypos, yypos));
<INITIAL>"!=" => (DIFERENTE(yypos, yypos));
<INITIAL>"<" => (MENOR_QUE(yypos, yypos));
<INITIAL>"<=" => (MENOR_OU_IGUAL(yypos, yypos));
<INITIAL>"::" => (CONSTRUTOR(yypos, yypos));
<INITIAL>":" => (DOIS_PONTOS(yypos, yypos));
<INITIAL>";" => (PONTO_VIRGULA(yypos, yypos));
<INITIAL>"," => (VIRGULA(yypos, yypos));
<INITIAL>"->" => (SETA(yypos, yypos));
<INITIAL>"|" => (BARRA_VERTICAL(yypos, yypos));
<INITIAL>"=>" => (SETA_FUNCIONAL(yypos, yypos));
<INITIAL>"(" => (ABRE_PARENTESES(yypos, yypos));
<INITIAL>")" => (FECHA_PARENTESES(yypos, yypos));
<INITIAL>"{" => (ABRE_CHAVES(yypos, yypos));
<INITIAL>"}" => (FECHA_CHAVES(yypos, yypos));
<INITIAL>"[" => (ABRE_COLCHETES(yypos, yypos));
<INITIAL>"]" => (FECHA_COLCHETES(yypos, yypos));
<INITIAL>. => (error("\n***Erro no Lexer: caracter nao reconhecido***\n");
    raise Fail("Erro no Lexer: caracter nao reconhecido" ^yytext));