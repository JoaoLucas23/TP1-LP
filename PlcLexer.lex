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
    let
        val i = Int.fromString string
    in
        case i of
            SOME i => i
            | NONE => raise Fail ("Could not convert '" ^ string ^ "' to integer")
    end

(* Get the current line being read. *)
fun getLineAsString() =
    let
        val lineNum = !lineNumber
    in
        Int.toString lineNum
    end

fun keyWord (w, l, r) = 
    case w of
    "var" => VAR (l, r)
    | "end" => FIM (l, r)
    | "fn" => FN (l, r)
    | "rec" => RECURSAO (l, r)
    | "if" => SE (l, r)
    | "then" => ENTAO (l, r)
    | "else" => SE_NAO (l, r)
    | "match" => CORRESPONDE (l, r)
    | "with" => COM (l, r)
    | "hd" => CABECA (l, r)
    | "tl" => CAUDA (l, r)
    | "ise" => VAZIO (l, r)
    | "print" => IMPRIME (l, r)
    | "_" => UNDERLINE (l, r)
    | "Nil" => NADA (l, r)
    | "Bool" => BOOLEANO (l, r)
    | "Int" => INTEIRO (l, r)
    | "true" => VERDADEIRO (l, r)
    | "false" => FALSO (l, r)
    | "fun" => FUNCAO (l, r)
    | _ => NOME (w, l, r)

(* Define what to do when the end of the file is reached. *)
fun eof () = Tokens.EOF(0,0)

(* Initialize the lexer. *)
fun init() = ()
%%
%header (functor PlcLexerFun(structure Tokens: PlcParser_TOKENS));
alpha=[A-Za-z];
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
<INITIAL>{digit}+ => (CINT(stringToInt(yytext), yypos, yypos));
<INITIAL>{identifier} => (keyWord(yytext, yypos, yypos));
<INITIAL>"!" => (NEGACAO(yypos, yypos));
<INITIAL>"&&" => (E(yypos, yypos));
<INITIAL>"+" => (SOMA(yypos, yypos));
<INITIAL>"-" => (SUBTRACAO(yypos, yypos));
<INITIAL>"*" => (MULTIPLICACAO(yypos, yypos));
<INITIAL>"/" => (DIVISAO(yypos, yypos));
<INITIAL>"=" => (IGUALDADE(yypos, yypos));
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
<INITIAL>. => (error("\n***Lexer error: bad character ***\n");
    raise Fail("Lexer error: bad character " ^yytext));