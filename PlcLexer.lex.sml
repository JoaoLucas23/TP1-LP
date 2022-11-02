functor PlcLexerFun(structure Tokens: PlcParser_TOKENS)=
   struct
    structure UserDeclarations =
      struct
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
end (* end of user routines *)
exception LexError (* raised if illegal leaf action tried *)
structure Internal =
	struct

datatype yyfinstate = N of int
type statedata = {fin : yyfinstate list, trans: string}
(* transition & final state table *)
val tab = let
val s = [ 
 (0, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (1, 
"\005\005\005\005\005\005\005\005\005\035\037\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\035\033\005\005\005\005\031\005\029\028\027\026\025\023\005\022\
\\020\020\020\020\020\020\020\020\020\020\018\017\015\013\005\005\
\\005\009\009\009\009\009\009\009\009\009\009\009\009\009\009\009\
\\009\009\009\009\009\009\009\009\009\009\009\012\005\011\005\009\
\\005\009\009\009\009\009\009\009\009\009\009\009\009\009\009\009\
\\009\009\009\009\009\009\009\009\009\009\009\008\007\006\005\005\
\\005"
),
 (3, 
"\038\038\038\038\038\038\038\038\038\038\037\038\038\038\038\038\
\\038\038\038\038\038\038\038\038\038\038\038\038\038\038\038\038\
\\038\038\038\038\038\038\038\038\038\038\039\038\038\038\038\038\
\\038\038\038\038\038\038\038\038\038\038\038\038\038\038\038\038\
\\038\038\038\038\038\038\038\038\038\038\038\038\038\038\038\038\
\\038\038\038\038\038\038\038\038\038\038\038\038\038\038\038\038\
\\038\038\038\038\038\038\038\038\038\038\038\038\038\038\038\038\
\\038\038\038\038\038\038\038\038\038\038\038\038\038\038\038\038\
\\038"
),
 (9, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\000\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\010\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\
\\000"
),
 (13, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\014\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (15, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\016\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (18, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\019\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (20, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\021\021\021\021\021\021\021\021\021\021\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (23, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\024\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (29, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\030\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (31, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\032\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (33, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\034\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (35, 
"\000\000\000\000\000\000\000\000\000\036\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\036\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (39, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\040\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
(0, "")]
fun f x = x 
val s = List.map f (List.rev (tl (List.rev s))) 
exception LexHackingError 
fun look ((j,x)::r, i: int) = if i = j then x else look(r, i) 
  | look ([], i) = raise LexHackingError
fun g {fin=x, trans=i} = {fin=x, trans=look(s,i)} 
in Vector.fromList(List.map g 
[{fin = [], trans = 0},
{fin = [], trans = 1},
{fin = [], trans = 1},
{fin = [], trans = 3},
{fin = [], trans = 3},
{fin = [(N 72)], trans = 0},
{fin = [(N 66),(N 72)], trans = 0},
{fin = [(N 55),(N 72)], trans = 0},
{fin = [(N 64),(N 72)], trans = 0},
{fin = [(N 18),(N 72)], trans = 9},
{fin = [(N 18)], trans = 9},
{fin = [(N 70),(N 72)], trans = 0},
{fin = [(N 68),(N 72)], trans = 0},
{fin = [(N 33),(N 72)], trans = 13},
{fin = [(N 58)], trans = 0},
{fin = [(N 38),(N 72)], trans = 15},
{fin = [(N 41)], trans = 0},
{fin = [(N 48),(N 72)], trans = 0},
{fin = [(N 46),(N 72)], trans = 18},
{fin = [(N 44)], trans = 0},
{fin = [(N 15),(N 72)], trans = 20},
{fin = [(N 15)], trans = 20},
{fin = [(N 31),(N 72)], trans = 0},
{fin = [(N 27),(N 72)], trans = 23},
{fin = [(N 53)], trans = 0},
{fin = [(N 50),(N 72)], trans = 0},
{fin = [(N 25),(N 72)], trans = 0},
{fin = [(N 29),(N 72)], trans = 0},
{fin = [(N 62),(N 72)], trans = 0},
{fin = [(N 60),(N 72)], trans = 29},
{fin = [(N 4)], trans = 0},
{fin = [(N 72)], trans = 31},
{fin = [(N 23)], trans = 0},
{fin = [(N 20),(N 72)], trans = 33},
{fin = [(N 36)], trans = 0},
{fin = [(N 12),(N 72)], trans = 35},
{fin = [(N 12)], trans = 35},
{fin = [(N 1)], trans = 0},
{fin = [(N 9)], trans = 0},
{fin = [(N 9)], trans = 39},
{fin = [(N 7)], trans = 0}])
end
structure StartStates =
	struct
	datatype yystartstate = STARTSTATE of int

(* start state definitions *)

val COMMENTARY = STARTSTATE 3;
val INITIAL = STARTSTATE 1;

end
type result = UserDeclarations.lexresult
	exception LexerError (* raised if illegal leaf action tried *)
end

fun makeLexer yyinput =
let	val yygone0=1
	val yyb = ref "\n" 		(* buffer *)
	val yybl = ref 1		(*buffer length *)
	val yybufpos = ref 1		(* location of next character to use *)
	val yygone = ref yygone0	(* position in file of beginning of buffer *)
	val yydone = ref false		(* eof found yet? *)
	val yybegin = ref 1		(*Current 'start state' for lexer *)

	val YYBEGIN = fn (Internal.StartStates.STARTSTATE x) =>
		 yybegin := x

fun lex () : Internal.result =
let fun continue() = lex() in
  let fun scan (s,AcceptingLeaves : Internal.yyfinstate list list,l,i0) =
	let fun action (i,nil) = raise LexError
	| action (i,nil::l) = action (i-1,l)
	| action (i,(node::acts)::l) =
		case node of
		    Internal.N yyk => 
			(let fun yymktext() = String.substring(!yyb,i0,i-i0)
			     val yypos = i0+ !yygone
			open UserDeclarations Internal.StartStates
 in (yybufpos := i; case yyk of 

			(* Application actions *)

  1 => (lineNumber := !lineNumber + 1; lex())
| 12 => (lex())
| 15 => let val yytext=yymktext() in CONST_INT(stringToInt(yytext), yypos, yypos) end
| 18 => let val yytext=yymktext() in keyWord(yytext, yypos, yypos) end
| 20 => (NEGACAO(yypos, yypos))
| 23 => (E(yypos, yypos))
| 25 => (SOMA(yypos, yypos))
| 27 => (SUBTRACAO(yypos, yypos))
| 29 => (MULTIPLICACAO(yypos, yypos))
| 31 => (DIVISAO(yypos, yypos))
| 33 => (IGUAL(yypos, yypos))
| 36 => (DIFERENTE(yypos, yypos))
| 38 => (MENOR_QUE(yypos, yypos))
| 4 => (YYBEGIN COMMENTARY; lex())
| 41 => (MENOR_OU_IGUAL(yypos, yypos))
| 44 => (CONSTRUTOR(yypos, yypos))
| 46 => (DOIS_PONTOS(yypos, yypos))
| 48 => (PONTO_VIRGULA(yypos, yypos))
| 50 => (VIRGULA(yypos, yypos))
| 53 => (SETA(yypos, yypos))
| 55 => (BARRA_VERTICAL(yypos, yypos))
| 58 => (SETA_FUNCIONAL(yypos, yypos))
| 60 => (ABRE_PARENTESES(yypos, yypos))
| 62 => (FECHA_PARENTESES(yypos, yypos))
| 64 => (ABRE_CHAVES(yypos, yypos))
| 66 => (FECHA_CHAVES(yypos, yypos))
| 68 => (ABRE_COLCHETES(yypos, yypos))
| 7 => (YYBEGIN INITIAL; lex())
| 70 => (FECHA_COLCHETES(yypos, yypos))
| 72 => let val yytext=yymktext() in error("\n***Erro no Lexer: caracter nao reconhecido***\n");
    raise Fail("Erro no Lexer: caracter nao reconhecido" ^yytext) end
| 9 => (lex())
| _ => raise Internal.LexerError

		) end )

	val {fin,trans} = Unsafe.Vector.sub(Internal.tab, s)
	val NewAcceptingLeaves = fin::AcceptingLeaves
	in if l = !yybl then
	     if trans = #trans(Vector.sub(Internal.tab,0))
	       then action(l,NewAcceptingLeaves
) else	    let val newchars= if !yydone then "" else yyinput 1024
	    in if (String.size newchars)=0
		  then (yydone := true;
		        if (l=i0) then UserDeclarations.eof ()
		                  else action(l,NewAcceptingLeaves))
		  else (if i0=l then yyb := newchars
		     else yyb := String.substring(!yyb,i0,l-i0)^newchars;
		     yygone := !yygone+i0;
		     yybl := String.size (!yyb);
		     scan (s,AcceptingLeaves,l-i0,0))
	    end
	  else let val NewChar = Char.ord(Unsafe.CharVector.sub(!yyb,l))
		val NewChar = if NewChar<128 then NewChar else 128
		val NewState = Char.ord(Unsafe.CharVector.sub(trans,NewChar))
		in if NewState=0 then action(l,NewAcceptingLeaves)
		else scan(NewState,NewAcceptingLeaves,l+1,i0)
	end
	end
(*
	val start= if String.substring(!yyb,!yybufpos-1,1)="\n"
then !yybegin+1 else !yybegin
*)
	in scan(!yybegin (* start *),nil,!yybufpos,!yybufpos)
    end
end
  in lex
  end
end
