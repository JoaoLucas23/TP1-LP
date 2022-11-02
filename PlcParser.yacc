%%

%name PlcParser

%pos int

%term VAR | FIM | FN | RECURSAO
    | SE | ENTAO | SE_NAO
    | CORRESPONDE | COM
    | NEGACAO | E
    | CABECA | CAUDA | VAZIO
    | IMPRIME | UNDERLINE
    | NADA | BOOLEANO | INTEIRO
    | VERDADEIRO | FALSO | FUNCAO | NOME of string
    | SOMA | SUBTRACAO | MULTIPLICACAO | DIVISAO 
    | IGUAL | DIFERENTE | MENOR_QUE | MENOR_OU_IGUAL 
    | CONSTRUTOR | DOIS_PONTOS | PONTO_VIRGULA | VIRGULA | SETA | BARRA_VERTICAL | SETA_FUNCIONAL
    | ABRE_PARENTESES | FECHA_PARENTESES | ABRE_CHAVES | FECHA_CHAVES | ABRE_COLCHETES | FECHA_COLCHETES
    | CONST_INT of int 
    | EOF

%nonterm Prog of expr 
    | Decl of expr
    | Expr of expr
    | AtomExpr of expr
    | AppExpr of expr
    | Const of expr
    | Comps of expr list
    | MatchExpr of (expr option * expr) list 
    | CondExpr of expr option
    | Args of (plcType * string) list
    | Params of (plcType * string) list
    | TypedVar of plcType * string
    | Type of plcType
    | AtomType of plcType
    | Types of plcType list

%eop EOF

%right PONTO_VIRGULA SETA
%nonassoc SE
%left SE_NAO 
%left E 
%left IGUAL DIFERENTE
%left MENOR_QUE MENOR_OU_IGUAL
%right CONSTRUTOR
%left SOMA SUBTRACAO
%left MULTIPLICACAO DIVISAO
%nonassoc NEGACAO CABECA CAUDA VAZIO IMPRIME NOME
%left ABRE_COLCHETES

%noshift EOF

%start Prog

%%

Prog: Expr (Expr) 
    | Decl (Decl)

Decl: VAR NOME IGUAL Expr PONTO_VIRGULA Prog (Let(NOME, Expr, Prog))
    | FUNCAO NOME Args IGUAL Expr PONTO_VIRGULA Prog (Let(NOME, makeAnon(Args, Expr), Prog))
    | FUNCAO RECURSAO NOME Args CONSTRUTOR Type IGUAL Expr PONTO_VIRGULA Prog (makeFun(NOME, Args, Type, Expr, Prog))

Expr: AtomExpr(AtomExpr)
    | AppExpr(AppExpr)
    | SE Expr ENTAO Expr SE_NAO Expr (If(Expr1, Expr2, Expr3))
    | CORRESPONDE Expr COM MatchExpr (Match (Expr, MatchExpr))
    | NEGACAO Expr (Prim1("!", Expr))
    | Expr E Expr (Prim2("&&", Expr1, Expr2))
    | CABECA Expr (Prim1("hd", Expr))
    | CAUDA Expr (Prim1("tl", Expr))
    | VAZIO Expr (Prim1("ise", Expr))
    | IMPRIME Expr (Prim1("print", Expr))
    | Expr SOMA Expr (Prim2("+", Expr1, Expr2))
    | Expr SUBTRACAO Expr (Prim2("-", Expr1, Expr2))
    | Expr MULTIPLICACAO Expr (Prim2("*", Expr1, Expr2))
    | Expr DIVISAO Expr (Prim2("/", Expr1, Expr2))
    | SUBTRACAO Expr (Prim1("-", Expr))
    | Expr IGUAL Expr (Prim2("=", Expr1, Expr2))
    | Expr DIFERENTE Expr (Prim2("!=", Expr1, Expr2))
    | Expr MENOR_QUE Expr (Prim2("<", Expr1, Expr2))
    | Expr MENOR_OU_IGUAL Expr (Prim2("<=", Expr1, Expr2))
    | Expr CONSTRUTOR Expr (Prim2("::", Expr1, Expr2))
    | Expr PONTO_VIRGULA Expr (Prim2(";", Expr1, Expr2))
    | Expr ABRE_COLCHETES CONST_INT FECHA_COLCHETES (Item (CONST_INT, Expr))

AtomExpr: Const (Const)
    | NOME (Var(NOME))
    | ABRE_CHAVES Prog FECHA_CHAVES (Prog)
    | ABRE_PARENTESES Comps FECHA_PARENTESES (List Comps)
    | ABRE_CHAVES Expr FECHA_PARENTESES (Expr)
    | FN Args SETA_FUNCIONAL Expr FIM (makeAnon(Args, Expr))

AppExpr: AtomExpr AtomExpr (Call(AtomExpr1, AtomExpr2))
    | AppExpr AtomExpr (Call(AppExpr, AtomExpr))

Const: VERDADEIRO (ConB true) | FALSO (ConB false)
    | CONST_INT (ConI CONST_INT)
    | ABRE_PARENTESES FECHA_PARENTESES (List [])
    | ABRE_PARENTESES Type ABRE_COLCHETES FECHA_COLCHETES FECHA_PARENTESES (ESeq(Type))

Comps: Expr VIRGULA Expr (Expr1 :: Expr2 :: [])
    | Expr VIRGULA Comps (Expr :: Comps)

MatchExpr: FIM ([])
    | BARRA_VERTICAL CondExpr SETA Expr MatchExpr ((CondExpr, Expr) :: MatchExpr)

CondExpr: Expr (SOME(Expr))
    | UNDERLINE (NONE)

Args: ABRE_PARENTESES FECHA_PARENTESES ([])
    | ABRE_PARENTESES Params FECHA_PARENTESES (Params)

Params : TypedVar (TypedVar::[])
    | TypedVar VIRGULA Params (TypedVar::Params)

TypedVar: Type NOME ((Type, NOME))

Type: AtomType(AtomType)
    | ABRE_PARENTESES Types FECHA_PARENTESES (ListT Types)
    | ABRE_COLCHETES Type FECHA_COLCHETES (SeqT Type)
    | Type SETA Type (FunT (Type1, Type2))

AtomType: NADA (ListT [])
    | BOOLEANO (BoolT)
    | INTEIRO (IntT)
    | ABRE_PARENTESES Type FECHA_PARENTESES (Type)

Types: Type VIRGULA Type (Type1 :: Type2 :: [])
    | Type VIRGULA Types (Type :: Types)