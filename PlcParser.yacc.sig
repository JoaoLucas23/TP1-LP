signature PlcParser_TOKENS =
sig
type ('a,'b) token
type svalue
val EOF:  'a * 'a -> (svalue,'a) token
val CONST_INT: (int) *  'a * 'a -> (svalue,'a) token
val FECHA_COLCHETES:  'a * 'a -> (svalue,'a) token
val ABRE_COLCHETES:  'a * 'a -> (svalue,'a) token
val FECHA_CHAVES:  'a * 'a -> (svalue,'a) token
val ABRE_CHAVES:  'a * 'a -> (svalue,'a) token
val FECHA_PARENTESES:  'a * 'a -> (svalue,'a) token
val ABRE_PARENTESES:  'a * 'a -> (svalue,'a) token
val SETA_FUNCIONAL:  'a * 'a -> (svalue,'a) token
val BARRA_VERTICAL:  'a * 'a -> (svalue,'a) token
val SETA:  'a * 'a -> (svalue,'a) token
val VIRGULA:  'a * 'a -> (svalue,'a) token
val PONTO_VIRGULA:  'a * 'a -> (svalue,'a) token
val DOIS_PONTOS:  'a * 'a -> (svalue,'a) token
val CONSTRUTOR:  'a * 'a -> (svalue,'a) token
val MENOR_OU_IGUAL:  'a * 'a -> (svalue,'a) token
val MENOR_QUE:  'a * 'a -> (svalue,'a) token
val DIFERENTE:  'a * 'a -> (svalue,'a) token
val IGUAL:  'a * 'a -> (svalue,'a) token
val DIVISAO:  'a * 'a -> (svalue,'a) token
val MULTIPLICACAO:  'a * 'a -> (svalue,'a) token
val SUBTRACAO:  'a * 'a -> (svalue,'a) token
val SOMA:  'a * 'a -> (svalue,'a) token
val NOME: (string) *  'a * 'a -> (svalue,'a) token
val FUNCAO:  'a * 'a -> (svalue,'a) token
val FALSO:  'a * 'a -> (svalue,'a) token
val VERDADEIRO:  'a * 'a -> (svalue,'a) token
val INTEIRO:  'a * 'a -> (svalue,'a) token
val BOOLEANO:  'a * 'a -> (svalue,'a) token
val NADA:  'a * 'a -> (svalue,'a) token
val UNDERLINE:  'a * 'a -> (svalue,'a) token
val IMPRIME:  'a * 'a -> (svalue,'a) token
val VAZIO:  'a * 'a -> (svalue,'a) token
val CAUDA:  'a * 'a -> (svalue,'a) token
val CABECA:  'a * 'a -> (svalue,'a) token
val E:  'a * 'a -> (svalue,'a) token
val NEGACAO:  'a * 'a -> (svalue,'a) token
val COM:  'a * 'a -> (svalue,'a) token
val CORRESPONDE:  'a * 'a -> (svalue,'a) token
val SE_NAO:  'a * 'a -> (svalue,'a) token
val ENTAO:  'a * 'a -> (svalue,'a) token
val SE:  'a * 'a -> (svalue,'a) token
val RECURSAO:  'a * 'a -> (svalue,'a) token
val FN:  'a * 'a -> (svalue,'a) token
val FIM:  'a * 'a -> (svalue,'a) token
val VAR:  'a * 'a -> (svalue,'a) token
end
signature PlcParser_LRVALS=
sig
structure Tokens : PlcParser_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
