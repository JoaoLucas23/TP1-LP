# TP1 - Linguagens de Programação

Neste projeto você irá desenvolver em SML um lexer e um parser relacionados, para a linguagem
PLC, que pode ser vista como uma extensão da linguagem “micro-ML” de expressões utilizada
durante a primeira parte da disciplina. No segundo projeto, a ser passado após este, você deverá
implementar um verificador de tipos e um interpretador para a linguagem PLC. Novas instruções
serão dadas quando o segundo projeto for disponibilizado.
A linguagem PLC incorpora vários conceitos de programação vistos durante a disciplina. Ela é
uma linguagem puramente funcional, estática e estritamente tipada, com escopo estático, e de ordem
superior. Entre as funcionalidades presentes em PLC que não abordamos diretamente durante as
aulas está um tipo para sequências e funções primitivas para sua manipulação. O tipo de sequências
em PLC é similar ao tipo de listas em SML, com valores consistindo de uma série ordenada e imutável
de elementos do mesmo tipo. A linguagem possui também funções anônimas, um casamento de
padrões simplificado, e um comando de impressão.
As principais limitações de PLC com respeito a linguagens funcionais “reais”, impostas em nome
da simplicidade, são a ausência de comandos para ler entrada do console ou de arquivos; funções
são monomórficas e podem ser recursivas mas não mutuamente recursivas; parâmetros formais de
funções devem ser explicitamente tipados; funções recursivas devem declarar seus tipos de retorno;
e casamento de padrões é restrito a comparação de valores, i.e. corresponde a açúcar sintático para
uma série de comandos if-then-else. Por último, não estão presentes tipos básicos comuns como
caracteres ou strings, ou tipos estruturados como tipos de dados algébricos e estruturas.

• Environ
Este módulo define o tipo de um ambiente genérico e uma função de lookup para ele.
Ele já é provido completo através do arquivo Environ.sml em project.zip. Você precisará
de instanciações deste tipo e usará lookup no verificador de tipos e no interpretador.
• Absyn
Este módulo define a sintaxe abstrata de PLC. Ele já é provido completo no arquivo Absyn.sml
em project.zip. Ele contém uma função auxiliar val2string que pode ser usada para
implementar print.
• PlcParserAux
Este módulo define funções auxiliares para parsing. Note que para algumas das funções em
PlcParserAux.sml apenas suas assinaturas são providas. As implementações deverão ser
completadas por você.
• PlcParser
Este módulo contem o parser para a linguagem PLC, nos arquivos PlcParser.yacc.sig e
PlcParser.yacc.sml, a serem gerados automaticamente através do processo descrito na Seção 6.
• Lexer
Este módulo contem o lexer para a linguagem PLC, no arquivo PlcLexer.lex.sml, a ser
gerado automaticamente através do processo descrito na Seção 6. O lexer pode prover suporte
a comentários, que em PLC seguem o formato (* ... *), mas isto não é obrigatório.
• Parse
Este módulo, provido no arquivo Parse.sml, define a função fromString, que faz parsing
de um programa PLC a partir de uma string, e a função fromFile, que faz parsing de um
programa PLC em um arquivo de texto. Você pode usar essas funções para testar seu parser,
seguindo o arquivo testParser.sml.
