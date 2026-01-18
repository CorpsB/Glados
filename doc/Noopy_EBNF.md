Noopy — EBNF Grammar (doc + code)

## Conventions

- Whitespace (and comments) is consumed by the lexer; the grammar below does not describe comments.
- Terminals are in double quotes.
- Non-terminals are in `camelCase`.
- `?` = optional, `*` = repetition (0..n), `+` = repetition (1..n).

---

## Lexemes (useful terminals)

> The rules below describe the expected *shapes*. The actual parser is based on Megaparsec.

```ebnf
letter           = "A".."Z" | "a".."z" ;
digit            = "0".."9" ;

identifier       = ( letter | "_" ), ( letter | digit | "_" )* ;

integerLiteral   = ( "+" | "-" )? , digit+ ;
boolLiteral      = "True" | "False" ;
voidLiteral      = "void" ;

stringLiteral    = '"' , stringChar* , '"' ;
charLiteral      = "'" , charChar , "'" ;

semicolon        = ";" ;
comma            = "," ;
colon            = ":" ;
```

---

## Program

```ebnf
program          = statement* , EOF ;
```

---

## Blocks

```ebnf
block            = "{" , statement* , "}" ;
```

---

## Statements (top-level and in blocks)

```ebnf
statement        = controlFlow
                 | declaration
                 | returnStmt
                 | varDefStmt
                 | exprStmt
                 ;

controlFlow      = ifStmt | whileStmt | forStmt ;

declaration      = importDecl | structDecl | funcDecl ;

returnStmt       = "ret" , expr , semicolon ;

varDefStmt       = varDef ;

exprStmt         = expr , semicolon ;
```

---

## Import

```ebnf
importDecl       = "import" , importPath , semicolon ;
importPath       = '"' , importPathChar* , '"' ;
```

---

## Types

```ebnf
type             = listType | "int" | "bool" | "void" | identifier ;
listType         = "[" , type , "]" ;
```

---

## Structures

```ebnf
structDecl       = "struct" , identifier , "{" , structField* , "}" ;

structField      = identifier , colon , type , semicolon ;

newExpr          = "new" , identifier , "{" , fieldInitList? , "}" ;
fieldInitList    = fieldInit , ( comma , fieldInit )* ;
fieldInit        = identifier , colon , expr ;
```

---

## Functions

```ebnf
funcDecl         = "func" , identifier , "(" , argDeclList? , ")" , retType? , block ;

argDeclList      = argDecl , ( comma , argDecl )* ;
argDecl          = identifier , ( colon , type )? ;

retType          = "->" , type ;
```

---

## Control Flow

### If (statement)

```ebnf
ifStmt           = "if" , "(" , ifInit? , expr , ")" , block , elseBranch? ;

ifInit           = varDef
                 | expr , semicolon
                 ;

elseBranch       = "else" , ( ifStmt | block ) ;
```

### While

```ebnf
whileStmt        = "while" , "(" , expr , ")" , block ;
```

### For

```ebnf
forStmt          = "for" , "(" , forInit , expr , semicolon , forUpdate? , ")" , block ;

forInit          = varDef
                 | expr , semicolon
                 | semicolon
                 ;

forUpdate        = identifier , "=" , expr
                 | expr
                 ;
```

---

## Variable Definition / Assignment

```ebnf
varDef           = identifier , accessor* , varDefTail ;

accessor         = indexAccessor | fieldAccessor ;
indexAccessor    = "[" , expr , "]" ;
fieldAccessor    = "." , identifier ;

varDefTail        = simpleVarDefTail | complexUpdateTail ;

simpleVarDefTail  = ( colon , type )? , assignOp , expr , semicolon ;
assignOp          = "=" | "+=" | "-=" | "*=" | "/=" | "%=" ;

complexUpdateTail = "=" , expr , semicolon ;
```

---

## Expressions

### Primary and suffixes

```ebnf
expr             = logicalOr ;

primary          = ifExpr
                 | lambdaExpr
                 | newExpr
                 | "(" , expr , ")"
                 | integerLiteral
                 | boolLiteral
                 | charLiteral
                 | stringLiteral
                 | listLiteral
                 | identifier
                 ;

postfix          = primary , suffix* ;

suffix           = callSuffix | indexSuffix | memberSuffix ;

callSuffix       = "(" , exprList? , ")" ;
exprList         = expr , ( comma , expr )* ;

indexSuffix      = "[" , expr , "]" ;
memberSuffix     = "." , identifier ;

listLiteral      = "[" , exprList? , "]" ;
```

### Lambda

```ebnf
lambdaExpr       = "lambda" , "(" , identList? , ")" , "->"? , expr ;
identList        = identifier , ( comma , identifier )* ;
```

### If (expression)

```ebnf
ifExpr           = "if" , "(" , expr , ")" , "{" , expr , "}" , ( "else" , "{" , expr , "}" )? ;
```

### Operator precedence (highest to lowest)

```ebnf
unary            = ( "!" | "++" | "--" ) , unary
                 | postfix
                 ;

postfixOp        = "++" | "--" ;
postfix          = primary , suffix* , postfixOp* ;

multiplicative   = unary , ( ( "*" | "/" | "%" ) , unary )* ;
additive         = multiplicative , ( ( "+" | "-" ) , multiplicative )* ;

comparison       = additive , ( compOp , additive )* ;
compOp           = "===" | "!==" | "==" | "!=" | "<=" | ">=" | "<" | ">" ;

logicalAnd       = comparison , ( "&&" , comparison )* ;
logicalOr        = logicalAnd , ( "||" , logicalAnd )* ;
```

---

## Notes (parser-accurate)

- `integerLiteral` accepts a leading `+` or `-` directly attached to digits.
- `if` exists both as a statement and as an expression.
- `===` and `!==` exist in addition to `==` and `!=`.
- `/` and `%` are tokens; internally they map to `div` and `mod`.
- Compound assignments (`/=` and `%=`) also map to `div` / `mod`.
- Chained updates (`x.y[0].z = ...;`) only allow `=`.