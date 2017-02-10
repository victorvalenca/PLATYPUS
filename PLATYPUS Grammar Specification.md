# PLATYPUS Language Specification

## 2. Lexical Specification (INCOMPLETE)

## NOTE: **This is incredibly incomplete and broken, I am simply writing this down and will fix it as I read along the informal language specification provided to me**

### 2.1 Input Elements and Tokens
```
<input character> ->
    ASCII characters but not SEOF

<input> ->
    <input elements> SEOF

<input elements> ->
    <input element> | <input elements> <input element>

<token> ->
    <variable identifier> | <keyword> | <floating-point literal>
    | <integer literal> | <string literal> | <separator> | <operator>
```
### 2.2 White Space
```
<white space> ->
    ASCII SP character (space)
    | ASCII HT character (horizontal tab)
    | ASCII VT character (vertical tab)
    | ASCII FF character (form feed)
    | <line terminator>

<line terminator> ->
    CR | LF | CR LF
```
### 2.3 Comments
```
<comment> ->
    !< <opt_characters in line> <line terminator>

<characters in line> ->
    <comment character> | <characters in line> <comment character>

<comment character> ->
    <input character> but not <line terminator>
```
### 2.4 Variable Identifiers
```
<variable identifier> ->
    <arithmetic variable identifier> | <string variable identifier>

<arithmetic identifier> ->
    <letter> <opt_letters or digits>

<letters or digits> ->
    <letter or digit> | <letters or digits> <letter or digit>

<letter> -> one of
    [a-z][A-Z]

<letter or digit> -> one of
    [a-z][A-Z][0-9]

<string variable identifier> ->
    <arithmetic variable identifier># 
```
### 2.5 Keywords
```
<keyword> ->
    PLATYPUS | IF | THEN | ELSE | USING | REPEAT | INPUT | OUTPUT
```
### 2.6 Integer Literals
```
<integer literal> ->
    <decimal integer literal> | <octal integer literal>

<decimal integer literal> ->
    0 | <non-zero digit> <opt_digits>

<digits> ->
    <digit> | <digits> <digit>

<digit> ->
    0 | <non-zero digit>

<non-zero digit> -> one of
    [1-9]

<octal integer literal> ->
    0 <octal digit> <octal digits>

<octal digit> ->
    0 | 1 | 2 | 3 | 4 | 5 | 6 | 7

<octal digits> ->
    <octal digit> | <octal digits> <octal digit>

```
### 2.7 Floating-point Literals
```
<floating-point literal> ->
    <decimal integer literal> . <opt_digits>
```
### 2.8 String Literals
```
<string literal> ->
    "<opt_string characters>"

<string chacters> ->
    <input character> | <string characters> <input character>
```
### 2.9 Separators
```
<separator> -> one of
    ( ) { } , ; " .
```
### 2.10 Operators
```
<operator> ->
    <arithmetic operator> | <string concatenation operator>
    | <relational operator> | <logical operator>
    | <assignment operator>

<arithmetic operator> -> one of
    + - * /

<string concatenation operator> -> one of
    > < == <>

<logical operator> ->
    .AND. | .OR.

<assignment operator> ->
    =
```
## 3. The PLATYPUS Syntatic Specification
### 3.1 PLATYPUS Program
```
<program> ->
    PLATYPUS {<opt_statements>} SEOF

<statements> ->
    <statement> | <statements> <statement>
```
### 3.2 Statements
```
<statement> ->
    <assignment statement>
    | <selection statement>
    | <iteration statement>
    | <input statement>
    | <output statement>
```
### 3.3 Assignment Statement
```
<assignment statement> ->
    <assignment expression>

<assignment expression> ->
    AVID = <arithmetic expression>
    | SVID = <string expression>
```
#### 3.2.2 Selection Statement (`if` statement)
```
<selection statement> ->
    IF (<conditional expression>) THEN <statements>
    ELSE {<opt_statements>};
```
#### 3.2.3 Iteration Statement (the loop statement)
```
<iteration statement> ->
    <TBC>
```
#### 3.2.4 Input Statement
```
<input statement> ->
    INPUT (<variable list>);

<variable list> ->
    <variable identifier> | <variable list>,<variable identifier>
```
#### 3.2.5 Output Statement
```
<output statement> ->
    <TBC>
```
### 3.3 Expressions
#### 3.3.1 Arithmetic Expressions
```
<arithmetic expression> ->
    <unary arithmetic expression>
    | <additive arithmetic expression>

<unary arithmetic expression> ->
    - <primary arithmetic expression>
    | + <primary arithmetic expression

<additive arithmetic expression> ->
    <additive arithmetic expression> + <multiplicative arithmetic expression>
    | <additive arithmetic expression> - <multiplicative arithmetic expression>
    | <multiplicative arithmetic expression>

<multiplicative arithmetic expression> ->
    <multiplicative arithmetic expression> * <primary arithmetic expression>
    | <multiplicative arithmetic expression> / <primary arithmetic expression>
    | <primary arithmetic expression>

<primary arithmetic expression> ->
    <variable identifier>
    | <floating-point literal>
    | <integer literal>
    | (<arithmetic expression>)
```
#### 3.3.2 String Expression
```
<string expression> ->
    <primary string expression>
    | <string expression> << <primary string expression>

<primary string expression> ->
    <string variable identifier>
    | <string literal>
```
#### 3.3.3 Conditional Expression
```
<conditional expression> ->
    <logical OR expression>

<logical OR expression> ->
    <TBC>

<logical AND expression> ->
    <TBC>
```
#### 3.3.4 Relational Expression
```
<relational expression> ->
    <primary a_relational expression> == <primary a_relational expression>
    | <primary a_relational expression> <= <primary a_relational expression>
    | <primary a_relational expression> > <primary a_relational expression>
    | <primary a_relational expression> < <primary a_relational expression>
    | <primary s_relational expression> == <primary s_relational expression>
    | <primary s_relational expression> <= <primary s_relational expression>
    | <primary s_relational expression> > <primary s_relational expression>
    | <primary s_relational expression> < <primary s_relational expression>

<primary a_relational expression> ->
    <floating-point literal>
    | <integer literal>
    | <variable identifier>

<primary s_relational expression> ->
    <TBC>
```