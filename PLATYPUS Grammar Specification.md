# PLATYPUS Language Specification

## Lexical Specification (INCOMPLETE)

## NOTE: **This is incredibly incomplete and broken, I am simply writing this down and will fix it as I read along the informal language specification provided to me**

1. Input Elements and Tokens
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
2. White Space
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
3. Comments
```
    <comment> ->
        !< <opt_characters in line> <line terminator>

    <characters in line> ->
        <comment character> | <characters in line> <comment character>

    <comment character> ->
        <input character> but not <line terminator>
```
4. Variable Identifiers
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
5. Keywords
```
    <keyword> ->
        PLATYPUS | IF | THEN | ELSE | USING | REPEAT | INPUT | OUTPUT
```
6. Integer Literals
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
    
