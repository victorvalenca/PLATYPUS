# Regular Expressions for PLATYPUS 
## Comments
```
L(COMMENT) = !< [^CR]*CR
```

## Keywords
```
L(KEYWORD) = PLATYPUS | IF | THEN | ELSE | USING | REPEAT | INPUT | OUTPUT
```
## Variable Identifiers
```
L(LETTER) = [a-zA-Z]

L(LETTER_OR_DIGIT) = [a-zA-Z0-9]

L(VID) = AVID | SVID

L(AVID) = [a-zA-Z]([a-zA-Z0-9])*

L(SVID) = AVID#

```
## Integer Literals
```

L(DEC_INT_LITERAL) = [0-9]*

L(NON_ZERO_DIGIT) = 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

L(DIGIT) = [0-9]+

L(OCT_DIGIT) = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7

L(OCT_DIGIT_LITERAL) = 0(OCT_DIGIT)+

L(INT_LITERAL) = (DEC_INT_LITERAL | OCT_INT_LITERAL)

```
## Floating Point Literal
```
L(FLP_LITERAL) = ([0-9]*).([0-9]+)
```
## String Literal
```
L(STR_LITERAL) = "([a-ZA-Z_0-9])*"
```