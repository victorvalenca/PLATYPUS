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
L(L) = [a-zA-Z]

L(LoD) = [a-zA-Z0-9]

L(VID) = AVID | SVID

L(AVID) = [a-zA-Z]([a-zA-Z0-9])*

L(SVID) = AVID#

```
## Integer Literals
```

L(DIL) = 0 | [1-9]*

L(NzD) = [1-9]

L(D) = [0-9]

L(OD) = [0-7]

L(OIL) = 0([0-7])*

L(IL) = (DIL | OIL)

```
## Floating Point Literal
```
L(FLPL) = (0 | [1-9]*).[0-9]*
```
## String Literal
```
L(STRL) = "([a-ZA-Z_0-9])*"
```