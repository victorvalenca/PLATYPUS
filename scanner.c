/* Filename: scanner.c
/* PURPOSE:
 *    SCANNER.C: Functions implementing a Lexical Analyzer (Scanner)
 *    as required for CST8152, Assignment #2
 *    scanner_init() must be called before using the scanner.
 *    The file is incomplete;
 *    Author: Victor Fernandes, 040772243
 *    Provided by: Svillen Ranev
 *    Version: 1.17.1
 *    Date: 30 January 2017
 */

/* The #define _CRT_SECURE_NO_WARNINGS should be used in MS Visual Studio projects
 * to suppress the warnings about using "unsafe" functions like fopen()
 * and standard sting library functions defined in string.h.
 * The define does not have any effect in Borland compiler projects.
 */
#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>   /* standard input / output */
#include <ctype.h>   /* conversion functions */
#include <stdlib.h>  /* standard library functions and constants */
#include <string.h>  /* string functions */
#include <limits.h>  /* integer types constants */
#include <float.h>   /* floating-point types constants */

/*#define NDEBUG        to suppress assert() call */
#include <assert.h>  /* assert() prototype */

/* project header files */
#include "buffer.h"
#include "token.h"
#include "table.h"

#define DEBUG  /* for conditional processing */
#undef  DEBUG

/* Global objects - variables */
/* This buffer is used as a repository for string literals.
   It is defined in platy_st.c */
extern Buffer * str_LTBL; /*String literal table */
int line; /* current line number of the source code */
extern int scerrnum;     /* defined in platy_st.c - run-time error number */

/* Local(file) global objects - variables */
static Buffer *lex_buf;/*pointer to temporary lexeme buffer*/

/* No other global variable declarations/definitiond are allowed */

/* scanner.c static(local) function  prototypes */ 
static int char_class(char c); /* character class function */
static int get_next_state(int, char, int *); /* state machine function */
static int iskeyword(char * kw_lexeme); /*keywords lookup functuion */
static long atool(char * lexeme); /* converts octal string to decimal value */

int scanner_init(Buffer * sc_buf) {
  	if(b_isempty(sc_buf)) return EXIT_FAILURE;/*1*/
	/* in case the buffer has been read previously  */
	b_setmark(sc_buf, 0);
	b_retract_to_mark(sc_buf);
	b_reset(str_LTBL);
	line = 1;
	return EXIT_SUCCESS;/*0*/
/*   scerrnum = 0;  *//*no need - global ANSI C */
}

Token malar_next_token(Buffer * sc_buf)
{
   Token t; /* token to return after recognition */
   unsigned char c; /* input symbol */
   int state = 0; /* initial state of the FSM */
   short lexstart;  /*start offset of a lexeme in the input buffer */
   short lexend;    /*end   offset of a lexeme in the input buffer */
   int accept = NOAS; /* type of state - initially not accepting */                                     
/* 
lexstart is the offset from the beginning of the char buffer of the
input buffer (sc_buf) to the first character of the current lexeme,
which is being processed by the scanner.
lexend is the offset from the beginning of the char buffer of the
input buffer (sc_buf) to the last character of the current lexeme,
which is being processed by the scanner.

*/ 
        
        
        //DECLARE YOUR VARIABLES HERE IF NEEDED 
        
                
        while (1){ /* endless loop broken by token returns it will generate a warning */
                
        //GET THE NEXT SYMBOL FROM THE INPUT BUFFER 
        
        c = b_getc(sc_buf);
		switch (c) {
			case 255: t.code = SEOF_T; return t; /* EOF */
			case '\0': t.code = SEOF_T; return t; /* Source EOF */
			case '\n': line++; continue; /* Ignore new line, increment line count */
			case '\r': line++; continue; /* CR, increment line count*/
			case ' ': continue; /* Ignore white space */
			case ';': t.code = EOS_T; return t; /* End of statement */
			case ',': t.code = COM_T; return t; /* Comma */
			case '{': t.code = RBR_T; return t; /* Right brace */
			case '}': t.code = LBR_T; return t; /* Left brace */
			case '(': t.code = RPR_T; return t; /* Right parenthesis */
			case ')': t.code = LPR_T; return t; /* Left parenthesis */
			case '+': t.code = ART_OP_T; t.attribute.arr_op = PLUS; return t; /* Addition operator */
			case '-': t.code = ART_OP_T; t.attribute.arr_op = MINUS; return t; /* Substraction operator */
			case '*': t.code = ART_OP_T; t.attribute.arr_op = MULT; return t; /* Multiplication operator */
			case '/': t.code = ART_OP_T; t.attribute.arr_op = DIV; return t; /* Devision operator */
			case '>': t.code = REL_OP_T; t.attribute.rel_op = GT; return t; /* Greater-than relational operator */
			case '<':
				if (c = b_getc(sc_buf) == '>') {
					t.code = REL_OP_T;
					t.attribute.rel_op = NE; /* Negation operator */
					return t;
				}
				else if (c == '<') {
					t.code == SCC_OP_T; /* String concatenation operator */
				}
				else {
					t.code = REL_OP_T;
					t.attribute.rel_op = LT; /* Less-than operator */
				}
				b_retract(sc_buf);
				c = b_getc(sc_buf);
				return t;
			case '.':
				b_setmark(sc_buf, b_getcoffset(sc_buf)
			default: /* TODO: Do alpha [a-zA-Z] stuff here*/
		}

              
/* special cases or token driven processing */

WRITE YOUR CODE FOR PROCESSING THE SPECIAL CASES HERE. 
COMMENTS AND STRING LITERALS ARE ALSO PROCESSED HERE.

WHAT FOLLOWS IS A PSEUDO CODE. YOU CAN USE switch STATEMENT
INSTEAD OF if-else TO PROCESS THE SPECIAL CASES
DO NOT FORGET TO COUNT THE PROGRAM LINES
   
             
   IF (c == SOME CHARACTER)  
                       ...
       SKIP CHARACTER (FOR EXAMPLE SPACE)
       continue;      
       OR SET TOKEN (SET TOKEN CODE AND TOKEN ATTRIBUTE(IF AVAILABLE))
       return t;
   EXAMPLE:
   if (c == ' ') continue;
   if (c == '{'){ t.code = RBR_T; /*no attribute */ return t; 
   if (c == '+'){ t.code = ART_OP_T; t.attribute.arr_op = PLUS */ return t;                 
   ...
   
   IF (c == '.') TRY TO PROCESS .AND. or .OR.
   IF SOMETHING ELSE FOLLOWS . OR THE LAST . IS MISSING
   RETURN AN ERROR TOKEN                                               
   IF (c == '!') TRY TO PROCESS COMMENT
   IF THE FOLLOWING IS NOT CHAR IS NOT < REPORT AN ERROR
   ELSE IN A LOOP SKIP CHARACTERS UNTIL line terminator is found THEN continue;
   ...
   IF STRING (FOR EXAMPLE, "text") IS FOUND      
      SET MARK TO MARK THE BEGINNING OF THE STRING
      IF THE STRING IS LEGAL   
         USING b_addc(..)COPY THE text FROM INPUT BUFFER INTO str_LTBL 
         ADD '\0' at the end make the string C-type string 
         SET STRING TOKEN
         (the attribute of the string token is the offset from
         the beginning of the str_LTBL char buffer to the beginning 
         of the string (TEXT in the example)) 
 
         return t;
      ELSE  
        THE STRING LITERAL IS ILLEGAL
        SET ERROR TOKEN FOR ILLEGAL STRING (see assignment)
        DO NOT STORE THE ILLEGAL STRINg IN THE str_LTBL

        return t;
   
   IF (c == ANOTHER CHARACTER)        
     SET TOKEN
     return t;                 
/* Process state transition table */  
        
  IF (c is a digit OR c is a letter){
  
  SET THE MARK AT THE BEGINING OF THE LEXEME
  b_setmark(sc_buf,forward);                      
    ....
  CODE YOUR FINATE STATE MACHINE HERE (FSM or DFA)
  IT IMPLEMENTS THE FOLLOWING ALGORITHM:
  
  FSM0. Begin with state = 0 and the input character c 
  FSM1. Get the next state from the transition table calling                       
        state = get_next_state(state, c, &accept);
  FSM2. Get the next character
  FSM3. If the state is not accepting (accept == NOAS), go to step FSM1
        If the step is accepting, token is found, leave the machine and
        call an accepting function as described below.     
   
                        
  RETRACT  getc_offset IF THE FINAL STATE IS A RETRACTING FINAL STATE
  GET THE BEGINNING AND THE END OF THE LEXEME
  lexstart = b_getmark(sc_buf);
  SET lexend TO getc_offset USING AN APPROPRIATE BUFFER FUNCTION
  CREATE  A TEMPORRARY LEXEME BUFFER HERE;
  lex_buf = b_create(...);
   . RETRACT getc_offset to the MARK SET PREVIOUSLY AT THE BEGINNING OF THE LEXEME AND
   . USING b_getc() COPY THE LEXEME BETWEEN lexstart AND lexend FROM THE INPUT BUFFER INTO lex_buf USING b_addc(...),
   . WHEN VID (KEYWORDS INCLUDED), FPL OR IL IS RECOGNIZED
   . YOU MUST CALL THE ACCEPTING FUNCTION USING THE ARRAY aa_table ,WHICH
   . CONTAINS POINTERS TO FUNCTIONS. THE ARRAY INDEX OF THE FUNCTION TO BE
   . CALLED IS STORED IN THE VARIABLE state.
   . YOU ARE NOT ALLOWED TO CALL ANY OF THE ACCEPTING FUNCTIONS BY NAME.
   . THE ARGUMENT TO THE FUNCTION IS THE STRING STORED IN lex_buf.
   ....
   b_free(lex_buf);
   return t;
      
     CHECK OTHER CHARS HERE if NEEDED, SET A TOKEN AND RETURN IT.
     FOR ILLEGAL CHARACTERS SET ERROR TOKEN. 
     THE ILLEGAL CHAR IS THE ATTRIBUTE OF THE ERROR TOKEN 
     IN A CASE OF RUNTIME ERROR, THE FUNCTION MUST STORE 
     A NON-NEGATIVE NUMBER INTO THE GLOBAL VARIABLE scerrnum
     AND RETURN AN ERROR TOKEN. THE ERROR TOKEN ATTRIBUTE MUST
     BE THE STRING "RUN TIME ERROR: "                
   }//end while(1)
}


DO NOT MODIFY THE CODE OF THIS FUNCTION
YOU CAN REMOVE THE COMMENTS

int get_next_state(int state, char c, int *accept)
{
	int col;
	int next;
	col = char_class(c);
	next = st_table[state][col];
#ifdef DEBUG
printf("Input symbol: %c Row: %d Column: %d Next: %d \n",c,state,col,next);
#endif
/*
The assert(int test) macro can be used to add run-time diagnostic to programs
and to "defend" from producing unexpected results.
assert() is a macro that expands to an if statement;
if test evaluates to false (zero) , assert aborts the program
(by calling abort()) and sends the following message on stderr:

Assertion failed: test, file filename, line linenum

The filename and linenum listed in the message are the source file name
and line number where the assert macro appears.
If you place the #define NDEBUG directive ("no debugging")
in the source code before the #include <assert.h> directive,
the effect is to comment out the assert statement.
*/
       assert(next != IS);

/*
The other way to include diagnostics in a program is to use
conditional preprocessing as shown bellow. It allows the programmer
to send more details describing the run-time problem. 
Once the program is tested thoroughly #define DEBUG is commented out
or #undef DEBUF is used - see the top of the file.
*/ 
#ifdef DEBUG
	if(next == IS){
	  printf("Scanner Error: Illegal state:\n");
	  printf("Input symbol: %c Row: %d Column: %d\n",c,state,col);
	  exit(1);
	}
#endif
	*accept = as_table[next];
	return next;
}

int char_class (char c)
{
        int val;

THIS FUNCTION RETURNS THE COLUMN NUMBER IN THE TRANSITION
TABLE st_table FOR THE INPUT CHARACTER c.
SOME COLUMNS MAY REPRESENT A CHARACTER CLASS .
FOR EXAMPLE IF COLUMN 1 REPRESENTS [A-Z]
THE FUNCTION RETURNS 1 EVERY TIME c IS ONE
OF THE LETTERS A,B,...,Z.
        
        return val;
}



HERE YOU WRITE THE DEFINITIONS FOR YOUR ACCEPTING FUNCTIONS. 
************************************************************

ACCEPTING FUNCTION FOR THE arithmentic variable identifier AND keywords (VID - AVID/KW)
REPLACE XX WITH THE CORRESPONDING ACCEPTING STATE NUMBER

Token aa_funcXX(char lexeme[]){

WHEN CALLED THE FUNCTION MUST
1. CHECK IF THE LEXEME IS A KEYWORD.
   IF YES, IT MUST RETURN A TOKEN WITH THE CORRESPONDING ATTRIBUTE
   FOR THE KEYWORD. THE ATTRIBUTE CODE FOR THE KEYWORD
   IS ITS INDEX IN THE KEYWORD LOOKUP TABLE (kw_table in table.h).
   IF THE LEXEME IS NOT A KEYWORD, GO TO STEP 2.

2. SET a AVID TOKEN.
   IF THE lexeme IS LONGER than VID_LEN (see token.h) CHARACTERS,
   ONLY FIRST VID_LEN CHARACTERS ARE STORED 
   INTO THE VARIABLE ATTRIBUTE ARRAY vid_lex[](see token.h) .
   ADD \0 AT THE END TO MAKE A C-type STRING.
  return t;
}

ACCEPTING FUNCTION FOR THE string variable identifier (VID - SVID)
REPLACE XX WITH THE CORRESPONDING ACCEPTING STATE NUMBER

Token aa_funcXX(char lexeme[]){

WHEN CALLED THE FUNCTION MUST
1. SET a SVID TOKEN.
   IF THE lexeme IS LONGER than VID_LEN characters,
   ONLY FIRST VID_LEN-1 CHARACTERS ARE STORED
   INTO THE VARIABLE ATTRIBUTE ARRAY vid_lex[],
   AND THEN THE # CHARACTER IS APPENDED TO THE NAME.
   ADD \0 AT THE END TO MAKE A C-type STRING.
  
  return t;
}

ACCEPTING FUNCTION FOR THE floating-point literal (FPL)

Token aa_funcXX(char lexeme[]){

THE FUNCTION MUST CONVERT THE LEXEME TO A FLOATING POINT VALUE,
WHICH IS THE ATTRIBUTE FOR THE TOKEN.
THE VALUE MUST BE IN THE SAME RANGE AS the value of 4-byte float in C.
IN CASE OF ERROR (OUT OF RANGE) THE FUNCTION MUST RETURN ERROR TOKEN
THE ERROR TOKEN ATTRIBUTE IS  lexeme. IF THE ERROR lexeme IS LONGER
than ERR_LEN characters, ONLY THE FIRST ERR_LEN-3 characters ARE
STORED IN err_lex. THEN THREE DOTS ... ARE ADDED TO THE END OF THE
err_lex C-type string. 
  return t;
}

ACCEPTING FUNCTION FOR THE integer literal(IL) - decimal constant (DIL)

Token aa_funcXX(char lexeme[]){

THE FUNCTION MUST CONVERT THE LEXEME REPRESENTING A DECIMAL CONSTANT
TO A DECIMAL INTEGER VALUE, WHICH IS THE ATTRIBUTE FOR THE TOKEN.
THE VALUE MUST BE IN THE SAME RANGE AS the value of 2-byte integer in C.
IN CASE OF ERROR (OUT OF RANGE) THE FUNCTION MUST RETURN ERROR TOKEN
THE ERROR TOKEN ATTRIBUTE IS  lexeme. IF THE ERROR lexeme IS LONGER
than ERR_LEN characters, ONLY THE FIRST ERR_LEN-3 characters ARE
STORED IN err_lex. THEN THREE DOTS ... ARE ADDED TO THE END OF THE
err_lex C-type string. 
  return t;
}

ACCEPTING FUNCTION FOR THE integer literal(IL) - octal constant (OIL)

Token aa_funcXX(char lexeme[]){

THE FUNCTION MUST CONVERT THE LEXEME REPRESENTING AN OCTAL CONSTANT
TO A DECIMAL INTEGER VALUE WHICH IS THE ATTRIBUTE FOR THE TOKEN.
THE VALUE MUST BE IN THE SAME RANGE AS the value of 2-byte integer in C.
THIS FUNCTION IS SIMILAR TO THE FUNCTION ABOVE AND THEY CAN BE
COMBINED INTO ONE FUNCTION
THE MAIN DIFFERENCE IE THAT THIS FUNCTION CALLS
THE FUNCTION atool(char * lexeme) WHICH CONVERTS AN ASCII STRING
REPRESENTING AN OCTAL NUMBER TO INTEGER VALUE
IN CASE OF ERROR (OUT OF RANGE) THE FUNCTION MUST RETURN ERROR TOKEN
THE ERROR TOKEN ATTRIBUTE IS  lexeme. IF THE ERROR lexeme IS LONGER
than ERR_LEN characters, ONLY THE FIRST ERR_LEN-3 characters ARE
STORED IN err_lex. THEN THREE DOTS ... ARE ADDED TO THE END OF THE
err_lex C-type string. 

  return t;
}

ACCEPTING FUNCTION FOR THE ERROR TOKEN 

Token aa_funcXX(char lexeme[]){

THE FUNCTION SETS THE ERROR TOKEN. lexeme[] CONTAINS THE ERROR
THE ATTRIBUTE OF THE ERROR TOKEN IS THE lexeme ITSELF
AND IT MUST BE STORED in err_lex. IF THE ERROR lexeme IS LONGER
than ERR_LEN characters, ONLY THE FIRST ERR_LEN-3 characters ARE
STORED IN err_lex. THEN THREE DOTS ... ARE ADDED TO THE END OF THE
err_lex C-type string. 

  return t;
}


CONVERSION FUNCTION

long atool(char * lexeme){

THE FUNCTION CONVERTS AN ASCII STRING
REPRESENTING AN OCTAL INTEGER CONSTANT TO INTEGER VALUE
}

HERE YOU WRITE YOUR ADDITIONAL FUNCTIONS (IF ANY).
FOR EXAMPLE

int iskeyword(char * kw_lexeme){}