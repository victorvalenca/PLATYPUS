/* Filename: scanner.c
   PURPOSE:
 * SCANNER.C: Functions implementing a Lexical Analyzer (Scanner)
 * as required for CST8152, Assignment #2
 * scanner_init() must be called before using the scanner.
 * The file is incomplete;
 * Author: Victor Fernandes, 040772243
 * Provided by: Svillen Ranev
 * Version: 1.17.1
 * Date: 30 January 2017
 * Function list: scanner_init, malar_next_token, get_next_state, char_class,
  aa_func02, aa_func03, aa_func05, aa_func08, aa_func10, aa_func12 aa_func13, atool, iskeyword
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
int line;                 /* current line number of the source code */
extern int scerrnum;      /* defined in platy_st.c - run-time error number */

/* Local(file) global objects - variables */
static Buffer *lex_buf;   /*pointer to temporary lexeme buffer*/

/* No other global variable declarations/definitiond are allowed */

/* scanner.c static(local) function  prototypes */
static int char_class(char c);               /* character class function */
static int get_next_state(int, char, int *); /* state machine function */
static int iskeyword(char * kw_lexeme);      /*keywords lookup functuion */
static long atool(char * lexeme);            /* converts octal string to decimal value */

/* Prepares the Scanner to read the source code buffer
 * Author: Svillen Ranev
 * Called functions: b_isempty, b_setmark, b_retract_to_mark, b_reset
 * Parameters:
 *	- pBuffer sc_buf
 * Return values:
 *  - 1 (failure), 0 (success)
*/
int scanner_init(Buffer * sc_buf) {
	if (b_isempty(sc_buf)) return EXIT_FAILURE;/*1*/
	/* in case the buffer has been read previously  */
	b_setmark(sc_buf, 0);
	b_retract_to_mark(sc_buf);
	b_reset(str_LTBL);
	line = 1;
	return EXIT_SUCCESS;/*0*/
/*   scerrnum = 0;  *//*no need - global ANSI C */
}
/* Reads the source code buffer and generates a token
 * Author: Victor Fernandes
 * Version: 0.0.1
 * Called functions: aa_table[], b_getc, b_setmark, b_getcoffset, b_retract_to_mark,
 b_retract, b_mark, b_eob, b_create, b_addc, b_free, isalpha, isalnum, get_next_state
 * Parameters:
  - pBuffer sc_buf
 * Return values: Token
 * Algorithm:
   Read a character from the source buffer, one by one, and match string patterns to tokens.
   If an illegal sequence is found while starting a pattern off of the first matching character,
   it returns a token with an error code with the infringing character. If the scanner matches
   a valid pattern it returns a Token with the appropriate code.
*/
Token malar_next_token(Buffer * sc_buf)
{
	Token t;           /* token to return after recognition */
	unsigned char c;   /* input symbol */
	int state = 0;     /* initial state of the FSM */
	short lexstart;    /* start offset of a lexeme in the input buffer */
	short lexend;      /* end offset of a lexeme in the input buffer */
	int accept = NOAS; /* type of state - initially not accepting */

	/* Counter for loops in string error case */
	int i;
	/*String offset for the str_LTBL*/
	static short str_offset = 0;
	/* temporary buffer used to store an erroneous string literal*/
	pBuffer err_lex_buf;

	if (sc_buf == NULL) {
		scerrnum = 1;
		return aa_table[ES]("RUN TIME ERROR: "); /* WHOOPS */
	}

	while (1) { /* endless loop broken by token returns; it will generate a warning */

		/* Get symbol from buffer */
		c = b_getc(sc_buf);

		switch (c) {
		case 255: t.code = SEOF_T; return t; /* EOF */
		case '\0': t.code = SEOF_T; return t; /* Source EOF */
		case '\n': line++; continue; /* Ignore new line, increment line count */
		case '\r': line++; continue; /* CR, increment line count*/
		case ' ': continue; /* Ignore white space */
		case '\t': continue; /* Ignore tabs */
		case ';': t.code = EOS_T; return t; /* End of statement */
		case ',': t.code = COM_T; return t; /* Comma */
		case '{': t.code = LBR_T; return t; /* Left brace */
		case '}': t.code = RBR_T; return t; /* Right brace */
		case '(': t.code = LPR_T; return t; /* Left parenthesis */
		case ')': t.code = RPR_T; return t; /* Right parenthesis */
		case '+': t.code = ART_OP_T; t.attribute.arr_op = PLUS; return t; /* Addition operator */
		case '-': t.code = ART_OP_T; t.attribute.arr_op = MINUS; return t; /* Substraction operator */
		case '*': t.code = ART_OP_T; t.attribute.arr_op = MULT; return t; /* Multiplication operator */
		case '/': t.code = ART_OP_T; t.attribute.arr_op = DIV; return t; /* Devision operator */
		case '>': t.code = REL_OP_T; t.attribute.rel_op = GT; return t; /* Greater-than relational operator */
		case '<':
			c = b_getc(sc_buf);
			if (c == '>') {
				t.code = REL_OP_T;
				t.attribute.rel_op = NE; /* Negation operator */
				return t;
			}
			else if (c == '<') {
				t.code = SCC_OP_T; /* String concatenation operator */
				return t;
			}
			else {
				t.code = REL_OP_T;
				t.attribute.rel_op = LT; /* Less-than operator */
				b_retract(sc_buf);
				return t;
			}

		case '.':
			b_setmark(sc_buf, b_getcoffset(sc_buf)); /* Set mark before continuing (AND|OR case) */
			c = b_getc(sc_buf);
			if (c == 'A' && b_getc(sc_buf) == 'N' && b_getc(sc_buf) == 'D' && b_getc(sc_buf) == '.') {
				t.code = LOG_OP_T;
				t.attribute.log_op = AND;
				return t;
			}
			else if (c == 'O' && b_getc(sc_buf) == 'R' && b_getc(sc_buf) == '.') {
				t.code = LOG_OP_T;
				t.attribute.log_op = OR;
				return t;
			}
			t.code = ERR_T; /* "That character's not supposed to be here" case */
			t.attribute.err_lex[0] = '.';
			t.attribute.err_lex[1] = '\0';
			b_retract_to_mark(sc_buf);
			return t;
		case '!':
			c = b_getc(sc_buf);
			if (c == '<') { /* It's a comment line */
				/* Consume chars until line ends */
				for (; c != '\0' && c != '\r' && c != '\n' && c != 255; c = b_getc(sc_buf));
				line++;
				continue;
			}
			else { /* Bad character, pump out an error token */
				t.code = ERR_T;
				b_retract(sc_buf);
				b_retract(sc_buf); /* Retract twice to re-read '!' */
				t.attribute.err_lex[0] = c = b_getc(sc_buf);
				t.attribute.err_lex[1] = c = b_getc(sc_buf);
				t.attribute.err_lex[2] = '\0';
				/* Consume the rest of the caracters to ignore the line*/
				for (; c != '\0' && c != '\r' && c != '\n' && c != 255; c = b_getc(sc_buf));
				return t;
			}
		case '=':
			c = b_getc(sc_buf);
			if (c == '=') { /* Relational equals-to operator */
				t.code = REL_OP_T;
				t.attribute.rel_op = EQ;
				return t;
			}
			b_retract(sc_buf);
			t.code = ASS_OP_T; /* Assignment operator */
			return t;
		case '\"': /* Don't quote me on this */

			/* Track the beginning of string */
			b_setmark(sc_buf, b_getcoffset(sc_buf));
			lexstart = b_mark(sc_buf);
			lexend = lexstart;
			c = b_getc(sc_buf);
			/* Step through the string literal and track progress  */
			for (; c != '\"'; c = b_getc(sc_buf), ++lexend) {
				if (c == '\n' || c == '\r')
					++line;
				if (c == '\0' || b_eob(sc_buf)) { /* Illegal string, make it an error token */
					b_retract_to_mark(sc_buf);
					b_retract(sc_buf); /* Retract one more time to re-read '"' into err_lex */
					t.code = ERR_T;

					err_lex_buf = b_create(1, 1, 'a'); /* Start up temporary buffer */

					c = b_getc(sc_buf);
					for (i = 0; i < (lexend - lexstart); c = b_getc(sc_buf), ++i) {
						/* Continue until the end of the lexeme where error was found */
						if (i < (ERR_LEN) || c != 255)
							b_addc(err_lex_buf, c);
					}
					/* Pass the complete erroneous string to error state accepting function*/
					t = aa_table[ES](b_setmark(err_lex_buf, 0));
					b_free(err_lex_buf); /* Clean up the temporary buffer */
					return t;
				}
			} /* end for loop, string finished and considered valid */
			b_retract_to_mark(sc_buf);

			/* Copy the matched string literal to str_LTBL */
			t.attribute.str_offset = str_offset;
			c = b_getc(sc_buf);
			for (; lexstart < lexend; c = b_getc(sc_buf), ++lexstart, ++str_offset) {
				b_addc(str_LTBL, c);
			}
			b_addc(str_LTBL, '\0'); ++str_offset; t.code = STR_T;
			return t;

		default:
			if (isalpha(c) || isalnum(c)) {

				/*Set mark to beginning of lexeme*/
				b_retract(sc_buf);
				b_setmark(sc_buf, b_getcoffset(sc_buf));
				lexstart = b_mark(sc_buf);
				lexend = lexstart;
				state = 0;

				while (accept == NOAS) {
					state = get_next_state(state, b_getc(sc_buf), &accept);
					if (accept != NOAS) { break; }
				}

				/*
				 * Entering Accepting State
				 */

				if (as_table[state] == ASWR) { b_retract(sc_buf); }

				/* Get end of lexeme */
				lexend = b_getcoffset(sc_buf);

				b_retract_to_mark(sc_buf);

				lex_buf = b_create(1, 1, 'a');

				/* Copy the scanned lexeme into lexical buffer */
				for (; lexstart < lexend; ++lexstart) {
					b_addc(lex_buf, b_getc(sc_buf));
				}
				b_addc(lex_buf, '\0');

				if (aa_table[state] != NULL) {
					t = aa_table[state](b_setmark(lex_buf, 0));
				}
				else {
					scerrnum = 1;
					t = aa_table[ES]("RUN TIME ERROR: ");
					return t;
				}
				b_free(lex_buf);
			}

			/* Invalid character */
			else {
				t.code = ERR_T;
				t.attribute.err_lex[0] = c;
				t.attribute.err_lex[1] = '\0';
			}
			return t;
		}



	} /*end while(1)*/
}

/* Looks up the transition table for the next state given the input character
   Author: Victor Fernandes
   Version: 0.0.1
   Called functions: char_class, assert, printf, as_table
   Parameters:
	- int state: the starting point for the transition table lookup
	- char c: the input character for table lookup
	- int *accept: pointer to the accepting state of the scanner
   Return values: int (the next state value of the scanner)
*/
int get_next_state(int state, char c, int *accept)
{
	int col;
	int next;
	col = char_class(c);
	next = st_table[state][col];
#ifdef DEBUG
	printf("Input symbol: %c Row: %d Column: %d Next: %d \n", c, state, col, next);
#endif

	assert(next != IS);

#ifdef DEBUG
	if (next == IS) {
		printf("Scanner Error: Illegal state:\n");
		printf("Input symbol: %c Row: %d Column: %d\n", c, state, col);
		exit(1);
	}
#endif
	*accept = as_table[next];
	return next;
}
/* Matches the column value in the transition table to the given input character
   Author: Victor Fernandes
   Version: 0.0.1
   Called functions: N/A
   Parameters:
	- char c: the input character to be matched in the transition table
   Return values: int (the value representing the column in the transition table)
*/
int char_class(char c)
{
	int val;

	if (isalpha(c))
		val = 0;
	else if (c == '0')
		val = 1;
	else if (c > '0' && c < '8')
		val = 2;
	else if (c == '8' || c == '9')
		val = 3;
	else if (c == '.')
		val = 4;
	else if (c == '#')
		val = 5;
	else
		val = 6;

	return val;
}


/*
HERE YOU WRITE THE DEFINITIONS FOR YOUR ACCEPTING FUNCTIONS.
************************************************************

ACCEPTING FUNCTION FOR THE arithmentic variable identifier AND keywords (VID - AVID/KW)
REPLACE XX WITH THE CORRESPONDING ACCEPTING STATE NUMBER
*/
/* Generates a token for an arithmetic variable identifer or keyword
   Author: Victor Fernandes
   Version: 0.0.1
   Called functions: iskeyword, calloc, aa_table[], strlen, strncpy, free
   Parameters:
	- char* lexeme: the string pattern matched by the FA
   Return values: Token
*/
Token aa_func02(char lexeme[]) {
	unsigned int i, kw_idx; /* Variable to contain keyword table index */
	Token t;
	char* temp_str;

#ifdef DEBUG
	printf("Lexeme: '%s'\n", lexeme);
#endif

	kw_idx = iskeyword(lexeme);
	if (kw_idx != -1) { /* Keyword check */
		t.code = KW_T;
		t.attribute.kwt_idx = kw_idx;
		return t;
	}

	/* Not a keyword? Must be AVID*/
	t.code = AVID_T;
	if ((temp_str = (char*)calloc(VID_LEN + 1, sizeof(char))) == NULL) {
		return aa_table[ES]("RUN TIME ERROR: ");
	}

	for (i = 0; i < (VID_LEN) && i < strlen(lexeme); i++) {
		temp_str[i] = lexeme[i];
	}

	strncpy(t.attribute.vid_lex, temp_str, VID_LEN);
	t.attribute.vid_lex[strlen(temp_str)] = '\0';
	free(temp_str);

	switch (lexeme[0]) { /* Read first character of lexeme for implicit type (not used yet?)*/
	case 'i':
	case 'o':
	case 'd':
	case 'n':
		/* Integer */
		break;
	default:
		/* Floating point*/
		break;
	}

	return t;

	/*
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
		*/
}
/*
ACCEPTING FUNCTION FOR THE string variable identifier (VID - SVID)
REPLACE XX WITH THE CORRESPONDING ACCEPTING STATE NUMBER
*/
/* Generates a token for an string variable identifer
   Author: Victor Fernandes
   Version: 0.0.1
   Called functions: calloc, aa_table[], strlen, strncpy, free
   Parameters:
	- char* lexeme: the string pattern matched by the FA
   Return values: Token
*/
Token aa_func03(char lexeme[]) {
	Token t;
	unsigned int i;
	char* temp_str;
	if ((temp_str = (char*)calloc(VID_LEN + 2, sizeof(char))) == NULL) {
		return aa_table[ES]("RUN TIME ERROR: ");
	}

	for (i = 0; i < (VID_LEN) && i < strlen(lexeme); i++) {
		temp_str[i] = lexeme[i];
	}

	temp_str[strlen(temp_str) - 1] = '#'; /* Add# to end of the SVID */
	temp_str[strlen(temp_str)] = '\0';

	strncpy(t.attribute.vid_lex, temp_str, VID_LEN);
	t.attribute.vid_lex[strlen(temp_str)] = '\0';
	free(temp_str);

	t.code = SVID_T;
	return t;

	/*
	WHEN CALLED THE FUNCTION MUST
	1. SET a SVID TOKEN.
	   IF THE lexeme IS LONGER than VID_LEN characters,
	   ONLY FIRST VID_LEN-1 CHARACTERS ARE STORED
	   INTO THE VARIABLE ATTRIBUTE ARRAY vid_lex[],
	   AND THEN THE # CHARACTER IS APPENDED TO THE NAME.
	   ADD \0 AT THE END TO MAKE A C-type STRING.
	  */
}

/*ACCEPTING FUNCTION FOR THE integer literal(IL)-decimal constant(DIL)*/
/* Generates a token for a decimal integer literal constant (DIL)
   Author: Victor Fernandes
   Version: 0.0.1
   Called functions: atol, aa_table[]
   Parameters:
	- char* lexeme: the string pattern matched by the FA
   Return values: Token
*/
Token aa_func05(char lexeme[]) {
	Token t;
	long temp_num;

	temp_num = atol(lexeme);

	if (temp_num > SHRT_MAX || temp_num < 0) { /* Overflow error */
		t = aa_table[ES](lexeme);
		return t;
	}
	t.code = INL_T;
	t.attribute.int_value = (int)temp_num;
	return t;
	/*
THE FUNCTION MUST CONVERT THE LEXEME REPRESENTING A DECIMAL CONSTANT
TO A DECIMAL INTEGER VALUE, WHICH IS THE ATTRIBUTE FOR THE TOKEN.
THE VALUE MUST BE IN THE SAME RANGE AS the value of 2-byte integer in C.
IN CASE OF ERROR (OUT OF RANGE) THE FUNCTION MUST RETURN ERROR TOKEN
THE ERROR TOKEN ATTRIBUTE IS  lexeme. IF THE ERROR lexeme IS LONGER
than ERR_LEN characters, ONLY THE FIRST ERR_LEN-3 characters ARE
STORED IN err_lex. THEN THREE DOTS ... ARE ADDED TO THE END OF THE
err_lex C-type string. */
}

/*ACCEPTING FUNCTION FOR THE floating - point literal (FPL)*/
/* Generates a token for a floating-point literal
   Author: Victor Fernandes
   Version: 0.0.1
   Called functions: strtof, aa_table[]
   Parameters:
	- char* lexeme: the string pattern matched by the FA
   Return values: Token
*/
Token aa_func08(char lexeme[]) {
	Token t;
	double temp_dbl = 0.0f;
	unsigned int i, check = 0;
	char* substr = (char*)calloc(ERR_LEN, sizeof(char));

	t.code = FPL_T;
	if (strcmp(lexeme, "0.0") == 0) {
		t.attribute.flt_value = 0.0f;
		return t;
	}
	else {
		for (i = 0; i < strlen(lexeme); ++i) {
			if (lexeme[i] == '.') {
				if (lexeme[i + 1] == '\0') {
					strncpy(substr, lexeme, i);
					substr[i] = '\0';
					temp_dbl = strtof(substr, NULL);
					check = TRUE;
				}
				else break;
			}
		}
	}  /* strtof() returns 0 if the value is out of range) */
	if (check != TRUE)
		temp_dbl = strtof(lexeme, NULL);
	t.attribute.flt_value = (float)temp_dbl;

#ifdef DEBUG
	printf("Lexeme: '%s' | FLT value: %f  | CHECK = %d\n", substr, temp_dbl, check);
#endif
	if ((temp_dbl > FLT_MAX) || ((temp_dbl != 0) && (temp_dbl < FLT_MIN))) { /* Overflow error */
		t = aa_table[ES](lexeme);
	}
	free(substr);
	return t;
	/*
THE FUNCTION MUST CONVERT THE LEXEME TO A FLOATING POINT VALUE,
WHICH IS THE ATTRIBUTE FOR THE TOKEN.
THE VALUE MUST BE IN THE SAME RANGE AS the value of 4-byte float in C.
IN CASE OF ERROR (OUT OF RANGE) THE FUNCTION MUST RETURN ERROR TOKEN
THE ERROR TOKEN ATTRIBUTE IS  lexeme. IF THE ERROR lexeme IS LONGER
than ERR_LEN characters, ONLY THE FIRST ERR_LEN-3 characters ARE
STORED IN err_lex. THEN THREE DOTS ... ARE ADDED TO THE END OF THE
err_lex C-type string. */
}



/*ACCEPTING FUNCTION FOR THE integer literal(IL) - octal constant (OIL)*/

/* Generates a token for an octal integer literal
   Author: Victor Fernandes
   Version: 0.0.1
   Called functions: strlen, aa_table[], atool
   Parameters:
	- char* lexeme: the string pattern matched by the FA
   Return values: Token
*/
Token aa_func10(char lexeme[]) {
	Token t;
	long new_olval;

	if (strlen(lexeme) > INL_LEN + 1) {
		t = aa_table[ES](lexeme);
	}

	t.code = INL_T;
	new_olval = atool(lexeme);

	if (new_olval < SHRT_MIN || new_olval > SHRT_MAX) {
		t = aa_table[ES](lexeme);
		return t;
	}

	t.code = INL_T;
	t.attribute.int_value = (int)new_olval;

	return t;
	/*
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
*/
}

/*ACCEPTING FUNCTION FOR THE ERROR TOKEN */
/* Generates a token for a general error token
   Author: Victor Fernandes
   Version: 0.0.1
   Called functions: aa_table[]
   Parameters:
	- char* lexeme: the string pattern matched by the FA
   Return values: Token
*/
Token aa_func12(char lexeme[]) {

	/*
	This function does the same as aa_func13, except that it is marked as
	non-retracting in the accepting function state, but the token is generated
	exactly the same way
	*/
	return aa_table[ESWR](lexeme);
}

/* Generates a token for a general error token
   Author: Victor Fernandes
   Version: 0.0.1
   Called functions: strlen, aa_table[]
   Parameters:
	- char* lexeme: the string pattern matched by the FA
   Return values: Token
*/
Token aa_func13(char lexeme[]) {
	Token t;
	unsigned int i;
	t.code = ERR_T;
	for (i = 0; i < (ERR_LEN) && i < strlen(lexeme); i++)
		t.attribute.err_lex[i] = lexeme[i];

	if (strlen(lexeme) > ERR_LEN) {
		t.attribute.err_lex[i - 1] = '.';
		t.attribute.err_lex[i - 2] = '.';
		t.attribute.err_lex[i - 3] = '.';
	}
	t.attribute.err_lex[i] = '\0';

	return t;
}


/*CONVERSION FUNCTION*/

/* Returns an octal representation of a string
   Author: Victor Fernandes
   Version: 0.0.1
   Called functions: N/A
   Parameters:
	- char* lexeme: the string pattern to convert
   Return values: long (integer representation of the octal string)
*/
long atool(char * lexeme) {
	int i, x = 1;
	long result = 0;

	for (i = strlen(lexeme); i > 0; i--, x *= 8)
		result += x*(lexeme[i - 1] - '0');
	return result;
}

/*HERE YOU WRITE YOUR ADDITIONAL FUNCTIONS (IF ANY).
FOR EXAMPLE*/

/* Looks up the string pattern on the keyword table
   Author: Victor Fernandes
   Version: 0.0.1
   Called functions: N/A
   Parameters:
	- char* lexeme: the string pattern to look up in kw_table
   Return values: int -1 (could not find a match),
  int [1 - KW_SIZE] index location of the matching keyword
*/
int iskeyword(char * kw_lexeme) {
	int i;

	if (kw_lexeme == NULL) return -1;

	for (i = 0; i < KWT_SIZE; i++) {
		if (strcmp(kw_table[i], kw_lexeme) == 0) { return i; }
	}
	return -1;
}