/* Filename: table.h
 * Transition Table and function declarations necessary for the scanner implementation
 * as required for CST8152 - Assignment #2.
 * Author: Victor Fernandes, 040772243
 * Version: 1.17.1
 * Date: 30 January 2017
 * Provided by: Svillen Ranev
 * The file is incomplete. You are to complete it.
 ***************************************************
 * REPLACE THIS HEADER WITH YOUR HEADER
 ***************************************************
 */

#ifndef  TABLE_H_
#define  TABLE_H_ 

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef NULL
#include <_null.h> /* NULL pointer constant is defined there */
#endif

 /*   Source end-of-file (SEOF) sentinel symbol
  *    '\0' or only one of the folowing constants: 255, 0xFF , EOF
  */

  /*  Single-lexeme tokens processed separately one by one
   *  in the token-driven part of the scanner
   *  '=' , ' ' , '(' , ')' , '{' , '}' , == , <> , '>' , '<' ,
   *       space
   *  !<comment , ',' , '"' , ';' , '-' , '+' , '*' , '/', << ,
   *  .AND., .OR. , SEOF, 'wrong symbol',
   */


   //REPLACE *ESN* WITH YOUR ERROR STATE NUMBER 
#define ES -2 /* Error state */
#define IS -1    /* Inavalid state */

/* State transition table definition */

//REPLACE *CN* WITH YOUR COLUMN NUMBER  

#define TABLE_COLUMNS 14
/*transition table - type of states defined in separate table */
int  st_table[][TABLE_COLUMNS] = {
	/*	INPUT COLUMNS:
				  [a-zA-Z]| 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 |  . | #  | other
	*/
	/* State 0 */	{1,     6 , 4 , 4 , 4 , 4 , 4 , 4 , 4 , 4 , 4 , IS , IS ,IS},
	/* State 1 */	{1,     1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 , ES , 3  , 2},
	/* State 2 */	{IS,   IS , IS, IS, IS, IS, IS, IS, IS, IS, IS, IS , IS , IS},
	/* State 3 */	{IS,   IS , IS, IS, IS, IS, IS, IS, IS, IS, IS, IS , IS , IS},
	/* State 4 */	{ES,    4 , 4 , 4 , 4 , 4 , 4 , 4 , 4 , 4 , 4 ,  7 ,  5 , 5},
	/* State 5 */	{IS,   IS , IS, IS, IS, IS, IS, IS, IS, IS, IS, IS , IS , IS},
	/* State 6 */	{ES,    9 ,  9,  9,  9,  9,  9,  9,  9, ES, ES,  7 , ES ,  5},
	/* State 7 */	{ES,    7 ,  7,  7,  7,  7,  7,  7,  7,  7,  7, ES ,  8 ,  8},
	/* State 8 */	{IS,   IS , IS, IS, IS, IS, IS, IS, IS, IS, IS, IS , IS , IS},
	/* State 9 */	{ES,    9 ,  9,  9,  9,  9,  9,  9,  9, ES, ES, ES , ES , 10},
	/* State 10 */	{IS,   IS , IS, IS, IS, IS, IS, IS, IS, IS, IS, IS , IS , IS},
	/* State 11 */	{ES,   ES , ES, ES, ES, ES, ES, ES, ES, ES, ES, ES , ES , ES},
	/* State 12 */	{IS,   IS , IS, IS, IS, IS, IS, IS, IS, IS, IS, IS , IS , IS},
	/* State 13 */	{IS,   IS , IS, IS, IS, IS, IS, IS, IS, IS, IS, IS , IS , IS}
	//
	//. YOUR TABLE INITIALIZATION HERE
	//.
	///* State N */  {YOUR INITIALIZATION}, 
};
/* Accepting state table definition */
//REPLACE *N1*, *N2*, and *N3* WITH YOUR NUMBERS
#define ASWR     1  /* accepting state with retract */
#define ASNR     2  /* accepting state with no retract */
#define NOAS     0  /* not accepting state */

int as_table[] = {

	/* State 0 */	NOAS,
	/* State 1 */	NOAS,
	/* State 2 */	ASWR,
	/* State 3 */	ASNR,
	/* State 4 */	NOAS,
	/* State 5 */	ASWR,
	/* State 6 */	NOAS,
	/* State 7 */	NOAS,
	/* State 8 */	ASWR,
	/* State 9 */	NOAS,
	/* State 10 */	ASWR,
	/* State 11 */	ASNR,
	/* State 12 */	ASNR,
	/* State 13 */	ASWR

};

/* Accepting action function declarations */

//FOR EACH OF YOUR ACCEPTING STATES YOU MUST PROVIDE
//ONE FUNCTION PROTOTYPE. THEY ALL RETURN Token AND TAKE
//ONE ARGUMENT: A string REPRESENTING A TOKEN LEXEME. 

// Example: Token aa_funcXX(char *lexeme);

Token aa_func02(char* lexeme); // VID AVID/KW
Token aa_func03(char *lexeme); // VID SVID
Token aa_func05(char *lexeme); // DIL
Token aa_func08(char *lexeme); // FPL
Token aa_func10(char *lexeme); // OIL
Token aa_func11(char *lexeme); // ES
//Replace XX with the number of the accepting state: 02, 03 and so on.

/* defining a new type: pointer to function (of one char * argument)
   returning Token
*/

typedef Token(*PTR_AAF)(char *lexeme);


/* Accepting function (action) callback table (array) definition */
/* If you do not want to use the typedef, the equvalent declaration is:
 * Token (*aa_table[])(char lexeme[]) = {
 */

PTR_AAF aa_table[] = {

	/* State 0 */	NULL,
	/* State 1 */	NULL,
	/* State 2 */	aa_func02,
	/* State 3 */	aa_func03,
	/* State 4 */	NULL,
	/* State 5 */	aa_func05,
	/* State 6 */	NULL,
	/* State 7 */	NULL,
	/* State 8 */	aa_func08,
	/* State 9 */	NULL,
	/* State 10 */	aa_func10,
	/* State 11 */	aa_func11,
	/* State 12 */	aa_func11,
	/* State 13 */	aa_func11

	//HERE YOU MUST PROVIDE AN INITIALIZATION FOR AN ARRAY OF POINTERS
	//TO ACCEPTING FUNCTIONS. THE ARRAY HAS THE SAME SIZE AS as_table[ ].
	//YOU MUST INITIALIZE THE ARRAY ELEMENTS WITH THE CORRESPONDING
	//ACCEPTING FUNCTIONS (FOR THE STATES MARKED AS ACCEPTING IN as_table[]).
	//THE REST OF THE ELEMENTS MUST BE SET TO NULL.

};

/* Keyword lookup table (.AND. and .OR. are not keywords) */

#define KWT_SIZE  8

char * kw_table[] = {
					  "ELSE",
					  "IF",
					  "INPUT",
					  "OUTPUT",
					  "PLATYPUS",
					  "REPEAT",
					  "THEN",
					  "USING"
};

#endif
