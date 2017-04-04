/* Filename: stable.h
 * Compiler: msvc (Visual Studio 2015)
 * Function declarations for the symbol table 
 * CST8152, Assignment #3 
 * Professor: Svillen Ranev
 * Author: Victor Fernandes, 040772243
 * Version: 0.1
 * Date: 24 March 2017
*/

#ifndef STABLE_H_
#define STABLE_H_
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "buffer.h"

/* Masks
   16-bit field  MSB-> 15 14 13 12 | 11 10  9  8 |  7  6  5  4 |  3  2  1  0 <-LSB
                        0  0  0  0 |  0  0  0  0 |  0  0  0  0 |  0  0  0  0
*/
#define DFT_MASK        0xFFF8 /* Default mask */
#define U_MASK          0x0001 /* Update mask */
#define INT_MASK        0x0004 /* Integer mask */
#define FLT_MASK        0x0002 /* Floating point mask */
#define STR_MASK        0x0006 /* String mask*/

#define CHK_MASK        0x0006 /* Type check mask */
#define DFT_U_MASK      0xFFF9 /* Default mask with update flag */

#define ST_FILE_NAME    "$stable.ste"

typedef union InitialValue {
    int int_val;     /* Integer variable initial value */
    float fpl_val;   /* Floating-point variable initial value */
    int str_offset;  /* String variable initial value (location offset) */
} Value;

typedef struct SymbolTableVidRecord {
    unsigned short status_field;  /* Variable record status field */
    char *plex;                   /* Pointer to lexeme (VID name) in CA (character array) */
    int o_line;                   /* Line of first occurence */
    Value i_value;                /* Variable initial value */
    void *reserved;               /* Reserved for future use, not needed right now */
} STVR;

typedef struct SymbolTableDescriptor {
    STVR *pstvr;      /* Pointer to array of STVR */
    int st_size;      /* Size in number of STVR elements */
    int st_offset;    /* Offset in number of STVR elements */
    Buffer *plsBD;    /* Pointer to the lexeme storage buffer descriptor */
} STD;

STD st_create(int);
int st_install(STD, char*, char, int);
int st_lookup(STD, char*);
int st_change_type(STD, int, char);
int st_change_value(STD, int, Value);
char st_get_type(STD, int);
Value st_get_value(STD, int);
void st_destroy(STD);
int st_print(STD);
int st_store(STD);
int st_sort(STD, char);

#endif
