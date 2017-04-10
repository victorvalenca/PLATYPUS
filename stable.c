/* Filename: stable.c
 * Compiler: msvc (Visual Studio 2015)
 * Store and provide functions for the symbol table database
 * CST8152, Assignment #3
 * Professor: Svillen Ranev
 * Author: Victor Fernandes, 040772243
 * Version: 0.1
 * Date: 24 March 2017
 */
#include "stable.h"

#define PLSBD_SZ 256
#define PLSBD_INC 8

#define DEBUG
/*#undef DEBUG*/

/* Forward function declarations */
static void st_setsize(void);
static void st_incoffset(void);
extern STD sym_table;

/* Create a symbol table
 * Author: Victor Fernandes, 040772243
 * Version: 0.0.1
 * Called functions: malloc, b_create, free
 * Parameters: 
    int - size of the symbol table
 * Return value: 
    STD - The symbol table
 * Algorithm:
 */
STD st_create(int st_size){
    STD new_stable;

    new_stable.plsBD = NULL;
	new_stable.pstvr = (STVR*)malloc((size_t)st_size * sizeof(STVR));
    if (st_size <= 0 || (new_stable.pstvr == NULL)) {
        new_stable.st_size = 0;
    }
	new_stable.plsBD = b_create(PLSBD_SZ, PLSBD_INC, 'a');
    if (new_stable.plsBD == NULL) {
        free(new_stable.plsBD);
        new_stable.st_size = 0;
    }

    new_stable.st_size = st_size;
    new_stable.st_offset = 0;

    return new_stable;
}

/* Install a new entry in the symbol table
 * Author: Victor Fernandes, 040772243
 * Version: 0.0.1
 * Called functions: st_lookup, b_setmark, b_size, b_addc, b_rflag, st_incoffset
 * Parameters:
    STD  - The symbol table
    char - The lexeme to be stored
    char - the type of the lexeme
    int  - the line in the source file where the lexeme was found
 * Return value:
    int - the element offset of where the symbol table is pointing to
    -1 if an internal error occurs
 * Algorithm:
*/
int st_install(STD s_table, char *lexeme, char type, int line){
    unsigned int offset, i, j, lex_len;
    short bd_offset, flag;

    /* Cannot add new entry, table full */
    if (s_table.st_offset >= s_table.st_size)
        return -1;
    
    /* Look for an existing entry in the symbol table */
    offset = st_lookup(s_table, lexeme);
    if (offset > -1)
        return offset;

    /* Add lexeme to the symbol table's lexeme buffer */
    lex_len = strlen(lexeme);
    for (i = 0; i <= lex_len; ++i){
        if (!b_addc(s_table.plsBD, lexeme[i]))
            return -1;
        if (b_rflag(s_table.plsBD) == SET_R_FLAG){ /* COPY NEW ADDRESSES TO PLEX ENTRIES */
            flag = 1;
        }
    }
    if (flag == 1){
        bd_offset = 0;
        for (j = 0; j <= s_table.st_offset; ++j){
            s_table.pstvr[j].plex = b_setmark(s_table.plsBD, bd_offset + s_table.pstvr[j].i_value.str_offset);
            if (j < s_table.st_offset) 
                bd_offset += s_table.pstvr[j+1].i_value.str_offset + 1; /*Add one because the offset doesn't include '\0' */
        }
    }
           

    /* Set proper pointer values on symbol table */
    s_table.pstvr[s_table.st_offset].plex = b_setmark(s_table.plsBD, b_size(s_table.plsBD));
    s_table.pstvr[s_table.st_offset].o_line = line;


    /* Set the default mask before setting the rest of the masks */
    s_table.pstvr[s_table.st_offset].status_field = DFT_MASK;

    switch (type){
        case 'I': /* Integer type */
            s_table.pstvr[s_table.st_offset].status_field |= INT_MASK;
            s_table.pstvr[s_table.st_offset].i_value.int_val = 0;
            break;
        case 'F': /* Floating point type */
            s_table.pstvr[s_table.st_offset].status_field |= FLT_MASK;
            s_table.pstvr[s_table.st_offset].i_value.fpl_val = 0.0f;
            break;
        case 'S': /* String type */
            s_table.pstvr[s_table.st_offset].status_field |= STR_MASK;
            s_table.pstvr[s_table.st_offset].i_value.str_offset = -1;
            break;
        default:
            return -1; /* Not supposed to fall into here */
    }


    /* Increment the symbol table offset */
    st_incoffset();
    return s_table.st_offset;

}

/* Look up the lexeme string in the symbol table's buffer 
 * Author: Victor Fernandes
 * Version: 0.1
 * Called functions: b_setmark, strcmp, strlen
 * Parameters:
    STD - The symbol table
    char - The lexeme to be searched
 * Return value:
    int - the element offset index in the symbol table
    -1 if no element was found
*/
int st_lookup(STD s_table, char *lexeme){
    int i; /* idx: index location for symbol table, i: increment counter*/

    for (i = s_table.st_offset; i >= 0; --i) {
        if (strcmp(lexeme, s_table.pstvr[i].plex) == 0)
            return i;
    }
#ifdef DEBUG
    printf("found nothing\n");
#endif
    return -1; /* Found nothing */
}

/* Change the data type indicator of the variable entry located in vid_offset.
 * Author: Victor Fernandes
 * Version: 0.1
 * Called functions: N/A
 * Parameters:
    STD - The symbol table
    int - the offset location of the VID
    char - the new type of the VID
 * Return value:
    1 if change successful, -1 if no change was made or internal error
*/
int st_change_type(STD s_table, int vid_offset, char v_type) {

    /* Do nothing if update flag has already been set */
    if ((s_table.pstvr[vid_offset].status_field & U_MASK)) return -1;

    /* Reset to new type with default mask and update flag
       Note: Can separate the statements to set the update flag at the
       end, but this resets AND updates at once
     */
    /*sym_table.pstvr[vid_offset].status_field = sym_table.pstvr[vid_offset].status_field & DFT_U_MASK;*/
    s_table.pstvr[vid_offset].status_field &= DFT_U_MASK;

    /*TODO: Ask if re-setting flags and "bailing out" is spec breaking, and
    if flags should only be set if v_type is valid */

    switch (v_type){
        case 'I': /* Integer type */
            s_table.pstvr[s_table.st_offset].status_field |= INT_MASK;
            break;
        case 'F': /* Floating point type */
            s_table.pstvr[s_table.st_offset].status_field |= FLT_MASK;
            break;
        case 'S': /* String type, do nothing as it cannot be changed */
            break;
        default:
            return -1; /* Not supposed to fall into here */
    }
    return 1;

}

/* Change the i_value of the variable located by vid_offset to new Value
 * Author: Victor Fernandes, 040772243
 * Version: 0.0.1
 * Called functions: N/A
 * Parameters:
    STD - The symbol table
    int - the offset location of the VID
    Value - the new value of the VID
 * Return value: the offset location of the VID
*/
int st_change_value(STD s_table, int vid_offset, Value value){
    s_table.pstvr[vid_offset].i_value = value;
    return vid_offset;
}

/* Get the type of the variable specified by vid_offset
 * Author: Victor Fernandes, 040772243
 * Version: 0.0.1
 * Called functions: N/A
 * Parameters:
    STD - The symbol table
    int - the offset of the VID in the table
 * Return value:
    char - the type of the VID ('I','F', or 'S')
    -1 if there is an invalid value set
*/
char st_get_type(STD s_table, int vid_offset){
    unsigned short mask;
    mask = s_table.pstvr[vid_offset].status_field & CHK_MASK;
    switch (mask){
        case INT_MASK:
            return 'I';
        case FLT_MASK:
            return 'F';
        case STR_MASK:
            return 'S';
        default: 
            return -1; /* Not supposed to fall into here */
    }
}

/* Return the i_value of the variable specified by vid_offset
 * Author: Victor Fernandes, 040772243
 * Version: 0.0.1
 * Called functions:
 * Parameters:
    STD - The symbol table
    int - the offset of the VID in the table
 * Return value: 
    Value - the value of the VID
    Incorrect parameters will cause undefined behaviour
*/
Value st_get_value(STD s_table, int vid_offset){
    return s_table.pstvr[vid_offset].i_value;
}

/* Free memory used by the symbol table
 * Author: Victor Fernandes, 040772243
 * Version: 0.0.1
 * Called functions: free, b_free
 * Parameters:
    STD - The symbol table
*/
void st_destroy(STD s_table){
    if (s_table.pstvr != NULL){
        free(s_table.pstvr);
        s_table.pstvr = NULL;
    }
    b_free(s_table.plsBD);
}

/* Print the contents of the symbol table to standard output
 * Author: Victor Fernandes, 040772243
 * Version: 0.0.1
 * Called functions: printf
 * Parameters:
    STD - The symbol table
 * Return value:
    int - the number of items printed to stdout
*/
int st_print(STD s_table){
    int i;
    
    if (s_table.st_size <=0) return -1;
    
    printf("Symbol Table\n____________\nLine Number\tVariable Identifier\n");
    for(i = 0; i < s_table.st_offset; ++i)
        printf("%2d\t\t%s\n", s_table.pstvr[i].o_line, s_table.pstvr[i].plex);
    
    return i;
 }

/* Store the symbol table to a file named $stable.ste. 
 It overwrites any existing file named $stable.ste.
 * Author: Victor Fernandes, 040772243
 * Version: 0.0.1
 * Called functions: fopen, fprintf, st_get_type, fclose, printf
 * Parameters:
    STD - The symbol table
 * Return value:
    int - the number of items stored
    -1 if the file stream cannot be opened
*/
int st_store(STD s_table){
    FILE *out; /* The target file*/
    int i;

	/* Windows does not like fopen, and fopen_s is Windows-only. This will ensure the
	 * use of the proper function call if PLATYPUS is being built on a non-windows system.
     * Best approach is to add _CRT_SECURE_NO_WARNINGS to the preprocessor definitions, but
     * this works, too.
	*/
#ifdef _WIN32
    if (fopen_s(&out, ST_FILE_NAME, "w+") != 0 || out == NULL)
#else
    if ((out = fopen(ST_FILE_NAME, "w+")) == NULL)
#endif
		return -1; /* Can't open file, stop. */

    fprintf(out, "%d", s_table.st_size);

    for(i = 0; i < s_table.st_size; ++i){
        fprintf(out, " %4X %d %s %d",
            s_table.pstvr[i].status_field,      /* Status flag */
            (int)strlen(s_table.pstvr[i].plex), /* Length of lexeme */
            s_table.pstvr[i].plex,              /* The lexeme itself */
            s_table.pstvr[i].o_line);           /* Line number */

        /* Initial value */
        char type = st_get_type(s_table, i);
        switch (type){
            case 'I':
            case 'F':
            case 'S':
                fprintf(out, " %c", type);
        }
    }
    fclose(out);
    printf("Symbol Table stored\n");
    return i;
}


/* Internal function to set the table size to 0.
 * Author: Victor Fernandes, 040772243
 * Version: 0.0.1
 * Called functions: N/A
 * Parameters: N/A
 * Return value: N/A
*/
static void st_setsize(void){
    sym_table.st_size = 0;
}
 
/* Internal function to increment st_offset by 1.
 * Author: Victor Fernandes, 040772243
 * Version: 0.0.1
 * Called functions: N/A
 * Parameters: N/A
 * Return value: N/A
*/
static void st_incoffset(void){
    ++sym_table.st_offset;
}


/* Sorts the table by variable name in ascending/descending order
 * Author: Victor Fernandes, 040772243
 * Version: 0.0.1
 * Called functions:
 * Parameters: 
 * Return value:
 * Algorithm:
*/
int st_sort(STD s_table, char s_order){
    /* Compiler warning about unused parameter, 
     * this is fine for this "future" implementation 
    */
     return 0; /* SYKE! */
}
