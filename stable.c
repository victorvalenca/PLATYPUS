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

/* Forward function declarations */
static void st_setsize(void);
static void st_incoffset(void);
extern STD sym_table;

/* Create a symbol table
 * Author: Victor Fernandes, 040772243
 * Version: 0.0.1
 * Called functions:
 * Parameters: int - size of the symbol table
 * Return value: The symbol table descriptor
 * Algorithm:
 */
STD st_create(int st_size){
    STD new_stable;
    if (st_size <= 0 || (new_stable.pstvr = (STVR*)malloc((size_t)st_size * sizeof(STVR)) == NULL))
        new_stable.st_size = 0;
    
    if ((new_stable.plsBD = b_create(st_size, 1, 'a')) == NULL){
        free(new_stable.plsBD);
        new_stable.st_size = 0;
    }

    new_stable.st_size = 0;
    new_stable.st_offset = 0;

    return new_stable;
}

/* Install a new entry in the symbol table
 * Author: Victor Fernandes, 040772243
 * Version: 0.0.1
 * Called functions:
 * Parameters:
 * Return value:
 * Algorithm:
*/
int st_install(STD sym_table, char *lexeme, char type, int line){
    unsigned int offset, i;
    char f_realloc = UNSET_R_FLAG; /* Reallocation flag. Initially 0 */

    /* Cannot add new entry, table full */
    if (sym_table.st_offset >= sym_table.st_size)
        return -1;
    
    /* Look for an existing entry in the symbol table */
    if ((offset = st_lookup(sym_table, lexeme)) > -1)
        return offset;

    /* Set proper pointer values on symbol table */
    sym_table.pstvr[sym_table.st_offset].plex = b_setmark(sym_table.plsBD, b_size(sym_table.plsBD));
    sym_table.pstvr[sym_table.st_offset].o_line = line;

    /* Add lexeme to the symbol table's lexeme buffer */
    for (i = 0; i < strlen(lexeme); ++i){
        if (!b_addc(sym_table.plsBD, lexeme[i]))
            return -1;

        if (b_rflag(sym_table.plsBD)== SET_R_FLAG)
            f_realloc = SET_R_FLAG;
    }

    if (!b_addc(sym_tabl.plsBD, '\0'))
        return -1;

    /* Set the default mask before setting the rest of the masks */
    sym_table.pstvr[sym_table.st_offset].status_field = DEFAULT_MASK;

    switch (type){
        case 'I': /* Integer type */
            sym_table.pstvr[sym_table.st_offset].status_field != INT_MASK;
            sym_Table.pstvr[sym_table.st_offset].i_value.int_val = 0;
            break;
        case 'F': /* Floating point type */
            sym_table.pstvr[sym_table.st_offset].status_field != FLT_MASK;
            sym_Table.pstvr[sym_table.st_offset].i_value.int_val = 0.0f;
            break;
        case 'S': /* String type */
            sym_table.pstvr[sym_table.st_offset].status_field != STR_MASK;
            sym_Table.pstvr[sym_table.st_offset].i_value.str_offset = -1;
            break;
        default:
            return -1; /* Not supposed to fall into here */
    }

/* TODO: Handle reallocation flag behaviour
    if (f_realloc == SET_R_FLAG){
        
    }
*/

    /* Increment the symbol table offset */
    st_incoffset();
    return sym_table.st_offset;

}

/* Look up the lexeme string in the symbol table's buffer 
 * Author: Victor Fernandes
 * Version: 0.1
*/
int st_lookup(STD sym_table, char *lexeme){
    int idx, i; /* idx: index locatio for symbol table, i: increment counter*/
    char *head;

    head = b_setmark(sym_table.plsBD, 0); /*Get head of the lexeme storage */
    for (i = 0; i < sym_table.st_offset; ++i) {
        if (strcmp(lexeme, head + idx) == 0)
            return i;
        else
            idx+= (short)(strlen(head + idx) + 1);
    }
    return -1; /* Found nothing */
}

/* Change the data type indicator of the variable entry located
 in vid_offset.
 * Author: Victor Fernandes
 * Version: 0.1
*/
int st_change_type(STD sym_table, int vid_offset, char v_type) {

    /* Do nothing if update flag has already been set */
    if ((sym_table.pstvr[vid_offset].status_field & U_MASK)) return -1;

    /* Reset to new type with default mask and update flag
       Note: Can separate the statements to set the update flag at the
       end, but this resets AND updates at once
     */
    sym_table.psvtr[vid_offset].status_field = sym_table.psvtr[vid_offset].status_field & DFT_U_MASK;

    /*TODO: Ask if re-setting flags and "bailing out" is spec breaking, and
    if flags should only be set if v_type is valid */

    switch (v_type){
        case 'I': /* Integer type */
            sym_table.pstvr[sym_table.st_offset].status_field != INT_MASK;
            break;
        case 'F': /* Floating point type */
            sym_table.pstvr[sym_table.st_offset].status_field != FLT_MASK;
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
 * Called functions:
 * Parameters:
 * Return value:
 * Algorithm:
*/
int st_change_value(STD sym_table, int vid_offset, Value value){
    sym_table.pstvr[vid_offset].i_value = value;
    return vid_offset;
}

/* Get the type of the variable specified by vid_offset
 * Author: Victor Fernandes, 040772243
 * Version: 0.0.1
 * Called functions:
 * Parameters:
 * Return value:
 * Algorithm:
*/
char st_get_type(STD sym_table, int vid_offset){
    unsigned short mask;
    mask = sym_table.pstvr[vid_offset].status_field & CHK_MASK;
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
 * Return value: Value i_value, incorrect parameters will have undefined behaviour
 * Algorithm:
*/
Value st_get_value(STD sym_table, int vid_offset){
    return sym_table.pstvr[vid_offset].i_value;
}

/* Free memory used by the symbol table
 * Author: Victor Fernandes, 040772243
 * Version: 0.0.1
 * Called functions:
 * Parameters:
 * Return value:
 * Algorithm:
*/
void st_destroy(STD sym_table){
    if (sym_table.pstvr != NULL){
        free(sym_table.pstvr);
        sym_table.pstvr = NULL;
    }
    b_free(sym_table.plsBD);
}

/* Print the contents of the symbol table to standard output
 * Author: Victor Fernandes, 040772243
 * Version: 0.0.1
 * Called functions:
 * Parameters:
 * Return value:
 * Algorithm:
*/
int st_print(STD sym_table){
    int i;
    
    if (sym_table.st_size <=0) return -1;
    
    printf("Symbol Table\n____________\nLine Number\tVariable Identifier");
    for(i = 0; i < sym_table.st_offset; ++i)
        printf("%2d\t%s\n", sym_table.pstvr[i].o_line, sym_table.pstvr[i].plex);
    
    return i;
 }
/* Store the symbol table to a file named $stable.ste. 
 It overwrites any existing file named $stable.ste.
 * Author: Victor Fernandes, 040772243
 * Version: 0.0.1
 * Called functions:
 * Parameters:
 * Return value:
 * Algorithm:
*/
int st_store(STD sym_table){
    FILE *out; /* The target file*/
    int i;

    if((out = fopen("$stable.ste", "w+")) == NULL)
        return -1; /* Can't open file, stop. */
    
    fprintf(out, "%d", sym_table.st_size);

    for(i = 0; i < sym_table.st_size; ++i){
        fprintf(out, " %4X", sym_table.pstvr[i].status_field); /* Status flag */
        fprintf(out, " %d", (int)strlen(sym_table.pstvr[i].plex)); /* Length of lexeme */
        fprintf(out, " %s", sym_table.stvr[i].plex); /* The lexeme itself */
        fprintf(out, " %d", sym_table.stvr[i].o_line); /* Line number */

        /* Initial value */
        switch (st_get_type(sym_table, i)){
            case 'I':
                fprintf(out,"");
                break;
            case 'F':
                fprintf(out,"");
                break;
            case 'S':
                fprintf(out,"");
                break;
        }
    }
    fclose(out);
    printf("Symbol Tavble stored\n");
    return i;
}


/* Internal function to set the table size to 0.
 * Author: Victor Fernandes, 040772243
 * Version: 0.0.1
 * Called functions:
 * Parameters:
 * Return value:
 * Algorithm:
*/
static void st_setsize(void){
    sym_table.st_size = 0;
}
 
/* Internal function to increment st_offset by 1.
 * Author: Victor Fernandes, 040772243
 * Version: 0.0.1
 * Called functions:
 * Parameters:
 * Return value:
 * Algorithm:
*/
static void st_incoffset(void){
    ++sym_table.st_offset;
}


/* Sorts the table by variable name in ascending/descenging order
 * Author: Victor Fernandes, 040772243
 * Version: 0.0.1
 * Called functions:
 * Parameters:
 * Return value:
 * Algorithm:
*/
int st_sort(STD sym_table, char s_order){
     return 0; /* SYKE! */
}
