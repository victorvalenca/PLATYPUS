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

}

/* Look up the lexeme string in the symbol table's buffer 
 * Author: Victor Fernandes
 * Version: 0.1
*/
int st_lookup(STD sym_table, char *lexeme){

}

/* Change the data type indicator of the variable entry located
 in vid_offset.
 * Author: Victor Fernandes
 * Version: 0.1
*/
int st_change_type(STD sym_table, int vid_offset, char v_type) {

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
