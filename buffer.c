/*
 * File Name: buffer.c
 * Compiler: GCC 6.2.0
 * Author: Victor Fernandes, 040772243
 * Course: CST8152 - Compilers, Lab Section: 011
 * Date: February 1, 2017
 * Professor: Svillen Ranev
 * A character buffer utility with three modes of self-incrementation
 through dynamic memory allocation, and ability to set a mark flag.
 * Function list:
 * TODO: Function list and finish algorithm descriptions
 */


#include <limits.h>
#include <stdlib.h>
#include "buffer.h"

/* Initializes and allocates memory for the buffer descriptor
 * Author: Victor Fernandes
 * Version: 0.0.1
 * Called functions: calloc(), malloc(), free()
 * Parameters:
    - short init_capacity (0 - SHRT_MAX)
    - char inc_factor (1 to 100 for multiplicative mode, 1 to 255 in additive mode, 0 for fixed mode)
    - char o_mode (m, a, f)
 * Return values: pBuffer or NULL
 * Algorithm: Allocates memory for the buffer descriptor. If successful, do bound
 checks on function parameters and assign them to the buffer's variables. If
 all is clear, allocate the character buffer. Otherwise return NULL.
 */
Buffer* b_create(short init_capacity, char inc_factor, char o_mode) {
    pBuffer pBD;    /* Pointer to buffer descriptor */

    /* BEGIN CONFIGURING BUFFER */

    /* Check if init_capacity is within acceptable range */
    if (init_capacity <= ZERO_CAPACITY) {
        return NULL;
    }
    /* Leaving cb_head allocation for last in the event of bad input */

    
    /* Memory allocation */
    pBD = (Buffer *) calloc(1, sizeof(Buffer));
    if (!pBD) {
        return NULL; /* Abort execution immediatelly if allocation fails */
    }

    /* Check and set operation mode */
    if (o_mode == 'f' || inc_factor == FIX_INC_FACTOR) {
        pBD->mode = FIX_OP_MODE;
        pBD->inc_factor = FIX_INC_FACTOR;
    } else if (o_mode == 'a'){
        pBD->mode = ADD_OP_MODE;
        pBD->inc_factor = inc_factor;
    } else if (o_mode == 'm' 
            && (inc_factor >= MIN_MUL_INC_FACTOR)
            && (inc_factor <= MAX_MUL_INC_FACTOR)) {
        pBD->mode = MUL_OP_MODE;
        pBD->inc_factor = inc_factor;
    } else { /* Abort everything if any parameters are bad */
        free(pBD);
    return NULL;
    }
    
    /* Attempt to initialize cb_head */
    pBD->cb_head = (char *) malloc(sizeof(char) * init_capacity);
    /* Abort configuration if allocation fails and release memory */
    if (!pBD->cb_head) {
        free(pBD);
        return NULL;
    }

    pBD->capacity = init_capacity;
    /* END CONFIGURING BUFFER */
    return pBD;
}

/* Reports whether the character buffer is full or not
 * Author: Victor Fernandes, 040772243
 * Version: 0.0.1
 * Called functions: N/A
 * Parameters:
    - pBuffer const pBD
 * Return values: -1, 0, 1
 */
int b_isfull(Buffer* const pBD) {
    if (!pBD) { return R_FAIL1; }
    if (b_size(pBD) == b_capacity(pBD)) {
        return TRUE;
    } else {
        return FALSE;
    }
}

/* Reports if character buffer is empty
 * Author: Victor Fernandes, 040772243
 * Version: 0.0.1
 * Called functions: N/A
 * Parameters:
    - pBuffer const pBD
 * Return value: -1, 0, 1
 */
int b_isempty(Buffer* const pBD) {
    if (!pBD) { return R_FAIL1; }
    if (pBD->addc_offset == OFFSET_RESET) {
        return TRUE;
    } else {
        return FALSE;
    }
}

/* Reports the current size of the character buffer
 * Author: Victor Fernandes, 040772243
 * Version: 0.0.1
 * Called functions: N/A
 * Parameters:
    - pBuffer const pBD
 * Return value: short
 */
short b_size(Buffer* const pBD) {
    if (!pBD) { return R_FAIL1; }
    return pBD->addc_offset;
}

/* Reports the current capacity of the character buffer
 * Author: Victor Fernandes, 040772243
 * Version: 0.0.1
 * Called functions: N/A
 * Parameters:
    - pBuffer const pBD
 * Return value: short
 */
short b_capacity(Buffer* const pBD) {
    if (!pBD) { return R_FAIL1; }
    return pBD->capacity;
}

/* Reports the current character buffer's operational mode
 * Author: Victor Fernandes, 040772243
 * Version: 0.0.1
 * Called functions: N/A
 * Parameters:
    - pBuffer const pBD
 * Return value: -2, -1, 0, 1
 */
int b_mode(Buffer* const pBD) {
    if (!pBD) { return R_FAIL2; }
    return pBD->mode;
}

/* Returns the non-negative value of the increment factor of the buffer
 * Author: Victor Fernandes, 040772243
 * Version: 0.0.1
 * Called functions: N/A
 * Parameters:
    - pBuffer const pBD
 * Return value: size_t
 */
size_t b_incfactor(Buffer* const pBD) {
	if (!pBD) { return ERR_INC_FACTOR; }
    return (size_t) pBD->inc_factor;
}

/* Reports the current position of the mark offset in the character buffer
 * Author: Victor Fernandes, 040772243
 * Version: 0.0.1
 * Called functions: N/A
 * Parameters:
    - pBuffer const pBD
 * Return value: short
 */
short b_mark(Buffer* const pBD) {
    if (!pBD) { return R_FAIL1; }
    return pBD->mark_offset;
}

/* Reports if the character buffer's memory space was relocated after resizing
 * Author: Victor Fernandes, 040772243
 * Version: 0.0.1
 * Called functions: N/A
 * Parameters:
    - pBuffer const pBD
 * Return value: char
 */
char b_flag(Buffer* const pBD) {
    if (!pBD) { return R_FAIL1; }
    return pBD->r_flag;
}

/* Reports character buffer's current character offset
 * Author: Victor Fernandes, 040772243
 * Version: 0.0.1
 * Called functions: N/A
 * Parameters:
    - pBuffer const pBD
 * Return value: short
 */
short b_getcoffset(Buffer* const pBD) {
    if (!pBD) { return R_FAIL1; }
    return pBD->getc_offset;
}

/* Returns the buffer's actual character buffer address
 * Author: Victor Fernandes, 040772243
 * Version: 0.0.1
 * Called functions: N/A
 * Parameters:
    - pBuffer const pBD
 * Return value: short
 */
char* b_cbhead(Buffer* const pBD) {
    if (!pBD || !pBD->cb_head) { return NULL; }
    return pBD->cb_head;
}

/* Sets a mark offset on the buffer's mark flag
 * Author: Victor Fernandes, 040772243
 * Version: 0.0.1
 * Called functions: N/A
 * Parameters:
    - pBuffer const pBD
 * Return value: char*
 */
char* b_setmark(Buffer* const pBD, short mark) {
    if (!pBD || mark < 0 || mark > pBD->addc_offset) { return NULL; }
    pBD->mark_offset = mark;
    return (pBD->cb_head) + pBD->mark_offset;
}

/* Reports the end-of-buffer flag state of the character buffer
 * Author: Victor Fernandes, 040772243
 * Version: 0.0.1
 * Called functions: N/A
 * Parameters:
    - pBuffer const pBD
 * Return value: 1, 0
 */
int b_eob(Buffer* const pBD) {
    if (!pBD) { return R_FAIL1; }
    return pBD->eob;
}

/* Adds one character symbol to the character buffer, incrementing its size if
possible and needed
 * Author: Victor Fernandes, 040772243
 * Version: 0.0.1
 * Called functions: b_isfull(), realloc()
 * Parameters:
    - pBuffer const pBD
    - char symbol (1-255)
 * Return value: pBuffer or NULL
 * Algorithm:
 */
pBuffer b_addc(pBuffer const pBD, char symbol) {
    /* Variables used for calculating required space for reallocating cb_head
    for additive or multiplicative modes */
    short avail_space, new_inc, new_cap = 0;

    /* These pointers are used to compare the address of cb_head, in the event it is
    moved to a new address space in memory after reallocation*/
    char *old_addr;
    char *tmp_addr;

    if (!pBD) {
        return NULL;
    }

    /* Reset reallocation flag */
    pBD->r_flag = UNSET_R_FLAG;

    /* BEGIN BUFFER INCREMENT */
    if (pBD->addc_offset == pBD->capacity){
        if (pBD->mode == FIX_OP_MODE) { /* Fixed mode, won't increment */
            return NULL;
        }
        else if (pBD->mode == ADD_OP_MODE) { /* Calculate new size for additive mode */
            new_cap = pBD->capacity + pBD->inc_factor;
            /* Make sure no short overflow happened */
            if (new_cap < MIN_CAPACITY){
                return NULL;
            }

        }
        else if (pBD->mode == MUL_OP_MODE) { /* Calculate new size in multiplicative mode */
            if (pBD->capacity == SHRT_MAX){ /* Do nothing if at maximum size */
                return NULL;
            }

            avail_space = SHRT_MAX - pBD->capacity;
            new_inc = avail_space * ((double) pBD->inc_factor) / 100;

            /* Check if there is enough space for the new increment and
            "trim" it if needed */
            if (new_inc >= avail_space) {
                new_cap = SHRT_MAX;
            }
            else {
                new_cap = pBD->capacity + new_inc;
            }
        }

        /* Reallocate memory to character buffer */
        old_addr = pBD->cb_head; /* Keep track of old pointer address to check if it changed */
        tmp_addr = (char *)realloc(pBD->cb_head, sizeof(char) * new_cap);
        
        if (tmp_addr == NULL){
            return NULL; /* Abort everything if allocation fails */
        }

        pBD->cb_head = tmp_addr;
        pBD->capacity = new_cap;
        if (old_addr == pBD->cb_head) { /* Compare the old and new addresses and set flag appropriately */
            pBD->r_flag = SET_R_FLAG;
        }
    } /* END BUFFER INCREASE */

    /* Finally, add new symbol to the buffer after increasing it (or not) */
    pBD->cb_head[pBD->addc_offset] = symbol;
    pBD->addc_offset++;
    return pBD;

}

/* Gets one character symbol from the buffer
 * Author: Victor Fernandes, 040772243
 * Version: 0.0.1
 * Called functions: N/A
 * Parameters:
    - pBuffer const pBD
 * Return value: char
 * Algorithm: Function will check if getc_offset is at the end
 * of the character array. If so, set flag and return failure.
 * Otherwise fetch character from cb_head, increment offset, and
 * check if it reached the EOB. If so, apply the flag before
 * returning the character.
 */
char b_getc(Buffer* const pBD) {
    char char_buf;

    if (!pBD) { return R_FAIL2; }

    if (pBD->getc_offset == pBD->addc_offset) {
        pBD->eob = SET_EOB_FLAG;
        return R_FAIL1;
    } else {
        pBD->eob = UNSET_EOB_FLAG;
    }

    char_buf = pBD->cb_head[pBD->getc_offset++];

    if (pBD->getc_offset == pBD->addc_offset) {
        pBD->eob = SET_EOB_FLAG;
    }

    return char_buf;
}

/* Prints the output of the buffer
 * Author: Victor Fernandes, 040772243
 * Version: 0.0.1
 * Called functions: b_isempty(), b_getc(), b_eob(), printf()
 * Parameters:
    - pBuffer const pBD
 * Return value: int
 */
int b_print(Buffer* const pBD) {
    int char_count = 0; /* Counter to track how many characters were sent to output */
    char char_buf;  /* "Buffer" character to load before output */
    short tmp_offset;

    if (!pBD || !pBD->cb_head) {
		return R_FAIL1;
	}

    if (b_isempty(pBD) == TRUE) {
		printf("The buffer is empty.\n");
	}

    else {
        /* Save getc_offset to restore after printing */
        tmp_offset = pBD->getc_offset;
        pBD->getc_offset = OFFSET_RESET;

        do {
            char_buf = b_getc(pBD);
            printf("%c", (char) char_buf);
            char_count++;
        } while (!b_eob(pBD));
        printf("\n");
        /* Restore the getc_offset */
        pBD->getc_offset = tmp_offset;
    }
    return char_count;
}

/* Loads symbols from a file to the buffer
 * Author: Victor Fernandes, 040772243
 * Version: 0.0.1
 * Called functions: fgetc, feof, b_addc
 * Parameters:
    - FILE* const fi
    - pBuffer const pBD
 * Return value: int
 */
int b_load(FILE* const fi, Buffer* const pBD) {
    char char_buf; /* "Buffer" character to be loaded before adding to cb_head */
    int char_count = 0; /* Character counter */

    if (!fi || !pBD) {
        return R_FAIL1;
    }

	while (!feof(fi)) {
        char_buf = (char) fgetc(fi);
		
        if (char_buf == EOF) {
            break;
        }

        if (!b_addc(pBD, char_buf)) { 
            return LOAD_FAIL;
        }
        char_count++;
    }

    return char_count;
}

/* Resets the buffer
 * Author: Victor Fernandes, 040772243
 * Version: 0.0.1
 * Called functions: N/A
 * Parameters:
    - pBuffer const pBD
 * Return value: int
 */
int b_reset(Buffer* const pBD) {
    if (!pBD) { return R_FAIL1; }

    pBD->addc_offset = OFFSET_RESET;
    pBD->getc_offset = OFFSET_RESET;
    pBD->mark_offset = OFFSET_RESET;
    pBD->eob = UNSET_EOB_FLAG;
	pBD->r_flag = UNSET_R_FLAG;
    return TRUE;
}

/* Retracts the buffer's character offset to the mark
 * Author: Victor Fernandes, 040772243
 * Version: 0.0.1
 * Called functions: N/A
 * Parameters:
    - pBuffer const pBD
 * Return value: short
 */
short b_retract_to_mark(Buffer* const pBD) {
    /* Check if any offsets are out of bounds */
    if(!pBD ||
        pBD->getc_offset < OFFSET_RESET || pBD->getc_offset > pBD->capacity ||
        pBD->mark_offset < OFFSET_RESET || pBD->mark_offset > pBD->capacity) {
        return R_FAIL1;
    }
    pBD->getc_offset = pBD->mark_offset;
    return pBD->getc_offset;
}

/* Retracts the buffer's character offset one character backwards
 * Author: Victor Fernandes, 040772243
 * Version: 0.0.1
 * Called functions: N/A
 * Parameters:
    - pBuffer const pBD
 * Return value: short
 */
short b_retract(Buffer* const pBD) {
    if (!pBD) { return R_FAIL1; }
    else if (pBD->getc_offset > OFFSET_RESET) {
        pBD->getc_offset--;
    }
    return pBD->getc_offset;
}
/* Returns the buffer's memory reallocation flag
 * Author: Victor Fernandes, 040772243
 * Version: 0.0.1
 * Called functions: N/A
 * Parameters:
    - pBuffer const pBD
 * Return value: char
 */
char b_rflag(Buffer* const pBD) {
    if (!pBD) { return R_FAIL1; }
    return pBD->r_flag;
}

/* Reduce the size of the character buffer to free up memory
 * Author: Victor Fernandes, 040772243
 * Version: 0.0.1
 * Called functions: realloc()
 * Parameters:
    - pBuffer const pBD
 * Return value: Buffer* const
 */
Buffer* b_pack(Buffer* const pBD) {
    short new_cap; /* Used to set the new buffer capacity value */
    
    /* Temporary pointers used to compare old and new pointer addresses */
    char *old_addr; 
    char *tmp_addr;

    if (!pBD || !pBD->cb_head) { return NULL; }

    /* Configure new capacity value */
    new_cap = pBD->addc_offset + 1;
    old_addr = pBD->cb_head;

    /* Reallocate cb_head to new size */
    tmp_addr = realloc(pBD->cb_head, sizeof(char) * new_cap);
    if (!tmp_addr){ return NULL; } /* Abort if realloc failed */

    /* Assign new address to cb_head and reassign capacity value */
    pBD->cb_head = tmp_addr;
    pBD->capacity = new_cap;

    /* Compare old and new addresses and set R_FLAG accordingly */
    if (old_addr != pBD->cb_head){
        pBD->r_flag = SET_R_FLAG;
    }
    return pBD;
}

/* Deallocate and release all memory used by the buffer.
 * Author: Victor Fernandes, 040772243
 * Version: 0.0.1
 * Called functions: free()
 * Parameters:
    - pBuffer const pBD
 * Return value: N/A
 */
void b_free(Buffer* const pBD) {
    free(pBD->cb_head);
    free(pBD);
}
