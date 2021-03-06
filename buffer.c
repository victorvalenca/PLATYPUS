/*
 * File Name: buffer.c
 * Compiler: GCC 6.3.0
 * Author: Victor Fernandes, 040772243
 * Course: CST8152 - Compilers, Lab Section: 011
 * Date: February 1, 2017
 * Professor: S^R
 * A character buffer utility with three modes of self-incrementation
 through dynamic memory allocation, and ability to set a mark flag.
 * Function list: b_create, b_isfull, b_isempty, b_size, b_capacity,
 b_mode, b_incfactor, b_mark, b_flag, b_getcoffset, b_cbhead, b_setmark,
 b_eob, b_addc, b_getc, b_print, b_load, b_reset, b_retract, b_pack, b_free
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
    if (init_capacity < ZERO_CAPACITY) {
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
 * Return values: -1 (R_FAIL1), 0 (FALSE), 1 (TRUE)
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
 * Return value: -1 (R_FAIL1), 0 (FALSE), 1 (TRUE)
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
 * Return value: -1 (R_FAIL1), short
 */
short b_size(Buffer* const pBD) {
    if (!pBD) { return R_FAIL1; }
    return (unsigned short) pBD->addc_offset;
}

/* Reports the current capacity of the character buffer
 * Author: Victor Fernandes, 040772243
 * Version: 0.0.1
 * Called functions: N/A
 * Parameters:
    - pBuffer const pBD
 * Return value: -1 (R_FAIL1), short
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
 * Return value: -2 (R_FAIL2),
 *               -1 (multiplicative),
 *                0 (fixed),
 *                1 (additive)
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
 * Return value: size_t, 256 (ERR_INC_FACTOR)
 */
size_t b_incfactor(Buffer* const pBD) {
	if (!pBD) { return ERR_INC_FACTOR; }
    return (size_t)(unsigned char) pBD->inc_factor;
}

/* Reports the current position of the mark offset in the character buffer
 * Author: Victor Fernandes, 040772243
 * Version: 0.0.1
 * Called functions: N/A
 * Parameters:
    - pBuffer const pBD
 * Return value: short, -1 (R_FAIL1)
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
 * Return value: char, -1 (R_FAIL1)
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
 * Return value: short, -1 (R_FAIL1)
 */
short b_getcoffset(Buffer* const pBD) {
    if (!pBD) { return R_FAIL1; }
    return pBD->getc_offset;
}

/* Sets a mark offset on the buffer's mark flag and returns a pointer
to cb_head at that offset
 * Author: Victor Fernandes, 040772243
 * Version: 0.0.1
 * Called functions: N/A
 * Parameters:
    - pBuffer const pBD
 * Return value: char*, NULL
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
 * Return value: 1, 0, -1 (R_FAIL1)
 */
int b_eob(Buffer* const pBD) {
    if (!pBD) { return R_FAIL1; }
    return (unsigned int) pBD->eob;
}

/* Adds one character symbol to the character buffer, incrementing its size if
possible and needed
 * Author: Victor Fernandes, 040772243
 * Version: 0.0.1
 * Called functions: b_isfull(), realloc()
 * Parameters:
    - pBuffer const pBD
    - char symbol (1-255)
 * Return value: pBuffer, NULL
 * Algorithm: Function checks if the cb_head's character size reached
 * maximum capacity and resizes if it's able (by the mode or SHRT_MAX),
 * then adds one character to cb_head.
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
            new_cap = pBD->capacity + (unsigned char) pBD->inc_factor;
            /* Make sure no short overflow happened */
            if (new_cap < ZERO_CAPACITY){
                return NULL;
            }

        }
        else if (pBD->mode == MUL_OP_MODE) { /* Calculate new size in multiplicative mode */
            if (pBD->capacity == SHRT_MAX){ /* Do nothing if at maximum size */
                return NULL;
            }

			/* msvc warns about possible loss of data when converting from double to short, this is fine 
			because the buffer doesn't deal with floating points*/
            avail_space = SHRT_MAX - pBD->capacity;
            new_inc = (short) (avail_space * (((double) pBD->inc_factor) / 100));

            /* Check if there is enough space for the new increment and
            "trim" it if needed*/
			/* Note: If the available space is 1 or 0, new_inc will evaluate to 0 (after truncation) */
            if (new_inc == ZERO_CAPACITY) {
                new_cap = SHRT_MAX;
            }
            else {
                new_cap = pBD->capacity + new_inc;
            }
        }

        /* Reallocate memory to character buffer */
        old_addr = pBD->cb_head; /* Keep track of old pointer address to check if it changed */
        tmp_addr = (char *)realloc(pBD->cb_head, sizeof(char) * (unsigned short) new_cap);
        
        if (tmp_addr == NULL){
            return NULL; /* Abort everything if allocation fails */
        }

        pBD->cb_head = tmp_addr;
        pBD->capacity = new_cap;
		/* Compare the old and new addresses and set flag appropriately */
        pBD->r_flag = (pBD->cb_head == old_addr);
    } /* END BUFFER INCREASE */

    /* Finally, add new symbol to the buffer after increasing it (or not) */
    pBD->cb_head[pBD->addc_offset++] = symbol;
    return pBD;

}

/* Gets one character symbol from the buffer
 * Author: Victor Fernandes, 040772243
 * Version: 0.0.1
 * Called functions: N/A
 * Parameters:
    - pBuffer const pBD
 * Return value: char, -2 (R_FAIL2), -1 (R_FAIL1)
 * Algorithm: Function will check if getc_offset is at the end
 * of the character array. If so, set flag and return failure.
 * Otherwise fetch character from cb_head, increment offset, and
 * check if it reached the EOB. If so, apply the flag before
 * returning the character.
 */
char b_getc(Buffer* const pBD) {

    if (!pBD) { return R_FAIL2; }
    /* Make sure the buffer isn't at the end of the read offset*/
    if (pBD->getc_offset == pBD->addc_offset) {
        pBD->eob = SET_EOB_FLAG;
        return R_FAIL1;
    }

    pBD->eob = UNSET_EOB_FLAG;
    /* Fetch character at read offset if EOB check passed */
    return pBD->cb_head[pBD->getc_offset++];

}

/* Prints the output of the buffer
 * Author: Victor Fernandes, 040772243
 * Version: 0.0.1
 * Called functions: b_isempty(), b_getc(), b_eob(), printf()
 * Parameters:
    - pBuffer const pBD
 * Return value: int, -1 (R_FAIL1)
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
            if (b_eob(pBD)) { break; }
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
     * Return value: int, -1 
 * Return value: int, -1 (R_FAIL1), -2 (LOAD_FAIL)
 */
int b_load(FILE* const fi, Buffer* const pBD) {
    char char_buf; /* "Buffer" character to be loaded before adding to cb_head */
    int char_count = 0; /* Character counter */

    if (!fi || !pBD) {
        return R_FAIL1;
    }

	while (!feof(fi)) {
        char_buf = (char) fgetc(fi);
		/* Check loaded character if it's at the end*/
        if (feof(fi)) {
            break;
        }
        /* Stop loading if adding a character fails */
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
 * Return value: 1 (TRUE), -1 (R_FAIL1)
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
 * Return value: short, -1 (R_FAIL1)
 */
short b_retract_to_mark(Buffer* const pBD) {
    /* Check if any offsets are out of bounds */
    if(!pBD ||
        pBD->mark_offset < OFFSET_RESET || 
        pBD->mark_offset > pBD->capacity){
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
 * Return value: short, -1 (R_FAIL1)
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
 * Return value: char, -1 (R_FAIL1)
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

    /* Avoid buffer destruction from overflow */
    if (new_cap <= 0) { return NULL;}

    old_addr = pBD->cb_head;

    /* Reallocate cb_head to new size */
    tmp_addr = realloc(pBD->cb_head, sizeof(char) * new_cap);
    if (!tmp_addr){ return NULL; } /* Abort if realloc failed */

    /* Assign new address to cb_head and reassign capacity value */
    pBD->cb_head = tmp_addr;
    pBD->capacity = new_cap;

    /* Compare old and new addresses and set R_FLAG accordingly */
    pBD->r_flag = (pBD->cb_head == old_addr);

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
    if (pBD){
        free(pBD->cb_head);
        free(pBD);
    }
}
