/* File Name: parser.c
 * The PLATYPUS parsing program for the final assignment of
 How To Train Your Dragon (AKA Compilers)
 * Author: Victor Fernandes, 040772243
 * Course: CST8152 - Compilers, Lab Section: 011
 * Date: April 21, 2017
 * Professor: Svillen Ravev
 * Version: 0.1
 */

#include "parser.h"

/* Global variables for the parser */
 Token lookahead;
 extern int synerrno;       /* Error counter */
 extern char* kw_table[];   /* Keyword table with matching enum */
 extern STD sym_table;      /* The symbol table */
 extern Buffer* sc_buf;     /* Scanner buffer */
 extern Buffer* str_LTBL;   /* String literal table */
 extern int line            /* Line position of the Scanner */


/* Begins the source file parsing
 * Author: Victor Fernandes, 040772243
 * Version: 0.0.1
 * Called functions: malar_next_token, program, match, gen_incode
 * Parameters: pBuffer - The input buffer
 * Return value: N/A
 */
void parser(pBuffer in_buf)
{
    sc_buf = in_buf;
    lookahead = malar_next_token(sc_buf);
    program();
    match(SEOF_T, NO_ATTR);
    gen_incode("PLATY: Source file parsed");
}

/* Matches the given token code+attribute pair to the current lookahead token
 * Author: Victor Fernandes, 040772243
 * Version: 0.0.1
 * Called functions: syn_eh, syn_printe, malar_next_token
 * Parameters: pr_token_code, pr_token_attribute
 * Return value:
 * Algorithm:
 */
void match(int pr_token_code, int pr_token_attribute)
{
    if (pr_token_code != lookahead.code)
    {
        syn_eh(pr_token_code);
        return;
    }

    switch (pr_token_code)
    {
    case ART_OP_T:
    case REL_OP_T:
    case LOG_OP_T:
    case KW_T:
        if (pr_token_attribute != lookahead.attribute.get_int)
        {
            break;
        }
    default:
        if (lookahead.code == SEOF_T)
            return;
        lookahead = malar_next_token(sc_buf);
        if (lookahead.code == ERR_T)
        {
            synerrno++;
            syn_printe();
            lookahead = malar_next_token(sc_buf);
        }
        return;
    }
    syn_eh(pr_token_code);
}

/* Error "recovery" function for the parser
 * Author: Victor Fernandes, 040772243
 * Version: 0.0.1
 * Called functions: syn_printe, malar_next_token, exit
 * Parameters: int pr_token_code
 * Return value:
 * Algorithm: Outputs a standard syn_printe error, and steps through
 the source file with the Scanner until it finds the next matching
 code needed by pr_token_code.
 */
void syn_eh(int pr_token_code)
{
    syn_printe();
    synerrno++;

    while (lookahead.code != SEOF_T)
    {
        lookahead = malar_next_token(sc_buf);
        if (lookahead.code == pr_token_code)
        {
            if (lookahead.code != SEOF_T)
            {
                lookahead = malar_next_token(sc_buf);
            }
            return;
        }
    }

    if (pr_token_code != SEOF_T)
        exit(synerrno);
}

/* Outputs an error message to stdout using the current lookahead token
 * Author: Victor Fernandes, 040772243
 * Version: 0.0.1
 * Called functions: printf, b_setmark
 * Parameters: N/A
 */
void syn_printe()
{
    printf("PLATY: Syntax error:   Line:%3d\n" + "*****  Token code: %3d Attribute: ", line, lookahead.code);
    switch (lookahead.code)
    {
    case ERR_T:
        printf("%s\n", lookahead.attribute.err_lex);
        break;
    case SEOF_T:
        printf("NA\n");
        break;
    case AVID_T:
    case SVID_T:
        printf("%s\n", sym_table.pstvr[lookahead.attribute.get_int].plex);
        break;
    case FPL_T:
        printf("%5.1f\n", lookahead.attribute.flt_value);
        break;
    case INL_T:
        printf("%d\n", lookahead.attribute.get_int);
        break;
    case STR_T:
        printf("%s\n", b_setmark(str_LTBL, lookahead.attribute.str_offset));
        break;
    case SCC_OP_T:
        printf("NA\n");
        break;
    case ASS_OP_T:
        printf("NA\n");
        break;
    case ART_OP_T:
        printf("%d\n", lookahead.attribute.get_int);
        break;
    case REL_OP_T:
        printf("%d\n", lookahead.attribute.get_int);
        break;
    case LOG_OP_T:
        printf("%d\n", lookahead.attribute.get_int);
        break;
    case LPR_T:
        printf("NA\n");
        break;
    case RPR_T:
        printf("NA\n");
        break;
    case LBR_T:
        printf("NA\n");
        break;
    case RBR_T:
        printf("NA\n");
        break;
    case KW_T:
        printf("%s\n", kw_table[lookahead.attribute.get_int]);
        break;
    case COM_T:
        printf("NA\n");
        break;
    case EOS_T:
        printf("NA\n");
        break;
    default:
        printf("PLATY: Scanner error: invalid token code: %d\n", lookahead.code);
    }
}

/* Generates a message to stdout (can be later modified to generate C code)
 * Author: Victor Fernandes, 040772243
 * Version: 0.0.1
 * Called functions: printf
 * Parameters: char* code
 * Return value: N/A
 */
void gen_incode(char *code)
{
    printf("%s\n", code);
}

/*
 * Grammar functions ahoy 
 */


/*
 * <additive_arithmetic_expression> ->
    <multiplicative_arithmetic_expression> <additive_arithmetic_expression_prime>
    FIRST Set = {AVID_T, FPL_T, INL_T, (}
 */
void additive_arithmetic_expression()
{
    multiplicative_arithmetic_expression();
    additive_arithmetic_expression_prime();
}

/*
 * <additive_arithmetic_expression> ->
    <multiplicative_arithmetic_expression> <additive_arithmetic_expression_prime>
    FIRST Set = {AVID_T, FPL_T, INL_T, (}
 */
void additive_arithmetic_expression_prime()
{
    if (lookahead.code == ART_OP &&
        lookahead.attribute.arr_op != MULT &&
        lookahead.attribute.arr_op != DIV)
    {
        match(lookahead.code, lookahead.attribute.arr_op);
        multiplicative_arithmetic_expression();
        additive_arithmetic_expression_prime();
        gen_incode("PLATY: Additive arithmetic expression parsed");
    }
}

/*
 * <opt_LPR_T> <unary_arithmetic_expression> <opt_RPR_T> | 
    <opt_LPR_T> <additive_arithmetic_expression> <opt_RPR_T>
    FIRST Set = {-, +, AVID_T, FPL_T, INL_T, (}
 */
void arithmetic_expression()
{
    switch (lookahead.code)
    {
    case ART_OP_T:
        switch (lookahead.attribute.arr_op)
        {
        case MULT:
        case DIV:
            syn_printe();
            return;
        }
        unary_arithmetic_expression();
        break;
    case AVID_T:
    case FPL_T:
    case INL_T:
    case LPR_T:
        additive_arithmetic_expression();
        break;
    case EOS_T:
        return;
    default:
        syn_printe();
        return;
    }
    gen_incode("PLATY: Arithmetic expression parsed");
}

/*
 * <assignment_expression> ->
    AVID = <arithmetic_expression> | SVID = <string_expression>
    FIRST Set = {AVID, SVID}
 */
void assignment_expression()
{
    switch (lookahead.code)
    {
    case AVID_T:
        match(AVID_T, NO_ATTR);
        match(ASS_OP_T, NO_ATTR);
        arithmetic_expression();
        gen_incode("PLATY: Assignment expression (arithmetic) parsed");
        break;
    case SVID_T:
        match(SVID_T, NO_ATTR);
        match(ASS_OP_T, NO_ATTR);
        string_expression();
        gen_incode("PLATY: Assignment expression (string) parsed");
        break;
    default:
        syn_printe();
    }
}

/*
 * <assignment_statement> ->
    <assignment_expression> ;
    FIRST Set = {AVID, SVID}
 */
void assignment_statement()
{
    assignment_expression();
    match(EOS_T, NO_ATTR);
    gen_incode("PLATY: Assignment statement parsed");
}

/*
 * <conditional_expression> ->
    <logical_or_expression>
    FIRST Set = {AVID_T, SVID_T, INL_T, SVID_T, STR_T}
 */
void conditional_expression()
{
    logical_or_expression();
    gen_incode("PLATY: Conditional expression parsed");
}

/*
 * <input_statement> ->
    INPUT ( <variable_list> );
    FIRST Set = {INPUT}
 */
void input_statement()
{
    match(KW_T, INPUT);
    match(LPR_T, NO_ATTR);
    variable_list();
    match(RPR_T, NO_ATTR);
    match(EOS_T, NO_ATTR);
    gen_incode("PLATY: Input statement parsed");
}

/*
 * <iteration_statement> ->
    USING ( <assignment_expression>, <conditional_expression>, <assignment_expression> ) REPEAT { <opt_statements> } ;
    FIRST Set = {USING}
 */
void iteration_statement()
{
    match(KW_T, USING);
    match(LPR_T, NO_ATTR);
    assignment_expression();
    match(COM_T, NO_ATTR);
    assignment_expression();
    match(RPR_T, NO_ATTR);
    match(KW_T, REPEAT);
    match(LBR_T, NO_ATTR);
    opt_statements();
    match(RBR_T, NO_ATTR);
    match(EOS_T, NO_ATTR);
    gen_incode("PLATY: USING statement parsed");
}

/*
 * <logical_and_expression> ->
    <relational_expression> <logical_and_expression_prime>
    FIRST Set = {AVID_T, FPL_T, INL_T, SVID_T, STR_T}
 */
void logical_and_expression()
{
    relational_expression();
    logical_and_expression_prime();
}

/*
 * <logical_and_expression_prime> ->
    .AND.  <relational_expression> <logical_and_expression_prime> | E
    FIRST Set = { .AND., E}
 */
void logical_and_expression_prime()
{
    if (lookahead.code == LOG_OP_T &&
        lookahead.attribute.log_op == AND)
    {
        match(LOG_OP_T, AND);
        relational_expression();
        logical_and_expression_prime();
        gen_incode("PLATY: Logical AND expression parsed");
    }
}

/*
 * <logical_or_expression> ->
    <logical_and_expression> <logical_or_expression_prime>
    FIRST Set = {AVID_T, FPL_T, INL_T, SVID_T, STR_T}
 */
void logical_or_expression()
{
    logical_and_expression();
    logical_or_expression_prime();
}

/*
 * <logical_or_expression_prime> ->
    .OR.  <logical_and_expression> <logical_or_expression_prime>
    FIRST Set = { .OR., E }
 */
voic logical_or_expression_prime()
{
    if (lookahead.code == LOG_OP_T &&
        lookahead.attribute.log_op == OR)
    {
        logical_and_expression();
        logical_or_expression_prime();
        gen_incode("PLATY: Logical OR expression parsed");
    }
}

/*
 * <multiplicative_arithmetic_expression> ->
    <primary_arithmetic_expression> <multiplicative_arithmetic_expression_prime>
    FIRST Set = {AVID_T, FPL_T, INL_T, (}
 */
void multiplicative_arithmetic_expression()
{
    primary_arithmetic_expression();
    multiplicative_arithmetic_expression_prime();
}

/*
 * <multiplicative_arithmetic_expression_prime> ->
    <primary_arithmetic_expression> <multiplicative_arithmetic_expression_prime>
    FIRST Set = {*, /, E}
 */
void multiplicative_arithmetic_expression_prime()
{
    if (lookahead.code == ART_OP_T &&
        lookahead.attribute.arr_op != PLUS &&
        lookahead.attribute.arr_op != MINUS)
    {
        match(lookahead.code, lookahead.attribute.arr_op);
        primary_arithmetic_expression();
        multiplicative_arithmetic_expression_prime();
        gen_incode("PLATY: Multiplicative arithmetic expression parsed");
    }
}

/*
 * <opt_statements> ->
    <statements> | E
    FIRST Set = {AVID_T, SVID_T, IF, USING, INPUT, OUTPUT, E}
 */
void opt_statements()
{
    switch (lookahead.code)
    {
    case KW_T:
        switch (lookahead.attribute.kwt_idx)
        {
        case PLATYPUS:
        case ELSE:
        case THEN:
        case REPEAT:
            gen_incode("PLATY: Opt_statements parsed");
            return;
        }
    case AVID_T:
    case SVID_T:
        statements() : break;
    default:
        gen_incode("PLATY: Opt_statements parsed");
    }
}

/*
 * <output_statement> ->
    OUTPUT ( <output_statement_prime> );

    <output_statement_prime> ->
    <variable_list> | <STR_T>

    FIRST(<output_statement>) = {OUTPUT}
    FIRST Set = {AVID_T, SVID_T, E}

    This can be done in one function
 */
void output_statement()
{
    match(KW_T, OUTPUT);
    match(LPR_T, NO_ATTR);

    switch (lookahead.code)
    {
    case AVID_T:
    case SVID_T:
        variable_list();
        break;
    case STR_T:
        match(STR_T, NO_ATTR);
        gen_incode("PLATY: Output list (string literal) parsed");
        break;
    default:
        gen_incode("PLATY: Output list (empty) parsed");
    }
    match(RPR_T, NO_ATTR);
    match(EOS_T, NO_ATTR);
    gen_incode("PLATY: OUTPUT statement parsed");
}

/*
 * <primary_a_relational_expression> ->
    AVID_T | FPL_T | INL_T
    FIRST Set = {AVID_T, FPL_T, INL_T}
 */
void primary_a_relational_expression()
{
    switch (lookahead.code)
    {
    case AVID_T:
    case FPL_T:
    case INL_T:
        match(lookahead.code, NO_ATTR);
        break;
    case LOG_OP_T:
        break;
    default:
        syn_printe();
    }
    gen_incode("PLATY: Primary a_relational expression parsed");
}

/*
 * <primary_arithmetic_expression> ->
    AVID_T | FPL_T | INL_T | ( <arithmetic_expression> )
    FIRST Set = {AVID_T, FPL_T, INL_T, (}
 */
void primary_arithmetic_expression()
{
    switch (lookahead.code)
    {
        case AVID_T:
        case FPL_T:
        case INL_T:
            match(lookahead.code, lookahead.attribute.arr_op);
            break;
        case LPR_T:
            match(lookahead.code, lookahead.attribute,arr_op);
            arithmetic_expression();
            match(RPR_T, NO_ATTR);
        default:
            syn_printe();
            return;
    }
    gen_incode("PLATY: Primary arithmetic expression parsed");
}

/*
 * <primary_s_relational_expression> ->
    <primary_string_expression>
    FIRST Set = {SVID_T, STR_T}
 */
void primary_s_relational_expression()
{
    switch (lookahead.code)
    {
    case SVID_T:
    case AVID_T:
        primary_string_expression();
        break;
    default:
        syn_printe();
    }
    gen_incode("PLATY: Primary s_relational expression parsed");
}

/*
 * <primary_string_expression> ->
    SVID_T | STR_T
    FIRST Set = {SVID_T, STR_T}
 */
void primary_string_expression()
{
    switch (lookahead.code)
    {
    case SVID_T:
    case STR_T:
        match(lookahead.code, NO_ATTR);
        break;
    default:
        syn_printe();
    }
    gen_incode("PLATY: Primary string expression parsed");
}

/*
 * <program> ->
    PLATYPUS { <opt_statements> }
    FIRST Set = {PLATYPUS}
 */
void program()
{
    match(KW_T, PLATYPUS);
    match(LBR_T, NO_ATTR);
    opt_statements();
    match(RBR_T, NO_ATTR);
    gen_incode("PLATY: Program parsed");
}

/*
 * <relational_expression> ->
    <primary_a_relational_expression> <primary_a_relational_expression>
    | <primary_s_relational_expression> <primary_s_relational_expression_prime>
    FIRST Set = {AVID_T, FPL_T, INL_T, SVID_T, STR_T}
 */
void relational_expression()
{
    switch (lookahead.code)
    {
    case AVID_T:
    case FPL_T:
    case INL_T:
        primary_a_relational_expression();
        relational_expression_prime();
        break;
    case SVID_T:
    case STR_T:
        primary_s_relational_expression();
        relational_expression_prime_string();
        break;
    default:
        syn_printe();
    }
    gen_incode("PLATY: Relational expression parsed");
}

/*
 * <relational_expression_prime> ->
    == <primary_a_relational_expression>
    | <> <primary_a_relational_expression>
    | > <primary_a_relational_expression>
    | < <primary_a_relational_expression>
    FIRST Set = { ==, <>, >, <}
 */
void relational_expression_prime()
{
    if (lookahead.code == REL_OP_T)
    {
        switch (lookahead.attribute.rel_op)
        {
        case EQ:
        case NE:
        case GT:
        case LT:
            match(lookahead.code, lookahead.attribute.arr_op);
            primary_a_relational_expression();
            return;
        }
    }
    syn_printe();
}

/*
 * <relational_expression_prime_string> ->
     == <primary_s_relational_expression>
     | <> <primary_s_relational_expression>
     | > <primary_s_relational_expression>
     | < <primary_s_relational_expression>
     FIRST Set = {==, <>, >, <}
 */
void relational_expression_prime_string()
{
    if (lookahead.code == REL_OP_T)
    {
        switch (lookahead.attribute.rel_op)
        {
        case EQ:
        case NE:
        case GT:
        case LT:
            match(lookahead.codem, lookahead.attribute.arr_op);
            primary_s_relational_expression();
            return;
        }
    }
    syn_printe();
}

/*
 * <selection_statement> ->
    IF ( <conditional_expression> ) THEN <opt_statements> ELSE { <opt_statements> } ;
    FIRST Set = {IF}
 */
void selection_statement()
{
    match(KW_T, IF);
    match(LPR_T, NO_ATTR);
    conditional_expression();
    match(RPR_T, NO_ATTR);
    match(KW_T, THEN);
    opt_statements();
    match(KW_T, ELSE);
    match(LBR_T, NO_ATTR);
    opt_statements();
    match(RBR_T, NO_ATTR);
    match(EOS_T, NO_ATTR);
    gen_incode("PLATY: IF statement parsed");
}

/*
 * <statement> ->
    <assignment_statement>
    | <selection_statement>
    | <iteration_statement>
    | <input_statement>
    | <output_statement>
    FIRST Set = {AVID_T, SVID_T, IF, USING, INPUT, OUTPUT}
 */
void statement()
{
    switch (lookahead.code)
    {
    case AVID_T:
    case SVID_T:
        assignment_statement();
        break;
    case KW_T:
        switch (lookahead.attribute.kwt_idx)
        {
        case IF:
            selection_statement();
            break;
        case USING:
            iteration_statement();
            break;
        case INPUT:
            input_statement();
            break;
        case OUTPUT:
            output_statement();
            break;
        default:
            syn_printe();
        }
        break;
    default:
        syn_printe();
    }
}

/*
 * <statements> ->
    <statement> <statements_prime>
    FIRST Set = {AVID_T, SVID_T, IF, USING, INPUT, OUTPUT}
 */
void statements()
{
    statement();
    statements_prime();
}

/*
 * <statements_prime> ->
    <statement> <statements_prime> | E
    FIRST Set = {AVID_T, SVID_T, IF, USING, INPUT, OUTPUT}
 */
void statements_prime()
{
    switch (lookahead.code)
    {
    case KW_T:
        switch (lookahead.attribute.kwt_idx)
        {
        case PLATYPUS:
        case ELSE:
        case THEN:
        case REPEAT:
            return;
        }
    case AVID_T:
    case SVID_T:
        statement();
        statements_prime();
        break;
    }
}

/*
 * <string_expression> ->
    <primary_string_expression> <string_expression_prime>
    FIRST Set = {SVID_T, STR_T}
 */
void string_expression()
{
    primary_string_expression();
    string_expression_prime();
    gen_incode("PLATY: String expression parsed");
}

/*
 * <string_expression_prime>
  << <primary_string_expression> <stirng_expression_prime> | E
  FIRST Set = {<< , E}
 */
void string_expression_prime()
{
    if (lookahead.code == SCC_OP_T)
    {
        match(SCC_OP_T, NO_ATTR);
        primary_string_expression();
        stirng_expression_prime();
    }
}

/*
 * <unary_arithmetic_expression> ->
    - <primary_arithmetic_expression>
    | + <primary_arithmetic_expression>
    FIRST Set = {-, +}
 */
void unary_arithmetic_expression()
{
    switch (lookahead.code)
    {
    case ART_OP_T:
        switch (lookahead.attribute.arr_op)
        {
        case MULT:
        case DIV:
            syn_printe();
            return;
        }
        match(lookahead.code, lookahead.attribute.arr_op);
        primary_arithmetic_expression();
        gen_incode("PLATY: Unary arithmetic expression parsed");
        break;
    default:
        syn_printe();
        return;
    }
}

/*
 * <variable_identifier> ->
    AVID_T | SVID_T
    FIRST Set = {AVID_T, SVID_T}
 */
void variable_identifier()
{
    switch (lookahead.code)
    {
    case AVID_T:
    case SVID_T:
        match(lookahead.code, NO_ATTR);
        break;
    default:
        syn_printe();
    }
}

/*
 * <variable_list> ->
    <variable_identifier> <variable_list_prime>
    FIRST Set = {AVID_T, SVID_T}
 */
void variable_list()
{
    variable_identifier();
    variable_list_prime();
    gen_incode("PLATY: Variable list parsed");
}

/*
 * <variable_list_prime> ->
    , <variable_identifier> <variable_list_prime> | E
    FIRST Set = {AVID_T, SVID_T, E}
 */
void variable_list_prime()
{
    if (lookahead.code != COM_T)
        return;
    match(COM_T, NO_ATTR);
    variable_identifier();
    variable_list_prime();
}
