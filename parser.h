/* File Name: parser.h
 * Parser function definitions for the PLATYPUS Compiler
 * Author: Victor Fernandes, 040772243
 * Course: CST8152 - Compilers, Lab Section: 011
 * Date: April 17, 2017
 * Professor: Svillen Ravev
 * Version: 0.1
 */
#ifndef PARSER_H_
#define PARSER_H_

#include "token.h"
#include "stable.h"
#include "buffer.h"

/* Keyword enum to match kw_table in table.h */
enum kw {
    NO_ATTR = -1,
    ELSE,
    IF,
    INPUT,
    OUTPUT,
    PLATYPUS,
    REPEAT,
    THEN,
    USING
};

/* Main parser functions */
 void parser(Buffer* const);
 void gen_incode(char*);
 void match(int, int);
 void syn_eh(int);
 void syn_printe(void);

 /* Literally the entire grammar */
void additive_arithmetic_expression(void);
void additive_arithmetic_expression_prime(void);
void arithmetic_expression(void);
void assignment_expression(void);
void assignment_statement(void);
void conditional_expression(void);
void input_statement(void);
void iteration_statement(void);
void logical_and_expression(void);
void logical_and_expression_prime(void);
void logical_or_expression(void);
void logical_or_expression_prime(void);
void multiplicative_arithmetic_expression(void);
void multiplicative_arithmetic_expression_prime(void);
void opt_statements(void);
void output_statement(void);
void primary_a_relational_expression(void);
void primary_arithmetic_expression(void);
void primary_s_relational_expression(void);
void primary_string_expression(void);
void program(void);
void relational_expression(void);
void relational_expression_prime(void);
void relational_expression_prime_string(void);
void selection_statement(void);
void statement(void);
void statements(void);
void statements_prime(void);
void string_expression(void);
void unary_arithmetic_expression(void);
void variable_identifier(void);
void variable_list(void);
void variable_list_prime(void);

#endif
