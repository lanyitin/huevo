/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

grammar huevo;
DEF_TOKEN: 'def';
IF_TOKEN: 'if';
ELSE_TOKEN: 'else';
BOOLEAN_OP: '&&' | '||';
LPARAN: '(';
RPARAN: ')';
LCURY: '{';
RCURY: '}';
GREAT_THAN: '>';
LESS_THAN: '<';
COLUMN: ':';
SEMI_COLUMN:';';
COMMA: ',';
DOT: '.';
PLUS: '+';
MINUS: '-';
ASTERISK: '*';
SLASH: '/';
EQUAL: '=';
NOT: '!';
NEWLINE: [\n\r]+;
SPACE: [\t ]+ -> channel(HIDDEN);
BOOLEAN_LITERAL: 'true' | 'false';
IDENTIFIER: [a-z][a-zA-Z0-9]*;
CLASSIFIER: [A-Z][a-zA-Z0-9]*;
UPPER_CASE: [A-Z];
LOWER_CASE: [a-z];
DIGIT: [0-9];

prog: expressions EOF;
expressions: (expression expr_sep)+;
expression: fun_decl|function_call|arith_expression|if_expression;

function_call: IDENTIFIER LPARAN (expression (COMMA expression)*)? RPARAN;
fun_def: DEF_TOKEN IDENTIFIER LPARAN argument_list RPARAN COLUMN type;
argument_list: argument (COMMA argument)* |;
argument: IDENTIFIER COLUMN type;
fun_decl: fun_def EQUAL LCURY NEWLINE? expressions NEWLINE? RCURY;
func_type: LPARAN (CLASSIFIER (COMMA CLASSIFIER)*)? RPARAN EQUAL GREAT_THAN CLASSIFIER;

arith_expression: arith_term arith_term_op arith_expression | arith_term;
arith_term: arith_factor arith_factor_op arith_term | arith_factor;
arith_factor: number | IDENTIFIER | function_call | LPARAN expression RPARAN;
arith_term_op: PLUS | MINUS;
arith_factor_op: ASTERISK | SLASH;

if_expression: IF_TOKEN LPARAN boolean_expression RPARAN (LCURY NEWLINE? expressions RCURY | expression) ELSE_TOKEN (LCURY NEWLINE? expressions RCURY | expression);
boolean_expression: bool_term BOOLEAN_OP boolean_expression | bool_term;
bool_term: (IDENTIFIER | BOOLEAN_LITERAL) | arith_expression arith_boolean_op arith_expression;
arith_boolean_op: EQUAL EQUAL| NOT EQUAL | GREAT_THAN | GREAT_THAN EQUAL | LESS_THAN | LESS_THAN EQUAL;

type: CLASSIFIER | func_type;
number: DIGIT DIGIT* (DOT DIGIT+)?;
expr_sep: NEWLINE | SEMI_COLUMN;