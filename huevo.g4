/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

grammar huevo;
DEF_TOKEN: 'def';
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
NEWLINE: [\n\r]+ -> channel(HIDDEN);
SPACE: [\t ]+ -> channel(HIDDEN);
UPPER_CASE: [A-Z];
LOWER_CASE: [a-z];
DIGIT: [0-9];

arith_term_op: PLUS | MINUS;
arith_factor_op: ASTERISK | SLASH;

prog: expressions EOF;
expressions: (expression SEMI_COLUMN)+;
expression: fun_decl|function_call|arith_expression;

function_call: identifier LPARAN (expression (COMMA expression)*)? RPARAN;
fun_def: DEF_TOKEN identifier LPARAN argument_list RPARAN COLUMN type;
argument_list: argument (COMMA argument)* |;
argument: identifier COLUMN type;
fun_decl: fun_def EQUAL LCURY expressions RCURY;

arith_expression: arith_term arith_term_op arith_expression | arith_term;
arith_term: arith_factor arith_factor_op arith_term | arith_factor;
arith_factor: number | identifier | function_call | LPARAN expression RPARAN;

identifier: LOWER_CASE (LOWER_CASE|UPPER_CASE|DIGIT)*;
classifier: UPPER_CASE (LOWER_CASE|UPPER_CASE|DIGIT)*;
type: classifier | func_type;
func_type: LPARAN (classifier (COMMA classifier)*)? RPARAN EQUAL GREAT_THAN classifier;
number: DIGIT DIGIT* (DOT DIGIT+)?;