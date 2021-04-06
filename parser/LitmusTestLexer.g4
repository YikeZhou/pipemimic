lexer grammar LitmusTestLexer;

WS: [ \t\n\r]+ -> skip ;

NUMBER: [0-9]+;
PROC0: 'P0';
PROC1: 'P1';
X: 'x';
Y: 'y';

COMMA: ',';
EQUAL: '=';
COLON: ':';
SEMI: ';';
AND: '/\\';
OR: '|';
LEFTPAREN: '(';
RIGHTPAREN: ')';
LEFTCUR: '{';
RIGHTCUR: '}';

LABEL: 'LC0' [0-1];
REG: 'x' NUMBER;
COREID: [0-1] ':';
EXISTS: 'exists';

FENCE: 'fence rw,rw';
BNE: 'bne';
XOR: 'xor';
ORI: 'ori';
ADD: 'add';
SW: 'sw';
LW: 'lw';
