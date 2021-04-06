parser grammar LitmusTestParser;

options { tokenVocab=LitmusTestLexer; }


body: init prog result;

init: LEFTCUR values RIGHTCUR;

prog: PROC0 OR PROC1 SEMI insts;

insts: (inst OR inst SEMI)+;

result: EXISTS LEFTPAREN exps RIGHTPAREN;

values: (exp SEMI)+;

inst: (label COLON | sw | lw | fence | bne | xor | ori | add | );

label: LABEL;

exps: exp AND exp;

exp: (COREID eq | eq);

eq: loc EQUAL value;

value: (NUMBER | X | Y);

loc: (REG | X | Y);

sw: SW REG COMMA addr;

lw: LW REG COMMA addr;

fence: FENCE;

bne: BNE REG COMMA REG COMMA label;

xor: XOR REG COMMA REG COMMA REG;

ori: ORI REG COMMA REG COMMA NUMBER;

add: ADD REG COMMA REG COMMA REG;

addr: NUMBER LEFTPAREN REG RIGHTPAREN;