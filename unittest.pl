:- use_module(brainfuck).

unit_test(+) :-
    brainfuck:interpret([+],[],[],[0],[1]).

unit_test(-) :-
    brainfuck:interpret([-],[],[],[1],[0]).

unit_test(>) :-
    brainfuck:interpret([>,+],[],[],[0,0],[0,1]).

unit_test(<) :-
    brainfuck:interpret([<,+],[],[0],[0],[1,0]).

test :-
    findall(Test,unit_test(Test),L),
    format("Succesful: ~q~n",[L]).