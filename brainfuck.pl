%% brainfuck.pl

%% An interpreter for the esoteric brainf*ck programming language.
%%
%% Copyright (c) 2010 Daniel de Kok <me@danieldk.eu>
%% Copyright (c) 2010 Harm Brouwer <harm.brouwer@rug.nl>
%%
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with this program.  If not, see <http://www.gnu.org/licenses/>.

:- module(brainfuck,[bfs/1,bfi/1]).

:- use_module(library(lists)).

bfs(SourceFile) :-
	read_source_file(SourceFile,Instructions),
	bfi(Instructions).

bfi(Instructions) :-
    make_data(30000,RData),
    interpret(Instructions,[],[],RData,_).

make_data(Len,List) :-
    length(List,Len),
    initialize_data(List).

initialize_data([]).
initialize_data([0|T]) :-
    initialize_data(T).

%%%%%%%%%%%%%%%
% read source %
%%%%%%%%%%%%%%%

read_source_file(SourceFile,Instructions) :-
	open(SourceFile,read,SourceStream),
	read_instruction_list(SourceStream,Instructions),
	close(SourceStream).

read_instruction_list(SourceStream,Instructions) :-
	read_instruction_list(SourceStream,[],Instructions).

read_instruction_list(SourceStream,Accumulator,Instructions) :-
	get_char(SourceStream,Char),
	(	Char \== end_of_file
	->	(	is_instruction(Char)
		->	append(Accumulator,[Char],Accumulator0),
			read_instruction_list(SourceStream,Accumulator0,Instructions)
		;	read_instruction_list(SourceStream,Accumulator,Instructions)
		)
	;	Instructions = Accumulator
	).

is_instruction(Char) :-
	memberchk(Char,['>','<','+','-','.',',','[',']','#']).

%%%%%%%%%%%%%%%
% Interpreter %
%%%%%%%%%%%%%%%

interpret([],_,LData0,RData,Data) :-
    lists:reverse(LData0,LData),
    lists:append(LData,RData,Data).
interpret([>|RInstr],LInstr,LData,[H|RData],Data) :-
    interpret(RInstr,[>|LInstr],[H|LData],RData,Data).
interpret([<|RInstr],LInstr,[H|LData],RData,Data) :-
    interpret(RInstr,[<|LInstr],LData,[H|RData],Data).
interpret([+|RInstr],LInstr,LData,[H|RData],Data) :-
    NewH is H + 1,
    interpret(RInstr,[+|LInstr],LData,[NewH|RData],Data).
interpret([-|RInstr],LInstr,LData,[H|RData],Data) :-
    NewH is H - 1,
    interpret(RInstr,[-|LInstr],LData,[NewH|RData],Data).
interpret([.|RInstr],LInstr,LData,[H|RData],Data) :-
    format("~c",[H]),
    interpret(RInstr,[.|LInstr],LData,[H|RData],Data).
interpret([','|RInstr],LInstr,LData,[_|RData],Data) :-
    read_line(user_input,[Byte]),
    interpret(RInstr,[','|LInstr],LData,[Byte|RData],Data).
interpret(['['|RInstr0],LInstr0,LData,[H|RData],Data) :-
    (  H == 0
    -> jump_fwd(RInstr0,['['|LInstr0],0,RInstr,LInstr)
    ;  RInstr = RInstr0, LInstr = ['['|LInstr0]
    ),
    interpret(RInstr,LInstr,LData,[H|RData],Data).
interpret([']'|RInstr0],LInstr0,LData,[H|RData],Data) :-
    (  H == 0
    -> RInstr = RInstr0, LInstr = [']'|LInstr0]
    ;  jump_bwd(LInstr0,[']'|RInstr0],0,LInstr,RInstr)
    ),
    interpret(RInstr,LInstr,LData,[H|RData],Data).
interpret([#|RInstr],LInstr,LData,RData,Data) :-
    debug_dump(RData,LData),
    interpret(RInstr,[#|LInstr],LData,RData,Data).

jump_fwd([']'|RInstr],LInstr,0,RInstr,[']'|LInstr]) :- !.
jump_fwd([']'|RInstr0],LInstr0,N,RInstr,LInstr) :-
    !,
    NNew is N - 1,
    jump_fwd(RInstr0,[']'|LInstr0],NNew,RInstr,LInstr).
jump_fwd(['['|RInstr0],LInstr0,N,RInstr,LInstr) :-
    NNew is N + 1,
    jump_fwd(RInstr0,['['|LInstr0],NNew,RInstr,LInstr).
jump_fwd([H|RInstr0],LInstr0,N,RInstr,LInstr) :-
    jump_fwd(RInstr0,[H|LInstr0],N,RInstr,LInstr).

jump_bwd(['['|LInstr],RInstr,0,['['|LInstr],RInstr) :- !.
jump_bwd(['['|LInstr0],RInstr0,N,LInstr,RInstr) :-
    !,
    NNew is N - 1,
    jump_bwd(LInstr0,['['|RInstr0],NNew,LInstr,RInstr).
jump_bwd([']'|LInstr0],RInstr0,N,LInstr,RInstr) :-
    !,
    NNew is N + 1,
    jump_bwd(LInstr0,[']'|RInstr0],NNew,LInstr,RInstr).
jump_bwd([H|LInstr0],RInstr0,N,LInstr,RInstr) :-
    jump_bwd(LInstr0,[H|RInstr0],N,LInstr,RInstr).

%%%%%%%%%%%%%
% debugging %
%%%%%%%%%%%%%

%% dump the values of the first ten memory cells
debug_dump(RData,LData0) :-
    lists:reverse(LData0,LData),
    lists:append(LData,RData,Data),
    list_first_n(Data,10,First),
    format(user_error,"~q~n",[First]).

list_first_n(List,N,Prefix) :-
    length(Prefix,N),
    lists:prefix(Prefix,List).
