%changed some IF statements around
%Need to change number of arguements and order next


:- ensure_loaded(library(clpfd)).
:- set_prolog_flag(answer_write_options,
                   [ quoted(true),
                     portray(true),
                     spacing(next_argument)
                   ]).

puzzle_solution(Puzzle, WordList) :-
	squareUtil(Puzzle,Square),
	fillUtil(Square, WordList).


squareUtil(Puzzle,Square):-
	squareHelper(Puzzle,S1),
	transpose(Puzzle,TPuzzle),
	squareHelper(TPuzzle,S2),
	append(S1,S2,Square).

same_chars([],_).
same_chars([Start|Last],[StartList|LastLists]):-
	same_chars(Last,LastLists).

canJoin(S1, W1) :-
	same_length(S1,W1),
	same_chars(S1,W1).

join(X2, X2).

getsquares([], Currentsquare, Square) :-
    length(Currentsquare, N),
    (   N =< 1
    ->  Square = []
    ;   Square = [Currentsquare]
    ).

getsquares([Var|Vars], Currentsquare, Square) :-
    (   Var == '#'
    ->	length(Currentsquare, N),
        (   N > 1
        ->  Square = [Currentsquare|S1]
        ;   Square = S1
        ),
        getsquares(Vars, [], S1)
	;  append(Currentsquare, [Var], Currentsquare1),
        getsquares(Vars, Currentsquare1, Square)  
    ).



squareHelper([], []).
squareHelper([Row|Rows], Square) :-
    getsquares(Row, [], S1),
    squareHelper(Rows, S2),
    append(S1, S2, Square).



fillUtil([], _).
fillUtil([Square|S2], WordList) :-
	
	combinationCheck(Square, WordList, 0, Matches),


	comboUtil(S2, WordList, Matches, Square, BestMatch, [], New),
	
	filler(BestMatch, WordList, [], NewWordList),

	
	fillUtil(New, NewWordList).

filler(Square, [Word|WordList], NewWordListIn, NewWordListOut) :-
	
	join(Square, Word),
	append(WordList, NewWordListIn, NewWordListOut);

	append(NewWordListIn, [Word], NewWordListTemp),
	filler(Square, WordList, NewWordListTemp, NewWordListOut).



comboUtil([], _, Least, Current, Current,Input, Input) :-
	Least > 0.
comboUtil([Square|OtherSquares], WordList, Least, Current,
	BestMatch, Input, Output) :-
	combinationCheck(Square, WordList, 0, Total),
	(	Total >= Least
	->	append(Input, [Square], InputTemp),
		comboUtil(OtherSquares, WordList, Least, Current,
			BestMatch, InputTemp, Output)

	;	append(Input, [Current], InputTemp),
		comboUtil(OtherSquares, WordList, Total, Square, BestMatch,
			InputTemp, Output)	
		
	).


combinationCheck(_, [], CurrentMatches, CurrentMatches).
combinationCheck(Square, [Word|OtherWords], CurrentMatches, Total) :-
	(	canJoin(Word, Square)
	->	combinationCheck(Square, OtherWords, CurrentMatches+1, Total)
	;	combinationCheck(Square, OtherWords, CurrentMatches, Total)
	).




