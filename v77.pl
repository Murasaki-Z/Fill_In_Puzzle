%Not final submission


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
	squareHelper(S1,Puzzle),
	transpose(Puzzle,TPuzzle),
	squareHelper(S2,TPuzzle),
	append(S1,S2,Square).

same_chars([],_).
same_chars(Item,List):-
	Item = [_|Last],
	List = [_|LastLists],
	same_chars(Last,LastLists).

canJoin(W1, S1) :-
	same_length(W1,S1),
	same_chars(W1,S1).

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
squareHelper(Square,RowVar) :-
	RowVar = [Row|Rows],
    getsquares(Row, [], S1),
    squareHelper(S2,Rows),
    append(S1, S2, Square).



fillUtil([], _).
fillUtil(Item, WordList) :-
	Item = [Square|S2],
	
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
comboUtil(Var, WordList, Least, Current,
	BestMatch, Input, Output) :-
	Var = [Square|OtherSquares],
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
	%(	canJoin(Word, Square)
	
	%->	
	combinationCheck(Square, OtherWords, CurrentMatches+1, Total).
	%;	combinationCheck(Square, OtherWords, CurrentMatches, Total)









