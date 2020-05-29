
:- ensure_loaded(library(clpfd)).


puzzle_solution(Puzzle, WordList) :-
	
	transpose(Puzzle, TPuzzle),
	slotUtil(Puzzle, [], S1),
	slotUtil(TPuzzle, S1, S2),
	fillUtil(S2, WordList).




slotUtil([], InSlot, InSlot).
slotUtil([Row|Others], InSlot, OutSlot) :-
	rowUtil(Row, [], InSlot, Temp),
	slotUtil(Others, Temp, OutSlot).


same_chars([],_).
same_chars([First|Last],[FirstLists|LastLists]):-
	(	var(First)
	->	same_chars(Last,LastLists)
	;	First = FirstLists,
		same_chars(Last,LastLists)
	).


canJoin(S1, W1) :-
	same_length(S1,W1),
	same_chars(S1,W1).
join(X2, X2).

%Maybe compatible with get_row_slots -- adam
rowUtil([], Word, InSlot, OutSlot) :-
	(	Word == []
	->	join(OutSlot, InSlot)
	;	(	length(Word, Length), 	Length > 1
		->	append(InSlot, [Word], OutSlot)
		;	join(OutSlot, InSlot)
		)
	).
rowUtil([Item|OtherItems], Word, InSlot, OutSlot) :-
	(	Item == '#'
	->	(	Word == []
		->	rowUtil(OtherItems, [], InSlot, OutSlot)
		;	(	length(Word, Length), Length > 1
			->	append(InSlot, [Word], Temp),
				rowUtil(OtherItems, [], Temp, OutSlot)
			;	rowUtil(OtherItems, [], InSlot, OutSlot)
			)
		)
	;	append(Word, [Item], NewCurrentWord),
		rowUtil(OtherItems, NewCurrentWord, InSlot, OutSlot)
	).


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



comboUtil([], _, Answer, Current, Current,
	Input, Input) :-
	
	Answer > 0.
comboUtil([Square|OtherSquares], WordList, Least, Current,
	BestMatch, Input, Output) :-
	
	combinationCheck(Square, WordList, 0, Total),
	(	Total < Least
	->	
		append(Input, [Current], InputTemp),
		comboUtil(OtherSquares, WordList, Total, Square, BestMatch,
			InputTemp, Output)

	;	
		append(Input, [Square], InputTemp),
		comboUtil(OtherSquares, WordList, Least, Current,
			BestMatch, InputTemp, Output)
	).


combinationCheck(_, [], CurrentMatches, CurrentMatches).
combinationCheck(Square, [Word|OtherWords], CurrentMatches, Total) :-
	(	canJoin(Square, Word)
	->	combinationCheck(Square, OtherWords, CurrentMatches+1, Total)
	;	combinationCheck(Square, OtherWords, CurrentMatches, Total)
	).





