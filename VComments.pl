%Not final submission
% * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * %
% Project 2 - Fill in Puzzle Implementation for COMP90048: Declarative Programming, Semester 2 2020.
% By Abhishek Anand - Student ID: 1005884. Submitted 
% 
% This project solves fill in puzzles by reconstructing the associated squares in the 'Puzzle', then It
% tries out the words from the 'WordList' until a suitable solution is found, then the solution is
% returned.
%
%
% To start of, we build a set of suitable squares(S1) using the 'SquareUtil' function, transpose the puzzle,
% and do the same with the transposed puzzle(S2); and then join both the suitable squares(From puzzle and
% transposed puzzle) as Square.
% 
% We then sort the input Wordlist and Square( To reduce the search needed) , and send it to be processed into
% word - Square combos and put into a suitable square in the puzzle using 'fillUtil'. We make a list containing
% the number of possible words in a square, and 'fillUtil' tries to put the given word into the correct square,
% failing which it will try the next square and so on. 
% 
% Once all slots are filled, the Puzzle and the given WordList are printed.
% 
% This program has been implemented with tail recursion wherever possible with suitable optimizations, but it
% is not over emphasized. Comments have been added with each function to summarise their function. 

% Note: A plus sign ('+') denotes input, and minus sign('-') denotes output and/or produced value.
%
% * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * %

% This is used for the tranpose function
:- ensure_loaded(library(clpfd)).

% This is use for the sorting function
:- use_module(library(pairs)).

% Entry point into the program.
% +Puzzle - a valid Puzzle such as [['#',h,'#'],[_,_,_],['#',_,'#']]
% +Wordlist - set of possible words like [[h,a,t], [b,a,g]]
% -S1 - set of squares produced from Puzzle
% -S2 - set of squares produced from Transposed puzzle.
% -Square - S1 and S2 appended into one.
puzzle_solution(Puzzle, WordList) :-

    % Produce available squares to fill, and sort by length into SortedSquare
	squareUtil(Puzzle,Square),
	sortbylen(Square,SortedSquare),!,
    
    % Sort wordlist into SortedWordList to reduce search needed.
	sortbylen(WordList,SortedWordList),!,

    % Add the SortedWordList into suitable squares, calling various functions in the process
	fillUtil(SortedSquare, SortedWordList).

% Build a set of squares using the input puzzle, transpose the puzzle, build another set
% and append the two sets of potential squares together. 
% +Puzzle
% -Square - appended set of squares from Puzzle (S1) and tranposed puzzle (S2).
squareUtil(Puzzle,Square):-
    
    % Make set of squares from Puzzle
	squareHelper(S1,Puzzle),
    
    % Tranpose puzzle using tranpose/2 predicate.
	transpose(Puzzle,TPuzzle),
    
    % Make set of squares from Transposed Puzzle
	squareHelper(S2,TPuzzle),
    
    %join S1 and S2 together into Square.
	append(S1,S2,Square).

% Checks if the same characters are present in the given arguemnents 
% +Item - The word(s) to be checked
% +List - The square(s) to be checked
% Outputs true or false.
same_chars([],_).
same_chars(Item,List):-
	Item = [_|Last],
	List = [_|LastLists],
	same_chars(Last,LastLists).

% Checks if the word and square can be joined, by checking if they contain similar characters and if they are of same length. 
% +W1 - the word(s).
% +S1 - potential matche(s)/ square(s)
% Outputs true or false based on results of
canJoin(W1, S1) :-
	same_length(W1,S1),
	same_chars(W1,S1).


% Performs a unification of the two arguements
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


% Helper function called by squareUtil to build the potential squares for the words
% +Puzzle as RowVar - Split into Row and Rows, from which the positions for words are extracted,
% -Square - which is the potential list of squares and positions for the words.
% -S1, -S2 - S1 is built with the first Row, and S2 is built recursively with the remaining Rows.
squareHelper([], []).
squareHelper(Square,RowVar) :-
    
	RowVar = [Row|Rows],
    
    % Function call to actually compute the associated squares.
    getsquares(Row, [], S1),
    
    % Do the same with remaing Rows
    squareHelper(S2,Rows),
    
    % Join S1 and S2 together as Square, and return this value.
    append(S1, S2, Square).

% Inputs the WordList and returns a sorted by length list - WbyLen. 
% This function is not only used to sort out the WordList, but also the squares. 
% + WordList - what items are to be sorted
% - WbyLen - the sorted list in a descending manner. 
sortbylen(WordList, WbyLen) :-
    
    % Map and create a key value pair using Wordlist - Predicate function.
    map_list_to_pairs(length, WordList, Pairs),
    
    % Use sort/4, First element is the key, 
    % Second value orders in a descending manner but keeps any duplicates,
    % third is list to be sorted and unified with fourth element.
    sort(1, @>=, Pairs, FinList),
    
    %Returns the key, values. 
    pairs_keys_values(FinList, _,WbyLen).


fillUtil([], _).
fillUtil(Item, WordList) :-
	Item = [Square|S2],
	
	combinationCheck(Square, WordList, 0, Matches),

	comboUtil(S2, WordList, Matches, Square, BestMatch, [], New), !,

	sortbylen(WordList,WbyLen),
	
	filler(BestMatch, WbyLen, [], NewWordList),
	
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

% Checks if a word and square can be unified and returns the current matches and total count
% Which is used to compare with the least matches in another function.
% +Square - Which to find the numbers for.
% +W
combinationCheck(_, [], CurrentMatches, CurrentMatches).
combinationCheck(Square, WordList, CurrentMatches, Total) :-
    WordList = [Word|OtherWords],
	(	canJoin(Word, Square)
	->	combinationCheck(Square, OtherWords, CurrentMatches+1, Total)
	;	combinationCheck(Square, OtherWords, CurrentMatches, Total)
	).









