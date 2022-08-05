:- dynamic detailed_mode_disabled/0.
:- ensure_loaded('files.pl').
% student file for Ultimate Tic Tac Toe implementation

index(nw, 0).
index(n, 1).
index(ne, 2).
index(w, 3).
index(c, 4).
index(e, 5).
index(sw, 6).
index(s, 7).
index(se, 8).
indexes([0, 1, 2, 3, 4, 5, 6, 7, 8]).

% initialState/1
% initialState(-State)
% Este adevărat pentru starea inițială a jocului.
initialState(([['', '', '', '', '', '', '', '', ''],
              ['', '', '', '', '', '', '', '', ''],
              ['', '', '', '', '', '', '', '', ''],
              ['', '', '', '', '', '', '', '', ''],
              ['', '', '', '', '', '', '', '', ''],
              ['', '', '', '', '', '', '', '', ''],
              ['', '', '', '', '', '', '', '', ''],
              ['', '', '', '', '', '', '', '', ''],
              ['', '', '', '', '', '', '', '', '']], null, x)).

% getBoards/2
% getBoards(+State, -Boards)
% Este adevărat dacă în starea State, informațiile din tablele individuale sunt
% cele din variabila Boards.
% Boards este legată la o listă de 9 elemente, fiecare element reprezentând o tablă.
% Ordinea tablelor este cea din lista positions (din utils.pl).
% Fiecare element din listă este o listă de 9 elemente, reprezentând
% pozițiile de pe tablă, ca x, 0, sau ''.
% Pozițiile sunt în ordinea din lista positions (din utils.pl).
getBoards(State, Boards) :- (Boards, _, _) = State.

% getBoard/3
% getBoard(+State, +UPos, -Board)
% Este adebărat dacă în starea State, la poziția UPos din tabla de UTTT, 
% se află tabla individuală cu reprezentarea din Board.
% Reprezentarea tablei este descrisă în predicatul getBoards/2.
getBoard(State, Upos, Board) :- getBoards(State, Boards), positions(S), nth0(I, S, Upos), nth0(I, Boards, Board), !.

% getUBoard/2
% getUBoard(stare(+Board, +UboardState, +Player, +NextMoves),
% -UboardState)
% Întoarce reprezentarea UBoard-ului, indicând tablele individuale câștigate,
% remizate, sau încă în desfășurare. Reprezentarea este aceeași ca a tablelor
% individuale (vezi getBoards/2).
getWinner(Board, Winner) :- player_wins(Winner, Board), !.
getWinner(Board, r) :- countBoard(0, Board, N0), countBoard(x, Board, NX), N0 + NX =:= 9, !.
getWinner(_, '').

getWinners([H], [Winner]) :- getWinner(H, Winner), !.
getWinners([HB | TB], [Winner | T]) :- getWinners(TB, T), getWinner(HB, Winner).

getUBoard(State, UboardState) :- initialState(State), empty_board(UboardState), !.
getUBoard(State, UboardState) :- getBoards(State, Boards), getWinners(Boards, UboardState). 


 
% getPos/4
% getPos(+State, +UPos, +Pos, -Cell).
% Este adevărat dacă în starea State, în tabla individuală de la poziția UPos în UBoard,
% la poziția Pos pe tablă, se află simbolul Cell (x, 0, sau '').
getPos(State, Upos, Pos, Cell) :- getBoard(State, Upos, Board), getPos(Board, Pos, Cell).

% getPos/3
% getPos(+Board, +Pos, -Cell).
% Este adevărat dacă în tabla individuală reprezentată în Board, la poziția Pos, 
% se află simbolul Cell (x, 0, sau ''). Predicatul poate fi folosit și pentru UBoard, caz 
% în care Cell poate fi și r.
getPos(Board, Pos, Cell) :- positions(S), nth0(I, S, Pos), nth0(I, Board, Cell).

% getNextPlayer/2
% getNextPlayer(+State), -NextPlayer)
% Este adevărat dacă în starea State, jucătorul care urmează este NextPlayer
% (poate fi x sau 0)..
getNextPlayer(State, NextPlayer) :- (_, _, NextPlayer) = State.

% getNextAvailableBoards/2
% getNextAvailableBoards(+State, -NextBoardsPoss)
% Este adevărat dacă în starea State, pozițiile din NextBoardsPoss sunt pozițiile 
% din UBoard ale tablelor disponibile pentru următoarea mutare.
getPrevMove(State, PrevMove) :- (_, PrevMove, _) = State.

getNextAvailableBoards(State, NextBoardsPoss) :- initialState(State), positions(NextBoardsPoss), !.
getNextAvailableBoards(State, [PrevMove]) :- getPrevMove(State, PrevMove),
                                             getBoard(State, PrevMove, Board), 
                                             getWinner(Board, Winner), 
                                             Winner = '', !.

getNextAvailableBoards(State, NextBoardsPoss) :- positions(Initial), include(isAvailable(State), Initial, NextBoardsPoss).

isAvailable(State, Position) :- getBoard(State, Position, Board), getWinner(Board, Winner), Winner = '', !.
% getBoardResult/2
% getBoardResult(+Board, -Result)
% Este adevărat dacă pentru o tablă individuală (sau UBoard) cu reprezentarea
% din Board, rezultatul este Result. Result poate fi:
% x sau 0, dacă jucătorul respectiv a câștigat jocul pe tabla dată;
% r, dacă s-a ajuns la remiză (toate pozițiile au fost completate dar
% tabla nu a fost câștigată);
% '', dacă tabla nu a fost câștigată și nu s-au completat toate pozițiile.
% NOTĂ: este deja definit predicatul player_wins/2 în utils.pl.
getBoardResult(Board, Result) :- getWinner(Board, Result), !.

% buildState/3
% buildState(+Boards, +PreviousPos, -State)
% Este adevărat dacă starea State corespunde stării jocului în care tablele
% individuale sunt cele din lista Boards, iar ultima mutare a fost în 
% poziția PreviousPos într-o tablă individuală.
% NOTĂ: nu contează în care tablă individuală s-a realizat ultima mutare.
countBoard(_, [], 0) :- !.
countBoard(X, [X | T], N) :- countBoard(X, T, N1), N is N1 + 1, !.
countBoard(X, [_ | T], N) :- countBoard(X, T, N).

countBoards(_, [], 0) :- !.
countBoards(X, [H | T], N) :- countBoards(X, T, N1), countBoard(X, H, Current), N is N1 + Current.

buildState(Boards, PreviousPos, State) :- countBoards(x, Boards, Nx), countBoards(0, Boards, N0), Nx =:= N0, Player = x, State = (Boards, PreviousPos, Player), !.
buildState(Boards, PreviousPos, State) :- State = (Boards, PreviousPos, 0).

% validMove/2
% validMove(+State, +Move)
% Este adevărat dacă mutarea Move este legală în starea State.
% Move este fie o poziție, în cazul în care este o singură tablă disponibilă
% pentru a următoarea mutare din starea State, fie o pereche de poziții, altfel.
validMove(State, Move) :- (getUBoard(State, UBoard), getBoardResult(UBoard, Result), Result = ''),

                          (getNextAvailableBoards(State, NextBoardsPoss), length(NextBoardsPoss, N), N =:= 1,
                          getPrevMove(State, PrevMove), getBoardResult(PrevMove, Result), Result = '',
                          getPos(State, PrevMove, Move, Cell), Cell = '', !);
                          
                          (getNextAvailableBoards(State, NextBoardsPoss), length(NextBoardsPoss, N), N =\= 1,
                          (Table, TableMove) = Move, member(Table, NextBoardsPoss), getBoardResult(Table, Result), Result = '',
                          getPos(State, Table, TableMove, Cell), Cell = '', !).

% makeMove/3
% makeMove(+State, +Move, -NewState)
% Este adevărat dacă în urma aplicării mutării Move în starea State
% rezulta starea NewState.
% Move este fie o poziție (din lista positions), în cazul în care nu sunt mai 
% multe table disponibile pentru a următoarea mutare din starea State,
% fie o pereche de poziții, altfel.
%
% Hint: folosiți validMove pentru a verifica mutarea și buildState pentru a construi o stare.
makeMove(State, Move, NewState) :- getNextAvailableBoards(State, NextBoardsPoss), length(NextBoardsPoss, N),
                                   N =:= 1, validMove(State, Move), index(Move, Index), getNextPlayer(State, NextPlayer), 
                                   getPrevMove(State, PrevMove), getBoard(State, PrevMove, Board),
                                   getBoards(State, Boards), nth0(Index, Board, _, R1), nth0(Index, R, NextPlayer, R1),
                                   index(PrevMove, LastIndex), nth0(LastIndex, Boards, _, R2), nth0(LastIndex, Result, R, R2),
                                   buildState(Result, Move, NewState), !.

makeMove(State, Move, NewState) :- getNextAvailableBoards(State, NextBoardsPoss), length(NextBoardsPoss, N),
                                   N =\= 1, validMove(State, Move), (Table, TableMove) = Move, index(Table, TIndex), 
                                   index(TableMove, Position), getNextPlayer(State, NextPlayer), getBoard(State, Table, Board),
                                   getBoards(State, Boards), nth0(Position, Board, _, R1), nth0(Position, R, NextPlayer, R1),
                                   nth0(TIndex, Boards, _, R2), nth0(TIndex, Result, R, R2),
                                   buildState(Result, TableMove, NewState), !.

% dummy_first/2
% dummy_first(+State, -NextMove)
% Predicatul leagă NextMove la următoarea mutare pentru starea State.
% Strategia este foarte simplă: va fi aleasă cea mai din stânga-sus mutare posibilă
% (prima din lista de poziții disponibile).
getFirstAvailablePosition(Board, [H | T], Pos) :- getPos(Board, H, Cell), Cell = '', Pos = H, !;
                                                  getFirstAvailablePosition(Board, T, Pos).

dummy_first(State, NextMove) :- getNextAvailableBoards(State, NextBoardsPoss), length(NextBoardsPoss, N),
                               [H | T] = NextBoardsPoss, getBoard(State, H, Board), positions(Positions),
                               ((N =\= 1, getFirstAvailablePosition(Board, Positions, Pos), NextMove = (H, Pos));
                               (N =:= 1, getFirstAvailablePosition(Board, Positions, Pos), NextMove = Pos)).
                                  

% dummy_last/2
% dummy_last(+State, -NextMove)
% Predicatul leagă NextMove la următoarea mutare pentru starea State.
% Strategia este foarte simplă: va fi aleasă cea mai din dreapta-jos mutare posibilă 
% (ultima din lista de poziții disponibile).
myConcat([], L1, L1).
myConcat([H1|T1], L2, [HR|TR]):- H1 = HR, myConcat(T1, L2, TR).

myReverse([], []).
myReverse([H|T],RevList):- H = L, myReverse(T, First), myConcat(First, [L], RevList).

dummy_last(State, NextMove) :- getNextAvailableBoards(State, NextBoardsPoss), length(NextBoardsPoss, N),
                               myReverse(NextBoardsPoss, NBPrev),
                               [H | T] = NBPrev, getBoard(State, H, Board), positions(Positions),
                               myReverse(Positions, RevPositions),
                               ((N =\= 1, getFirstAvailablePosition(Board, RevPositions, Pos), NextMove = (H, Pos));
                               (N =:= 1, getFirstAvailablePosition(Board, RevPositions, Pos), NextMove = Pos)).

