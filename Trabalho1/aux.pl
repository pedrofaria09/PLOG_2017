% Clear screen
cls :- write('\e[2J').

% get input from user
getNewLine:- get_code(T), (T == 10 -> ! ; getNewLine).
getDigit(D):- get_code(Dt), D is Dt - 48, (Dt == 10 -> ! ; getNewLine).
getChar(C) :- get_char(C), char_code(C, Co), (Co == 10 -> ! ; getNewLine).