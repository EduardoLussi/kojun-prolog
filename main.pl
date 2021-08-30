:- use_module(library(clpfd)).
:- use_module(library(occurs)).

% Soluciona o Kojun
kojun(Matrix, Groups, Matrix) :-
        append(Matrix, V), length(Matrix, L), M is L^2, V ins 1..M,
        groupSize(Matrix, Groups, Groups).

% Filtragem por tamanho do grupo
groupSize([], [], _).
groupSize([MatrixLine | RestMatrix], [GroupsLine | RestGroups], Groups) :- groupSizeLine(MatrixLine, GroupsLine, Groups), groupSize(RestMatrix, RestGroups, Groups).

groupSizeLine([], [], _).
groupSizeLine([Elem | RestElems], [Group | RestGroups], Groups) :- occurrences_of_term(Group, Groups, OccurGroup), between(1, OccurGroup, Elem), groupSizeLine(RestElems, RestGroups, Groups).

problem(1, [[_,_,_,_,_,_,_,_],
            [_,1,3,_,_,_,_,_],
            [_,_,_,_,_,3,_,_],
            [_,_,3,_,_,_,_,_],
            [_,5,_,3,_,_,_,_],
            [_,2,_,_,_,_,_,_],
            [_,_,_,_,_,_,3,_],
            [_,_,5,3,_,_,_,_]],
           [[0 ,0 ,1 ,1 ,2 ,3 ,4 ,4 ],
            [0 ,0 ,5 ,1 ,6 ,3 ,3 ,4 ],
            [5 ,5 ,5 ,7 ,6 ,8 ,9 ,9 ],
            [10,10,10,7 ,6 ,8 ,8 ,9 ],
            [11,7 ,7 ,7 ,7 ,8 ,8 ,9 ],
            [11,12,13,13,13,14,15,9 ],
            [12,12,12,12,16,14,14,14],
            [17,16,16,16,16,14,18,18]]).