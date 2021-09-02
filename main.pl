:- use_module(library(clpfd)).
:- use_module(library(occurs)).

% ==================== Main call
:- initialization(main).

main :- problem(1, Matrix, Groups),
        kojun(Matrix, Groups),
        print_matrix(Matrix),
        halt.

% Instâncias de problemas
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

problem(2, [[_,3,5,6,_,_,1,2,_,2,_,3,_,4,_,_,4],
            [2,_,_,2,_,4,_,_,_,_,_,_,_,_,1,_,_],
            [7,6,5,_,7,_,_,4,_,_,3,_,_,_,6,5,2],
            [3,_,1,_,5,_,_,_,_,2,_,1,_,_,_,2,_],
            [_,7,_,2,_,_,3,_,_,_,5,_,_,_,5,_,_],
            [2,_,5,_,_,_,_,_,_,5,4,_,_,_,6,_,_],
            [1,_,_,3,1,_,_,_,5,_,_,_,_,1,3,_,_],
            [4,2,_,_,_,6,_,_,_,_,_,_,_,3,_,5,_],
            [3,_,6,2,_,3,_,_,4,_,5,_,_,2,_,4,_],
            [_,_,_,_,3,5,_,_,1,_,_,5,4,_,7,_,7],
            [3,_,5,_,6,_,2,_,4,_,3,_,_,_,_,_,6],
            [_,_,_,2,_,_,_,_,_,_,_,5,2,_,1,_,4],
            [_,4,_,_,_,_,_,_,_,_,1,_,7,_,3,4,_],
            [1,3,_,_,6,_,_,6,4,_,_,5,_,6,2,_,_],
            [6,_,_,1,_,2,_,_,7,_,4,_,_,4,_,_,_],
            [_,3,_,4,_,6,5,_,4,_,_,7,_,1,_,_,3],
            [_,2,_,_,2,_,4,_,1,2,_,5,4,5,_,2,_]],
           [[0 ,1 ,1 ,1 ,2 ,2 ,3 ,3 ,3 ,4 ,4 ,5 ,6 ,6 ,6 ,7 ,7 ],
            [1 ,1 ,1 ,2 ,2 ,2 ,8 ,9 ,3 ,4 ,4 ,5 ,5 ,5 ,6 ,6 ,7 ],
            [10,10,10,2 ,11,11,8 ,12,3 ,13,4 ,14,14,14,14,14,7 ],
            [10,10,10,10,11,15,8 ,12,12,13,13,16,16,16,16,14,17],
            [18,18,18,11,11,15,12,12,12,13,13,13,16,16,16,19,17],
            [18,18,18,20,11,11,21,21,21,22,22,22,22,22,22,23,17],
            [18,24,20,20,20,25,25,25,25,26,27,27,28,28,23,23,17],
            [24,24,24,29,29,29,29,25,30,30,27,31,28,32,32,32,17],
            [24,24,24,33,29,29,34,34,35,31,31,31,31,32,36,32,17],
            [37,37,33,33,33,33,34,34,35,35,35,35,38,32,32,39,39],
            [40,40,41,41,42,42,42,43,43,43,44,38,38,38,45,39,39],
            [46,40,40,41,42,42,42,43,47,44,44,44,38,45,45,39,39],
            [46,46,41,41,48,49,49,43,47,44,44,45,45,45,50,50,39],
            [46,46,51,51,48,49,52,52,52,53,54,45,55,55,50,50,50],
            [56,57,57,57,48,52,52,52,58,53,54,59,55,55,55,55,50],
            [56,57,56,48,48,58,58,58,58,58,54,54,54,55,60,60,61],
            [56,56,56,48,48,62,62,62,62,58,54,54,61,61,61,61,61]]).

% ==================== Solver

% Soluciona o Kojun
kojun(Matrix, Groups) :-
        append(Matrix, V), length(Matrix, L), M is L^2, V ins 1..M,
        groupSize(Matrix, Groups, Groups),      % Xij é 1..TamanhoDoGrupo
        orthogonalAdjacent(Matrix),             % Xij != Xij+1
        transpose(Matrix, TMatrix),
        orthogonalAdjacent(TMatrix),            % Xij != Xi+1j
        groupRepetition(Matrix, Groups),        % Grupo não possui elementos repetidos
        upGreaterFilter(Matrix, Groups),        % Se elementos forem do mesmo grupo, o de cima deve ser maior  
        maplist(label, Matrix), !.

% ==================== Utils

% printtar matriz de maneira organizada
print_line_matrix([]).
print_line_matrix([H | T]) :- write(H), write(" "), print_line_matrix(T).

print_matrix([]).
print_matrix([H | T]) :- print_line_matrix(H), nl, print_matrix(T).

% Retorna maior elemento entre A e B
maxElem(A, B, A) :- A > B, !.
maxElem(_, B, B).

% Maior elemento de uma matrix
maxMatrix([], 0).
maxMatrix([Line | Rest], Max) :- 
        max_list(Line, MaxLine), maxMatrix(Rest, MaxRest), maxElem(MaxLine, MaxRest, Max).

% Insere elemento E na posição N de Xs
insertAt(E,N,Xs,Ys) :-
        same_length([E|Xs],Ys),
        append(Before,Xs0,Xs),
        length(Before,N),
        append(Before,[E|Xs0],Ys), !.

% Remove elemento da posição I
remove(0, [_ | Xt], Xt) :- !.
remove(I, [H | Xt], [H | Yt]) :- I1 is I - 1, remove(I1, Xt, Yt), !.

% ==================== Filters

upGreaterFilterCol([], [], [], []).
upGreaterFilterCol([UpElem | RestUpLine], [BottomElem | RestBottomLine], [Group | RestUpGroups], [Group | RestBottomGroups]) :-
        (UpElem #> BottomElem),
        upGreaterFilterCol(RestUpLine, RestBottomLine, RestUpGroups, RestBottomGroups).
upGreaterFilterCol([_ | RestUpLine], [_ | RestBottomLine], [_ | RestUpGroups], [_ | RestBottomGroups]) :-
        upGreaterFilterCol(RestUpLine, RestBottomLine, RestUpGroups, RestBottomGroups).

% Filtra dado que o elemento de baixo precisa ser menor (se forem do mesmo grupo)
upGreaterFilter([_ | []], [_ | []]).
upGreaterFilter([UpLine, BottomLine | RestMatrix], [UpGroups, BottomGroups | RestGroups]) :-
        upGreaterFilterCol(UpLine, BottomLine, UpGroups, BottomGroups),
        upGreaterFilter([BottomLine | RestMatrix], [BottomGroups | RestGroups]).

% Filtragem por repetição de elementos em um grupo
groupRepetition(Matrix, Groups) :- 
        maxMatrix(Groups, MaxGroup), Length is MaxGroup + 1,    % Obtém tamanho da lista de grupos
        length(InitList, Length), maplist(=([]), InitList),     % Inicializa lista para QtGrupos * []
        getGroupsList(Matrix, Groups, InitList, List),          % Obtém lista de elementos dos grupos
        maplist(all_distinct, List).                            % Elementos dos grupos são distintos

% Retorna lista de elementos de cada grupo
getGroupsList([], [], List, List).
getGroupsList([MatrixLine | RestMatrix], [GroupsLine | RestGroups], List, FinalList) :- 
        getGroupsListLine(MatrixLine, GroupsLine, List, NewList),
        getGroupsList(RestMatrix, RestGroups, NewList, FinalList).

getGroupsListLine([], [], List, List).
getGroupsListLine([El | RestEl], [Group | RestGroups], List, FinalList) :- 
        nth0(Group, List, GroupList),                                   % Obtém lista respectiva do grupo
        append(GroupList, [El], NewGroupList),                          % Adiciona El na lista do grupo correspondente
        remove(Group, List, DelList),                                   % Remove lista antiga respectiva do grupo
        insertAt(NewGroupList, Group, DelList, NewList),                % Insere nova lista respectiva do grupo
        getGroupsListLine(RestEl, RestGroups, NewList, FinalList).

% Filtragem pela regra dos ortogonais adjacentes
orthogonalAdjacentLine([]).
orthogonalAdjacentLine([_ | []]).
orthogonalAdjacentLine([El0, El1 | RestEl]) :- all_distinct([El0, El1]), orthogonalAdjacentLine([El1 | RestEl]).

orthogonalAdjacent([]).
orthogonalAdjacent([MatrixLine | RestMatrix]) :- orthogonalAdjacentLine(MatrixLine), orthogonalAdjacent(RestMatrix).

% Filtragem por tamanho do grupo
groupSize([], [], _).
groupSize([MatrixLine | RestMatrix], [GroupsLine | RestGroups], Groups) :- 
        groupSizeLine(MatrixLine, GroupsLine, Groups),
        groupSize(RestMatrix, RestGroups, Groups).

groupSizeLine([], [], _).
groupSizeLine([Elem | RestElems], [Group | RestGroups], Groups) :- 
        occurrences_of_term(Group, Groups, OccurGroup), % Obtém tamanho do grupo
        Elem #=< OccurGroup,                            % Elemento <= Tamanho do grupo
        groupSizeLine(RestElems, RestGroups, Groups).
