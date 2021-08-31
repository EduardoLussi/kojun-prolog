:- use_module(library(clpfd)).
:- use_module(library(occurs)).

% ==================== Solver

% Soluciona o Kojun
kojun(Matrix, Groups) :-
        append(Matrix, V), length(Matrix, L), M is L^2, V ins 1..M,
        groupSize(Matrix, Groups, Groups),      % Xij é 1..TamanhoDoGrupo
        orthogonalAdjacent(Matrix),             % Xij != Xij+1
        transpose(Matrix, TMatrix),
        orthogonalAdjacent(TMatrix),            % Xij != Xi+1j
        groupRepetition(Matrix, Groups),        % Grupo não possui elementos repetidos
        upGreaterFilter(Matrix, Groups, L),        % Se elementos forem do mesmo grupo, o de cima deve ser maior
        !.

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

% ==================== Utils

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

upGreaterFilterCol(Matrix, Groups, Length, I, J) :-
        J #< Length,
        I1 is I + 1,

        nth0(I, Matrix, IElem),
        nth0(J, IElem, JElem),

        nth0(I1, Matrix, IElem1),
        nth0(J, IElem1, JElem1),

        nth0(I, Groups, IGroup),
        nth0(J, IGroup, JGroup),

        nth0(I1, Groups, IGroup1),
        nth0(J, IGroup1, JGroup1),

        % print(JGroup),
        % print(JGroup1),
        % print(''),

        
        (JElem #> JElem1,
        JGroup #= JGroup1) ;

        % (JGroup = JGroup1 -> JElem #> JElem1),

        (J1 is J + 1,
        upGreaterFilterCol(Matrix, Groups, Length, I, J1)).


upGreaterFilterList(Matrix, Groups, Length, I) :-
        I #< Length,

        upGreaterFilterCol(Matrix, Groups, Length, I, 0) ;

        (I1 is I + 1,
        upGreaterFilterList(Matrix, Groups, Length, I1)).


% Filtra dado que o elemento de baixo precisa ser menor (se forem do mesmo grupo)
upGreaterFilter(Matrix, Groups, Length) :-
        upGreaterFilterList(Matrix, Groups, Length, 0).

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
