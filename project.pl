% Andre Marreiros Correia | ist1102666

:- [codigo_comum].

%----------------------------------------------------------------------------------------------
% extrai_ilhas_linha(N, Line, Ilhas)
% - N e um inteiro positivo correspodente ao numero da linha.
% - Line e uma lista correspondente  a uma linha de um puzzle.
% - Ilhas e  a lista ordenada cujos elementos sao as ilhas da linha Line.
%----------------------------------------------------------------------------------------------

extrai_ilhas_linha(N, Line, Ilhas) :- extrai_ilhas_linha(N, Line, [], 1, Ilhas).

extrai_ilhas_linha(_, [], Ilhas, _, Ilhas).

extrai_ilhas_linha(N, [Head | Tail], Temp, Column, Ilhas) :- % Caso exista uma ilha na posicao
    not(Head == 0),
    append(Temp,[ilha( Head, (N,Column))], NewTemp),
    NewColumn is Column + 1,
    extrai_ilhas_linha(N, Tail, NewTemp, NewColumn, Ilhas).

extrai_ilhas_linha(N, [_| Tail], Temp, Column, Ilhas) :- % Caso nao exista uma ilha na posicao
    NewColumn is Column + 1,
    extrai_ilhas_linha(N, Tail, Temp, NewColumn, Ilhas).

%----------------------------------------------------------------------------------------------
% ilhas(Puz, Isles)
% - Puz e um puzzle.
% - Isles e a lista ordenada cujos elementos sao as ilhas de Puz.
%----------------------------------------------------------------------------------------------

ilhas(Puz,Isles) :- ilhas(Puz, [], 1, Isles).

ilhas([],Isles,_,Isles).

ilhas([Line|Tail],Temp,Row,Isles) :-
    extrai_ilhas_linha(Row,Line,Ilhas),
    not(Ilhas == []),
    append(Temp,Ilhas,NewTemp),
    NewRow is Row + 1,
    ilhas(Tail,NewTemp,NewRow,Isles).

ilhas([_|Tail],Temp,Row,Isles) :-
    NewRow is Row + 1,
    ilhas(Tail,Temp,NewRow,Isles).

%----------------------------------------------------------------------------------------------
% vizinhas(Ilhas, Ilha, Vizinhas)
% - Ilhas e a lista de ilhas de um puzzle.
% - Ilha e uma dessas ilhas.
% - Vizinhas e a lista ordenada cujos elementos sao as ilhas vizinhas de Ilha.
%----------------------------------------------------------------------------------------------

vizinhas(Ilhas, ilha(_,(Row,Column)), Vizinhas) :-
    findall(X,member(ilha(_,(Row,X)),Ilhas),AllInLine),  % Encontra as ilhas na mesma linha.
    getNear(Column,AllInLine,Left,Right),
    findall(X,member(ilha(_,(X,Column)),Ilhas),AllInLine2), % Encontra as ilhas na mesma coluna.
    getNear(Row,AllInLine2,Up,Down),
    ( not(Up == 0) ->
        nth1(_,Ilhas,ilha(X,(Up,Column))),
        append([],[ilha(X,(Up,Column))],Temp1)
    ;   append([],[],Temp1)
    ),
    ( not(Left == 0) ->
        nth1(_,Ilhas,ilha(Y,(Row,Left))),
        append(Temp1,[ilha(Y,(Row,Left))],Temp2)
    ;   append(Temp1,[],Temp2)
    ),
    ( not(Right == 0) ->
        nth1(_,Ilhas,ilha(Z,(Row,Right))),
        append(Temp2,[ilha(Z,(Row,Right))],Temp3)
    ;   append(Temp2,[],Temp3)
    ),
    ( not(Down == 0) ->
        nth1(_,Ilhas,ilha(W,(Down,Column))),
        append(Temp3,[ilha(W,(Down,Column))],Temp4)
    ;   append(Temp3,[],Temp4)
    ),
    append([],Temp4,Vizinhas).  

% getNear encontra as ilhas mais proximas da ilha origem numa respetiva direcao (horizontal ou vertical), 
% devolvendo respetivamente se existirem as ilhas a direita/esquerda ou cima/embaixo.
getNear(Target,AllInLine,Viz1,Viz2) :- getNear(Target,AllInLine,0,Viz1,0,Viz2). 
getNear(_,[],Viz1,Viz1,Viz2,Viz2).

getNear(Target,[Head|Tail],Temp1,Viz1,Temp2,Viz2) :-
    Target > Head, Temp1 < Head,
    getNear(Target,Tail,Head,Viz1,Temp2,Viz2). 

getNear(Target,[Head|Tail],Temp1,Viz1,Temp2,Viz2) :-
    (Target < Head, Temp2 > Head) ; (Target < Head, Temp2 == 0),
    getNear(Target,Tail,Temp1,Viz1,Head,Viz2).

getNear(Target,[_|Tail],Temp1,Viz1,Temp2,Viz2) :-
    getNear(Target,Tail,Temp1,Viz1,Temp2,Viz2).

%----------------------------------------------------------------------------------------------
% estado(Ilhas, Estado)
% - Ilhas e a lista de ilhas de um puzzle.
% - Estado e a lista ordenada cujos elementos sao as entradas referentes a cada uma das ilhas de Ilhas.
%----------------------------------------------------------------------------------------------

estado(Ilhas,Estado) :- estado(Ilhas, Ilhas, [], Estado).

estado([],_,Estado,Estado).

estado([Head|Tail], Ilhas, Temp, Estado):-
    vizinhas(Ilhas,Head,Vizinhas),
    append(Temp,[[Head,Vizinhas,[]]],NewTemp),
    estado(Tail,Ilhas,NewTemp,Estado).

%----------------------------------------------------------------------------------------------
% posicoes_entre(Pos1, Pos2, Posicoes)
% - Pos1 e Pos2 sao posicoes.
% - Posicoes e a lista ordenada de posicoes entre Pos1 e Pos2.
%----------------------------------------------------------------------------------------------

posicoes_entre((X1,Y1), (X2,Y2), Posicoes) :-
    ( (X1 < X2; Y1 < Y2) ->
        findall((X,Y),(between(X1, X2, X),between(Y1, Y2, Y)),Temp)
    ;   findall((X,Y),(between(X2, X1, X),between(Y2, Y1, Y)),Temp)
    ),
    ( (not(Temp == [])) -> % Remove a Pos1 e Pos2 do resultado.
        removeHead(Temp,Temp2),
        removeLast(Temp2,Posicoes)
        ; Posicoes = []
    ).

% removeHead e removeLast removem respetivamente o primeiro e ultimo elemento de uma lista.
removeHead([_|Tail],Tail).
removeLast(Temp,Posicoes) :- removeLast(Temp,[],Posicoes).
removeLast([_],Posicoes,Posicoes).
removeLast([Head|Tail],Temp,Posicoes) :-
    append(Temp,[Head],NewTemp),
    removeLast(Tail,NewTemp,Posicoes).

%----------------------------------------------------------------------------------------------
% cria_ponte(Pos1, Pos2, Ponte)
% - Pos1 e Pos2 sao posicoes.
% - Ponte e uma ponte entre essas 2 posicoes.
%----------------------------------------------------------------------------------------------

cria_ponte((X1,Y1),(X2,Y2),Ponte):-
    ( (X1 < X2; Y1 < Y2) -> % O if serve para ordenadar caso os args estajam desordenados.
        Ponte = ponte((X1,Y1),(X2,Y2)) 
    ;   Ponte = ponte((X2,Y2),(X1,Y1))
    ).
    
%----------------------------------------------------------------------------------------------
% caminho_livre(Pos1, Pos2, Posicoes, I, Vz)
% - Pos1 e Pos2 sao posicoes.
% - Posicoes e a lista ordenada de posicoes entre Pos1 e Pos2.
% - I e uma ilha, e Vz e uma das suas vizinhas.
%----------------------------------------------------------------------------------------------

caminho_livre(Pos1,Pos2,_,ilha(_,Pos3),ilha(_,Pos4)):- 
    (Pos1 == Pos3, Pos2 == Pos4);(Pos2 == Pos3, Pos1 == Pos4).
    % Verifica se as ilhas e posicoes sao iguais.

caminho_livre(Pos1,Pos2,Posicoes,ilha(_,Pos3),ilha(_,Pos4)):-
    posicoes_entre(Pos1,Pos2,Posicoes),
    posicoes_entre(Pos3,Pos4,PosEntreIlhas),
    findall(X,(member(X,Posicoes),member(X,PosEntreIlhas)),[]).
    
%----------------------------------------------------------------------------------------------
% actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Entrada, Nova_Entrada)
% - Pos1 e Pos2 sao posicoes.
% - Posicoes e a lista ordenada de posicoes entre Pos1 e Pos2.
% - Entrada e uma entrada.
% - Nova_Entrada e igual a Entrada com as ilhas vizinhas atualizadas.
%----------------------------------------------------------------------------------------------

actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, [Isle,Viz,Pontes], [Isle,NewViz,Pontes]) :- actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Isle, Viz, [], NewViz).

actualiza_vizinhas_entrada(_, _, _, _, [], NewViz, NewViz).

actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Isle, [Vizinha|Tail], Temp, NewViz) :-
    caminho_livre(Pos1,Pos2,Posicoes,Isle,Vizinha),
    append(Temp,[Vizinha],NewTemp),
    actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Isle, Tail, NewTemp, NewViz).

actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Isle, [_|Tail], Temp, NewViz) :-
    actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Isle, Tail, Temp, NewViz).

%----------------------------------------------------------------------------------------------
% actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_estado)
% - Pos1 e Pos2 sao posicoes.
% - Estado e um estado.
% - Novo_estado e igual a Estado com as ilhas vizinhas atualizadas em todas as entradas.
%----------------------------------------------------------------------------------------------

actualiza_vizinhas_apos_pontes(State, Pos1, Pos2, NewState) :- actualiza_vizinhas_apos_pontes(State, Pos1, Pos2, [], NewState).

actualiza_vizinhas_apos_pontes([], _, _, NewState, NewState).

actualiza_vizinhas_apos_pontes([Head|Tail], Pos1, Pos2, Temp, NewState) :-
    getIslePosFromEntry(Head,Pos), not((Pos == Pos1);(Pos == Pos2)),
    posicoes_entre(Pos1,Pos2,Posicoes),
    actualiza_vizinhas_entrada(Pos1,Pos2,Posicoes,Head,NewEntry),
    append(Temp,[NewEntry],NewTemp),
    actualiza_vizinhas_apos_pontes(Tail, Pos1, Pos2, NewTemp, NewState).

actualiza_vizinhas_apos_pontes([Head|Tail], Pos1, Pos2, Temp, NewState) :-
    append(Temp,[Head],NewTemp),
    actualiza_vizinhas_apos_pontes(Tail, Pos1, Pos2, NewTemp, NewState).

% getIslePosFromEntry obtem a posicao da ilha da respetiva entrada.
getIslePosFromEntry([ilha(_,Pos)|_],Pos). 

%----------------------------------------------------------------------------------------------
% ilhas_terminadas(Estado, Ilhas_term)
% - Estado e um estado.
% - Ilhas_term e a lista de ilhas que ja tem todas as pontes associadas.
%----------------------------------------------------------------------------------------------

ilhas_terminadas(State, Completed) :- ilhas_terminadas(State, [], Completed).

ilhas_terminadas([],Completed,Completed).

ilhas_terminadas([[ilha(N,(X,Y)),_,P]|Tail],Temp,Completed) :-
    not(N = 'X'), length(P, N),
    append(Temp,[ilha(N,(X,Y))],NewTemp),
    ilhas_terminadas(Tail,NewTemp,Completed).

ilhas_terminadas([_|Tail],Temp,Completed) :-
    ilhas_terminadas(Tail,Temp,Completed).
    
%-----------------------------------------------------------------------------------------------
% tira_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_entrada)
% - Entrada e uma entrada.
% - Ilhas_term e a lista de ilhas que ja tem todas as pontes associadas.
% - Nova_entrada e a entrada resultante de remover as ilhas de Ilhas_term.
%-----------------------------------------------------------------------------------------------

tira_ilhas_terminadas_entrada(Completed, [Ilha,Viz,Pontes], NewEntry) :- tira_ilhas_terminadas_entrada(Completed, Viz, [], NewEntry,Ilha,Pontes).

tira_ilhas_terminadas_entrada(_,[],NewViz,NewEntry,Ilha,Pontes) :-
    NewEntry = [Ilha,NewViz,Pontes].

tira_ilhas_terminadas_entrada(Completed,[Head|Tail],Temp,NewEntry,Ilha,Pontes) :-
    member(Head,Completed),
    tira_ilhas_terminadas_entrada(Completed,Tail,Temp,NewEntry,Ilha,Pontes).

tira_ilhas_terminadas_entrada(Completed,[Head|Tail],Temp,NewEntry,Ilha,Pontes) :-
    append(Temp,[Head],NewTemp),
    tira_ilhas_terminadas_entrada(Completed,Tail,NewTemp,NewEntry,Ilha,Pontes).

%----------------------------------------------------------------------------------------------
% tira_ilhas_terminadas(Estado, Ilhas_term, Novo_estado)
% - Estado e um estado.
% - Ilhas_term e a lista de ilhas que ja tem todas as pontes associadas.
% - Novo_estado e o estado resultante de aplicar o predicado tira_ilhas_terminadas_entrada a cada uma das entradas de Estado.
%----------------------------------------------------------------------------------------------

tira_ilhas_terminadas(State, Completed, NewState) :- tira_ilhas_terminadas(State, [], Completed, NewState).

tira_ilhas_terminadas([], NewState, _, NewState).

tira_ilhas_terminadas([Entry|Tail],Temp,Completed,NewState) :-
    tira_ilhas_terminadas_entrada(Completed,Entry,NewEntry),
    append(Temp,[NewEntry],NewTemp),
    tira_ilhas_terminadas(Tail,NewTemp,Completed,NewState).

%----------------------------------------------------------------------------------------------
% marca_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_entrada)
% - Entrada e uma entrada.
% - Ilhas_term e a lista de ilhas que ja tem todas as pontes associadas.
% - Nova_entrada e a entrada com as ilhas terminadas marcadas.
%----------------------------------------------------------------------------------------------
    
marca_ilhas_terminadas_entrada(Completed, [ilha(N,(X,Y)),Viz,Pontes], NewEntry) :-
    ( member(ilha(N,(X,Y)),Completed) ->
        NewEntry = [ilha('X',(X,Y)),Viz,Pontes]
    ;   NewEntry = [ilha(N,(X,Y)),Viz,Pontes]
    ).

%---------------------------------------------------------------------------------------------
% marca_ilhas_terminadas(Estado, Ilhas_term, Novo_estado)
% - Estado e um estado.
% - Ilhas_term e a lista de ilhas que ja tem todas as pontes associadas.
% - Novo_estado e o estado resultante de aplicar o predicado marca_ilhas_terminadas_entrada a cada uma das entradas de Estado.
%---------------------------------------------------------------------------------------------

marca_ilhas_terminadas(State, Completed, NewState) :- marca_ilhas_terminadas(State, [], Completed, NewState).

marca_ilhas_terminadas([], NewState, _, NewState).

marca_ilhas_terminadas([Entry|Tail],Temp,Completed,NewState) :-
    marca_ilhas_terminadas_entrada(Completed,Entry,NewEntry),
    append(Temp,[NewEntry],NewTemp),
    marca_ilhas_terminadas(Tail,NewTemp,Completed,NewState).

%----------------------------------------------------------------------------------------------
% trata_ilhas_terminadas(Estado, Novo_estado)
% - Estado e um estado.
% - Ilhas_term e a lista de ilhas que ja tem todas as pontes associadas.
% - Novo_estado e o estado resultante de aplicar os predicados tira_ilhas_terminadas e marca_ilhas_terminadas a Estado.
%----------------------------------------------------------------------------------------------

trata_ilhas_terminadas(State, NewState) :-
    ilhas_terminadas(State, Completed),
    marca_ilhas_terminadas(State,Completed,Marked),
    tira_ilhas_terminadas(Marked,Completed,NewState).

%----------------------------------------------------------------------------------------------
% junta_pontes(Estado, Num_pontes, Ilha1, Ilha2, Novo_estado)
% - Estado e um estado.
% - Num_pontes e o numero de pontes a adicionar.
% - Ilha1 e Ilha2 sao 2 ilhas.
% - Novo_estado e o estado resultando da adicao de Num_pontes pontes entre Ilha1 e Ilha2 no Estado.
%----------------------------------------------------------------------------------------------

junta_pontes(State, NumBridges, Isle1, Isle2, NewState) :-
    Isle1 = ilha(_,Pos1), Isle2 = ilha(_,Pos2), %Converte as ilhas em posicoes.
    bridgeMaker(State,NumBridges,Pos1,Pos2,StateWithBridges),
    actualiza_vizinhas_apos_pontes(StateWithBridges, Pos1, Pos2, StateUpdated),
    trata_ilhas_terminadas(StateUpdated,NewState).
    
% bridgeMaker cria e adiciona a um estado uma ponte.
bridgeMaker(NewState,0,_,_,NewState).
bridgeMaker(State,NumBridges,Pos1,Pos2,NewState):-
    NewNum is NumBridges - 1,
    cria_ponte(Pos1,Pos2,Ponte),
    findall(NewEntry,
        (member([Pos,Viz,X],State), 
            ( Pos = ilha(N,Pos1) -> append(X,[Ponte],NewX), NewEntry = [ilha(N,Pos1),Viz,NewX]
            ; Pos = ilha(N,Pos2) -> append(X,[Ponte],NewX), NewEntry = [ilha(N,Pos2),Viz,NewX]
            ; NewEntry = [Pos,Viz,X])
        ),Temp),
    bridgeMaker(Temp,NewNum,Pos1,Pos2,NewState).


    
