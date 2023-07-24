%Start Week 1 Jan Challenges

deleteNums(List,Res) :- deleteNums(List,[],Res).

deleteNums([],Res,Res).

deleteNums([Head | Tail], Temp, Res) :-
    number(Head), !,
    deleteNums(Tail,Temp,Res).

deleteNums([Head | Tail], Temp, Res) :-
    append(Temp, [Head], NewTemp),
    deleteNums(Tail,NewTemp,Res).


programa1Dia3(SequenciaFonte, SequenciaAlvo, ListaParesCodigo) :- programa1Dia3(SequenciaFonte, SequenciaAlvo, [], ListaParesCodigo). % Recursion Set Up

programa1Dia3([], [], ListaParesCodigo, ListaParesCodigo).% Recursion End

programa1Dia3([HeadFont|TailFont], [HeadTarget|TailTarget], Temp, ListaParesCodigo):- % Creates List of pairs
    append(Temp,[par(HeadFont,HeadTarget)], NewTemp),
    programa1Dia3(TailFont, TailTarget, NewTemp, ListaParesCodigo).

programa2Dia3(ListaParesCodigo, MensagemCodificada, MensagemDescodificada) :- programa2Dia3(ListaParesCodigo, MensagemCodificada, [], MensagemDescodificada).

programa2Dia3(ListaParesCodigo, [], MensagemDescodificada, MensagemDescodificada).

programa2Dia3(ListaParesCodigo, [HeadCode|TailCode], Temp, MensagemDescodificada) :-
    nth1(I, ListaParesCodigo, par(X, HeadCode)),
    append(Temp,[X],NewTemp),
    programa2Dia3(ListaParesCodigo, TailCode, NewTemp, MensagemDescodificada).


programaDia4(Matriz, Coordenadas, MensagemDescodificada) :- programaDia4(Matriz, Coordenadas, [], MensagemDescodificada).

programaDia4(Matriz, [], MensagemDescodificada, MensagemDescodificada).

programaDia4(Matriz, [HeadCoord|TailCoord], Temp, MensagemDescodificada) :-
    nth1(1,HeadCoord,X),
    nth1(2,HeadCoord,Y),
    nth1(X,Matriz,Row),
    nth1(Y,Row,Res),
    append(Temp,[Res],NewTemp),
    programaDia4(Matriz, TailCoord, NewTemp, MensagemDescodificada).

    
encontraSuspeitos(PessoasBanco, CaracteristicasCriminoso, Suspeitos) :- encontraSuspeitos(PessoasBanco, CaracteristicasCriminoso, [], Suspeitos).

encontraSuspeitos([], CaracteristicasCriminoso, Suspeitos, Suspeitos).

encontraSuspeitos([HeadBank|TailBank], CaracteristicasCriminoso, Temp, Suspeitos) :-
    HeadBank = pessoa(X,Caracteristicas),
    subset(CaracteristicasCriminoso, Caracteristicas),
    append(Temp,[X],NewTemp),
    encontraSuspeitos(TailBank, CaracteristicasCriminoso, NewTemp, Suspeitos).

encontraSuspeitos([HeadBank|TailBank], CaracteristicasCriminoso, Temp, Suspeitos) :-
    encontraSuspeitos(TailBank, CaracteristicasCriminoso, Temp, Suspeitos).

%End Week 1 Jan Challenges
