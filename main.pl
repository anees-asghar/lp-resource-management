% Syed Anees Ur Rehman Asghar | 107328

:- set_prolog_flag(answer_write_options,[max_depth(0)]). % para listas completas
:- ['dados.pl'], ['keywords.pl']. % ficheiros a importar.



/* ########################## UTILITY PREDICATES ########################## */

/*
    membroListaPeridos(Periodo, ListaPeriodos)
    True if one of the following is true:
        - the period provided is a member of the list of periods
        - the period provided is p1_2 and the list contains either p1 or p2 or both
        - the period provided is p3_4 and the list contains either p3 or p4 or both
*/
membroListaPeriodos(p1_2, [p1|_]) :- !.
membroListaPeriodos(p1_2, [p2|_]) :- !.
membroListaPeriodos(p3_4, [p3|_]) :- !.
membroListaPeriodos(p3_4, [p4|_]) :- !.
membroListaPeriodos(H, [H|_]) :- !.
membroListaPeriodos(Periodo, [_|Resto]) :- membroListaPeriodos(Periodo, Resto).


/*
    insereOrdenado(El, Lst1, Lst2)
    True if Lst2 is the resulting list after inserting the integer El
    into the ordered list of integers Lst1, such that the increasing order 
    is maintained. El is only inserted if Lst1 doesn't already contain it.
*/
insereOrdenado(El, [], [El]).
insereOrdenado(El, [El|Resto], [El|Resto]) :- !. % Lst1 already contains El
insereOrdenado(El, [H|Resto], [H|Z]) :-
    El > H,
    insereOrdenado(El, Resto, Z), !.
insereOrdenado(El, [H|Resto], [El,H|Resto]) :- El < H.


/*
    insereOrdenadoAlpha(El, Lst1, Lst2)
    True if Lst2 is the resulting list after inserting El (string or char)
    into the ordered list of strings/chars Lst1, such that the increasing order 
    is maintained. El is only inserted if Lst1 doesn't already contain it.
*/
insereOrdenadoAlpha(El, [], [El]).
insereOrdenadoAlpha(El, [El|Resto], [El|Resto]) :- !. % Lst1 already contains El
insereOrdenadoAlpha(El, [H|Resto], [H|Z]) :-
    El @> H,
    insereOrdenadoAlpha(El, Resto, Z), !.
insereOrdenadoAlpha(El, [H|Resto], [El,H|Resto]) :- El @< H.


/*
    somaLista(Lst, Soma)
    True if Soma is the sum of the elements of the list of integers Lst.
*/
somaLista([], 0).
somaLista([H|Resto], Soma) :-
    somaLista(Resto, SomaSemH),
    Soma is SomaSemH + H.


/*
    max(Num1, Num2, Max)
    True if Max is the larger number between Num1 and Num2.
*/
max(Num1, Num2, Num1) :- Num1 >= Num2, !.
max(Num1, Num2, Num2) :- Num1 < Num2.


/*
    min(Num1, Num2, Min)
    True if Min is the smaller number between Num1 and Num2.
*/
min(Num1, Num2, Num1) :- Num1 =< Num2, !.
min(Num1, Num2, Num2) :- Num1 > Num2.


/*
    combinacoesPossiveis2(Lista1, Lista2, Combinacoes)
    True if Combinacoes is a list of all possible unique combinations (x, y) 
    such that x belongs to Lista1 and y belongs to Lista2.
*/
combinacoesPossiveis2(Lista1, Lista2, Combinacoes) :- 
    findall((X, Y), (member(X, Lista1), member(Y, Lista2)), Combinacoes).


/*
    combinacoesPossiveis3(Lista1, Lista2, Lista3, Combinacoes)
    True if Combinacoes is a list of all possible unique combinations (x, y, z) 
    such that x belongs to Lista1, y belongs to Lista2 and z belongs to Lista3.
*/
combinacoesPossiveis3(Lista1, Lista2, Lista3, Combinacoes) :- 
    findall((X, Y, Z), (member(X, Lista1), member(Y, Lista2), member(Z, Lista3)), Combinacoes).



/* ########################## SPACE MANAGEMENT PROBLEMS ########################## */

/*
    1 (a) - eventosSemSalas(EventosSemSala)
    True if EventosSemSala is a list, ordered and without repititions,
    of IDs of events that dont have a room associated.
*/
eventosSemSalas(EventosSemSala) :-
    setof(
        ID, 
        Disciplina^Tipo^NumeroAlunos^evento(ID, Disciplina, Tipo, NumeroAlunos, semSala), 
        EventosSemSala
    ), !;
    EventosSemSala = []. % empty list if no such events found


/*
    1 (b) - eventosSemSalasDiaSemana(DiaDaSemana, EventosSemSala)
    True if EventosSemSala is a list, ordered and without repititions, of 
    IDs of events that occur on day DiaDaSemana and have no room associated.
*/
eventosSemSalasDiaSemana(DiaDaSemana, EventosSemSala) :-
    setof(
        ID, 
        Disciplina^Tipo^NumeroAlunos^HoraInicio^HoraFim^Duracao^Periodo^(
            evento(ID, Disciplina, Tipo, NumeroAlunos, semSala), 
            horario(ID, DiaDaSemana, HoraInicio, HoraFim, Duracao, Periodo)
        ), 
        EventosSemSala
    ), !;
    EventosSemSala = []. % empty list if no such events found


/*
    1 (c) - eventosSemSalasPeriodo(ListaPeriodos, EventosSemSala)
    True if EventosSemSala is a list, ordered and without repititions, of
    IDs of events that occur in periods listed by ListaPeriodos and have
    no room associated.
*/
eventosSemSalasPeriodo(ListaPeriodos, EventosSemSala) :-
    setof(
        ID, 
        Disciplina^Tipo^NumeroAlunos^Dia^HoraInicio^HoraFim^Duracao^Periodo^(
            evento(ID, Disciplina, Tipo, NumeroAlunos, semSala),
            horario(ID, Dia, HoraInicio, HoraFim, Duracao, Periodo),
            membroListaPeriodos(Periodo, ListaPeriodos)
        ), 
        EventosSemSala
    ), !;
    EventosSemSala = []. % empty list if no such events found


/*
    2 (a) - organizaEventos(ListaEventos, Periodo, EventosNoPeriodo)
    True if EventosNoPeriodo is a list, ordered and with no repititions, of
    event IDs in list ListaEventos that occur in period Periodo.
*/
organizaEventos([], _, []).

organizaEventos([H|Resto], Periodo, EventosNoPeriodo) :-
    horario(H, _, _, _, _, P),
    membroListaPeriodos(P, [Periodo]),
    organizaEventos(Resto, Periodo, EventosNoPeriodoSemH),
    insereOrdenado(H, EventosNoPeriodoSemH, EventosNoPeriodo), !;
    /* if the event at the head of the list didn't satisfy our condition, 
    we simply go to the next event in ListaEventos */
    organizaEventos(Resto, Periodo, EventosNoPeriodo).


/*
    2 (b) - eventosMenoresQue(DuracaoDada, ListaEventosMenoresQue)
    True if ListaEventosMenoresQue is a list, ordered and without repititions, of
    IDs of events that have a duration less than or equal to DuracaoDada.
*/
eventosMenoresQue(DuracaoDada, ListaEventosMenoresQue) :-
    setof(
        ID, 
        Dia^HoraInicio^HoraFim^Duracao^Periodo^(
            horario(ID, Dia, HoraInicio, HoraFim, Duracao, Periodo),
            Duracao =< DuracaoDada
        ), 
        ListaEventosMenoresQue
    ), !;
    ListaEventosMenoresQue = []. % empty list if no such events found


/*
    2 (c) - eventosMenoresQueBool(ID, DuracaoDada)
    True if the event associated to ID has a duration less than or equal to DuracaoDada.
*/
eventosMenoresQueBool(ID, DuracaoDada) :-
    horario(ID, _, _, _, Duracao, _),
    Duracao =< DuracaoDada.


/*
    2 (d) - procuraDisciplinas(Curso, ListaDisciplinas)
    True if ListaDisciplinas is an alphabetically ordered list of disciplines of 
    course Curso.
*/
procuraDisciplinas(Curso, ListaDisciplinas) :-
    setof(
        Disciplina, 
        ID^Curso^Ano^Turma^Tipo^NumeroAlunos^Sala^(
            turno(ID, Curso, Ano, Turma),
            evento(ID, Disciplina, Tipo, NumeroAlunos, Sala)
        ), 
        ListaDisciplinas
    ), !;
    ListaDisciplinas = []. % empty list if no such disciplines found


/*
    2 (e) - organizaDisciplinas(ListaDisciplinas, Curso, Semestres)
    True if Semestres is a list with two lists. The first list contains the 
    disciplines of ListaDisciplinas of course Curso that take place in the 
    first semester, while the second list contains the ones that take place in
    the second semester. Both lists are alphabetically ordered with no repeated
    elements.
*/
organizaDisciplinas([], _, [[], []]).

organizaDisciplinas([Disciplina|Resto], Curso, [S1, S2]) :-
    evento(ID, Disciplina, _, _, _),
    horario(ID, _, _, _, _, Periodo),
    turno(ID, Curso, _, _),
    (
        member(Periodo, [p1, p2, p1_2]), % if true, discipline belongs to first semester
        organizaDisciplinas(Resto, Curso, [Tmp, S2]),
        insereOrdenadoAlpha(Disciplina, Tmp, S1), !;
        
        organizaDisciplinas(Resto, Curso, [S1, Tmp]),
        insereOrdenadoAlpha(Disciplina, Tmp, S2)
    ).


/*
    2 (f) - horasCurso(PeriodoDado, Curso, Ano, TotalHoras)
    True if TotalHoras is the total duration of events associated to 
    course Curso, in the year Ano and period PeriodoDado.
*/
horasCurso(PeriodoDado, Curso, Ano, TotalHoras) :-
    % ListaIDs - set of all event IDs associated to this course and year
    setof(
        ID,
        Turma^turno(ID, Curso, Ano, Turma),
        ListaIDs
    ),
    % ListaDuracoes - list of durations of all events associated to this course, year and period
    findall(
        Duracao,
        (
            horario(I, _, _, _, Duracao, Periodo),
            member(I, ListaIDs),
            membroListaPeriodos(Periodo, [PeriodoDado])
        ),
        ListaDuracoes
    ),
    somaLista(ListaDuracoes, TotalHoras), !;
    TotalHoras = 0. % zero if no events are associated to course Curso in year Ano


/*
    2 (g) - evolucaoHorasCurso(Curso, Evolucao) 
    True if Evolucao is a list of tuples of form (Ano, Periodo, NumHoras), in which 
    NumHoras is the total hours associated to the course Course, in the year Ano and 
    the period Periodo.
*/
evolucaoHorasCurso(Curso, Evolucao) :-
    % Combinacoes - all possible combinations of year and period
    combinacoesPossiveis2([1, 2, 3], [p1, p2, p3, p4], Combinacoes),
    evolucaoHorasCursoAux(Curso, Combinacoes, Evolucao).

evolucaoHorasCursoAux(_, [], []) :- !.

evolucaoHorasCursoAux(Curso, [(Ano, Periodo)|Resto], [(Ano, Periodo, Horas)|Evolucao]) :-
    horasCurso(Periodo, Curso, Ano, Horas),
    evolucaoHorasCursoAux(Curso, Resto, Evolucao).


/*
    3 (a) - ocupaSlot(HoraInicioDada, HoraFimDada, HoraInicioEvento, HoraFimEvento, Horas)
    True if Horas is the duration of overlap between event that starts at HoraInicioEvento 
    and finishes at HoraFimEvento, and the slot that starts at HoraInicioDada and ends at
    HoraFimDada.
*/
ocupaSlot(HoraInicioDada, HoraFimDada, HoraInicioEvento, HoraFimEvento, Horas) :-
    HoraFimDada - HoraInicioEvento >= 0,
    HoraFimEvento - HoraInicioDada >= 0,
    % the above two statements being true suggests that there is an overlap
    min(HoraFimDada, HoraFimEvento, A),
    max(HoraInicioDada, HoraInicioEvento, B),
    Horas is A - B.


/* 
    3 (b) - numHorasOcupadas(Periodo, TipoSala, DiaSemana, HoraInicio, HoraFim, SomaHoras)
    True if SomaHoras is the total number of hours the rooms of type TipoSala are occupied between 
    hours HoraInicioDada and HoraFimDada, on day DiaSemana in period Periodo.
*/
numHorasOcupadas(Periodo, TipoSala, DiaSemana, HoraInicio, HoraFim, SomaHoras) :-
    salas(TipoSala, ListaSalas), % ListaSalas - all rooms of type TipoSala
    numHorasOcupadasListaSalas(Periodo, ListaSalas, DiaSemana, HoraInicio, HoraFim, SomaHoras).

/*
    numHorasOcupadasListaSalas(Periodo, ListaSalas, DiaSemana, HoraInicio, HoraFim, SomaHoras)
    True if SomaHoras is the total number of hours the rooms in list ListaSalas are occupied between 
    hours HoraInicioDada and HoraFimDada, on day DiaSemana in period Periodo.
*/
numHorasOcupadasListaSalas(_, [], _, _, _, 0) :- !.
numHorasOcupadasListaSalas(Periodo, [Sala|Resto], DiaSemana, HoraInicio, HoraFim, SomaHoras) :-
    numHorasOcupadasUmaSala(Periodo, Sala, DiaSemana, HoraInicio, HoraFim, HorasSala),
    numHorasOcupadasListaSalas(Periodo, Resto, DiaSemana, HoraInicio, HoraFim, HorasResto),
    SomaHoras is HorasSala + HorasResto.

/*
    numHorasOcupadasUmaSala(PeriodoDado, Sala, DiaSemana, HoraInicioDada, HoraFimDada, Horas)
    True if Horas is the total number of hours room Sala is occupied between hours 
    HoraInicioDada and HoraFimDada, on day DiaSemana in period PeriodoDado.
*/
numHorasOcupadasUmaSala(PeriodoDado, Sala, DiaSemana, HoraInicioDada, HoraFimDada, Horas) :-
    /* Sobreposicoes - list of durations of overlap between the slot (represented by HoraInicioDada and 
    HoraFimDada) and all events that take place in room Sala on day DiaSemana in period PeriodoDado */
    findall(
        Sobreposicao,
        (
            evento(ID, _, _, _, Sala),
            horario(ID, DiaSemana, HoraInicioEvento, HoraFimEvento, _, Periodo),
            membroListaPeriodos(Periodo, [PeriodoDado]),
            ocupaSlot(HoraInicioDada, HoraFimDada, HoraInicioEvento, HoraFimEvento, Sobreposicao)
        ),
        Sobreposicoes
    ),
    somaLista(Sobreposicoes, Horas).


/*
    3 (c) - ocupacaoMax(TipoSala, HoraInicio, HoraFim, Max)
    True if Max is the total number of hours available to book for rooms 
    of type TipoSala between HoraInicio and HoraFim.
*/
ocupacaoMax(TipoSala, HoraInicio, HoraFim, Max) :-
    salas(TipoSala, Salas),
    length(Salas, NumeroSalas),
    Max is NumeroSalas * (HoraFim - HoraInicio).


/*
    3 (d) - percentagem(SomaHoras, Max, Percentagem)
    True if Percentagem is SomaHoras divided by Max, multiplied by 100.
*/
percentagem(SomaHoras, Max, Percentagem) :-
    Percentagem is (SomaHoras / Max) * 100.


/*
    3 (e) - ocupacaoCritica(HoraInicio, HoraFim, Threshold, Resultados)
    True if Resultados is an ordered list of tuples of type casosCriticos(DiaSemana, 
    TipoSala, Percentagem), where Percentagem is the occupancy percentage of rooms of type
    TipoSala on day DiaSemana between hours HoraInicio and HoraFim, and this percentage 
    is greater than the Threshold value.
*/
ocupacaoCritica(HoraInicio, HoraFim, Threshold, Resultados) :-
    % Combinacoes - all possible combinations of periods, hall types and day names
    combinacoesPossiveis3(
        [p1, p2, p3, p4],
        [grandesAnfiteatros, pequenosAnfiteatros, salasAula, labsPC, labsElectro, labsQuimica, labsFisica, labsRedes, labsJogos, videoConf],
        [segunda-feira, terca-feira, quarta-feira, quinta-feira, sexta-feira, sabado],
        Combinacoes
    ),
    ocupacaoCriticaAux(HoraInicio, HoraFim, Threshold, Combinacoes, Resultados).

ocupacaoCriticaAux(_, _, _, [], []).

ocupacaoCriticaAux(HoraInicio, HoraFim, Threshold, [(Periodo, TipoSala, DiaSemana)|Resto], Resultados) :-
    numHorasOcupadas(Periodo, TipoSala, DiaSemana, HoraInicio, HoraFim, Horas), 
    ocupacaoMax(TipoSala, HoraInicio, HoraFim, Max), 
    percentagem(Horas, Max, Percent),
    Percent > Threshold,
    PercentCeil is ceiling(Percent),
    ocupacaoCriticaAux(HoraInicio, HoraFim, Threshold, Resto, Z),
    insereOrdenadoAlpha(casosCriticos(DiaSemana, TipoSala, PercentCeil), Z, Resultados), !.

ocupacaoCriticaAux(HoraInicio, HoraFim, Threshold, [_|Resto], Resultados) :-
    /* if this predicate is reached, we already know that for the parameters provided by the head of the 
    list, the occupancy percentage was below threshold, so we simply continue to the next item in list */
    ocupacaoCriticaAux(HoraInicio, HoraFim, Threshold, Resto, Resultados).



/* ########################## TABLE ARRANGEMENT PUZZLE ########################## */

/*
    ocupacaoMesa(ListaPessoas, ListaRestricoes, OcupacaoMesa)
    True if OcupacaoMesa is a list of lists representing the manner in which the 8 people of 
    list ListaPessoas must be seated at a rectangular table having observed all the restrictions
    imposed by ListaRestricoes.
*/
ocupacaoMesa(ListaPessoas, ListaRestricoes, OcupacaoMesa) :-
    OcupacaoMesa = [[X1, X2, X3], [X4, X5], [X6, X7, X8]],
    % all 8 people need to occupy one and only one seat
    [A, B, C, D, E, F, G, H] = ListaPessoas,
    Lugares = [X1, X2, X3, X4, X5, X6, X7, X8],
    member(A, Lugares),
    member(B, Lugares),
    member(C, Lugares),
    member(D, Lugares),
    member(E, Lugares),
    member(F, Lugares),
    member(G, Lugares),
    member(H, Lugares),
    ocupacaoMesaAux(ListaPessoas, ListaRestricoes, OcupacaoMesa).

/* apply restrictions one by one */
ocupacaoMesaAux(_, [], [[_, _, _], [_, _], [_, _, _]]).
ocupacaoMesaAux(ListaPessoas, [Restricao|Resto], OcupacaoMesa) :-
    aplicarRestricao(Restricao, OcupacaoMesa),
    ocupacaoMesaAux(ListaPessoas, Resto, OcupacaoMesa).

/* cab1 restriction */
aplicarRestricao(Restricao, [[_, _, _], [NomePessoa, _], [_, _, _]]) :-
    functor(Restricao, Nome, _),
    Nome == cab1,
    cab1(NomePessoa) = Restricao, !.

/* cab2 restriction */
aplicarRestricao(Restricao, [[_, _, _], [_, NomePessoa], [_, _, _]]) :-
    functor(Restricao, Nome, _),
    Nome == cab2,
    cab2(NomePessoa) = Restricao, !.

/* honra restriction */
aplicarRestricao(Restricao, OcupacaoMesa) :-
    functor(Restricao, Nome, _),
    Nome == honra,
    honra(NomePessoa1, NomePessoa2) = Restricao,
    OcupacaoMesa = [[_, _, X3], [X4, X5], [X6, _, _]],
    member([NomePessoa1, NomePessoa2], [[X5, X3], [X4, X6]]), !.

/* lado restriction */
aplicarRestricao(Restricao, OcupacaoMesa) :-
    functor(Restricao, Nome, _),
    Nome == lado,
    lado(NomePessoa1, NomePessoa2) = Restricao,
    OcupacaoMesa = [[X1, X2, X3], [_, _], [X6, X7, X8]],
    member([NomePessoa1, NomePessoa2], [[X1, X2], [X2, X1], [X2, X3], [X3, X2], [X6, X7], [X7, X6], [X7, X8], [X8, X7]]), !.

/* naoLado restriction */
aplicarRestricao(Restricao, OcupacaoMesa) :-
    functor(Restricao, Nome, _),
    Nome == naoLado,
    naoLado(NomePessoa1, NomePessoa2) = Restricao,
    OcupacaoMesa = [[X1, X2, X3], [_, _], [X6, X7, X8]],
    not(member([NomePessoa1, NomePessoa2], [[X1, X2], [X2, X1], [X2, X3], [X3, X2], [X6, X7], [X7, X6], [X7, X8], [X8, X7]])), !.

/* frente restriction */
aplicarRestricao(Restricao, OcupacaoMesa) :-
    functor(Restricao, Nome, _),
    Nome == frente,
    frente(NomePessoa1, NomePessoa2) = Restricao,
    OcupacaoMesa = [[X1, X2, X3], [_, _], [X6, X7, X8]],
    member([NomePessoa1, NomePessoa2], [[X1, X6], [X6, X1], [X2, X7], [X7, X2], [X3, X8], [X8, X3]]), !.

/* naoFrente restriction */
aplicarRestricao(Restricao, OcupacaoMesa) :-
    functor(Restricao, Nome, _),
    Nome == naoFrente,
    naoFrente(NomePessoa1, NomePessoa2) = Restricao,
    OcupacaoMesa = [[X1, X2, X3], [_, _], [X6, X7, X8]],
    not(member([NomePessoa1, NomePessoa2], [[X1, X6], [X6, X1], [X2, X7], [X7, X2], [X3, X8], [X8, X3]])).
