:- use_module(library(sgml)).
:- consult('Phenesthe/phenesthe').
:- initialization(recognize).

input_phenomenon(exchange(id(_),req(output(language(_),what(_)))), event).
input_phenomenon(exchange(id(_),req(output(language(_),what(_,_)))), event).
input_phenomenon(exchange(id(_),req(output(language(_),what(_,_,_)))), event).
input_phenomenon(exchange(id(_),req(updateInterfaces(interfaceStatus(_)))), event).
input_phenomenon(exchange(id(_),req(print(type(_)))), event).
input_phenomenon(exchange(id(_),rsp(_)), event).
input_phenomenon(exchange(id(_),ntf(events(_))), event).
input_phenomenon(exchange(id(_),ntf(events(_,_))), event).
input_phenomenon(exchange(id(_),ntf(events(_,_,_))), event).
input_phenomenon(exchange(id(_),ntf(events(_,_,_,_))), event).
input_phenomenon(exchange(id(_),ntf(events(_,_,_,_,_))), event).
input_phenomenon(exchange(id(_),ntf(events(_,_,_,_,_,_))), event).
input_phenomenon(exchange(id(_),ntf(events(_,_,_,_,_,_,_))), event).

event_phenomenon prv:output(Lang, Msg, Id) :=
    exchange(id(Id), req(output(language(Lang),what(Msg)))) or
    exchange(id(Id), req(output(language(Lang),what(Msg,_)))) or
    exchange(id(Id), req(output(language(Lang),what(_,Msg)))) or
    exchange(id(Id), req(output(language(Lang),what(Msg,_,_)))) or
    exchange(id(Id), req(output(language(Lang),what(_,Msg,_)))) or
    exchange(id(Id), req(output(language(Lang),what(_,_,Msg)))).

event_phenomenon notification(E) :=
    exchange(id(_),ntf(events(E))) or
    exchange(id(_),ntf(events(E,_))) or
    exchange(id(_),ntf(events(_,E))) or
    exchange(id(_),ntf(events(E,_,_))) or
    exchange(id(_),ntf(events(_,E,_))) or
    exchange(id(_),ntf(events(_,_,E))) or
    exchange(id(_),ntf(events(E,_,_,_))) or
    exchange(id(_),ntf(events(_,E,_,_))) or
    exchange(id(_),ntf(events(_,_,E,_))) or
    exchange(id(_),ntf(events(_,_,_,E))) or
    exchange(id(_),ntf(events(E,_,_,_,_))) or
    exchange(id(_),ntf(events(_,E,_,_,_))) or
    exchange(id(_),ntf(events(_,_,E,_,_))) or
    exchange(id(_),ntf(events(_,_,_,E,_))) or
    exchange(id(_),ntf(events(_,_,_,_,E))).

event_phenomenon prv:ack(Id) :=
    exchange(id(Id), rsp(ack)).

state_phenomenon prv:req_ack(Lang, Msg) :=
    prv:output(Lang, Msg, Id) ~> prv:ack(Id).

event_phenomenon notification(Msg) :=
    exchange(id(_), ntf(events(L))) and
    memberchk(Msg, L).

event_phenomenon output(Lang, Msg) :=
    end(prv:req_ack(Lang, Msg)).

event_phenomenon updateInterfaces(Status) :=
    end(exchange(id(Id), req(updateInterfaces(interfaceStatus(Status)))) ~> prv:ack(Id)).

event_phenomenon print(Type) :=
    end(exchange(id(Id), req(print(type(Type)))) ~> prv:ack(Id)).

% Pass Criteria
state_phenomenon test:dut_asks_to_insert_card_after_swipe :=
    notification(cardSwiped) ~> output(_, msg(crdhldrEmvInsertCard)).
state_phenomenon test:dut_eventually_approves_transaction_using_chip :=
    notification(cardInserted) ~> output(_, msg(crdhldrEmvApproved)).

recognize :-
    assert_all_input_events,
    preprocess_phenomena_definitions,
    recognition_query(100, 100, 100).

assert_all_input_events :-
    findall(input_event_instant(E,T), input_event(E,T), D),
    maplist(assertz, D).
input_event(exchange(Id,Payload), Time) :-
    load_xml('/tmp/events', Xml, []),
    term_xml(Term, Xml),
    ['EventLogRecord'(ts(First),_)|_] = Term,
    member('EventLogRecord'(ts(T),ev(Id,pd(Payload))), Term),
    Time is truncate((T - First + 1) * 100000000).

query(P) :-
    (
        G = event_instants;
        G = state_intervals;
        G = dynamic_phenomenon_intervals
    ),
    P =.. [G,_,_],
    call(P).

query_single(Q) :-
    query(P),
    P =.. [G,T,L],
    member(X, L),
    Q =.. [G,T,X].

main :-
    findall(P, query(P), L),
    phrase(answer(L), W),
    atom_string(A, W),
    write(A).

answer([]) --> [].
answer([H|T]) --> term(H), ['\n'], answer(T).
term(T) --> { term_string(T, S), string_chars(S, C) }, C.

term_xml([], []).
term_xml([Y], [X]) :-
    atom(X),
    (
        atom_number(X, Y) -> true; Y = X
    ).
term_xml([A|B], [element(N,_,C)|T]) :-
    term_xml(X, C),
    A =.. [N|X],
    term_xml(B,T).
