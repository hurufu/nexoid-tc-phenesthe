:- use_module(library(sgml)).
:- consult('Phenesthe/phenesthe').
:- multifile phenomenon_conditions/2.

input_phenomenon(exchange(_Id,_Payload), event).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SCAP (Sale, Cardholder and Attendant Protocol) rules

% Any notification may have multiple events in any order, this rule just unwraps
% them, to simplify the reasoning.
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

% SCAP module have selected payment as a current service. Note: this doesn't
% mean that payment is immidiately started, it is a mere indication that when
% minimal service start condition will be satisfied then payment should be
% started. There many more different services defined by nexo.
event_phenomenon payment :=
    notification(serviceSelection(serviceId(payment))).
event_phenomenon refund :=
    notification(serviceSelection(serviceId(refund))).
event_phenomenon cancellation :=
    notification(serviceSelection(serviceId(cancellation))).
event_phenomenon cardValidityCheck :=
    notification(serviceSelection(serviceId(cardValidityCheck))).

% Amount was provided, it may trigger any currently selected service if minimal
% conditions for the service are set to be amount only. Amount is necessary to
% start payment, unless the default amount is configured, eg. for parking lots.
% Rules are different for other services.
event_phenomenon amount(T,S,C) :=
    notification(amountEntry(totalAmount(T),supplementaryAmount(amount(S)),cashbackAmount(C))).

% Card data was entered manually it also may start a currently selected service.
event_phenomenon manual(P,Y,M) :=
    notification(manualEntry(pan(P),expirationDate(year(Y),month(M)),_CVD)).

% Card was inserted into the chip reader, the actual behavior depends on the
% configuration. It may start a service or it may be ignored and used later when
% transaction technology will be selected by the terminal.
event_phenomenon inserted :=
    notification(cardInserted).

% Update current state of input devices, like contact chip reader, NFC, etc.
% If you want to decode status use number_interface_status/2
dynamic_phenomenon update(Id,S) :=
    exchange(Id,req(updateInterfaces(interfaceStatus(S)))) before exchange(Id,rsp(ack)).

% Print receipt
dynamic_phenomenon receipt(Id,P) :=
    exchange(Id,req(print(type(P)))) before exchange(Id,rsp(ack)).

% Display a message to a cardholder xor attendant
dynamic_phenomenon output(Id,Language,Message) :=
    (
        exchange(Id,req(output(language(Language),what(Message)))) or
        exchange(Id,req(output(language(Language),what(Message,_)))) or
        exchange(Id,req(output(language(Language),what(_,Message)))) or
        exchange(Id,req(output(language(Language),what(Message,_,_)))) or
        exchange(Id,req(output(language(Language),what(_,Message,_)))) or
        exchange(Id,req(output(language(Language),what(_,_,Message))))
    ) before (
        exchange(Id, rsp(ack))
    ).

% Request some data from cardholder xor attendant
dynamic_phenomenon entry(Id,prompt(Language,Message),entry(EnteredData)) := 
    (
        exchange(Id,req(entry(Language,what(Message)))) or
        exchange(Id,req(entry(Language,what(Message,_)))) or
        exchange(Id,req(entry(Language,what(_,Message)))) or
        exchange(Id,req(entry(Language,what(Message,_,_)))) or
        exchange(Id,req(entry(Language,what(_,Message,_)))) or
        exchange(Id,req(entry(Language,what(_,_,Message))))
    ) before (
        exchange(Id,rsp(ackEntry(EnteredData)))
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main :-
    main('/tmp/events').
main(XmlFile, AdditionalRules) :-
    consult(AdditionalRules),
    main(XmlFile).
main(XmlFile) :-
    recognize(XmlFile),
    setof(P, query(P), L),
    phrase(answer(L), W),
    atom_string(A, W),
    write(A).

recognize(XmlFile) :-
    findall(input_event_instant(E,T), input_event(XmlFile,T:E), D),
    maplist(assertz, D),
    preprocess_phenomena_definitions,
    recognition_query(100, 100, 100).

input_event(XmlFile, Time:exchange(Id,Payload)) :-
    load_xml(XmlFile, Xml, []),
    term_xml(Term, Xml),
    member('EventLogRecord'(ts(T),ev(Id,pd(Payload))), Term),
    ts_atom_timestamp(T, Time).

query(P) :-
    (
        G = event_instants;
        G = state_intervals;
        G = dynamic_phenomenon_intervals
    ),
    P =.. [G,_,_],
    call(P).

answer([]) --> [].
answer([H|T]) --> term(H), ['\n'], answer(T).
term(T) --> { term_string(T, S), string_chars(S, C) }, C.

ts_atom_timestamp(T,N) :-
    atom_chars(T, Ts),
    phrase(gtime(Ms,Ss), Ts),
    number_chars(M, Ms),
    number_chars(S, Ss),
    N is M * 60 + S.

gtime(M,S) --> any, minute(M), second(S), timezone.
timezone --> (['+']|['-']), any.
minute([M1,M2]) --> [M1,M2].
second([S1,S2,'.'|F]) --> [S1,S2], ['.'], fraction(F).
fraction([]) --> [].
fraction([H|T]) --> [H], fraction(T).
any --> [] | [_], any.

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

number_digits(Padding, Number, NumberList) :-
    format(chars(A), "~`0t~d~*|", [Number, Padding]),
    maplist(atom_number, A, NumberList).

list_service_start_events([A,B,C,D,E,F,G,H], S) :-
    S = serviceStartEvents(
        cardInserted(A),
        cardSwiped(B),
        amountEntry(C),
        manualEntry(D),
        referenceEntry(E),
        accept(F),
        cardholderDetect(G),
        rfu(H)).

number_interface_status(N, S) :-
    number_digits(8, N, [A,B,C,D,E,F,G,H]),
    S = interfaceStatus(
        % Cardholder facing interfaces
        cardholder(
            chipReader(A),
            magneticStripeReader(B),
            contactlessReader(G),
            % Unspecified, any proprietary type of cardholder detection
            detect(H)
        ),
        % Attendant facing interfaces
        attendant(
            numericKeypad(C),
            fKeyManualEntry(D),
            fKeyReferenceEntry(E),
            fKeyAccept(F)
        )
    ).
