:- use_module(library(sgml)).
:- consult('Phenesthe/phenesthe').
:- initialization(recognize).

input_phenomenon(exchange(id(_),req(output(language(_),what(_)))), event).
input_phenomenon(exchange(id(_),req(output(language(_),what(_,_)))), event).
input_phenomenon(exchange(id(_),req(output(language(_),what(_,_,_)))), event).
input_phenomenon(exchange(id(_),req(entry(language(_),what(_)))), event).
input_phenomenon(exchange(id(_),req(entry(language(_),what(_,_)))), event).
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

event_phenomenon prv:prompt(Lang, Msg, Id) :=
    exchange(id(Id), req(entry(language(Lang),what(Msg)))) or
    exchange(id(Id), req(entry(language(Lang),what(Msg,_)))) or
    exchange(id(Id), req(entry(language(Lang),what(_,Msg)))).

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

event_phenomenon prv:ackEntry(Id, Data) :=
    exchange(id(Id), rsp(ackEntry(Data))).

state_phenomenon prv:req_ack(Lang, Msg) :=
    prv:output(Lang, Msg, Id) ~> prv:ack(Id).

state_phenomenon prv:req_ack_entry(Lang, Msg, Data) :=
    prv:prompt(Lang, Msg, Id) ~> prv:ackEntry(Id, Data).

event_phenomenon notification(Msg) :=
    exchange(id(_), ntf(events(L))) and
    memberchk(Msg, L).

event_phenomenon output(Lang, Msg) :=
    end(prv:req_ack(Lang, Msg)).

event_phenomenon prompt(Lang, Msg) :=
    end(prv:req_ack_entry(Lang, Msg, _)).

event_phenomenon updateInterfaces(Status) :=
    end(exchange(id(Id), req(updateInterfaces(interfaceStatus(Status)))) ~> prv:ack(Id)).

event_phenomenon print(Type) :=
    end(exchange(id(Id), req(print(type(Type)))) ~> prv:ack(Id)).

event_phenomenon entry(Data) :=
    end(prv:req_ack_entry(_, _, Data)).

% Pass criteria for POI.USE.PAY.MAN.P.01100

% FIXME: It should handle notifications sent separately
state_phenomenon test:payment_service_is_triggered_by_entering_pan_and_expiry_data_and_amount :=
    (
        notification(serviceSelection(serviceId(payment))) and
        notification(manualEntry(pan(_),expirationDate(year(_),month(_)))) and
        notification(amountEntry(totalAmount(_),supplementaryAmount(amount(_)),cashbackAmount(_)))
    ) ~> output(_, msg(crdhldrMsgWelcome)).

% It is impossible for nexoid to ask for an amount, so this test always passes
%event_phenomenon test:dut_prompts_for_an_amount :=
%    false.
%
event_phenomenon testn:dut_prompts_for_cvd :=
    prompt(_, msg(crdhldrEntCvd)) or
    prompt(_, msg(crdhldrEntCvdPresence)).

% FIXME: This event should happend before actual transaction processing
event_phenomenon test:contactless_reader_isnt_activated :=
    updateInterfaces(interfaceStatus(0)).

event_phenomenon test:dut_outputs_irrelevant_messages :=
    output(_, msg(crdhldrMsgPresentCard)) or
    output(_, msg(crdhldrMsgPresentCardOrUseMagstripe)) or
    output(_, msg(crdhldrMsgInsertOrPresentCard)) or
    output(_, msg(crdhldrMsgPleaseInsertCard)).

% TODO: Add support for internal data validation, probably through acquirer
%       message, receipt or internally.
%event_phenomenon test:tag_are_present :=
%    technologySelected: manualEntry
%    selectedService: payment
%    pan: <enterend value>
%    expirationDate: <entered value>
%    processingStatus: { cardProductSelectionForNonChip
%                        appProfileSelectionForNonChip
%                        fullMagStripeOrManualEntry
%                        technologySelectionNonFallbackMode }
%    selectedApplicationProfileNumber: <see test description>

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
