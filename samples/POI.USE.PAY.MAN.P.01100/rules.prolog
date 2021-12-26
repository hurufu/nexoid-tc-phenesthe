% Pass criteria for POI.USE.PAY.MAN.P.01100

% FIXME: Actually payment may be selected at the same time as an amount or manual
% entry or even it may be set beforehead as a default service
dynamic_phenomenon test:payment_service_is_triggered_by_entering_pan_and_expiry_data_and_amount(amount(T,S,C), manual(P,Y,M)) :=
    payment before (manual(P,Y,M) and amount(T,S,C)).

dynamic_phenomenon test:contactless_reader_isnt_activated(Id1,Id2) :=
    (
        exchange(Id1,req(updateInterfaces(interfaceStatus(0)))) before exchange(Id1,rsp(ack))
    ) before (
        output(Id2,_,msg(crdhldrEmvPleaseWait))
    ).

% It is impossible for libnexoid to ask for an amount, so this test always passes
event_phenomenon neg_test:dut_prompts_for_an_amount := false.

dynamic_phenomenon neg_test:dut_prompts_for_cvd_presence(I,L,D) :=
    entry(I,L,msg(crdhldrEntCvdPresence),D).
dynamic_phenomenon neg_test:dut_prompts_for_cvd(I,L,D) :=
    entry(I,L,msg(crdhldrEntCvd),D).

event_phenomenon neg_test:dut_outputs_irrelevant_messages(Id) :=
    output(Id,_,msg(crdhldrMsgPresentCard)) or
    output(Id,_,msg(crdhldrMsgPresentCardOrUseMagstripe)) or
    output(Id,_,msg(crdhldrMsgInsertOrPresentCard)) or
    output(Id,_,msg(crdhldrMsgPleaseInsertCard)).

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
