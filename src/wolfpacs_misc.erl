-module(wolfpacs_misc).
-export([anonymize_file/2,
	 anonymize_dataset_to_file/2,
	 anonymize/1]).

anonymize_file(Src, Dst) ->
    {ok, Content} = file:read_file(Src),
    anonymize_dataset_to_file(Content, Dst).

anonymize_dataset_to_file(Content, Dst) ->
    Flow = no_flow,
    Strategy = {explicit, little},
    {ok, {_Meta, Info}, <<>>} = wolfpacs_file_format:decode(Flow, Strategy, Content),
    Anonymized = anonymize(Info),
    Encoded = wolfpacs_file_format:encode(Flow, Strategy, Anonymized),
    file:write_file(Dst, Encoded).

anonymize(DataSet) ->
    priv_anonymize(maps:to_list(DataSet), []).

priv_anonymize([], Acc) ->
    maps:from_list(Acc);
priv_anonymize([{{G, E}, Value}|Rest], Acc) ->
    VR = wolfpacs_group_elements:vr(G, E),
    priv_anonymize_item(G, E, Value, VR, Rest, Acc).

priv_anonymize_item(G, _E, _Value, _VR, Rest, Acc) when G rem 2 =:= 1 ->
    %% Private headers get removed
    priv_anonymize(Rest, Acc);

priv_anonymize_item(16#10, _E, _Value, _VR, Rest, Acc) ->
    %% Patient info gets removed
    priv_anonymize(Rest, Acc);

priv_anonymize_item(16#8, E, _Value, _VR, Rest, Acc) when E >= 16#20 andalso E =< 16#21 ->
    %% Study and series time and date gets removed
    priv_anonymize(Rest, Acc);

priv_anonymize_item(16#70, _E, _Value, _VR, Rest, Acc) ->
    %% Content and creator info gets removed
    priv_anonymize(Rest, Acc);

priv_anonymize_item(G, E, Value, _VR, Rest, Old) ->
    New = [{{G, E}, Value}|Old],
    priv_anonymize(Rest, New).
