-module(upnp_util).

-export([simple_request/2, simple_request/3, simple_request/4,
         to_binary/1,
         to_upper/1, to_lower/1,
         merge_proplists/2,
         get_value/2, get_value/3,
         parse_ip/1]).

simple_request(Method, Url) ->
    simple_request(Method, Url, [], <<>>).

simple_request(Method, Url, Headers) ->
    simple_request(Method, Url, Headers, <<>>).

simple_request(Method, Url, Headers, Body) ->
    case hackney:request(Method, Url, Headers, Body, [{pool, default}]) of
        {ok, Status, RespHeaders, Client} ->
            case hackney:body(Client) of
                {ok, RespBody, _} ->
                    {ok, Status, RespHeaders, RespBody};
                Error ->
                    io:format("Error body ~p~n", [Error]),
                    Error
            end;
        Error ->
            io:format("Error req ~p~n", [Error]),
            Error
    end.

to_binary(V) when is_binary(V) ->
    V;
to_binary(V) when is_list(V) ->
    try
        list_to_binary(V)
    catch
        _ ->
            list_to_binary(io_lib:format("~p", [V]))
    end;
to_binary(V) when is_atom(V) ->
    list_to_binary(atom_to_list(V));
to_binary(V) ->
    V.

%% @doc converts all characters in the specified binary to uppercase.
-spec to_upper(binary()) -> binary().
to_upper(Bin) ->
  to_upper(Bin, <<>>).

%% @private
to_upper(<<>>, Acc) ->
  Acc;
to_upper(<<C, Rest/binary>>, Acc) when $a =< C, C =< $z ->
  to_upper(Rest, <<Acc/binary, (C-32)>>);
to_upper(<<195, C, Rest/binary>>, Acc) when 160 =< C, C =< 182 -> %% A-0 with tildes plus enye
  to_upper(Rest, <<Acc/binary, 195, (C-32)>>);
to_upper(<<195, C, Rest/binary>>, Acc) when 184 =< C, C =< 190 -> %% U and Y with tilde plus greeks
  to_upper(Rest, <<Acc/binary, 195, (C-32)>>);
to_upper(<<C, Rest/binary>>, Acc) ->
  to_upper(Rest, <<Acc/binary, C>>).

%% @doc converts all characters in the specified binary to lowercase
-spec to_lower(binary()) -> binary().
to_lower(Bin) ->
  to_lower(Bin, <<>>).

to_lower(<<>>, Acc) ->
  Acc;
to_lower(<<C, Rest/binary>>, Acc) when $A =< C, C =< $Z ->
  to_lower(Rest, <<Acc/binary, (C+32)>>);
to_lower(<<195, C, Rest/binary>>, Acc) when 128 =< C, C =< 150 -> %% A-0 with tildes plus enye
  to_lower(Rest, <<Acc/binary, 195, (C+32)>>);
to_lower(<<195, C, Rest/binary>>, Acc) when 152 =< C, C =< 158 -> %% U and Y with tilde plus greeks
  to_lower(Rest, <<Acc/binary, 195, (C+32)>>);
to_lower(<<C, Rest/binary>>, Acc) ->
  to_lower(Rest, <<Acc/binary, C>>).

%% @doc Returns a proplist formed by merging OldProp and NewProp. If a key
%%      presents only in OldProp or NewProp, the tuple is picked. If a key
%%      presents in both OldProp and NewProp, the tuple from NewProp is
%%      picked.
%% @end
-spec merge_proplists(proplists:proplist(), proplists:proplist()) ->
    proplists:proplist().
merge_proplists(OldProp, NewProp) ->
    lists:ukeymerge(1, lists:ukeysort(1, NewProp), lists:ukeysort(1, OldProp)).


%% @doc emulate proplists:get_value/2,3 but use faster lists:keyfind/3
-spec(get_value/2 :: (Key :: term(), Prop :: [term()] ) -> term()).
get_value(Key, Prop) ->
    get_value(Key, Prop, undefined).

-spec(get_value/3 :: (Key :: term(), Prop :: [term()], Default :: term() ) -> term()).
get_value(Key, Prop, Default) ->
    case lists:keyfind(Key, 1, Prop) of
	false ->
	    case lists:member(Key, Prop) of
		true -> true;
		false -> Default
	    end;
	{Key, V} -> % only return V if a two-tuple is found
	    V;
	Other when is_tuple(Other) -> % otherwise return the default
	    Default
    end.

parse_ip(Ip0) ->
    {ok, Ip} = inet_parse:address(Ip0),
    parse_ip1([Ip]).

parse_ip1([]) ->
    nil;
parse_ip1([{0, 0, 0, 0}]) ->
    {ok, Ifs} = inet:getif(),
    parse_ip1(lists:reverse([Ip || {Ip, _, _} <- Ifs]));
parse_ip1([{192, 168, _, _}=Ip|_]) ->
    inet_parse:ntoa(Ip);
parse_ip1([{172, 16, _, _}=Ip|_]) ->
    inet_parse:ntoa(Ip);
parse_ip1([{10, _, _, _}=Ip|_]) ->
    inet_parse:ntoa(Ip);
parse_ip1([_|Rest]) ->
    parse_ip1(Rest).
