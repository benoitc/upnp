-module(upnp).

-export([start/2, stop/1,
         child_spec/2]).


%% @doc Start port mapping.
%%
%% <em>Specs</> are port mapping options. Ex:
%% <pre>[{ip, 0.0.0.0}, {maps, [{tcp, 8000}]}]</pre>
-spec start(any(), any()) -> {ok, pid()}.
start(Ref, Specs) ->
    supervisor:start_child(upnp_sup, child_spec(Ref, Specs)).

%% @doc stop the port mapping
-spec stop(any()) -> ok | {error, not_found}.
stop(Ref) ->
    case supervisor:terminate_child(upnp_sup, {upnp_handler_sup, Ref}) of
        ok ->
            supervisor:delete_child(upnp_sup, {upnp_handler_sup, Ref});
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Return a child spec suitable for embedding.
-spec child_spec(any(), any()) -> supervisor:child_spec().
child_spec(Ref, Specs) ->
    {{upnp_handler_sup, Ref},
     {upnp_handler_sup, start_link, [Specs]},
     permanent, 5000, supervisor, [upnp_handler_sup]}.
