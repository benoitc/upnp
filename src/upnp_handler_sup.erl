%% @author Edward Wang <yujiangw@gmail.com>
%% @doc Supervises all processes of UPnP subsystem.
%% @end

-module(upnp_handler_sup).
-behaviour(supervisor).


-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([start_link/1,
         add_upnp_entity/3]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(Specs) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Specs).


init(Specs) ->
    UPNP_NET = {upnp_net, {upnp_net, start_link, [Specs]},
                permanent, 2000, worker, [upnp_net]},
    HTTPd_Dispatch = [ {'_', [{'_', upnp_handler, []}]} ],
    HTTPd = ranch:child_spec(upnp_cowboy,
                              10, ranch_tcp, [{port, 0}],
                              cowboy_protocol, [{dispatch, HTTPd_Dispatch}]),
    Children = [UPNP_NET, HTTPd],
    RestartStrategy = {one_for_one, 1, 60},
    {ok, {RestartStrategy, Children}}.


add_upnp_entity(Category, Proplist, Maps) ->
    %% Each UPnP device or service can be uniquely identified by its
    %% category + type + uuid.
    ChildID = {upnp_entity, Category,
               proplists:get_value(type, Proplist),
               proplists:get_value(uuid, Proplist)},
    ChildSpec = {ChildID,
                 {upnp_entity, start_link, [Category, Proplist, Maps]},
                 permanent, 2000, worker, dynamic},
    case supervisor:start_child(?SERVER, ChildSpec) of
        {ok, _} -> ok;
        {error, {already_started, _}} ->
            upnp_entity:update(Category, Proplist, Maps)
    end.


-ifdef(EUNIT).

setup_upnp_sup_tree() ->
    {event, Event} = {event, upnp_event:start_link()},
    {table, Table} = {table, upnp_table:start_link()},
    {upnp,  UPNP}  = {upnp,  upnp_sup:start_link()},
    {Event, Table, UPNP}.

teardown_upnp_sup_tree({Event, Table, UPNP}) ->
    Shutdown = fun
        ({ok, Pid}) -> upnp_utils:shutdown(Pid);
        ({error, Reason}) -> Reason
    end,
    EventStatus = Shutdown(Event),
    TableStatus = Shutdown(Table),
    UPNPStatus  = Shutdown(UPNP),
    ?assertEqual(ok, EventStatus),
    ?assertEqual(ok, TableStatus),
    ?assertEqual(ok, UPNPStatus).

upnp_sup_test_() ->
    {foreach,
        fun setup_upnp_sup_tree/0,
        fun teardown_upnp_sup_tree/1, [
        ?_test(upnp_sup_tree_start_case())]}.


upnp_sup_tree_start_case() ->
    upnp_entity:create(device, [{type, <<"InternetGatewayDevice">>},
                                         {uuid, <<"whatever">>}]),
    upnp_entity:create(service, [{type, <<"WANIPConnection">>},
                                          {uuid, <<"whatever">>}]),
    upnp_entity:update(service, [{type, <<"WANIPConnection">>},
                                          {uuid, <<"whatever">>},
                                          {loc, {192,168,1,1}}]),
    ?assert(true).


-endif.
