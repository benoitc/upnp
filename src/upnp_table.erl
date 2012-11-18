%% @author Jesper Louis Andersen <jesper.louis.andersen@gmail.com>
%% @doc Handle various state about torrents in ETS tables.
%% <p>This module implements a server which governs 3 internal ETS
%% tables. As long as the process is living, the tables are there for
%% other processes to query. Also, the server acts as a serializer on
%% table access.</p>
%% @end
-module(upnp_table).
-behaviour(gen_server).


%% API
%% Startup/init
-export([start_link/0]).

%% UPnP entity information
-export([register_upnp_entity/3,
         lookup_upnp_entity/2,
         update_upnp_entity/3]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-record(state, { monitoring :: dict() }).

-define(SERVER, ?MODULE).
-define(TAB_UPNP, upnp_entity).

%%====================================================================

%% @doc Start the server
%% @end
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Inserts a UPnP entity into its dedicated ETS table.
%% @end
-spec register_upnp_entity(pid(), device | service,
                           upnp_types:upnp_device() |
                           upnp_types:upnp_service()) -> true.
register_upnp_entity(Pid, Cat, Entity) ->
    Id = upnp_entity:id(Cat, Entity),
    add_monitor({upnp, Cat, Entity}, Pid),
    true = ets:insert(?TAB_UPNP, {Id, Pid, Cat, Entity}).

%% @doc Returns the pid that governs given UPnP entity.
%% @end
-spec lookup_upnp_entity(device | service,
                         upnp_types:upnp_device() |
                         upnp_types:upnp_service()) ->
    {ok, pid()} | {error, not_found}.
lookup_upnp_entity(Cat, Entity) ->
    case ets:lookup(?TAB_UPNP, upnp_entity:id(Cat, Entity)) of
        [{_Id, Pid, _Cat, _Entity}] -> {ok, Pid};
        [] -> {error, not_found}
    end.

%% @doc Updates UPnP ETS table with information of given UPnP entity.
%% @end
-spec update_upnp_entity(pid(), device | service,
                         upnp_types:upnp_device() |
                         upnp_types:upnp_service()) -> true.
update_upnp_entity(Pid, Cat, Entity) ->
    EntityId = upnp_entity:id(Cat, Entity),
    true = ets:insert(?TAB_UPNP, {EntityId, Pid, Cat, Entity}).

%%====================================================================

%% @private
init([]) ->
    ets:new(?TAB_UPNP, [named_table, public, set]),
    {ok, #state{ monitoring = dict:new() }}.

%% @private
handle_call({monitor_pid, Type, Pid}, _From, S) ->
    Ref = erlang:monitor(process, Pid),
    {reply, ok,
     S#state {
            monitoring = dict:store(Ref, {Pid, Type}, S#state.monitoring)}};
handle_call(Msg, _From, S) ->
    lager:error("Unknown handle_call: ~p", [Msg]),
    {noreply, S}.

%% @private
handle_cast(Msg, S) ->
    lager:error("Unknown handle_cast: ~p", [Msg]),
    {noreply, S}.

%% @private
handle_info({'DOWN', Ref, _, _, _}, S) ->
    {ok, {_X, Type}} = dict:find(Ref, S#state.monitoring),
    case Type of
        {upnp, Cat, Entity} ->
            EntityId = upnp_entity:id(Cat, Entity),
            upnp_entity:unsubscribe(Cat, Entity),
            true = ets:delete(?TAB_UPNP, EntityId)
    end,
    {noreply, S#state { monitoring = dict:erase(Ref, S#state.monitoring) }};
handle_info(Msg, S) ->
    lager:error("Unknown handle_info call: ~p", [Msg]),
    {noreply, S}.

%% @private
code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

%% @private
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------

add_monitor(Type, Pid) ->
    gen_server:call(?SERVER, {monitor_pid, Type, Pid}).
