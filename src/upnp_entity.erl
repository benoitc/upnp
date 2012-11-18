%% @author Edward Wang <yujiangw@gmail.com>
%% @doc Represents a UPnP device or service.
%% @end

-module(upnp_entity).
-behaviour(gen_server).


-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([start_link/3,
         id/2,
         create/3,
         update/3,
         unsubscribe/2,
         notify/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


-record(state, {cat     :: 'device' | 'service',
                prop    :: upnp_types:upnp_device() |
                           upnp_types:upnp_service(),
                maps    :: any()}).

-define(SERVER, ?MODULE).
-define(UPNP_RD_NAME, <<"rootdevice">>). %% Literal name of UPnP root device

%%===================================================================
%% API
%%===================================================================
start_link(Cat, Prop, Maps) ->
    Args = [{cat, Cat}, {prop, Prop}, {maps, Maps}],
    gen_server:start_link(?MODULE, Args, []).


id(Cat, Prop) ->
    Type = proplists:get_value(type, Prop),
    UUID = proplists:get_value(uuid, Prop),
    _Id = erlang:phash2({Cat, Type, UUID}).


create(Cat, Proplist, Maps) ->
    upnp_handler_sup:add_upnp_entity(Cat, Proplist, Maps).


update(Cat, Prop, Maps) ->
    case lookup_pid(Cat, Prop) of
        {ok, Pid} ->
            gen_server:cast(Pid, {update, Cat, Prop, Maps});
        {error, not_found} ->
            create(Cat, Prop, Maps)
    end.


unsubscribe(Cat, Prop) ->
    case lookup_pid(Cat, Prop) of
        {ok, Pid} ->
            gen_server:cast(Pid, {unsubscribe, Cat, Prop});
        {error, not_found} ->
            do_unsubscribe(Cat, Prop)
    end.


%% @todo See explanation in ``upnp_httpd''.
-spec notify(upnp_types:upnp_notify()) -> ok.
notify(_Content) ->
    ok.

%%===================================================================
%% gen_server callbacks
%%===================================================================
init(Args) ->
    lager:info("got ~n~p", [Args]),
    %% We trap exits to unsubscribe from UPnP service.
    process_flag(trap_exit, true),
    register_self(Args),
    {ok, #state{cat     = proplists:get_value(cat, Args),
                prop    = proplists:get_value(prop, Args)}, 0}.


handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({update, Cat, NewProp, NewMaps}, #state{prop = Prop} = State) ->
    Merged = upnp_util:merge_proplists(Prop, NewProp),

    case can_do_port_mapping(Cat, Merged) of
        true ->
            lists:foreach(fun
                    ({Name, Type, Fun}) when is_function(Fun) ->
                        Port = Fun(),
                        add_port_mapping(self(), Name, Type, Port);
                    ({Name, Type, {Fun, Args}}) when is_function(Fun) ->
                        Port = Fun(Args),
                        add_port_mapping(self(), Name, Type, Port);
                    ({Name, Type, Port}) ->
                        add_port_mapping(self(), Name, Type, Port)
                end, NewMaps);
        _ ->
            ignore
    end,
    case can_subscribe(Cat, Merged) of
        true ->
            subscribe(self());
        _ ->
            ignore
    end,
    upnp_table:update_upnp_entity(self(), Cat, Merged),
    {noreply, State#state{prop = Merged, maps=NewMaps}};
handle_cast({unsubscribe, Cat, Prop}, State) ->
    NewProp = do_unsubscribe(Cat, Prop),
    upnp_table:update_upnp_entity(self(), Cat, NewProp),
    {noreply, State#state{prop = NewProp}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
    %% If root device, retrieves its description.
    #state{cat = Cat, prop = Prop} = State,
    case is_root_device(Cat, Prop) of
        true ->
            upnp_net:description(Cat, Prop);
        _ -> ok
    end,
    {noreply, State};
handle_info({add_port_mapping, Name, Proto, Port}, State) ->
    #state{prop = Prop} = State,
    case upnp_net:add_port_mapping(Name, Prop, Proto, Port) of
        ok ->
            lager:info("Port mapping added: ~p (~p)", [Proto, Port]),
            upnp_event:notify({port_mapping_added, Proto, Port});
        {failed, ErrCode, ErrDesc} ->
            lager:info("Port mpping failed: ~p (~p) error(~B): ~p",
                       [Proto, Port, ErrCode, ErrDesc]),
            upnp_event:notify({add_port_mapping_failed,
                                   Proto,
                                   Port,
                                   ErrCode,
                                   ErrDesc});
        _ -> ignore
    end,
    {noreply, State};
handle_info(subscribe, State) ->
    #state{cat = Cat, prop = Prop} = State,
    NewProp = case is_subscribed(Prop) of
        false ->
            case upnp_net:subscribe(Prop) of
                {ok, Sid} ->
                    upnp_util:merge_proplists(Prop, [{sid, Sid}]);
                {error, _} -> Prop
            end;
        true -> Prop
    end,
    upnp_table:update_upnp_entity(self(), Cat, NewProp),
    {noreply, State#state{prop = NewProp}};
handle_info(Info, State) ->
    lager:error("Unknown handle_info event ~p", [Info]),
    {noreply, State}.


terminate(_Reason, State) ->
    #state{cat = Cat, prop = Prop} = State,
    do_unsubscribe(Cat, Prop).


code_change(_OldVer, S, _Extra) ->
    {ok, S}.


%%===================================================================
%% private
%%===================================================================

register_self(Args) ->
    Cat = proplists:get_value(cat, Args),
    Prop = proplists:get_value(prop, Args),
    upnp_table:register_upnp_entity(self(), Cat, Prop).

lookup_pid(Cat, Prop) ->
    upnp_table:lookup_upnp_entity(Cat, Prop).


is_root_device(Cat, Prop) ->
    Type = proplists:get_value(type, Prop),
    Cat =:= device andalso Type =:= ?UPNP_RD_NAME.


can_do_port_mapping(Cat, Prop) ->
    Type = proplists:get_value(type, Prop),
    CtlPath = proplists:get_value(ctl_path, Prop),
    Cat =:= service
        andalso CtlPath =/= undefined
        andalso (Type =:= <<"WANIPConnection">>
                 orelse Type =:= <<"WANPPPConnection">>).


add_port_mapping(Pid, Name, Proto, Port) ->
    Pid ! {add_port_mapping, Name, Proto, Port}.


can_subscribe(Cat, Prop) ->
    EventPath = proplists:get_value(event_path, Prop),
    Cat =:= service andalso EventPath =/= undefined.

subscribe(Pid) ->
    Pid ! subscribe.

is_subscribed(Prop) ->
    Sid = proplists:get_value(sid, Prop),
    Sid =/= undefined.

-spec do_unsubscribe(device | service,
                     upnp_types:upnp_device() | upnp_types:upnp_service()) ->
                    upnp_types:upnp_device() | upnp_types:upnp_service().
do_unsubscribe(Cat, Prop) ->
    NewProp = case Cat of
        service ->
            Sid = proplists:get_value(sid, Prop),
            case Sid of
                undefined -> Prop;
                _ ->
                    upnp_net:unsubscribe(Prop),
                    proplists:delete(sid, Prop)
            end;
        _ -> Prop
    end,
    NewProp.

