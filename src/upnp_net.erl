%% @author Edward Wang <yujiangw@gmail.com>
%% @doc Implements UPnP networking steps, namely, step 1 discovery,
%%      step 2 description, step 3 control and step 4 eventing.
%%
%%      Note that there's no discovery operation exported by this
%%      module, because discovery is initiated automatically when
%%      starting.
%%
%% @todo May be able to substitute all upnp_entity:method(Entity)
%%       call with Entity:method. spots that pattern in mochiweb
%% @end
-module(upnp_net).
-behaviour(gen_server).

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([start_link/1,
         description/2,
         add_port_mapping/4,
         subscribe/1,
         unsubscribe/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {ssdp_sock :: inet:socket(),
                ip,
                maps = []}).

-define(SERVER, ?MODULE).
-define(HTTP_SCHEME, "http://").


%% UPnP discovery multicast group
-define(SSDP_ADDR, "239.255.255.250").
-define(SSDP_PORT, 1900).


%%===================================================================
%% API
%%===================================================================
start_link(Specs) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Specs, []).



%% @doc Retrieves given UPnP device's detailed description.
%%
%%      This is step 2 in UPnP networking. According to UPnP Device
%%      Architecture 1.0 spec section 2.8, a UPnP control point sends
%%      an HTTP GET request to retrieve device description:
%%      ```
%%      GET path to description HTTP/1.1
%%      HOST: host for description:port for description
%%      ACCEPT-LANGUAGE: language preferred by control point'''
%%
%%      Note that a single UPnP physical device may include multiple
%%      logical devices. Quotes UPnP Device Architecture 1.0 spec:
%%      "Multiple logical devices can be modeled as
%%      a single root device with embedded devices (and services)
%%      or as multiple root devices (perhaps with no embedded devices).
%%      In the former case, there is one UPnP device description for
%%      the root device, and that device description contains a description
%%      for all embedded devices. In the latter case, there are multiple
%%      UPnP device descriptions, one for each root device."
%%
%%      After this step, more information about available UPnP services,
%%      namely, their description Url, control Url, and event subscription
%%      Url will be learned.
%%
%%      It is also worth metion that in this step, ``upnp_net''
%%      learns which local address certain UPnP device uses to communicate
%%      with it. This is important later on when  UPnP subsystem
%%      subcribes to UPnP eventing.
%% @end
description(Cat, Prop) ->
    case Cat of
        device ->
            gen_server:cast(?SERVER, {description, Prop});
        service ->
            %% Description step only makes sense for device.
            ignore
    end.


%% @doc Subscribe to given UPnP service.
%%
%%      4.1.1
%%      ```
%%      SUBSCRIBE publisher path HTTP/1.1
%%      HOST: publisher host:publisher port
%%      CALLBACK: <delivery URL>
%%      NT: upnp:event
%%      TIMEOUT: Second-requested subscription duration'''
%% @end
-spec subscribe(upnp_types:upnp_service()) -> {ok, string()} | {error, _Reason}.
subscribe(Service) ->
    PubSub = build_sub_url(Service),

    case PubSub of
        {error, Reason} ->
            {error, Reason};
        {PubUrl, SubUrl} ->
            case upnp_util:simple_request(subscribe, SubUrl,
                                          [{<<"TIMEOUT">>, <<"infinite">>},
                                           {<<"NT">>, <<"upnp:event">>},
                                           {<<"CALLBACK">>, PubUrl}]) of
                {ok, 200, Headers, _Body} ->
                    lager:debug("uPnP subscription done: ~s", [SubUrl]),
                    Sid = upnp_proto:parse_sub_resp(Headers),
                    case Sid of
                        undefined ->
                            upnp_event:notify({malformed_upnp_sub_resp, Headers}),
                            {error, <<"malformed upnp sub resp">>};
                        _ ->
                            {ok, Sid}
                    end;
                {ok, Status, _, Body} ->
                    upnp_event:notify({upnp_sub_error, {Status, Body}}),
                    {error, {Status, Body}};
                {error, Reason} ->
                    upnp_event:notify({upnp_sub_error, Reason}),
                    {error, Reason}
            end
    end.


%% @doc Unsubscribe from given UPnP service.
%%
%%      4.1.3
%%      ```
%%      UNSUBSCRIBE publisher path HTTP/1.1
%%      HOST: publisher host:publisher port
%%      SID: uuid:subscription UUID'''
%% @end
-spec unsubscribe(upnp_types:upnp_service()) -> ok.
unsubscribe(Service) ->
    {_PubUrl, SubUrl} = build_sub_url(Service),
    upnp_util:simple_request(unsubscribe, SubUrl,
                             [{<<"SID">>, "uuid:" ++
                               proplists:get_value(sid, Service)}]).

%% @doc Add a port mapping to given UPnP service.
%% @end
-spec add_port_mapping(string(), upnp_types:upnp_service(), tcp | udp,
                       integer()) ->
    ok | {failed, integer(), string()} | {error, _Reason}.
add_port_mapping(Name, Service, Proto, Port) ->
    Args = [{"NewRemoteHost", ""},
            {"NewExternalPort", integer_to_list(Port)},
            {"NewProtocol", string:to_upper(atom_to_list(Proto))},
            {"NewInternalPort", integer_to_list(Port)},
            {"NewInternalClient", proplists:get_value(local_addr, Service)},
            {"NewEnabled", "1"},
            {"NewPortMappingDescription", Name},
            {"NewLeaseDuration", "0"}],
    invoke_action(Service, "AddPortMapping", Args).


%%===================================================================
%% gen_server callbacks
%%===================================================================
init(Specs) ->
    Ip = upnp_util:parse_ip(proplists:get_value(ip, Specs, "0.0.0.0")),
    Maps = proplists:get_value(maps, Specs, []),
    lager:info("maps ~p~n", [Maps]),

    {ok, Sock} = gen_udp:open(0, [{active, true}, inet, binary]),
    {ok, #state{ssdp_sock = Sock, ip=Ip, maps=Maps}, 0}.


handle_call({invoke_action, Service, Action, Args}, _From, S) ->
    %% To invoke an action, a control point sends following messages:
    %% ```
    %% POST path of control URL HTTP/1.1
    %% HOST: host of control URL:port of control URL
    %% CONTENT-LENGTH: bytes in body
    %% CONTENT-TYPE: text/xml; charset="utf-8"
    %% SOAPACTION: "urn:schemas-upnp-org:service:serviceType:v#actionName"
    %%
    %% soap payload'''
    Type = proplists:get_value(type, Service),
    Ver = proplists:get_value(ver, Service),
    ActionUrl = build_ctl_url(Service),
    case ActionUrl of
        {error, Reason} ->
            {reply, {error, Reason}, S};
        _ ->
            ReqBody = upnp_proto:build_ctl_msg(Service, Action, Args),
            SoapAct = lists:append(["\"urn:schemas-upnp-org:service:",
                                    binary_to_list(Type),
                                    ":", binary_to_list(Ver),
                                    "#", Action, "\""]),

            case upnp_util:simple_request(post, ActionUrl,
                                          [{<<"SOAPACTION">>, SoapAct},
                                           {<<"CONTENT-TYPE">>,
                                            <<"text/xml; charset=\"utf-8\"">>}
                                          ], ReqBody) of
                {ok, 200, _, _} ->
                    {reply, ok, S};
                {ok, 405, _, _} ->
                    %% UPnP 1.0 spec indicates an invocation request may be rejected
                    %% with a response of "405 Method Not Allowed", then a control
                    %% point should retry the same request with HTTP M-POST method.
                    %%
                    %% Unfortunately Erlang httpc module doesn't support HTTP
                    %% extension method, yet; ignores it and doesn't retry.
                    lager:warning("uPnP action failed, 405, M-POST not supported"),
                    {reply, {failed, 405, "M-POST not supported"}, S};

                {ok, _, _, RespBody} ->
                    {ECode, EDesc} = upnp_proto:parse_ctl_err_resp(RespBody),
                    {reply, {failed, ECode, EDesc}, S};

                Error ->
                    {reply, Error, S}
            end
    end;
handle_call(_Request, _From, S) ->
    {reply, ok, S}.


handle_cast({discover, ST}, #state{ssdp_sock = Sock} = S) ->
    %% According to UPnP Device Architecture 1.0 spec section 1.2,
    %% a UPnP control point sends a UDP multicast request to
    %% discover UPnP devices and services.
    %%
    %% The request is a multicast variant of HTTP with method
    %% M-SEARCH:
    %% ```
    %% M-SEARCH * HTTP/1.1
    %% HOST: 239.255.255.250:1900
    %% MAN: "ssdp:discover"
    %% MX: seconds to delay response
    %% ST: search target'''
    %%
    %% Note that MAN header should be enclosed in double quotes,
    %% while ST header in single quotes.
    MSearch = [<<"M-SEARCH * HTTP/1.1\r\n"
                "HOST: 239.255.255.250:1900\r\n"
                "MAN: \"ssdp:discover\"\r\n"
                "ST: ">>, list_to_binary(ST), <<"\r\n"
                "MX: 3"
                "\r\n\r\n">>],
    ok = gen_udp:send(Sock, ?SSDP_ADDR, ?SSDP_PORT, iolist_to_binary(MSearch)),
    {noreply, S};
handle_cast({description, Device}, #state{ip=Ip, maps=Maps}=State) ->
    case recv_desc(Ip, Device) of
        {ok, Devices, Services} ->
            lager:debug("updating ~p on ip ~p", [Device, Ip]),
            [upnp_entity:update(device,  D, Maps) || D <- Devices],
            [upnp_entity:update(service, S, Maps) || S <- Services];
        Error ->
            lager:debug("receiving description from ~p on ip ~p error ~p", [Device, Ip, Error])
    end,
    {noreply, State}.


handle_info(timeout, State) ->
    discover_all(),
    {noreply, State};
handle_info({udp, _Socket, _IP, _Port, Packet}, #state{maps=Maps}=State) ->
    %% UPnP only uses UDP unicast/multicast in its discovery step,
    %% so this must be a UPnP device responds to our M-SEARCH request.
    %%
    %% @todo: UPnP device also announces its presence by sending to this
    %%        multicast group. Hanlde that.
    case upnp_proto:parse_msearch_resp(Packet) of
        {ok, device, D} ->
            upnp_entity:create(device, D, Maps);
        {ok, service, S} ->
            upnp_entity:create(service, S, Maps);
        {ok, uuid} ->
            ok;
        {error, Reason} ->
            %% malformed UPNP headers
            lager:debug("malformed UPNP header: ~p [~p]~n", [Reason, Packet]),
            ok
    end,
    {noreply, State};
handle_info(Info, State) ->
    lager:warning("Unknown handle_info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _S) ->
    ok.


code_change(_OldVer, S, _Extra) ->
    {ok, S}.


%%===================================================================
%% private
%%===================================================================

%% @doc Discovers all UPnP devices.
%%
%%      Equivalent to discover("ssdp:all").
%% @end
discover_all() ->
    discover("ssdp:all").


%% @doc Given search target ST, discovers that type of UPnP entity.
%%
%%      This is step 1 in UPnP networking.
%%
%%      Possible STs are:
%%        ```
%%        ssdp:all
%%        upnp:rootdevice
%%        uuid:device-UUID
%%        urn:schemas-upnp-org:device:deviceType:v
%%        urn:schemas-upnp-org:service:serviceType:v
%%        urn:domain-name:device:deviceType:v
%%        urn:domain-name:service:serviceType:v'''
%% @end
-spec discover(string()) -> ok.
discover(ST) ->
    gen_server:cast(?SERVER, {discover, ST}).


%% @doc Retrieve given UPnP device's detailed description and extract
%%      devices and services info from it.
%% @end
-spec recv_desc(any(), upnp_types:upnp_device()) ->
                       {ok, [upnp_types:upnp_device()],
                            [upnp_types:upnp_service()]}.
recv_desc(Ip, D) ->
    Url = binary_to_list(proplists:get_value(loc, D)),
    case upnp_util:simple_request(get, Url, [{<<"ACCEPT-LANGUAGE">>,
                                              <<"en">>}]) of
        {ok, _Status, _Headers, Body} ->
            case upnp_proto:parse_description(Ip, Body) of
                {ok, DS, SS} ->
                    {ok, DS, SS};
                {error, Reason} ->
                    upnp_event:notify({malformed_upnp_desc, Body}),
                    {error, Reason}
            end;
        {error, Reason} ->
            upnp_event:notify({upnp_desc_error, Reason}),
            {error, Reason}
    end.


%% @doc Invoke an action upon given UPnP service.
%%
%%      This is step 3 in UPnP networking. According to UPnP Device
%%      Architecture 1.0 specification section 3.2, "Control: Action",
%%      a UPnP control point can invoke actions on a UPnP device's
%%      services and receives results and errors back. All communications
%%      are sent as SOAP messages via HTTP.
%% @end
-spec invoke_action(upnp_types:upnp_service(), string(), [{string(), string()}]) ->
                    ok | {failed, integer(), string()} | {error, _Reason}.
invoke_action(Service, Action, Args) ->
    gen_server:call(?SERVER, {invoke_action, Service, Action, Args}, infinity).


%% Construct publisher and subscriber Url for given sercvice.
-spec build_sub_url(upnp_types:upnp_service()) -> {string(), string()}.
build_sub_url(Service) ->
    HostValue = proplists:get_value(loc, Service),

    case HostValue of
        undefined ->
            {error, no_host};
        _ ->
            PubHost = lists:append([proplists:get_value(local_addr, Service),
                                    ":", integer_to_list(upnp_handler:get_port())]),

            Host = decode_host(binary_to_list(HostValue)),
            EventPath = binary_to_list(proplists:get_value(event_path, Service)),
            %% enclosing <> is required by the spec.
            PubUrl = lists:append(["<http://", PubHost, "/callme>"]),
            SubUrl = lists:append(["http://", Host, EventPath]),
            {PubUrl, SubUrl}
    end.


%% Construct given service's full control url.
-spec build_ctl_url(proplists:proplist()) -> string().
build_ctl_url(Service) ->
    HostValue = proplists:get_value(loc, Service),
    case HostValue of
        undefined ->
            {error, no_host};
        _ ->
            Url = binary_to_list(HostValue),
            {Scheme, _UserInfo, _Host, _Port, _Path, _Query} =
                upnp_http_uri:parse(Url),
            _CtlUrl = lists:append([atom_to_list(Scheme), "://",
                                    decode_host(Url),
                                    binary_to_list(proplists:get_value(ctl_path, Service))])
    end.


% Steals this from upnp_http.
decode_host(URL) ->
    {_Scheme, _UserInfo, Host, Port, _Path, _Query} =
        upnp_http_uri:parse(URL),
    case Port of
        80 -> Host;
        N when is_integer(N) ->
            Host ++ ":" ++ integer_to_list(N)
    end.
