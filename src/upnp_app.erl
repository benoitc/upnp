-module(upnp_app).

-behaviour(application).

-export([start/0]).

%% Application callbacks
-export([start/2, stop/1]).

start() ->
    start_app_deps(upnp),
    application:start(upnp).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    upnp_sup:start_link().

stop(_State) ->
    ok.

%% @spec start_app_deps(App :: atom()) -> ok
%% @doc Start depedent applications of App.
start_app_deps(App) ->
    {ok, DepApps} = application:get_key(App, applications),
    [ensure_started(A) || A <- DepApps],
    ok.

%% @spec ensure_started(Application :: atom()) -> ok
%% @doc Start the named application if not already started.
ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.
