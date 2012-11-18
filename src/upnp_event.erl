%% @doc handle events in upnp
%%
-module(upnp_event).

-export([start_link/0,
         add_handler/2,
         delete_handler/2]).

-export([notify/1]).

-define(SERVER, ?MODULE).

%% @doc Notify the event system of an event
%% <p>The system accepts any term as the event.</p>
%% @end
-spec notify(term()) -> ok.
notify(What) ->
    gen_event:notify(?SERVER, What).

%% @doc Add an event handler
%% @end
-spec add_handler(atom() | pid(), [term()]) -> ok.
add_handler(Handler, Args) ->
    gen_event:add_handler(?SERVER, Handler, Args).

%% @doc Delete an event handler
%% @end
-spec delete_handler(atom() | pid(), [term()]) -> ok.
delete_handler(Handler, Args) ->
    gen_event:delete_handler(?SERVER, Handler, Args).


%% ====================================================================

%% @doc Start the event handler
%% @end
start_link() ->
    gen_event:start_link({local, ?SERVER}).
