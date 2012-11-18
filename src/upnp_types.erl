-module(upnp_types).

-export_type([
        ipaddr/0,
        portnum/0,
        upnp_device/0,
        upnp_notify/0,
        upnp_service/0]).



% Types used by the DHT subsystem
-type ipaddr() :: {byte(), byte(), byte(), byte()}.
-type portnum() :: 1..16#FFFF.

%% Types used by UPnP subsystem
-type upnp_device() :: proplists:proplist().
-type upnp_service() :: proplists:proplist().
-type upnp_notify() :: proplists:proplist().
