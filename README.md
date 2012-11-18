# upnp support

Application that provides a way to map a local port to the external
using UPnP IGD.

Code extracted from
[etorrent](http://github.com/jlouis/etorrent_core.git) . 


Example of usage:


    1> ssl:start().
    2> application:start(hackney).
    3> application:start(ranch).  
    4> application:start(cowboy).
    5> lager:start().            
    6> application:start(upnp).  
    7> upnp:start(test, [{ip, "0.0.0.0"}, {maps, [{"example.com", tcp, 8000}]}]).


This will map the port 8000 on the external IP to your application
