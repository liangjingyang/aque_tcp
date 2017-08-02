%% Copyright (c) 2013-2015, Liangjingyang <simple.continue@gmail.com>

-module(aque_tcp).

-export([
        start/0,
        start/2,
        stop/1
    ]).
-export([
	start_listener/3,
	start_listener/4
    ]).

-define(APP, ?MODULE).

start() ->
    application:start(?APP).

start(_Type, _Args) ->
    aque_tcp_sup:start_link().

start_listener(AcceptorNum, Port, TcpOptions) ->
    start_listener(AcceptorNum, Port, TcpOptions, tcp_client_callback).

start_listener(AcceptorNum, Port, TcpOptions, CBMod) ->
    supervisor:start_child(tcp_listener_sup, 
	{
	    tcp_listener:name(Port),
	    {tcp_listener, start_link, [AcceptorNum, Port, TcpOptions, CBMod]},
	    transient,
	    100,
	    worker,
	    [tcp_listener]
	}).

stop(_State) ->
    ok.
    
