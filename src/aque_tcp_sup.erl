%% Copyright (c) 2013-2015, Liangjingyang <simple.continue@gmail.com>

-module(aque_tcp_sup).

-export([
        start_link/0, 
        init/1
    ]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok,
        {{one_for_one, 10, 10},
            [
                {
                    tcp_client_sup,
                    {tcp_client_sup, start_link, []},
                    transient,
                    infinity,
                    supervisor,
                    [tcp_client_sup]
                },
                {
                    tcp_acceptor_sup,
                    {tcp_acceptor_sup, start_link, []},
                    transient,
                    infinity,
                    supervisor,
                    [tcp_acceptor_sup]
                },
                {
                    tcp_listener_sup,
                    {tcp_listener_sup, start_link, []},
                    transient,
                    infinity,
                    supervisor,
                    [tcp_listener_sup]
                }
            ]
        }
    }.


