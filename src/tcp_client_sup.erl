%% LastChange: 2013-07-02 13:41:43
%% Copr. (c) 2013-2015, Simple <ljy0922@gmail.com>

-module(tcp_client_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local,?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{simple_one_for_one, 0, 1},
          [{tcp_client, {tcp_client, start_link,[]},
            temporary, brutal_kill, worker, [tcp_client]}]}}.
