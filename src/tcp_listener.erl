%% Copyright (c) 2013-2015, Liangjingyang <simple.continue@gmail.com>

-module(tcp_listener).

-behaviour(gen_server).

-export([start_link/4]).
-export([
	init/1, 
	handle_call/3, 
	handle_cast/2, 
	handle_info/2,
	terminate/2, 
	code_change/3
    ]).
-export([name/1]).

start_link(AcceptorCount, Port, TcpOptions, CBMod) ->
    gen_server:start_link(?MODULE, [AcceptorCount, Port, TcpOptions, CBMod], []).

name(Port) ->
    list_to_atom(lists:concat(["tcp_listener_", Port])).

init([AcceptorCount, Port, TcpOptions, CBMod]) ->
    process_flag(trap_exit, true),
    case gen_tcp:listen(Port, TcpOptions) of
	{ok, LSock} ->
	    lists:foreach(fun (_) ->
			{ok, _APid} = supervisor:start_child(tcp_acceptor_sup, [LSock, CBMod])
		end,
		lists:duplicate(AcceptorCount, dummy)),
	    {ok, LSock};
	Error ->
	    erlang:throw(Error)
    end.

handle_call(_Request, _From, State) ->
    {reply, State, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    gen_tcp:close(State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
