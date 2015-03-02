%% Copyright (c) 2013-2015, Liangjingyang <simple.continue@gmail.com>

-module(tcp_acceptor).

-behaviour(gen_server).

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {sock, cb_mod, ref}).

start_link(LSock, CBMod) ->
    gen_server:start_link(?MODULE, [LSock, CBMod], []).

init([LSock, CBMod]) ->
    gen_server:cast(self(), accept),
    {ok, #state{sock=LSock, cb_mod = CBMod}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(accept, State) ->
    accept(State);

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({inet_async, LSock, Ref, {ok, Sock}}, 
    State = #state{sock=LSock, ref=Ref}) ->
    case set_sockopt(LSock, Sock) of
        ok -> 
	    start_client(State, Sock),
	    accept(State);
        Error -> 
	    {stop, Error, State}
    end;

handle_info({inet_async, LSock, Ref, {error, closed}}, State=#state{sock=LSock, ref=Ref}) ->
    {stop, normal, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    gen_tcp:close(State#state.sock),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-------------私有函数--------------

set_sockopt(LSock, Sock) ->
    true = inet_db:register_socket(Sock, inet_tcp),
    case prim_inet:getopts(LSock, [active, nodelay, keepalive, delay_send, priority, tos]) of
        {ok, Opts} ->
            case prim_inet:setopts(Sock, Opts) of
                ok -> 
		    ok;
                Error -> 
                    gen_tcp:close(Sock),
                    Error
            end;
        Error ->
            gen_tcp:close(Sock),
            Error
    end.

accept(#state{sock=LSock} = State) ->
    case prim_inet:async_accept(LSock, -1) of
        {ok, Ref} -> 
            {noreply, State#state{ref=Ref}};
        Error     -> 
            {stop, Error, State}
    end.

%% 开启客户端服务
start_client(#state{cb_mod = CBMod}, Socket) ->
    {ok, Child} = supervisor:start_child(tcp_client_sup, [CBMod]),
    ok = gen_tcp:controlling_process(Socket, Child),
    Child ! {start, Socket}.

