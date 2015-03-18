%% Copyright (c) 2013-2015, Liangjingyang <simple.continue@gmail.com>

-module(tcp_client).

-behaviour(gen_server).

-export([start_link/1]).
-export([
         init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2, 
         terminate/2, 
         code_change/3
        ]).
-export([
         i/1,
         tcp_client_name/1,
         get_data_size/0,
         get_package_num/0
        ]).

-record(state, {
          socket,
          heartbeat_timer = undefined,
          last_heartbeat = 0,
          cb_mod,
          cb_state 
         }).

start_link(CBMod) ->
    gen_server:start_link(?MODULE, [CBMod], []).

i(PlayerId) ->
    ProcName = tcp_client_name(PlayerId),
    Format = gen_server:call(ProcName, {show_i}),
    io:format(Format, []). 

init([CBMod]) ->
    erlang:process_flag(trap_exit, true),
    {ok, #state{cb_mod = CBMod}}.


%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_call(Request, From, State) ->
    Reply = do_handle_call(Request, From, State),
    {reply, Reply, State}.

do_handle_call({show_i}, _From, State) ->
    io_lib:format("~p~n ", [State]);
do_handle_call(_Request, _From, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_cast(_Request, _State) ->
    {noreply, _State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_info({inet_async, Socket, _, {ok, Binary}}, #state{cb_mod = CBMod, cb_state = CBState} = State) ->
    add_package_num(),
    recv_data_size(Binary),
    Data = CBMod:decode(Binary),
    CBState2 = CBMod:router(CBState, Data),
    async_recv(Socket, 0, -1),
    {noreply, State#state{last_heartbeat = nowsec(), cb_state = CBState2}};
handle_info({inet_async, Socket, _, {error,timeout}}, State) ->
    async_recv(Socket, 0, -1),
    {noreply, State};
handle_info({inet_async, Socket, _, {error,closed}}, State) ->
    {stop, {error, closed, inet:peername(Socket)}, State};
handle_info({send, Data}, #state{cb_mod = CBMod} = State) ->
    Binary = CBMod:encode(Data),
    send_data_size(Binary),
    gen_tcp:send(State#state.socket, Binary),
    {noreply, State};
handle_info({start, Socket}, State) ->
    async_recv(Socket, 0, -1),
    CBState = (State#state.cb_mod):start(Socket),
    Timer = erlang:send_after(40 * 1000, self(), {timer, check_heartbeat}),
    erlang:send_after(1000, self(), {timer, check_hack}),
    erase_package_num(),
    State2 = State#state{socket = Socket, cb_state = CBState, last_heartbeat = nowsec(), heartbeat_timer = Timer},
    {noreply, State2};
handle_info({timer, check_heartbeat}, State) ->
    Timer = erlang:send_after(120 * 1000, self(), {timer, check_heartbeat}),
    Last = State#state.last_heartbeat,
    Now = nowsec(),
    case Now - Last > 240 of
        true ->
            login_lost(no_heartbeat);
        false ->
            ignore
    end,
    {noreply, State#state{heartbeat_timer = Timer}};
handle_info({timer, check_hack}, State) ->
    erlang:send_after(1 * 1000, self(), {timer, check_hack}),
    Num = get_package_num(),
    case Num > 30 of
        true ->
            login_lost({hack, Num});
        false ->
            erase_package_num()
    end,
    {noreply, State};
handle_info({'DOWN', _MonitorRef, _Type, Pid, _Info}, State) ->
    {stop, {'DOWN', Pid}, State};
handle_info({'EXIT', From, Reason}, State) ->
    {stop, {'EXIT', From, Reason}, State};
handle_info({stop, Reason}, State) ->
    {stop, {stop, Reason}, State};
handle_info({cb, Request}, #state{cb_mod = CBMod, cb_state = CBState}) ->
    CBState2 = CBMod:handle_info(Request, CBState),
    {noreply, CBState2};
handle_info(_Request, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, #state{cb_mod = CBMod, cb_state = CBState}) ->
    CBMod:terminate(Reason, CBState),
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_oldvsn, State, _extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%% Internal 
%% --------------------------------------------------------------------

async_recv(Sock, Length, Timeout) when is_port(Sock) ->
    case catch prim_inet:async_recv(Sock, Length, Timeout) of
        {ok, Res} -> 
            Res; 
        {error, Reason} -> 
            erlang:throw(Reason);
        Res -> 
            erlang:throw(Res)
    end.

login_lost(Reason) ->
    self() ! {stop, Reason}.

tcp_client_name(PlayerId) ->
    list_to_atom(lists:concat(["tcp_client_", PlayerId])).


send_data_size(Data) ->
    Size = byte_size(Data),
    case erlang:get(send_data_size) of
        undefined ->
            erlang:put(send_data_size, Size);
        OldSize ->
            erlang:put(send_data_size, Size + OldSize)
    end.

recv_data_size(Data) ->
    Size = byte_size(Data),
    case erlang:get(recv_data_size) of
        undefined ->
            erlang:put(recv_data_size, Size);
        OldSize ->
            erlang:put(recv_data_size, Size + OldSize)
    end.

get_data_size() ->
    {erlang:get(recv_data_size), erlang:get(send_data_size)}.

add_package_num() ->
    case erlang:get(package_num) of
        undefined ->
            erlang:put(package_num, 1);
        Num ->
            erlang:put(package_num, Num + 1)
    end.

erase_package_num() ->
    erlang:put(package_num, 0).

get_package_num() ->
    case erlang:get(package_num) of
        Num when is_integer(Num) ->
            Num;
        _ ->
            0
    end.

nowsec() ->
    {A, B, _} = erlang:now(),
    A * 1000000 + B.
