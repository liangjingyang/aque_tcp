%% Copyright (c) 2013-2015, Liangjingyang <simple.continue@gmail.com>

%% tcp_client进程调用

-module(tcp_client_behaviour).

-callback start(Socket::port()) -> CBState::record().

-callback terminate(Reason::term(), CBState::record()) -> ok.

-callback router(CBState::record(), Data::term()) -> CBState2::record().

-callback decode(Any::any()) -> Term::term().

-callback encode(Term::term()) -> Any::any().


