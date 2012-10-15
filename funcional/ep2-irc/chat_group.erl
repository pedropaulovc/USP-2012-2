%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---

-module(chat_group).
-import(lib_chan_mm, [send/2, controller/2]).
-import(lists, [foreach/2, reverse/2]).

-export([start/3]).

start(C, Nick, Server) ->
    process_flag(trap_exit, true),
    controller(C, self()),
    send(C, ack),
    self() ! {chan, C, {relay, Nick, "I'm starting the group"}},
    group_controller([{C,Nick}], Server).



delete(Pid, [{Pid,Nick}|T], L) -> {Nick, reverse(T, L)};
delete(Pid, [H|T], L)          -> delete(Pid, T, [H|L]);
delete(_, [], L)               -> {"????", L}.

send_private(_C, _From, _To, _Str, []) -> not_found;
send_private(C, From, To, Str, [{Pid, To}|_]) -> send(Pid, {private_msg, From, C, Str});
send_private(C, From, To, Str, [_|T]) -> send_private(C, From, To, Str, T).


group_controller([], _Server) ->
    exit(allGone);


group_controller(L, Server) ->
    receive
	{chan, C, {relay, Nick, Str}} ->
	    foreach(fun({Pid,_}) -> send(Pid, {msg,Nick,C,Str}) end, L),
	    group_controller(L, Server);
	{chan, C, {private, From, To, Str}} ->
		send_private(C, From, To, Str, L),
		group_controller(L, Server);
	{login, C, Nick} ->
	    Users = [{C,Nick}|L],
	    controller(C, self()),
	    send(C, ack),
	    self() ! {chan, C, {relay, Nick, "I'm joining the group"}},
	    group_controller(Users, Server);
	{chan_closed, C} ->
	    {Nick, L1} = delete(C, L, []),
	    self() ! {chan, C, {relay, Nick, "I'm leaving the group"}},
        if
	        L1 =/= [] -> Server ! {logout, self(), Nick};
	        true -> ok
	    end,
	    group_controller(L1, Server);
	{update_users, Groups} ->
	    foreach(fun({Pid,_}) -> send(Pid, {update_users, Groups}) end, L),
	    group_controller(L, Server);
	Any ->
	    io:format("group controller received Msg=~p~n", [Any]),
	    group_controller(L, Server)
    end.

