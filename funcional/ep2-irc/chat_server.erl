%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---

-module(chat_server).
-import(lib_chan_mm, [send/2, controller/2]).
-import(lists, [delete/2, foreach/2, map/2, member/2,reverse/2]).

-compile(export_all).


start() ->
    start_server(),
    lib_chan:start_server("chat.conf").

start_server() ->
    register(chat_server, 
	     spawn(fun() ->
			   process_flag(trap_exit, true),
			   Val= (catch server_loop([])),
			   io:format("Server terminated with:~p~n",[Val])
		   end)).



server_loop(L) ->
    receive
	{mm, Channel, {login, Group, Nick}} ->
	    L1 = case lookup(Group, L) of
		{ok, Pid} ->
		    Pid ! {login, Channel, Nick},
		    update_users_list(Nick, Group, Pid, L);
		error ->
		    S = self(),
		    Pid = spawn_link(fun() ->
					     chat_group:start(Channel, Nick, S) 
				     end),
		    [{Group,Pid, [Nick]}|L]
	    end,
	    notify_updated_users(L1),
	    server_loop(L1);
	{mm_closed, _} ->
	    server_loop(L); 
	{logout, Pid, User} ->
	    L1 = delete_user_list(User, Pid, L),
	    notify_updated_users(L1),
	    server_loop(L1);
	{'EXIT', Pid, allGone} ->
	    L1 = remove_group(Pid, L),
	    notify_updated_users(L1),
	    server_loop(L1);
	Msg ->
	    io:format("Server received Msg=~p~n",
		      [Msg]),
	    server_loop(L)
    end. 

get_groups_members(L) -> lists:map(fun({Group, _Pid, Users}) -> {Group, Users} end, L).

notify_updated_users(L) -> 
    Groups = get_groups_members(L),
    lists:foreach(fun({_Group, Pid, _Users}) -> Pid ! {update_users, Groups} end, L).

lookup(G, [{G, Pid, _Users}|_]) -> {ok, Pid};
lookup(G, [_|T])       -> lookup(G, T);
lookup(_,[])           -> error.

update_users_list(U, G, P, [{G, P, Users}|T]) -> [{G, P, [U | Users]} | T];
update_users_list(U, G, P, [H|T]) -> [H | update_users_list(U, G, P, T)];
update_users_list(_U, _G, _P, []) -> [].

delete_user_list(U, P, [{G, P, Users}|T]) -> [{G, P, lists:delete(U, Users)} | T];
delete_user_list(U, P, [H|T]) -> [H | delete_user_list(U, P, T)];
delete_user_list(_U, _P, []) -> [].

remove_group(Pid, [{G,Pid, _Users}|T]) -> io:format("~p removed~n",[G]), T;
remove_group(Pid, [H|T])       -> [H|remove_group(Pid, T)];
remove_group(_, [])            -> [].

