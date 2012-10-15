%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Alunos: 
%         Alessandro Calò          - 4325393
%         Pedro Paulo Vezzá Campos - 7538743
%
% MAC0319-2012 - Programação Funcional Contemporânea - EP2: Melhorias no IRC
% Lite do Armstrong
%
% Sobre o arquivo: Contém o código do modelo lógico de um cliente do IRC Lite
% modificado para atender ao enunciado proposto
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(chat_client).

-import(io_widget, 
	[get_state/1, insert_str/2, set_prompt/2, set_state/2, 
	 set_title/2, set_handler/2, update_state/3, update_users/2, update_groups/2,
	 whole_group/0]).

-export([start/0, test/0, connect/5, connect_local/2]).


connect_local(User, Group) ->
    connect("localhost", 2223, "AsDT67aQ", Group, User).

start() -> 
    connect("localhost", 2223, "AsDT67aQ", "general", "joe").


test() ->
    connect("localhost", 2223, "AsDT67aQ", "general", "joe"),
    connect("localhost", 2223, "AsDT67aQ", "general", "jane"),
    connect("localhost", 2223, "AsDT67aQ", "general", "jim"),
    connect("localhost", 2223, "AsDT67aQ", "general", "sue"),
	connect("localhost", 2223, "AsDT67aQ", "alternative", "jay"),
	connect("localhost", 2223, "AsDT67aQ", "alternative", "kay"),
	connect("localhost", 2223, "AsDT67aQ", "fightclub", "tylerdurden").

%%%	só pra testar, agora tem 3 grupos.
	   

connect(Host, Port, HostPsw, Group, Nick) ->
    spawn(fun() -> handler(Host, Port, HostPsw, Group, Nick) end).
				 
handler(Host, Port, HostPsw, Group, Nick) ->
    process_flag(trap_exit, true),
    Widget = io_widget:start(self()),
%a lista de grupos aparece numa janela separada, chamada GroupWindow
	GroupWindow = io_widget:start_group_window(self()),
    set_title(Widget, Nick),
    set_state(Widget, Nick),
    set_prompt(Widget, [Nick, " > "]),
    set_handler(Widget, fun parse_command/1),
    start_connector(Host, Port, HostPsw),    
% a funcao disconnected tem um parâmetro a mais (o primeiro) pra lidar com o fechamento da janela de grupos
    disconnected(GroupWindow, Widget, Group, Nick).



disconnected(GroupWindow, Widget, Group, Nick) ->
    receive
	{connected, MM} ->
	    insert_str(Widget, "connected to server\nsending data\n"),
	    lib_chan_mm:send(MM, {login, Group, Nick}),
	    wait_login_response(GroupWindow, Widget, MM, Group);
	{Widget, destroyed} ->
	    exit(died);
	{status, S} ->
	    insert_str(Widget, to_str(S)),
	    disconnected(GroupWindow, Widget, Group, Nick);
	Other ->
	    io:format("chat_client disconnected unexpected:~p~n",[Other]),
	    disconnected(GroupWindow, Widget, Group, Nick)
    end.



wait_login_response(GroupWindow, Widget, MM, Group) ->
    receive
	{chan, MM, ack} ->
	    active(GroupWindow, Widget, MM, Group);
	Other ->
	    io:format("chat_client login unexpected:~p~n",[Other]),
	    wait_login_response(GroupWindow, Widget, MM, Group)
    end. 



active(GroupWindow, Widget, MM, Group) ->
     WholeGroup = whole_group(),
     receive
	 {Widget, From, WholeGroup, Str} ->
	     lib_chan_mm:send(MM, {relay, From, Str}),
	     active(GroupWindow, Widget, MM, Group);
	 {Widget, From, To, Str} ->
	     lib_chan_mm:send(MM, {private, From, To, Str}),
	     active(GroupWindow, Widget, MM, Group);
	 {chan, MM, {update_users, Groups}} ->
	     Users = get_users(Group, Groups),
	     update_users(Widget, Users),
	     update_groups(GroupWindow, Groups),
	     active(GroupWindow, Widget, MM, Group);
	 {chan, MM, {msg, From, Pid, Str}} ->
	     insert_str(Widget, [From,"@",pid_to_list(Pid)," ", Str, "\n"]),
	     active(GroupWindow, Widget, MM, Group);
	 {chan, MM, {private_msg, From, Pid, Str}} ->
	     insert_str(Widget, [From,"@",pid_to_list(Pid)," ", "*private* ", Str, "\n"]),
	     active(GroupWindow, Widget, MM, Group);
	 {'EXIT',Widget,windowDestroyed} ->
	     lib_chan_mm:close(MM),
	     exit(GroupWindow, kill);
	 {'EXIT',GroupWindow,windowDestroyed} ->
	     lib_chan_mm:close(MM),
	     exit(Widget, kill);
	 {close, MM} ->
	     exit(serverDied);
	 Other ->
	     io:format("chat_client active unexpected:~p~n",[Other]),
	     active(GroupWindow, Widget, MM, Group)
     end. 

get_users(Group, [{Group, Users}|_]) -> Users;
get_users(Group, [_|T]) -> get_users(Group, T);
get_users(_Group, []) -> [].


start_connector(Host, Port, Pwd) ->
    S = self(),
    spawn_link(fun() -> try_to_connect(S, Host, Port, Pwd) end).
    
try_to_connect(Parent, Host, Port, Pwd) ->
    %% Parent is the Pid of the process that spawned this process
    case lib_chan:connect(Host, Port, chat, Pwd, []) of
	{error, _Why} ->
	    Parent ! {status, {cannot, connect, Host, Port}},
	    sleep(2000),
	    try_to_connect(Parent, Host, Port, Pwd);
	{ok, MM} ->
	    lib_chan_mm:controller(MM, Parent),
	    Parent ! {connected, MM},
	    exit(connectorFinished)
    end.


sleep(T) ->
    receive
    after T -> true
    end.
	    
to_str(Term) ->
    io_lib:format("~p~n",[Term]).

parse_command(Str) -> skip_to_gt(Str).

skip_to_gt(">" ++ T) -> T;
skip_to_gt([_|T])    -> skip_to_gt(T);
skip_to_gt([])       -> exit("no >").
