%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material, 
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose. 
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%%---
-module(io_widget).

-export([get_state/1,
	 start/1, test/0, 
	 set_handler/2, 
	 set_prompt/2,
	 set_state/2,
	 set_title/2,
	 insert_str/2,
	 update_state/3,
	 update_users/2,
	 update_groups/2,
	 start_group_window/1,
	 whole_group/0]).

start(Pid) ->
    gs:start(),
    spawn_link(fun() -> widget(Pid) end).


% analogo ao metodo de cima (start/1)
start_group_window(Pid) ->
	gs:start(),
	spawn_link(fun() -> group_window(Pid) end).

get_state(Pid)          -> rpc(Pid, get_state).
set_title(Pid, Str)     -> Pid ! {title, Str}.
set_handler(Pid, Fun)   -> Pid ! {handler, Fun}.
set_prompt(Pid, Str)    -> Pid ! {prompt, Str}.
set_state(Pid, State)   -> Pid ! {state, State}.
insert_str(Pid, Str)    -> Pid ! {insert, Str}.
update_users(Pid, Users)-> Pid ! {update_users, Users}.
%%% metodo que manda a mensagem pro chat_client
update_groups(Pid, Group_List) -> Pid ! {update_list, Group_List}.
update_state(Pid, N, X) -> Pid ! {updateState, N, X}. 

rpc(Pid, Q) ->    
    Pid ! {self(), Q},
    receive
	{Pid, R} ->
	    R
    end.

%%% criacao da janela dos grupos. No final, ele tem o seu proprio loop (group_loop/3)
group_window(Pid) ->
	Size_Groups = [{width,300},{height,200}],

    Win_Groups = gs:window (gs:start(), [{map, true}, {configure,true},{title,"Groups window"}|Size_Groups]),

	gs:frame(packer, Win_Groups,[{packer_x, [{stretch,1,200}, {stretch,1,200}]},
			  	{packer_y, [{stretch,1,200}]}]),
    

	gs:create(listbox, groups,packer, [{pack_x,1},{pack_y,1},{vscroll,right},
              {hscroll,false}, {click,true}]),
    gs:create(listbox, users, packer, [{pack_x,2},{pack_y,1},{vscroll,right},
              {hscroll,false}]),
    gs:config(packer, Size_Groups),

	group_loop (Win_Groups, Pid, nil, []).
	


widget(Pid) ->
    Size = [{width,600},{height,200}],
	

    Win = gs:window(gs:start(),
		    [{map,true},{configure,true},{title,"window"}|Size]),

	
	
	gs:frame(packer, Win,[{packer_x, [{stretch,1,400}, {stretch,1,200}]},
			  	{packer_y, [{stretch,10,100,120}, {stretch,1,15,15}]}]),
    

	gs:create(editor, editor,packer, [{pack_x,1},{pack_y,1},{vscroll,right}]),
    gs:create(entry,  entry, packer, [{pack_x,{1,2}},{pack_y,2},{keypress,true}]),
    gs:create(listbox, group, packer, [{pack_x,2},{pack_y,1},{vscroll,right},
              {hscroll,false}, {add, whole_group()}, {selection, 0}]),
    gs:config(packer, Size),
    Prompt = " > ",
    State = nil,
    gs:config(entry, {insert,{0,Prompt}}),
    loop(Win, Pid, Prompt, State, fun parse/1). 



%%% o loop da janela de grupos, com alguns receives que achei relevante. Talvez alguns deles sejam inuteis, mas deixei ai em todo caso.
group_loop (Win, Pid, State, Group_List) ->

	receive
		{From, get_state} ->
			From ! {self(), State},
			group_loop(Win, Pid, State, Group_List);
		{state, S} ->
			group_loop(Win, Pid, S, Group_List);
		{title, Str} ->
			gs:config(Win, [{title, Str}]),
			group_loop(Win, Pid, State, Group_List);
		%%%% aqui eh onde ele (em teoria) consulta a lista e re-imprime os nomes na janela.
		{update_list, Group_List1} ->
			gs:config(groups, clear),
			gs:config(groups, {items, get_groups(Group_List1)}),
			gs:config(groups, {selection, 0}),
			gs:config(users, clear),
			[{_, UsersH}|_] = Group_List1,
			gs:config(users, {items, UsersH}),
			group_loop(Win, Pid, State, Group_List1);
		{updateState, N, X} ->
			io:format("setelemtn N=~p X=~p State=~p~n",[N,X,State]),
			State1 = setelement(N, State, X),
			group_loop(Win, Pid, State1, Group_List);
		{gs,_,destroy,_,_} ->
			io:format("Destroyed~n",[]),
			exit(windowDestroyed);
		{gs,_,configure,[],[W,H,_,_]} ->
			gs:config(packer, [{width,W},{height,H}]),
			group_loop(Win, Pid, State, Group_List);
        {gs,groups,click,[],[_,GroupName,_|_]} ->
            gs:config(users, clear),
            gs:config(users, {items, get_users_group(GroupName, Group_List)}),
            group_loop(Win, Pid, State, Group_List);
		Any ->
			io:format("Group Loop Discarded:~p~n",[Any]),
			group_loop(Win, Pid, State, Group_List)
	end.

get_groups(GroupsUsers) -> lists:map(fun({Group, _Users}) -> Group end, GroupsUsers).

get_users_group(Group, [{Group, Users}|_T]) -> Users;
get_users_group(Group, [_|T]) -> get_users_group(Group, T);
get_users_group(_Group, _) -> [].

loop(Win, Pid, Prompt, State, Parse) ->   
    receive
	{From, get_state} ->
	    From ! {self(), State},
	    loop(Win, Pid, Prompt, State, Parse);
	{handler, Fun} ->
	    loop(Win, Pid, Prompt, State, Fun);
	{prompt, Str} ->
	    %% this clobbers the line being input ...
	    %% this could be fixed - hint
	    gs:config(entry, {delete,{0,last}}),
	    gs:config(entry, {insert,{0,Str}}),
	    loop(Win, Pid, Str, State, Parse);
	{state, S} ->
	    loop(Win, Pid, Prompt, S, Parse);
	{title, Str} ->
	    gs:config(Win, [{title, Str}]),
	    loop(Win, Pid, Prompt, State, Parse);
	{insert, Str} ->
	    gs:config(editor, {insert,{'end',Str}}),
	    scroll_to_show_last_line(),
	    loop(Win, Pid, Prompt, State, Parse);
	{update_users, Users} ->
	    gs:config(group, clear),
	    gs:config(group, {items, add_prefix("To ", [whole_group() | Users])}),
	    gs:config(group, {selection, 0}),
	    loop(Win, Pid, Prompt, State, Parse);
	{updateState, N, X} ->
	    io:format("setelemtn N=~p X=~p State=~p~n",[N,X,State]),
	    State1 = setelement(N, State, X),
	    loop(Win, Pid, Prompt, State1, Parse);
	{gs,_,destroy,_,_} ->
	    io:format("Destroyed~n",[]),
	    exit(windowDestroyed);
	{gs, entry,keypress,_,['Return'|_]} ->
	    Text = gs:read(entry, text),
	    [Selected|_] = gs:read(group, selection),
	    To = string:substr(gs:read(group, {get, Selected}), 4),
	    %% io:format("Read:~p~n",[Text]),
	    gs:config(entry, {delete,{0,last}}),
	    gs:config(entry, {insert,{0,Prompt}}),
	    try Parse(Text) of
		Term ->
		    Pid ! {self(), State, To, Term}
	    catch
		_:_ ->
		    self() ! {insert, "** bad input**\n** /h for help\n"}
	    end,
	    loop(Win, Pid, Prompt, State, Parse);
	{gs,_,configure,[],[W,H,_,_]} ->
	    gs:config(packer, [{width,W},{height,H}]),
	    loop(Win, Pid, Prompt, State, Parse);
	{gs, entry,keypress,_,_} ->
	    loop(Win, Pid, Prompt, State, Parse);
	Any ->
	    io:format("Discarded:~p~n",[Any]),
	    loop(Win, Pid, Prompt, State, Parse)
    end.

add_prefix(Pref, L) -> lists:map(fun(S) -> string:concat(Pref, S) end, L).

scroll_to_show_last_line() ->
    Size       = gs:read(editor, size),
    Height     = gs:read(editor, height),
    CharHeight = gs:read(editor, char_height),
    TopRow     = Size - Height/CharHeight,
    if  TopRow > 0 -> gs:config(editor, {vscrollpos, TopRow});
	true       -> gs:config(editor, {vscrollpos, 0})
    end.

test() ->
    spawn(fun() -> test1() end).

test1() ->
    W = io_widget:start(self()),
    io_widget:set_title(W, "Test window"),
    loop(W).

loop(W) ->
    receive
	{W, {str, Str}} ->
	    Str1 = Str ++ "\n",
	    io_widget:insert_str(W, Str1),
	    loop(W)
    end.

parse(Str) ->
    {str, Str}.

whole_group() -> "group".
    
    
		  
