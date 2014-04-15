% game_tcp.erl -- TCP socket handling for game clients (real players)
% Copyright (C) 2014 Fred Barnes, University of Kent <frmb@kent.ac.uk>
% GPL 2

-module (game_tcp).
-compile ([debug_info]).

-export ([game_tcp_start/2, game_tcp_reload/1, game_tcp_cli_first/2, game_tcp_svr_listening/4, game_tcp_cli_ioloop/5]).

-import (game_util, [game_version/0, game_name/0, valid_exits/2]).
-import (game_player, [game_player_create/3]).


%{{{  game_tcp_cli_first (GMgr, CSock): initial client connection, say hello and prompt for name.
%
game_tcp_cli_first (GMgr, CSock) ->
	gen_tcp:send (CSock, io_lib:format ("\r\n\r\n~s (~s)\r\n\r\n", [game_name(), game_version()])),
	game_tcp_cli_pickname (GMgr, CSock).

game_tcp_cli_pickname (GMgr, CSock) ->
	gen_tcp:send (CSock, "Player name? >> "),
	receive
		{tcp, CSock, BDat} ->
			Str = binary_to_list (BDat),
			Xs = string:tokens (lists:filter (fun (C) -> ((C >= 32) and (C < 127)) end, Str), " \t"),
			XsLen = length (Xs),
			if (XsLen =/= 1) ->
				gen_tcp:send (CSock, "\r\nName must be a single thing!\r\n"),
				game_tcp_cli_pickname (GMgr, CSock);
			true ->
				Name = hd (Xs),
				NLen = length (Name),
				if (NLen == 0) ->
					gen_tcp:send (CSock, "\r\nName must be printable characters only!\r\n"),
					game_tcp_cli_pickname (GMgr, CSock);
				true ->
					% check with game manager that this name is okay to use!
					% [there is a race here -- two an get through query and only one register;  dealt with later]
					GMgr ! {query_name, Name, self ()},
					receive
						{name_inuse, _} ->
							gen_tcp:send (CSock, "\r\nName already in use :(\r\n"),
							game_tcp_cli_pickname (GMgr, CSock);
						{name_free, _} ->
							% got a sensible name, move on.
							game_tcp_cli_pickedname (GMgr, CSock, Name)
					end
				end
			end;
		{tcp_closed, CSock} ->
			io:format ("game_tcp_cli_pickname(): client closed connection (~p).~n", [CSock]),
			exit (normal)
	end.

%}}}
%{{{  game_tcp_cli_pickedname (GMgr, CSock, Name): once a valid name is chosen, create player
%
game_tcp_cli_pickedname (GMgr, CSock, Name) ->
	case game_player_create (GMgr, Name, self ()) of
		{ok, PlrPid} ->
			% means we're good and player is in the game!
			game_tcp_cli_ioloop_prompt (GMgr, CSock, Name, PlrPid, "");
		{error, _} ->
			gen_tcp:send (CSock, "\r\nFailed to create player in game, sorry :(\r\n"),
			gen_tcp:close (CSock),
			exit (normal)
	end.

%}}}
%{{{  game_tcp_cli_ioloop (GMgr, CSock, Name, PlrPid, Prompt): main I/O loop for game client.
%
game_tcp_cli_ioloop (GMgr, CSock, Name, PlrPid, Prompt) ->
	receive
		code_switch ->
			%{{{  switch implementations
			game_tcp:game_tcp_cli_ioloop (GMgr, CSock, Name, PlrPid, Prompt);
			%}}}
		{tcp, CSock, BDat} ->
			%{{{  process TCP data (probably a whole line)
			Str = binary_to_list (BDat),
			Xs = string:tokens (lists:filter (fun (C) -> ((C >= 32) and (C < 127)) end, Str), " \t"),
			XsLen = length (Xs),

			% if none-zero, write a particular string back to say what the game saw
			case XsLen of
				0 -> true;
				_ ->
					RStr = ansi_attr (bold_magenta) ++ "<< " ++ ansi_attr (normal) ++ ansi_attr (magenta) ++
						lists:foldl (fun (S,L) -> L ++ " " ++ S end, "", Xs) ++ ansi_attr (normal) ++ "\r\n",
					gen_tcp:send (CSock, RStr)
			end,
			Ate = case XsLen of
				0 ->
					% nothing, just redisplay prompt
					true;
				1 ->
					case hd (Xs) of
						"n" ->
							trigger_move (CSock, Name, PlrPid, north), true;
						"north" ->
							trigger_move (CSock, Name, PlrPid, north), true;
						"e" ->
							trigger_move (CSock, Name, PlrPid, east), true;
						"east" ->
							trigger_move (CSock, Name, PlrPid, east), true;
						"s" ->
							trigger_move (CSock, Name, PlrPid, south), true;
						"south" ->
							trigger_move (CSock, Name, PlrPid, south), true;
						"w" ->
							trigger_move (CSock, Name, PlrPid, west), true;
						"west" ->
							trigger_move (CSock, Name, PlrPid, west), true;
						"look" ->
							trigger_look (CSock, Name, PlrPid), true;
						"l" ->
							trigger_look (CSock, Name, PlrPid), true;
						"help" ->
							show_help (CSock), true;
						"i" ->
							inv_look (CSock, Name, PlrPid), true;
						"inventory" ->
							inv_look (CSock, Name, PlrPid), true;
						"t" ->
							qry_status (CSock, Name, PlrPid), true;
						"status" ->
							qry_status (CSock, Name, PlrPid), true;
						"unwield" ->
							do_unwield (CSock, Name, PlrPid), true;
						_ ->
							false
					end;
				2 ->
					Arg2 = hd (tl (Xs)),
					case hd (Xs) of
						"x" ->
							do_examine (CSock, Arg2, Name, PlrPid), true;
						"examine" ->
							do_examine (CSock, Arg2, Name, PlrPid), true;
						"g" ->
							do_pickup (CSock, Arg2, Name, PlrPid), true;
						"get" ->
							do_pickup (CSock, Arg2, Name, PlrPid), true;
						"d" ->
							do_drop (CSock, Arg2, Name, PlrPid), true;
						"drop" ->
							do_drop (CSock, Arg2, Name, PlrPid), true;
						"u" ->
							do_use (CSock, Arg2, Name, PlrPid), true;
						"use" ->
							do_use (CSock, Arg2, Name, PlrPid), true;
						"wield" ->
							do_wield (CSock, Arg2, Name, PlrPid), true;
						"attack" ->
							do_attack (CSock, Arg2, Name, PlrPid), true;
						"a" ->
							do_attack (CSock, Arg2, Name, PlrPid), true;
						_ ->
							false
					end;
				_ ->
					false
			end,
			% test for variable length commands.
			ReAte = if (Ate == true) ->
				true;
			(XsLen > 1) ->
				case hd (Xs) of
					"say" ->
						% just forward up to player
						S = lists:foldl (fun (B, "") -> B; (B, L) -> L ++ " " ++ B end, "", tl (Xs)),
						PlrPid ! {do_say, Name, S},
						true;
					_ ->
						false
				end;
			true ->
				false
			end,

			if (ReAte == false) ->
				game_tcp_cli_writedstr (CSock, {msg, io_lib:format ("\r\nUnhandled command(s) ~p\r\n", [Xs])});
			true ->
				true
			end,

			% recurse but print prompt on way
			game_tcp_cli_ioloop_prompt (GMgr, CSock, Name, PlrPid, Prompt);

			%}}}
		{tcp_closed, CSock} ->
			%{{{  TCP socket disconnected -- quit the abstract player and ourselves.
			io:format ("game_tcp_cli_ioloop(): client socket (~p) closing..~n", [CSock]),
			PlrPid ! {player_quit, self()},
			exit (normal);
			%}}}
		{entered, LNum, LPid} ->
			%{{{  sent by our player when we have entered a room -- look around
			game_tcp_cli_clearprompt (CSock),
			look_in_location (LNum, LPid, CSock, PlrPid),
			game_tcp_cli_ioloop_prompt (GMgr, CSock, Name, PlrPid, Prompt);
			%}}}
		{exit_appearing, _Pid, Direction} ->
			%{{{  sent by our player when an exit appears
			game_tcp_cli_clearprompt (CSock),
			game_tcp_cli_writedstr (CSock, {high4, "an exit appears to the " ++ exit_str(Direction)}),
			game_tcp_cli_ioloop_prompt (GMgr, CSock, Name, PlrPid, Prompt);
			%}}}
		{exit_vanishing, _Pid, Direction} ->
			%{{{  sent by our player when an exit vanishes
			game_tcp_cli_clearprompt (CSock),
			game_tcp_cli_writedstr (CSock, {high4, "the exit to the " ++ exit_str(Direction) ++ " disappears"}),
			game_tcp_cli_ioloop_prompt (GMgr, CSock, Name, PlrPid, Prompt);
			%}}}
		{exit_changing, _Pid, Direction} ->
			%{{{  sent by our player when an exit changes
			game_tcp_cli_clearprompt (CSock),
			game_tcp_cli_writedstr (CSock, {high4, "the exit to the " ++ exit_str(Direction) ++ " flickers briefly"}),
			game_tcp_cli_ioloop_prompt (GMgr, CSock, Name, PlrPid, Prompt);
			%}}}
		{person_entering, OName} ->
			%{{{  sent from a room via our player when someone else enters the room we're in
			game_tcp_cli_clearprompt (CSock),
			game_tcp_cli_writedstr (CSock, {high3, OName ++ " entered the room"}),
			game_tcp_cli_ioloop_prompt (GMgr, CSock, Name, PlrPid, Prompt);
			%}}}
		{person_leaving, OName} ->
			%{{{  sent from a room via our player when someone is forcibly removed (maybe client disconnect)
			game_tcp_cli_clearprompt (CSock),
			game_tcp_cli_writedstr (CSock, {high3, OName ++ " vanishes in a puff of smoke"}),
			game_tcp_cli_ioloop_prompt (GMgr, CSock, Name, PlrPid, Prompt);
			%}}}
		{person_leaving_exit, OName, Direction} ->
			%{{{  sent from a room via our player when someone leaves by a normal exit
			game_tcp_cli_clearprompt (CSock),
			game_tcp_cli_writedstr (CSock, {high3, OName ++ " left the room (" ++ exit_str (Direction) ++ ")"}),
			game_tcp_cli_ioloop_prompt (GMgr, CSock, Name, PlrPid, Prompt);
			%}}}
		{person_leaving_noexit, OName} ->
			%{{{  sent from a room via our player when someone leaves by an abnormal exit (e.g. door object)
			game_tcp_cli_clearprompt (CSock),
			game_tcp_cli_writedstr (CSock, {high3, OName ++ " left the room"}),
			game_tcp_cli_ioloop_prompt (GMgr, CSock, Name, PlrPid, Prompt);
			%}}}
		{person_leaving_death, OName} ->
			%{{{  sent from a room via our player when someone leaves by dying
			game_tcp_cli_clearprompt (CSock),
			game_tcp_cli_writedstr (CSock, {high3, OName ++ " died and vanished"}),
			game_tcp_cli_ioloop_prompt (GMgr, CSock, Name, PlrPid, Prompt);
			%}}}
		{player_drop_object, PN, OName} ->
			%{{{  sent from a room via our player when someone drops something in the room (maybe ourselves)
			game_tcp_cli_clearprompt (CSock),
			game_tcp_cli_writedstr (CSock, {high3, PN ++ " dropped " ++ OName}),
			game_tcp_cli_ioloop_prompt (GMgr, CSock, Name, PlrPid, Prompt);
			%}}}
		{player_picked_up, PN, OName} ->
			%{{{  sent from a room via our player when someone picks up an object in the room (but not us)
			game_tcp_cli_clearprompt (CSock),
			game_tcp_cli_writedstr (CSock, {high3, PN ++ " picked up " ++ OName}),
			game_tcp_cli_ioloop_prompt (GMgr, CSock, Name, PlrPid, Prompt);
			%}}}
		{object_appear, OName} ->
			%{{{  sent from a room via our player when an object appears.
			game_tcp_cli_clearprompt (CSock),
			game_tcp_cli_writedstr (CSock, {high3, OName ++ " appeared."}),
			game_tcp_cli_ioloop_prompt (GMgr, CSock, Name, PlrPid, Prompt);
			%}}}
		{say, Who, What} ->
			%{{{  sent from a room via our player when someone says something.
			game_tcp_cli_clearprompt (CSock),
			S = ansi_attr (bold_green) ++ Who ++ ansi_attr (normal) ++ ansi_attr (green) ++ ": " ++
				ansi_attr (yellow) ++ What ++ ansi_attr (normal) ++ "\r\n",
			gen_tcp:send (CSock, S),
			game_tcp_cli_ioloop_prompt (GMgr, CSock, Name, PlrPid, Prompt);
			%}}}
		{action, Who, What} ->
			%{{{  sent from a room via our player when someone does something.
			game_tcp_cli_clearprompt (CSock),
			S = ansi_attr (bold_blue) ++ Who ++ " " ++ What ++ ansi_attr (normal) ++ "\r\n",
			gen_tcp:send (CSock, S),
			game_tcp_cli_ioloop_prompt (GMgr, CSock, Name, PlrPid, Prompt);
			%}}}
		{attack, Who, What, Damage} ->
			%{{{  sent by the player when we are attacked.
			game_tcp_cli_clearprompt (CSock),
			S = io_lib:format ("~s attacks you with a ~s, ~w points damage", [Who, What, Damage]),
			S2 = ansi_attr (bold_red) ++ S ++ ansi_attr (normal) ++ "\r\n",
			gen_tcp:send (CSock, S2),
			game_tcp_cli_ioloop_prompt (GMgr, CSock, Name, PlrPid, Prompt);
			%}}}
		{died} ->
			%{{{  we died: need to handle carefully -- wait for "resurrect" message once we've played out.
			game_tcp_cli_clearprompt (CSock),
			timer:sleep (500),
			S1 = ansi_attr (bold_white) ++ "The world starts to fade away..." ++ ansi_attr (normal) ++ "\r\n",
			gen_tcp:send (CSock, S1),
			game_tcp_cli_ioloop_resurrect (GMgr, CSock, Name, PlrPid, Prompt);
			%}}}
		{message, M} ->
			%{{{  general message -- usually sent by death-code, but could be from elsewhere
			game_tcp_cli_clearprompt (CSock),
			S = ansi_attr (bold_white) ++ M ++ ansi_attr (normal) ++ "\r\n",
			gen_tcp:send (CSock, S),
			game_tcp_cli_ioloop_prompt (GMgr, CSock, Name, PlrPid, Prompt);
			%}}}
		Other ->
			io:format ("game_tcp_cli_ioloop(): got unhandled message: ~p~n", [Other]),
			game_tcp_cli_ioloop (GMgr, CSock, Name, PlrPid, Prompt)
	end.


% used in conjunction with the above: prints the prompt
% for user input first though.
%
game_tcp_cli_ioloop_prompt (GMgr, CSock, Name, PlrPid, Prompt) ->
	gen_tcp:send (CSock, Prompt ++ ">> "),
	game_tcp_cli_ioloop (GMgr, CSock, Name, PlrPid, Prompt).


% clears the prompt.
% Note: I'm not convinced about this (at all!) -- but the result with
% tinyfugue (tf) doesn't look too bad!
%
game_tcp_cli_clearprompt (_CSock) ->
	%gen_tcp:send (CSock, "\x1b[1K\r\n").
	%gen_tcp:send (CSock, "\r    \r").
	true.

% slightly special version that is used when the player has been killed.
%
game_tcp_cli_ioloop_resurrect (GMgr, CSock, Name, PlrPid, Prompt) ->
	receive
		{message, M} ->
			%{{{  general message -- usually sent by death-code, but could be from elsewhere
			S = ansi_attr (bold_white) ++ M ++ ansi_attr (normal) ++ "\r\n",
			gen_tcp:send (CSock, S),
			game_tcp_cli_ioloop_resurrect (GMgr, CSock, Name, PlrPid, Prompt);
			%}}}
		{resurrect} ->
			%{{{  done! -- loop back into the main game.
			S = ansi_attr (bold_yellow) ++ "You feel the world returning.." ++ ansi_attr (normal) ++ "\r\n",
			gen_tcp:send (CSock, S),
			timer:sleep (500),
			game_tcp_cli_ioloop_prompt (GMgr, CSock, Name, PlrPid, Prompt)
			%}}}
	end.

%}}}

%{{{  show_help (CSock): shows basic help stuff

show_help (CSock) ->
	gen_tcp:send (CSock, "\r\n" ++ ansi_attr (bold_white) ++ game_name() ++ ansi_attr (normal) ++ ansi_attr (green) ++
			"    (" ++ game_version() ++ ")" ++ ansi_attr (normal) ++ "\r\n"),
	gen_tcp:send (CSock, ansi_attr (white) ++ "----------------------------------------------------------" ++
			ansi_attr (normal) ++ "\r\n\r\n"),
	lists:map (fun ({C,A,D}) ->
			S1 = "    " ++ ansi_attr (yellow) ++ io_lib:format ("~-11s", [C]),
			S2 = case A of
					"" -> "     ";
					_ -> ansi_attr (cyan) ++ io_lib:format ("~-5s", [A])
				end,
			S3 = " " ++ ansi_attr (white) ++ D ++ ansi_attr(normal) ++ "\r\n",
			gen_tcp:send (CSock, S1 ++ S2 ++ S3),
			true
		end, [	{"help",	"",	"show this help"},
			{"[n]orth",	"",	"move north"},
			{"[e]ast",	"",	"move east"},
			{"[s]outh",	"",	"move south"},
			{"[w]est",	"",	"move west"},
			{"[l]ook",	"",	"look around"},
			{"[i]nventory",	"",	"list inventory"},
			{"[g]et",	"what",	"pick up object"},
			{"[d]rop",	"what",	"drop object"},
			{"[u]se",	"what",	"use object (eat food)"},
			{"[a]ttack",	"who",	"attack someone"},
			{"e[x]amine",	"what",	"examine something/someone"},
			{"s[t]atus",	"",	"status of self"},
			{"wield",	"what",	"wield something"},
			{"unwield",	"",	"unwield currently wielded thing"}
			]),
	gen_tcp:send (CSock, "\r\n"),
	true.

%}}}
%{{{  trigger_move (CSock, _Name, PlrPid, Direction): using the abstract player, triggers a move from the current location.

trigger_move (CSock, _Name, PlrPid, Direction) ->
	PlrPid ! {move, self (), Direction},
	receive
		{cannot_move, PlrPid} ->
			game_tcp_cli_writedstr (CSock, {msg, "Cannot go that way!"}),
			false;
		{moving, PlrPid} ->
			true
	end.

%}}}
%{{{  trigger_look (CSock, Name, PlrPid): using the abstract player, triggers a look in the current location.

trigger_look (CSock, _Name, PlrPid) ->
	% find out where we are and look accordingly.
	PlrPid ! {curlocn, self()},
	receive
		{curlocn, PlrPid, InLNum, InLocn} ->
			look_in_location (InLNum, InLocn, CSock, PlrPid)
	end.

%}}}
%{{{  inv_look (CSock, Name, PlrPid): look at inventory.

inv_look (CSock, _Name, PlrPid) ->
	PlrPid ! {inv_look, self ()},
	S = receive {inventory, IList} ->
		case IList of
			[] -> ansi_attr (cyan) ++ "Not carrying anything!" ++ ansi_attr (normal) ++ "\r\n";
			_ -> ansi_attr (cyan) ++ objects_str (IList, "Carrying: ") ++ ansi_attr (normal) ++ "\r\n"
		end
	end,
	gen_tcp:send (CSock, S),
	true.

%}}}
%{{{  qry_status (CSock, Name, PlrPid): query player status.

qry_status (CSock, _Name, PlrPid) ->
	PlrPid ! {qry_status, self ()},
	S = receive
		{status, Health, Vital, Wielding, Immortal} ->
			io_lib:format ("Health (~w/100), Vitality (~w/100)" ++ (if Immortal == true -> " immortal"; true -> "" end)
					++ "\r\n", [Health, Vital]) ++
				(case Wielding of
					undefined -> "";
					{ONm,_} -> io_lib:format ("Wielding ~s\r\n", [ONm])
				end)
	end,
	gen_tcp:send (CSock, S),
	true.

%}}}
%{{{  do_examine (CSock, OName, Name, PlrPid): examine an object/person.
%

do_examine (CSock, OName, _Name, PlrPid) ->
	PlrPid ! {do_examine, OName, self ()},
	receive
		{examined, Desc, Attrs} ->
			lists:map (fun (X) -> game_tcp_cli_writedstr (CSock, X), true end, Desc),
			case Attrs of
				[] -> true;
				_ ->
					game_tcp_cli_writedstr (CSock, {highlight, str_objattrs (Attrs)})
			end;
		{examine_fail, _Msg} ->
			game_tcp_cli_writedstr (CSock, {high2, "Cannot examine that!"});
		{examine_resist, _Msg} ->
			game_tcp_cli_writedstr (CSock, {high2, OName ++ " resists your examination."})
	end,
	true.

str_objattrs ([]) -> "";
str_objattrs ([{health, H}|R]) ->
	io_lib:format ("[health: ~w] ", [H]) ++ str_objattrs (R);
str_objattrs ([{damage, H}|R]) ->
	io_lib:format ("[damage: ~w] ", [H]) ++ str_objattrs (R);
str_objattrs ([_|R]) ->
	str_objattrs (R).

%}}}
%{{{  do_attack (CSock, Who, Name, PlrPid): attack someone.
%

do_attack (CSock, Who, _Name, PlrPid) ->
	PlrPid ! {do_attack, Who, self ()},
	receive
		{attack_ok, Msg, _Damage} ->
			game_tcp_cli_writedstr (CSock, {high6, Msg});
			% game_tcp_cli_writedstr (CSock, {high5, io_lib:format ("~w points of damage", [Damage])});
		{attack_fail, Msg} ->
			game_tcp_cli_writedstr (CSock, {high6, Msg})
	end,
	true.

%}}}
%{{{  do_pickup (CSock, OName, Name, PlrPid): pick up an object (assuming it's in the room).
%

do_pickup (CSock, OName, _Name, PlrPid) ->
	PlrPid ! {do_pickup, OName, self ()},
	receive
		{picked_up, ONm, _OPid} ->
			game_tcp_cli_writedstr (CSock, {high2, "Picked up " ++ ONm ++ "."});
		{pickup_fail, _Msg} ->
			game_tcp_cli_writedstr (CSock, {high2, "Cannot pick that up."})
	end,
	true.

%}}}
%{{{  do_drop (CSock, OName, Name, PlrPid): drop an object (assuming we're holding it).
%

do_drop (CSock, OName, _Name, PlrPid) ->
	PlrPid ! {do_drop, OName, self ()},
	receive
		{dropped, _ONm} ->
			true;		% will see message via room
		{drop_fail, _Msg} ->
			game_tcp_cli_writedstr (CSock, {high2, "Cannot drop that."})
	end,
	true.
		
%}}}
%{{{  do_use (CSock, OName, Name, PlrPid): use an object (assuming it's in the room or held by us).
%

do_use (CSock, OName, _name, PlrPid) ->
	PlrPid ! {do_use, OName, self ()},
	receive
		{use_ok} ->
			true;				% ignore
		{use_noobj, _Msg} ->
			game_tcp_cli_writedstr (CSock, {high2, "No such object."});
		{use_fail, _Msg} ->
			game_tcp_cli_writedstr (CSock, {high2, "Cannot use that."});
		{use_eated, OldH, NewH} ->
			Str = if NewH < OldH ->
					io_lib:format ("Ate ~s, must have been bad, health now [~w/100]", [OName, NewH]);
				true ->
					io_lib:format ("Ate ~s, health now [~w/100]", [OName, NewH])
				end,
			game_tcp_cli_writedstr (CSock, {high2, Str})
	end,
	true.

%}}}
%{{{  do_wield (CSock, OName, Name, PlrPid): wield an object (assuming it's held).
%

do_wield (CSock, OName, _Name, PlrPid) ->
	PlrPid ! {do_wield, OName, self ()},
	receive
		{wield_ok, ONm} ->
			game_tcp_cli_writedstr (CSock, {high2, "Now wielding " ++ ONm});
		{wield_fail, _Msg} ->
			game_tcp_cli_writedstr (CSock, {high2, "Cannot wield that."})
	end,
	true.

%}}}
%{{{  do_unwield (CSock, Name, PlrPid): unwield currently wielded object.
%

do_unwield (CSock, _Name, PlrPid) ->
	PlrPid ! {do_unwield, self ()},
	receive
		{unwield_ok, ONm} ->
			game_tcp_cli_writedstr (CSock, {high2, "No longer wielding " ++ ONm});
		{unwield_fail, _Msg} ->
			game_tcp_cli_writedstr (CSock, {high2, "Cannot unwield :(.  Not wielding anything?"})
	end,
	true.

%}}}

%{{{  look_in_location (LocnNum, LocnPid, CSock, PlrPid): looks in a location given location number and PID.

look_in_location (_LocnNum, LocnPid, CSock, PlrPid) ->
	LocnPid ! {look, self ()},
	receive
		{looked, LDesc, LObjects, LPlayers, LExits} ->
			lists:map (fun (X) -> game_tcp_cli_writedstr (CSock, X), true end, LDesc),

			case LObjects of
				[] -> true;			% no objects, do nothing
				_ ->
					ObjStr = objects_str (LObjects),
					game_tcp_cli_writedstr (CSock, {high2, ObjStr})
			end,
			case LPlayers of
				[] -> game_tcp_cli_writedstr (CSock, {high3, "Nobody here, not even you!"});
				_ ->
					PlrStr = players_str (lists:filter (fun ({_,P}) -> (P =/= PlrPid) end, LPlayers)),
					game_tcp_cli_writedstr (CSock, {high3, PlrStr})
			end,
			game_tcp_cli_writedstr (CSock, {high4, exits_str (LExits)}),
			game_tcp_cli_writedstr (CSock, {desc, ""})
	end.

%}}}
%{{{  exits_str (Exits): turns a list of exits into a descriptive list.

exits_str (Exits) ->
	V = valid_exits (Exits, [north, east, south, west]),
	VL = length (V),
	case VL of
		0 -> "You are trapped!";
		1 -> "There is an exit to the " ++ exit_str (hd (V));
		_ -> "There are exits to the " ++ exit_str2 (V)
	end.


exit_str (north) -> "North";
exit_str (east) -> "East";
exit_str (south) -> "South";
exit_str (west) -> "West".

exit_str2 ([X|Xs]) ->
	case Xs of
		[S] -> exit_str (X) ++ " and " ++ exit_str (S);
		_ -> exit_str (X) ++ ", " ++ exit_str2 (Xs)
	end.


%}}}
%{{{  objects_str (OList): turns a list of objects into a string (empty string if none).

objects_str ([]) -> "";
objects_str (Objs) ->
	objects_str (Objs, "Objects: ").


objects_str ([], Str) -> Str;
objects_str ([Obj|Rest], Str) ->
	NStr = case Rest of
		[] -> Str ++ Obj;
		_ -> Str ++ Obj ++ ", "
	end,
	objects_str (Rest, NStr).

%}}}
%{{{  players_str (PList): turns a list of (other) players into a string.
players_str ([]) -> "You are alone.";
players_str ([{N,_}]) -> N ++ " is here.";
players_str ([{N,_}|Ps]) -> N ++ players_str2 (Ps) ++ " are here.".

players_str2 ([{N, _}]) -> " and " ++ N;
players_str2 ([{N, _}|R]) -> ", " ++ N ++ players_str2 (R).

%}}}

%{{{  ansi_attr (...): ANSI attribute to escape string.
% ANSI attributes (for VT220 or equivalent terminal)
%
ansi_attr (normal) ->		"\x1b[0m";
ansi_attr (bold) ->		"\x1b[1m";
ansi_attr (bold_black) ->	"\x1b[1;30m";
ansi_attr (bold_red) ->		"\x1b[1;31m";
ansi_attr (bold_green) ->	"\x1b[1;32m";
ansi_attr (bold_yellow) ->	"\x1b[1;33m";
ansi_attr (bold_blue) ->	"\x1b[1;34m";
ansi_attr (bold_magenta) ->	"\x1b[1;35m";
ansi_attr (bold_cyan) ->	"\x1b[1;36m";
ansi_attr (bold_white) ->	"\x1b[1;37m";
ansi_attr (black) ->		"\x1b[30m";
ansi_attr (red) ->		"\x1b[31m";
ansi_attr (green) ->		"\x1b[32m";
ansi_attr (yellow) ->		"\x1b[33m";
ansi_attr (blue) ->		"\x1b[34m";
ansi_attr (magenta) ->		"\x1b[35m";
ansi_attr (cyan) ->		"\x1b[36m";
ansi_attr (white) ->		"\x1b[37m".

%}}}
%{{{  game_tcp_cli_writedstr (CSock, {Type, Str}): writes a whole line with assorted formatting

game_tcp_cli_writedstr (CSock, {desc, Str}) ->
	gen_tcp:send (CSock, Str ++ "\r\n");
game_tcp_cli_writedstr (CSock, {highlight, Str}) ->
	gen_tcp:send (CSock, ansi_attr(bold_yellow) ++ Str ++ ansi_attr(normal) ++ "\r\n");
game_tcp_cli_writedstr (CSock, {high2, Str}) ->
	gen_tcp:send (CSock, ansi_attr(cyan) ++ Str ++ ansi_attr(normal) ++ "\r\n");
game_tcp_cli_writedstr (CSock, {high3, Str}) ->
	gen_tcp:send (CSock, ansi_attr(green) ++ Str ++ ansi_attr(normal) ++ "\r\n");
game_tcp_cli_writedstr (CSock, {high4, Str}) ->
	gen_tcp:send (CSock, ansi_attr(yellow) ++ Str ++ ansi_attr(normal) ++ "\r\n");
game_tcp_cli_writedstr (CSock, {high5, Str}) ->
	gen_tcp:send (CSock, ansi_attr(bold_green) ++ Str ++ ansi_attr(normal) ++ "\r\n");
game_tcp_cli_writedstr (CSock, {high6, Str}) ->
	gen_tcp:send (CSock, ansi_attr(bold_red) ++ Str ++ ansi_attr(normal) ++ "\r\n");
game_tcp_cli_writedstr (CSock, {msg, Str}) ->
	gen_tcp:send (CSock, ansi_attr(bold_white) ++ Str ++ ansi_attr(normal) ++ "\r\n").

%}}}
	

%{{{  game_tcp_svr_listening (GMgr, Port, LSock, PPid): listens for incomming connections.
% Note: when a socket connection is accepted, this process becomes the recipient of future
% messages sent to the socket (active mode).
%
game_tcp_svr_listening (GMgr, Port, LSock, PPid) ->
	link (PPid),					% attach to original parent if not already.
	GMgr ! {set_tcp_server, {self(), Port}},	% tell game-manager who we are: for code-reload.
	case gen_tcp:accept (LSock) of
		{ok, CSock} ->
			io:format ("game_tcp_svr_listening(): got connection (~p)~n", [CSock]),
			spawn_link (?MODULE, game_tcp_svr_listening, [GMgr, Port, LSock, PPid]),
			game_tcp:game_tcp_cli_first (GMgr, CSock);
		{error, R} ->
			io:format ("game_tcp_svr_listening(): failed to accept new connection: ~p~n", [R]),
			io:format ("game_tcp_svr_listening(): waiting 10 seconds and trying again..~n"),
			timer:sleep (10000),
			game_tcp:game_tcp_svr_listening (GMgr, Port, LSock, PPid)
	end.


%}}}
%{{{  game_tcp_start (GMgr, Port): starts the TCP server on the specified port.
% returns true if started okay, false if not (e.g. address in use).
%
game_tcp_start (GMgr, Port) ->
	case gen_tcp:listen (Port, [binary, {packet, 0}, {reuseaddr, true}, {active, true}]) of
		{ok, L} ->
			spawn_link (?MODULE, game_tcp_svr_listening, [GMgr, Port, L, self ()]),
			true;
		{error, R} ->
			io:format ("game_tcp_server_start(): failed to listen on port ~p: ~p~n", [Port, R]),
			false
	end.

%}}}
%{{{  game_tcp_reload (TCPRef): reloads the TCP server, needs a kick via TCP.
game_tcp_reload ({_Pid, Port}) ->
	spawn (fun () -> game_tcp_doreload (Port) end),
	true.

game_tcp_doreload (Port) ->
	{ok, S} = gen_tcp:connect ("localhost", Port, [binary, {packet, 0}]),
	gen_tcp:close (S).

%}}}

