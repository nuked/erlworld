% game_main.erl -- main game stuff.
% Copyright (C) 2014 Fred Barnes, University of Kent <frmb@kent.ac.uk>
% GPL 2

-module (game_main).
-compile ([debug_info]).

-export ([game_console/1, game_manager/0, game_run/0, game_healthgen/1]).

-import (game_util, [game_version/0, game_name/0, purge_mods/1, compile_mods/1, reload_mods/1, lexit_atom/1]).
-import (game_tcp, [game_tcp_start/2, game_tcp_reload/1]).
-import (game_locn, [game_locn_create/3]).
-import (game_player, [game_player_create/2]).
-import (game_object, [game_object_create/3]).
-import (game_bots, [game_bot_create/4, game_bot_create/7]).


%{{{  game_manager(): main game manager process.
% This has "global" knowledge of what locations, players and objects are in
% the game.
%
%	LTab is of {locn-num, pid}
%	PTab is of {player-name(string), pid}
%	OTab is of {obj-name, pid}		<-- may (will) be duplicates!
%
game_manager () ->
	LTab = ets:new (game_locations, [ordered_set, private]),
	PTab = ets:new (game_players, [ordered_set, private]),
	OTab = ets:new (game_objects, [bag, private]),

	game_manager_run (LTab, PTab, OTab, none).

game_manager_run (LTab, PTab, OTab, TCPRef) ->
	receive
		{set_tcp_server, NewTCPRef} -> %{{{  stores PID/etc. of TCP server.
			game_manager_run (LTab, PTab, OTab, NewTCPRef);
		%}}}
		{status, Pid} -> %{{{  respond to `Pid' with status string.
			Str = io_lib:format ("~s (~s)~n" ++
					"Players:   ~-4w~n" ++
					"Objects:   ~-4w~n" ++
					"Locations: ~-4w~n",
				[game_name(), game_version(), ets:info (PTab, size),
					ets:info (OTab, size), ets:info (LTab, size)]),
			Pid ! {status, Str},
			game_manager_run (LTab, PTab, OTab, TCPRef);
			%}}}
		{register_locn, LNum, Pid} -> %{{{  respond to `Pid' with fact.
			Locns = ets:lookup (LTab, LNum),
			case length (Locns) of
				0 ->
					ets:insert (LTab, {LNum, Pid}),
					Pid ! {registered_locn, self(), LNum};
				1 ->
					Pid ! {error, self(), "already registered!"}
			end,
			game_manager_run (LTab, PTab, OTab, TCPRef);
			%}}}
		{unregister_locn, LNum, Pid} -> %{{{  responds to `Pid' with fact.
			ets:delete (LTab, LNum),
			Pid ! {unregistered_locn, self(), LNum},
			game_manager_run (LTab, PTab, OTab, TCPRef);
			%}}}
		{register_player, PName, Pid} -> %{{{  responds to `Pid' with fact.
			People = ets:lookup (PTab, PName),
			case length (People) of
				0 ->
					ets:insert (PTab, {PName, Pid}),
					Pid ! {registered_player, self(), PName};
				1 ->
					Pid ! {error, self(), "already registered!"}
			end,
			game_manager_run (LTab, PTab, OTab, TCPRef);
			%}}}
		{unregister_player, PName, Pid} -> %{{{  responds to `Pid' with fact.
			ets:delete (PTab, PName),
			Pid ! {unregistered_player, self(), PName},
			game_manager_run (LTab, PTab, OTab, TCPRef);
			%}}}
		{register_object, OName, OPid, Pid} -> %{{{  responds to `Pid' with fact; registers object with `OPid'.
			ets:insert (OTab, {OName, OPid}),
			Pid ! {registered_object, self(), OName},
			game_manager_run (LTab, PTab, OTab, TCPRef);
			%}}}
		{unregister_object, OName, OPid, Pid} -> %{{{  responds to `Pid' with fact.
			ets:delete_object (OTab, {OName, OPid}),
			Pid ! {unregistered_object, self(), OName},
			game_manager_run (LTab, PTab, OTab, TCPRef);
			%}}}
		{query_name, PName, Pid} -> %{{{  queries whether a name is in use, responds to `Pid'.
			People = ets:lookup (PTab, PName),
			LP = length (People),
			if (LP == 0) ->
				Pid ! {name_free, PName};
			true ->
				Pid ! {name_inuse, PName}
			end,
			game_manager_run (LTab, PTab, OTab, TCPRef);
			%}}}
		{lookup_locn, LNum, Pid} -> %{{{  looks up a location, responds to `Pid'.
			Locns = ets:lookup (LTab, LNum),
			case length (Locns) of
				0 ->
					Pid ! {error, self(), "no such location"};
				1 ->
					{_, LPid} = hd (Locns),
					Pid ! {locn, self(), LPid}
			end,
			game_manager_run (LTab, PTab, OTab, TCPRef);
			%}}}
		{random_locn, Pid} -> %{{{  looks up a random location, responds to `Pid'.
			NLocns = ets:info (LTab, size),
			XFun = fun (Tail) -> case ets:lookup (LTab, random:uniform (NLocns)) of
						[] -> Tail (Tail);
						[{_, LPid}|_] -> LPid		% result of fun
					end end,
			RandLPid = XFun (XFun),
			Pid ! {locn, self(), RandLPid},
			game_manager_run (LTab, PTab, OTab, TCPRef);
		%}}}
		{code_switch_players} -> %{{{  sends messages to all players instructing them to code_switch -- forwarded to the implementations (tcp_client and bot)
			case TCPRef of
				none -> true;
				_ -> game_tcp_reload (TCPRef)
			end,
			ets:foldl (fun ({_, P}, _) -> P ! code_switch, true end, true, PTab),
			game_manager_run (LTab, PTab, OTab, TCPRef);
			%}}}
		{code_switch_objects} -> %{{{  sends messages to all objects instructing them to code_switch.
			ets:foldl (fun ({_, P}, _) -> P ! code_switch, true end, true, OTab),
			game_manager_run (LTab, PTab, OTab, TCPRef);
			%}}}
		Other ->
			io:format ("game_manager_run(): unhandled message: ~p~n", [Other]),
			game_manager_run (LTab, PTab, OTab, TCPRef)
	end.


%}}}


%{{{  gc_... (...): superuser console helper functions.
% These get called with the number of arguments as defined in the table,
% plus a handle on the game-manager.

gc_help (_) -> %{{{
	io:format ("Available commands:~n"),
	io:format ("    help                    this help~n"),
	io:format ("    quit                    quit game~n"),
	io:format ("    status                  general status~n"),
	io:format ("    spobj <Obj> <Locn>      spawn object in specific location~n"),
	io:format ("    spdobj <Obj> <GenObj> <Locn>~n"),
	io:format ("                            spawn dispenser object in specific location~n"),
	io:format ("    spdoor <Obj> <Locn> <TLocn>~n"),
	io:format ("                            spawn door object in specific location~n"),
	io:format ("    sproom <Name> <Locn>    spawn location of the given name/type~n"),
	io:format ("    spbot <Type> <Name> <Locn> <TLocn> <TSrc> <TName>~n"),
	io:format ("                            spawn bot in specific location~n"),
	io:format ("    sphealth                spawn a random health generator~n"),
	io:format ("    link <Lc> <Exit> <To>   link Lc:Exit to To~n"),
	io:format ("    reloadbots              re-compile and re-load bot code~n"),
	io:format ("    reloadplayers           re-compile and re-load player code (includes bots)~n"),
	true.

%}}}
gc_quit (_GMgr) -> %{{{
	io:format ("shutting down...~n"),
	exit (die).

%}}}
gc_spawnobj (GMgr, Obj, Locn) -> %{{{
	%io:format ("want to spawn ~p in ~p~n", [Obj, Locn]),

	O = game_object_create (GMgr, Obj, []),

	if (O == false) ->
		io:format ("failed to spawn object..~n");
	true ->
		% dump object in specific location.
		GMgr ! {lookup_locn, list_to_integer (Locn), self()},
		InitLocn = receive
			{error, GMgr, _} ->
				io:format ("cannot spawn ~p here (~p), no such location!", [Obj, Locn]),

				% dump it in the construct..
				GMgr ! {lookup_locn, 0, self()},
				receive {locn, GMgr, L} -> L end;
			{locn, GMgr, L} ->
				L
		end,

		% place object there!
		InitLocn ! {drop, Obj, O}
	end,
	true.

%}}}
gc_spawnhealth (GMgr) -> %{{{
	spawn_link (?MODULE, game_healthgen, [GMgr]),
	true.

%}}}
gc_spawndobj (GMgr, Obj, GenObj, Locn) -> %{{{
	O = game_object_create (GMgr, Obj, [{dispenser, GenObj}]),

	if (O == false) ->
		io:format ("failed to spawn dispenser-object..~n");
	true ->
		% dump object in specific location.
		GMgr ! {lookup_locn, list_to_integer (Locn), self ()},
		InitLocn = receive
			{error, GMgr, _} ->
				io:format ("cannot spawn ~p here (~p), no such location!", [Obj, Locn]),

				% dump it in the construct..
				GMgr ! {lookup_locn, 0, self()},
				receive {locn, GMgr, L} -> L end;
			{locn, GMgr, L} ->
				L
		end,

		% place object there!
		InitLocn ! {drop, Obj, O}
	end,
	true.

%}}}
gc_spawndoorobj (GMgr, Obj, Locn, TLocn) -> %{{{
	O = game_object_create (GMgr, Obj, [{door, list_to_integer (TLocn)}]),

	if (O == false) ->
		io:format ("failed to spawn door-object..~n");
	true ->
		% dump object in specific location.
		GMgr ! {lookup_locn, list_to_integer (Locn), self ()},
		InitLocn = receive
			{error, GMgr, _} ->
				io:format ("cannot spawn ~p here (~p), no such location!", [Obj, Locn]),

				% dump it in the construct..
				GMgr ! {lookup_locn, 0, self()},
				receive {locn, GMgr, L} -> L end;
			{locn, GMgr, L} ->
				L
		end,

		% place object there!
		InitLocn ! {drop, Obj, O}
	end,
	true.

%}}}
gc_spawnlocn (GMgr, Name, Locn) -> %{{{
	R = game_locn_create (GMgr, Name, list_to_integer (Locn)),
	if (R == false) ->
		io:format ("failed to spawn location..~n");
	true ->
		%io:format ("spawned location successfully, pid ~w~n", [R])
		true
	end,
	true.

%}}}
gc_spawnbot (GMgr, Type, Name, Locn, TLocn, TSrc, TName) -> %{{{
	R = game_bot_create (GMgr, Name, list_to_atom (Type), list_to_integer (Locn),
				list_to_integer (TLocn), list_to_integer (TSrc), TName),
	if (R == false) ->
		io:format ("failed to spawn bot..~n"),
		true;
	true ->
		true
	end.

%}}}
gc_status (GMgr) -> %{{{
	GMgr ! {status, self()},
	receive
		{status, Str} -> io:format ("~s", [Str])
	end,
	true.

%}}}
gc_linklocn (GMgr, Src, Exit, Dst) -> %{{{
	ISrc = list_to_integer (Src),
	IExit = list_to_integer (Exit),
	IDst = list_to_integer (Dst),

	GMgr ! {lookup_locn, ISrc, self ()},
	receive
		{locn, GMgr, SrcLocn} ->
			GMgr ! {lookup_locn, IDst, self ()},
			receive
				{locn, GMgr, DstLocn} ->
					% io:format ("link ~p exit ~p to ~p~n", [ISrc, IExit, IDst]),
					SrcLocn ! {set_exit, self (), lexit_atom (IExit), DstLocn},
					true;
				_ ->
					io:format ("failed to link ~p to ~p, no such destination~n", [ISrc, IDst]),
					false
			end;
		_ ->
			io:format ("failed to link ~p to ~p, no such source~n", [ISrc, IDst]),
			false
	end.

%}}}
gc_reloadbots (GMgr) -> %{{{
	Mods = [game_bots],
	case purge_mods (Mods) andalso compile_mods (Mods) andalso reload_mods (Mods) of
		true ->
			GMgr ! {code_switch_players},
			true;
		false ->
			false
	end.

%}}}
gc_reloadplayers (GMgr) -> %{{{
	Mods = [game_player, game_tcp, game_bots],
	case purge_mods (Mods) andalso compile_mods (Mods) andalso reload_mods (Mods) of
		true ->
			GMgr ! {code_switch_players},
			true;
		false ->
			false
	end.

%}}}
gc_reloadobjects (GMgr) -> %{{{
	Mods = [game_object],
	case purge_mods (Mods) andalso compile_mods (Mods) andalso reload_mods (Mods) of
		true ->
			GMgr ! {code_switch_objects},
			true;
		false ->
			false
	end.

%}}}

%}}}
%{{{  game_console (GMgr): game's superuser console.
% This expects to interact with the given `GMgr' (game_manager) server.
game_console (GMgr) ->
	CmdTab = ets:new (console_commands, [ordered_set, private]),

	% populate with some functions.  The structure is this:
	% {{cmd_atom, arity}, fun}
	ets:insert (CmdTab, {{help, 0}, fun gc_help/1}),
	ets:insert (CmdTab, {{quit, 0}, fun gc_quit/1}),
	ets:insert (CmdTab, {{status, 0}, fun gc_status/1}),
	ets:insert (CmdTab, {{spobj, 2}, fun gc_spawnobj/3}),
	ets:insert (CmdTab, {{spdobj, 3}, fun gc_spawndobj/4}),
	ets:insert (CmdTab, {{spdoor, 3}, fun gc_spawndoorobj/4}),
	ets:insert (CmdTab, {{sproom, 2}, fun gc_spawnlocn/3}),
	ets:insert (CmdTab, {{spbot, 6}, fun gc_spawnbot/7}),
	ets:insert (CmdTab, {{link, 3}, fun gc_linklocn/4}),
	ets:insert (CmdTab, {{reloadbots, 0}, fun gc_reloadbots/1}),
	ets:insert (CmdTab, {{reloadplayers, 0}, fun gc_reloadplayers/1}),
	ets:insert (CmdTab, {{reloadobjects, 0}, fun gc_reloadobjects/1}),
	ets:insert (CmdTab, {{sphealth, 0}, fun gc_spawnhealth/1}),

	% create first room: the construct (default for abandoned people/objects/etc.)
	gc_spawnlocn (GMgr, "construct", "0"),
	game_script_run ("data/SCRIPT", GMgr, CmdTab),
	game_console_run (GMgr, CmdTab).


% read and process the named script file.
game_script_run (FName, GMgr, CmdTab) ->
	case file:open (FName, read) of
		{ok, Han} ->
			game_read_process_script (Han, GMgr, CmdTab, 1),
			file:close (Han),
			true;
		{error, Reason} ->
			io:format ("game_script_run(): failed to open script file ~s because: ~p~n", [FName, Reason]),
			false
	end.


% gets and processes a single line of input (script).
game_read_process_script (FHan, GMgr, CmdTab, LineNo) ->
	case io:get_line (FHan, '') of
		eof -> true;
		Line ->
			SLine = string:strip (string:strip (Line, both, $\n), both, $ ),
			SLen = length (SLine),

			if (SLen == 0) ->
				% ignore blank line
				true;
			true ->
				Xs = string:tokens (SLine, " \t"),
				XsLen = length (Xs),

				if (XsLen == 0) ->
					% nothing after tokenising, ignore
					true;
				true ->
					First = list_to_atom (hd (Xs)),
					Cmds = ets:lookup (CmdTab, {First, XsLen-1}),
					case length (Cmds) of
						0 ->
							io:format ("unrecognised command [~p] in script line ~p~n", [First, LineNo]);
						1 ->
							{_, F} = hd (Cmds),
							apply (F, [GMgr | tl (Xs)])
					end
				end
			end,

			game_read_process_script (FHan, GMgr, CmdTab, LineNo + 1)
	end.


% interactive console loop.
game_console_run (GMgr, CmdTab) ->
	X = io:get_line ("ErlangAdventure> "),
	Xs = string:tokens (string:strip (X, both, $\n), " \t"),
	XsLen = length (Xs),

	if (XsLen == 0) ->
		game_console_run (GMgr, CmdTab);
	true ->
		First = list_to_atom (hd (Xs)),
		Cmds = ets:lookup (CmdTab, {First, XsLen-1}),
		case length (Cmds) of
			0 ->
				io:format ("unrecognised command: ~p~n", [First]);
			1 ->
				{_, F} = hd (Cmds),
				apply (F, [GMgr | tl (Xs)])
		end,
		game_console_run (GMgr, CmdTab)
	end.

%}}}
%{{{  game_healthgen (GMgr): random healthy-thing object generator.
%
game_healthgen (GMgr) ->
	random:seed (now ()),
	game_healthgen2 (GMgr).

game_healthgen2 (GMgr) ->
	timer:sleep ((random:uniform (10) + 10) * 5000),		% wait 50 upwards seconds.

	GMgr ! {random_locn, self ()},					% get random location
	ILocn = receive {locn, GMgr, L} -> L end,

	{OName, OPid} = case random:uniform (3) of
		1 -> {"bread", game_object_create (GMgr, "bread", [])};
		2 -> {"cookies", game_object_create (GMgr, "cookies", [])};
		3 -> {"ribena", game_object_create (GMgr, "ribena", [])}
	end,

	% io:format ("game_healthgen2(): dropping new object ~s (~p) in location ~p..~n", [OName, OPid, ILocn]),

	ILocn ! {drop, OName, OPid},

	game_healthgen2 (GMgr).
%}}}


%{{{  game_run (): start the game.
% This creates the necessary infrastructure and starts the console.
%
game_run () ->
	GMgrPid = spawn_link (?MODULE, game_manager, []),
	case game_tcp_start (GMgrPid, 4040) of
		true -> game_console (GMgrPid);
		false ->
			io:format ("game_run(): failed to start TCP server, giving up..~n"),
			exit (die)
	end.


%}}}

