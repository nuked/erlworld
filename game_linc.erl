% game_linc.erl -- specific LINC location stuff.
% Copyright (C) 2014 Fred Barnes, University of Kent <frmb@kent.ac.uk>
% GPL 2

-module (game_linc).
-export ([game_linc/3, game_linc_run/4, game_linc_create/2]).

%{{{  game_linc_create (GMgr, LNum): create new linc location
% returns Pid of newly created LINC location (that will register itself).

game_linc_create (GMgr, LNum) ->
	LPid = spawn_link (?MODULE, game_linc, [GMgr, LNum, self()]),

	% wait for registration
	receive
		{location_start, LPid} -> LPid;
		{location_fail, LPid} -> false
	end.


%}}}
%{{{  game_linc (GMgr, LNum, PPid): game LINC system process.
%
% Note the ETS table that holds people is tuples of the form:
%  {PName, PlrPid, {OrigLNum, LincLocn}}

game_linc (GMgr, LNum, PPid) ->
	PTab = ets:new (linc_people, [ordered_set, private]),

	% register with the game.
	GMgr ! {register_locn, LNum, self()},
	receive
		{registered_locn, GMgr, _} -> true;
		{error, GMgr, Msg} ->
			io:format ("game_linc(): failed to register location ~w: ~s~n", [LNum, Msg]),
			PPid ! {location_fail, self()},
			exit (normal)				% abandon ourselves, tables will be freed etc.
	end,

	% sensible room, tell parent
	PPid ! {location_start, self()},

	GMgr ! {lookup_locn, 0, self()},
	DefLPid = receive {locn, GMgr, X} -> X end,

	game_linc_run (GMgr, LNum, PTab, DefLPid).

linc_lmenu (Locn) ->
	case Locn of
	0 ->
		[{desc, " 1 - city operations"}];
	1 ->
		[{desc, " 9 - back"}];
	_ ->
		[]
	end.

linc_ldesc (0) -> "top-level";
linc_ldesc (1) -> "city operations";
linc_ldesc (_) -> "unauthorised access detected".

linc_desc (Locn) ->
	[{desc, ""},
	 {highlight, "||     ++  ,+===+.  ,+=== "},
	 {highlight, "||     ||  ||   ||  ||    "},
	 {highlight, "||     ||  ||   ||  ||    "},
	 {highlight, "||     ||  ||   ||  ||    "},
	 {highlight, "`+===  ||  ||   ||  `+=== "},
	 {highlight, "Be vigilant player 1"},
	 {desc, ""},
	 {desc, io_lib:format ("System page: ~w (~s)", [Locn, linc_ldesc (Locn)])},
	 {desc, ""},
	 {desc, " 0 - disconnect from linc"}] ++
	linc_lmenu (Locn) ++
	[{desc, ""}].

game_linc_run (GMgr, LNum, PTab, DefLPid) ->
	receive
		code_switch -> %{{{  switch code
			io:format ("game_linc_run(): location ~p switching code..~n", [LNum]),
			ets:foldl (fun ({_, P, _}, _) -> P ! {message, "LINC undergoes an upgrade."}, true end, 0, PTab),
			game_linc:game_linc_run (GMgr, LNum, PTab, DefLPid);
			%}}}
		{enter, PrevLNum, {Name, PPid}} -> %{{{  player entering the room, send notifications to those already here and add player
			ets:foldl (fun ({_, P, _}, _) -> P ! {person_entering, Name}, true end, 0, PTab),
			ets:insert (PTab, {Name, PPid, {PrevLNum, 0}}),
			PPid ! {entered, LNum, self ()},
			game_linc_run (GMgr, LNum, PTab, DefLPid);
			%}}}
		{leave, Name} -> %{{{  player leaving the game (rather than leaving the room) -- perhaps forced.  Reponds to player.
			[{_, PPid}] = ets:lookup (PTab, Name),
			ets:delete (PTab, Name),
			ets:foldl (fun ({_, P, _}, _) -> P ! {person_leaving, Name}, true end, horses, PTab),
			PPid ! {left, LNum},
			game_linc_run (GMgr, LNum, PTab, DefLPid);
			%}}}
		{leave_noexit, Name} -> %{{{  player being removed from the room
			% [{_, PPid}] = ets:lookup (PTab, Name),
			ets:delete (PTab, Name),
			ets:foldl (fun ({_, P, _}, _) -> P ! {person_leaving_noexit, Name}, true end, true, PTab),
			game_linc_run (GMgr, LNum, PTab, DefLPid);
			%}}}
		{leave_death, Name} -> %{{{  player being removed from the room (because they died).  Reponds to player.
			[{_, PPid}] = ets:lookup (PTab, Name),
			ets:delete (PTab, Name),
			ets:foldl (fun ({_, P, _}, _) -> P ! {person_leaving_death, Name}, true end, true, PTab),
			PPid ! {left_death, LNum},
			game_linc_run (GMgr, LNum, PTab, DefLPid);
		%}}}
		{look, PlrPid, Pid} -> %{{{  player (or bot) looking around.
			People = ets:foldl (fun ({NN, NP, _}, L) -> [{NN, NP} | L] end, [], PTab),
			Me = ets:foldl (fun (E = {_, NP, _}, L) -> if (NP == PlrPid) -> E; true -> L end end, undefined, PTab),
			{_, _, {_, LincLocn}} = Me,
			Desc = linc_desc (LincLocn),
			Pid ! {looked, Desc, [], People, [none,none,none,none]},
			game_linc_run (GMgr, LNum, PTab, DefLPid);
			%}}}
		{set_exit, _Pid, Exit, LocnPid} -> %{{{  connecting up one of the exits, or disconnecting it (no-op).
			game_linc_run (GMgr, LNum, PTab, DefLPid);
			%}}}
		{do_move, Name, Pid, Direction} -> %{{{  player wanting to move in a particular direction -- `Pid' will be waiting for response.
			Pid ! {cannot_move, self ()},
			game_linc_run (GMgr, LNum, PTab, DefLPid);
			%}}}
		{player_drop, PName, OName, OPid} -> %{{{  player dropping an object in the room (no response to player).
			% cannot drop here, send onto default location.
			DefLPid ! {drop, OName, OPid},
			game_linc_run (GMgr, LNum, PTab, DefLPid);
			%}}}
		{drop, OName, OPid} -> %{{{  miscellaneous appearance of an object (probably spawned here).
			DefLPid ! {drop, OName, OPid},
			game_linc_run (GMgr, LNum, PTab, DefLPid);
			%}}}
		{examine, OName, Pid} -> %{{{  examine object/person in the room (if it exists!).  Responds to `Pid'.
			% try people?
			People = ets:lookup (PTab, OName),
			case People of
				[] ->
					Pid ! {examine_fail, "No such object/person"};
				[{_, PP, _}|_] ->
					PP ! {examine, Pid}
			end,
			game_linc_run (GMgr, LNum, PTab, DefLPid);
			%}}}
		{do_pickup, PName, OName, Pid} -> %{{{  pickup an object in the room, if it exists.  Responds to `Pid'.
			Pid ! {pickup_fail, "No such object"},
			game_linc_run (GMgr, LNum, PTab, DefLPid);
			%}}}
		{do_attack, PName, Who, What, Damage, Pid} -> %{{{  player `PName' performing an attack (on `Who' with `What' inflicting `Damage').  Reponds to `Pid'.
			Pid ! {attack_fail, "Cannot do combat here!"},
			game_linc_run (GMgr, LNum, PTab, DefLPid);
			%}}}
		{do_say, Who, What} -> %{{{  player saying something (no response) -- handles LINC interactions too.
			% we use this as a way of interacting with LINC
			Me = case ets:lookup (PTab, Who) of
				[] -> undefined;
				[X|_] -> X
			end,
			case Me of
			undefined ->
				% don't know who this person is, skip!
				true;
			{PName, PPid, {PrevLNum, LincLocn}} ->
				SelNum = try list_to_integer (What)
						catch error: Y -> -1
						end,
				case {LincLocn, SelNum} of
				{_, -1} ->
					% just broadcast out to anyone in the room
					ets:foldl (fun ({_, P, _}, _) -> P ! {say, Who, What}, true end, true, PTab);
				{_, 0} ->
					% generic disconnect
					ets:delete (PTab, PName),
					ets:foldl (fun ({_, P, _}, _) -> P ! {action, Who, "disconnected from LINC"}, true end, true, PTab),
					% find out where player came from, default to DefLPid
					GMgr ! {lookup_locn, PrevLNum, self()},
					LocnPid = receive
							{locn, GMgr, L} -> L;
							_ -> DefLPid
						end,
					LocnPid ! {enter, LNum, {PName, PPid}};
				{0, 1} ->
					% selecting city operations
					ets:insert (PTab, {PName, PPid, {PrevLNum, 1}}),
					PPid ! {entered, LNum, self()};
				{1, 9} ->
					% back to main menu
					ets:insert (PTab, {PName, PPid, {PrevLNum, 0}}),
					PPid ! {entered, LNum, self()};
				{_, _} ->
					% unhandled, do nothing
					true
				end
			end,
			game_linc_run (GMgr, LNum, PTab, DefLPid);
			%}}}
		{do_action, Who, What} -> %{{{  player (bot) doing something, no response.
			% just broadcast out to anyone in the room
			ets:foldl (fun ({_, P, _}, _) -> P ! {action, Who, What}, true end, true, PTab),
			game_linc_run (GMgr, LNum, PTab, DefLPid);
			%}}}
		{do_use, PName, OName, Pid} -> %{{{  player using an object in the room.  Responds to `Pid'.
			Pid ! {use_noobj, "No such object"},
			game_linc_run (GMgr, LNum, PTab, DefLPid);
			%}}}
		Other ->
			io:format ("game_linc_run(): got unhandled message: ~p~n", [Other]),
			game_linc_run (GMgr, LNum, PTab, DefLPid)
	end.

%}}}


