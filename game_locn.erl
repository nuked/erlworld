% game_locn.erl -- game location stuff.
% Copyright (C) 2014 Fred Barnes, University of Kent <frmb@kent.ac.uk>
% GPL 2

-module (game_locn).
-export ([game_locn/4, game_locn_create/3, game_locn_run/6]).


%{{{  game_locn_create (GMgr, Name, LNum): create new location.
% returns Pid of the newly created location (that will register itself).

game_locn_create (GMgr, Name, LNum) ->
	DFName = "data/" ++ Name ++ ".desc",
	case filelib:is_file (DFName) of
		true ->
			% open descriptor file
			case file:open (DFName, read) of
				{ok, Han} ->
					Desc = game_read_desc (Han, []),
					file:close (Han),

					% io:format ("game_locn_create(): created location [~s] desc: ~p~n", [Name, Desc]),

					LPid = spawn_link (?MODULE, game_locn, [GMgr, LNum, Desc, self ()]),

					% wait for it to register itself
					receive
						{location_start, LPid} -> LPid;		% result is Pid of new location
						{location_fail, LPid} -> false
					end;
				{error, Reason} ->
					io:format ("game_locn_Create(): cannot opsn ~s: ~p~n", [DFName, Reason]),
					false
			end;
		false ->
			io:format ("game_locn_create(): no such file ~s~n", [DFName]),
			false
	end.


game_read_desc (FHan, SoFar) ->
	case io:get_line (FHan, '') of
		eof -> SoFar;
		Line ->
			SLine = string:strip (string:strip (Line, both, $\n), both, $ ),
			SLen = length (SLine),

			if (SLen == 0) ->
				game_read_desc (FHan, SoFar ++ [{desc, SLine}]);
			true ->
				FC = hd (SLine),
				if (FC == $*) ->
					game_read_desc (FHan, SoFar ++ [{highlight, tl (SLine)}]);
				true ->
					game_read_desc (FHan, SoFar ++ [{desc, SLine}])
				end
			end
	end.


%}}}

%{{{  game_locn (GMgr, LNum, Desc, PPid): game location process.
%
% objects are held as they are in game_manager: bag of {Name, Pid} tuples.

game_locn (GMgr, LNum, Desc, PPid) ->
	PTab = ets:new (locn_people, [ordered_set, private]),
	OTab = ets:new (locn_objects, [bag, private]),

	% register with the game at this point (now we've been mostly created).
	GMgr ! {register_locn, LNum, self()},
	receive
		{registered_locn, GMgr, _} -> true;
		{error, GMgr, Msg} ->
			io:format ("game_locn(): failed to register location ~w: ~s~n", [LNum, Msg]),
			PPid ! {location_fail, self()},
			exit (normal)				% abandon ourselves, tables will be freed etc.
	end,

	% sensible room, tell parent process
	PPid ! {location_start, self()},

	game_locn_run (GMgr, LNum, Desc, PTab, OTab, [none, none, none, none]).


game_locn_run (GMgr, LNum, Desc, PTab, OTab, Exits) ->
	receive
		code_switch -> %{{{  switch code
			io:format ("game_locn_run(): location ~p switching code..~n", [LNum]),
			game_locn:game_locn_run (GMgr, LNum, Desc, PTab, OTab, Exits);
			%}}}
		{enter, {Name, PPid}} -> %{{{  player entering the room, send notifications to those already here and add player
			ets:foldl (fun ({_, P}, _) -> P ! {person_entering, Name}, true end, 0, PTab),
			ets:insert (PTab, {Name, PPid}),
			PPid ! {entered, LNum, self ()},
			game_locn_run (GMgr, LNum, Desc, PTab, OTab, Exits);
			%}}}
		{leave, Name} -> %{{{  player leaving the game (rather than leaving the room) -- perhaps forced
			[{_, PPid}] = ets:lookup (PTab, Name),
			ets:delete (PTab, Name),
			ets:foldl (fun ({_, P}, _) -> P ! {person_leaving, Name}, true end, horses, PTab),
			PPid ! {left, LNum},
			game_locn_run (GMgr, LNum, Desc, PTab, OTab, Exits);
			%}}}
		{leave_noexit, Name} -> %{{{  player being removed from teh room
			[{_, PPid}] = ets:lookup (PTab, Name),
			ets:delete (PTab, Name),
			ets:foldl (fun ({_, P}, _) -> P ! {person_leaving_noexit, Name}, true end, true, PTab),
			game_locn_run (GMgr, LNum, Desc, PTab, OTab, Exits);
			%}}}
		{look, Pid} -> %{{{  player (or bot) looking around.
			Objects = ets:foldl (fun ({Name, Pid}, L) -> [Name | L] end, [], OTab),
			People = ets:foldl (fun (NamePid, L) -> [NamePid | L] end, [], PTab),

			Pid ! {looked, Desc, Objects, People, Exits},
			game_locn_run (GMgr, LNum, Desc, PTab, OTab, Exits);
			%}}}
		{set_exit, Pid, Exit, LocnPid} -> %{{{  connecting up one of the exits, or disconnecting it.
			[N,E,S,W] = Exits,
			{Cur, NewExits} = case Exit of
				north -> {N, [LocnPid, E, S, W]};
				east -> {E, [N, LocnPid, S, W]};
				south -> {S, [N, E, LocnPid, W]};
				west -> {W, [N, E, S, LocnPid]}
			end,
			case {Cur,LocnPid} of
				{none, none} ->
					% nothing changed! (still disconnected)
					true;
				{_, none} ->
					ets:foldl (fun ({_, P}, _) -> P ! {exit_vanishing, Exit}, true end, 0, PTab);
				{none, _} ->
					ets:foldl (fun ({_, P}, _) -> P ! {exit_appearing, Exit}, true end, 0, PTab);
				{_, _} ->
					ets:foldl (fun ({_, P}, _) -> P ! {exit_changing, Exit}, true end, 0, PTab)
			end,
			game_locn_run (GMgr, LNum, Desc, PTab, OTab, NewExits);
			%}}}
		{do_move, Name, Pid, Direction} -> %{{{  player wanting to move in a particular direction -- `Pid' will be waiting for response.
			[N,E,S,W] = Exits,
			X = case Direction of
				north -> N;
				east -> E;
				south -> S;
				west -> W
			end,
			case X of
				none ->
					Pid ! {cannot_move, self ()};
				NextUp ->
					% remove from this room, enter into next
					ets:delete (PTab, Name),
					ets:foldl (fun ({_, P}, _) -> P ! {person_leaving_exit, Name, Direction}, true end, 0, PTab),

					Pid ! {moving, self ()},
					NextUp ! {enter, {Name, Pid}}
			end,
			game_locn_run (GMgr, LNum, Desc, PTab, OTab, Exits);
			%}}}
		{player_drop, PName, OName, OPid} -> %{{{  player dropping an object in the room (no response to player).
			ets:insert (OTab, {OName, OPid}),
			ets:foldl (fun ({_, P}, _) -> P ! {player_drop_object, PName, OName}, true end, true, PTab),
			game_locn_run (GMgr, LNum, Desc, PTab, OTab, Exits);
			%}}}
		{drop, OName, OPid} -> %{{{  miscellaneous appearance of an object (probably spawned here).
			ets:insert (OTab, {OName, OPid}),
			ets:foldl (fun ({_, P}, _) -> P ! {object_appear, OName}, true end, true, PTab),
			game_locn_run (GMgr, LNum, Desc, PTab, OTab, Exits);
			%}}}
		{examine, OName, Pid} -> %{{{  examine object in the room (if it exists!).  Responds to `Pid'.
			OObjs = ets:lookup (OTab, OName),
			case OObjs of
				[] ->
					% try people?
					People = ets:lookup (PTab, OName),
					case People of
						[] ->
							Pid ! {examine_fail, "No such object/person"};
						[{_, PP}|_] ->
							PP ! {examine, Pid}
					end;
				[{_,OPid}|_] ->
					OPid ! {examine, Pid}
			end,
			game_locn_run (GMgr, LNum, Desc, PTab, OTab, Exits);
			%}}}
		{do_pickup, PName, OName, Pid} -> %{{{  pickup an object in the room, if it exists.  Responds to `Pid'.
			OObjs = ets:lookup (OTab, OName),
			case OObjs of
				[] ->
					% not here.
					Pid ! {pickup_fail, "No such object"};
				[{ONm,OPid}|_] ->
					% check if object can be picked up..
					OPid ! {can_pickup, self ()},
					CanPick = receive {can_pickup, OPid, CP} -> CP end,
					if (CanPick == false) ->
						Pid ! {pickup_fail, "Cannot pick that up"};
					true ->
						ets:delete_object (OTab, {ONm, OPid}),
						Pid ! {picked_up, ONm, OPid},
						% tell anyone else about this.
						ets:foldl (fun ({N, P}, _) ->
								if (N == PName) -> true;
								true -> P ! {player_picked_up, PName, ONm}, true
								end
							end, true, PTab)
					end
			end,
			game_locn_run (GMgr, LNum, Desc, PTab, OTab, Exits);
			%}}}
		{do_say, Who, What} -> %{{{  player saying something (no response).
			% just broadcast out to anyone in the room
			ets:foldl (fun ({_, P}, _) -> P ! {say, Who, What}, true end, true, PTab),
			game_locn_run (GMgr, LNum, Desc, PTab, OTab, Exits);
			%}}}
		{do_action, Who, What} -> %{{{  player (bot) doing something, no response.
			% just broadcast out to anyone in the room
			ets:foldl (fun ({_, P}, _) -> P ! {action, Who, What}, true end, true, PTab),
			game_locn_run (GMgr, LNum, Desc, PTab, OTab, Exits);
			%}}}
		{do_use, PName, OName, Pid} -> %{{{  player using an object in the room.  Responds to `Pid'.
			OObjs = ets:lookup (OTab, OName),
			case OObjs of
				[] ->
					% not here.
					Pid ! {use_noobj, "No such object"};
				[{ONm, OPid}|_] ->
					% try this one..
					OPid ! {use_type, self ()},
					UType = receive {can_use, OPid, U} -> U end,
					case UType of
						use_none ->
							Pid ! {use_fail, "Cannot use that"};
						use_inlocn ->
							OPid ! {use_in_locn, LNum, self (), self ()},
							Used = receive {use_fail, M} -> {use_fail, M}; {use_ok} -> {use_ok} end,
							Pid ! Used;
						use_inplayer ->
							Pid ! {use_in_player, OPid};
						use_eat ->
							OPid ! {get_attr, health, self ()},
							H = receive {attr, health, HX} -> HX end,
							ets:delete_object (OTab, {ONm, OPid}),
							Pid ! {use_ate, ONm, H},
							OPid ! {destroy},			% trash object.
							% tell anyone else about this.
							ets:foldl (fun ({N, P}, _) ->
									if (N == PName) -> true;
									true -> P ! {player_ate, PName, ONm}, true
									end
								end, true, PTab)
					end
			end,
			game_locn_run (GMgr, LNum, Desc, PTab, OTab, Exits);
			%}}}
		Other ->
			io:format ("game_locn_run(): got unhandled message: ~p~n", [Other]),
			game_locn_run (GMgr, LNum, Desc, PTab, OTab, Exits)
	end.

%}}}

