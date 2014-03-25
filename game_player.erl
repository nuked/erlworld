% game_player.erl -- game player processes
% Copyright (C) 2014 Fred Barnes, University of Kent <frmb@kent.ac.uk>
% GPL 2

-module (game_player).
-export ([game_player/5, game_player_create/3, game_player_create/4, game_player_run/6]).


%{{{  game_player_create (...): creates a new abstract player process.  Returns {ok, Pid} | {error, Reason}.
% `IPid' is the PID of the implementation (e.g. TCP thing or bot).
% 

game_player_create (GMgr, PName, IPid) ->
	% by default, players end up in location 0.
	game_player_create (GMgr, PName, IPid, 0).

game_player_create (GMgr, PName, IPid, ILocn) ->
	% create new `game_player' process.
	io:format ("game_player_create(): creating new player..~n"),

	PlayPid = spawn_link (?MODULE, game_player, [GMgr, PName, IPid, ILocn, self()]),

	% then wait for it to respond.
	receive
		{player_fail, PlayPid} ->
			io:format ("game_player_create(): failed to create player..~n"),
			{error, "Failed to create player in game"};
		{player_start, PlayPid} ->
			io:format ("game_player_create(): created player!~n"),
			{ok, PlayPid}
	end.

%}}}


%{{{  game_player (GMgr, PName, IPid, ILocn, PPid): main game player code.
%
% Note: the object table (OTab) is pairs of {Name, Pid}.

game_player (GMgr, PName, IPid, ILocn, PPid) ->
	OTab = ets:new (player_objects, [bag, private]),

	% register with the game at this point.
	GMgr ! {register_player, PName, self()},
	receive
		{registered_player, GMgr, _} -> true;
		{error, GMgr, Msg} ->
			io:format ("game_player(): failed to register player ~w: ~s~n", [PName, Msg]),
			PPid ! {player_fail, self()},
			exit (normal)				% abandon ourselves, tables will be freed etc.
	end,

	GMgr ! {lookup_locn, ILocn, self()},
	SetLocn = receive
		{locn, GMgr, InLocn} -> InLocn;
		{error, GMgr, Msg2} ->
			io:format ("game_player(): failed to find location ~p!: ~s~n", [ILocn, Msg2]),
			PPid ! {player_fail, self()},
			exit (normal)
	end,

	% sensible player, tell parent
	PPid ! {player_start, self()},

	% now put the player in the room (fabricated `enter' message).
	SetLocn ! {enter, {PName, self()}},

	game_player_run (GMgr, PName, IPid, OTab, ILocn, SetLocn).


game_player_run (GMgr, PName, IPid, OTab, InLNum, InLocn) ->
	receive
		code_switch ->
			%{{{  switching code implementation
			io:format ("game_player_run(): for ~p, telling impl. to switch and going ourselves..~n", [PName]),
			IPid ! code_switch,
			game_player:game_player_run (GMgr, PName, IPid, OTab, InLNum, InLocn);
			%}}}
		{player_quit, IPid} ->
			%{{{  message from implementation (human or bot) about quitting
			% disconnect from current location and unregister from game.
			game_player_disc (GMgr, PName, IPid, OTab, InLNum, InLocn);

			%}}}
		{entered, NewLNum, NewLocn} ->
			%{{{  message from location saying we entered it (possibly same location).
			IPid ! {entered, NewLNum, NewLocn},
			game_player_run (GMgr, PName, IPid, OTab, NewLNum, NewLocn);
			%}}}
		{curlocn, Pid} ->
			%{{{  something asking where this player is (could be the implementation for a look)
			Pid ! {curlocn, self(), InLNum, InLocn},
			game_player_run (GMgr, PName, IPid, OTab, InLNum, InLocn);
			%}}}
		{exit_appearing, Direction} ->
			%{{{  an exit appearing in a specific direction
			IPid ! {exit_appearing, self(), Direction},
			game_player_run (GMgr, PName, IPid, OTab, InLNum, InLocn);
			%}}}
		{move, IPid, Direction} ->
			%{{{  implementation telling us we should move
			InLocn ! {do_move, PName, self (), Direction},
			receive
				{cannot_move, InLocn} ->
					IPid ! {cannot_move, self ()};
				{moving, InLocn} ->
					IPid ! {moving, self ()}
			end,
			game_player_run (GMgr, PName, IPid, OTab, InLNum, InLocn);
			%}}}
		{person_entering, OName} ->
			%{{{  someone entering the room
			IPid ! {person_entering, OName},
			game_player_run (GMgr, PName, IPid, OTab, InLNum, InLocn);
			%}}}
		{person_leaving, OName} ->
			%{{{  someone being deleted from the room
			IPid ! {person_leaving, OName},
			game_player_run (GMgr, PName, IPid, OTab, InLNum, InLocn);
			%}}}
		{person_leaving_exit, OName, Direction} ->
			%{{{  someone leaving the room in a specific direction
			IPid ! {person_leaving_exit, OName, Direction},
			game_player_run (GMgr, PName, IPid, OTab, InLNum, InLocn);
			%}}}
		{person_leaving_noexit, OName} ->
			%{{{  someone being removed from the room
			IPid ! {person_leaving_noexit, OName},
			game_player_run (GMgr, PName, IPid, OTab, InLNum, InLocn);
			%}}}
		{player_drop_object, Name, OName} ->
			%{{{  player dropping an object, forward.
			IPid ! {player_drop_object, Name, OName},
			game_player_run (GMgr, PName, IPid, OTab, InLNum, InLocn);
			%}}}
		{player_picked_up, Name, OName} ->
			%{{{  player picking an object up, forward.
			IPid ! {player_picked_up, Name, OName},
			game_player_run (GMgr, PName, IPid, OTab, InLNum, InLocn);
			%}}}
		{object_appear, OName} ->
			%{{{  object appearing (probably spawned).
			IPid ! {object_appear, OName},
			game_player_run (GMgr, PName, IPid, OTab, InLNum, InLocn);
			%}}}
		{say, Who, What} ->
			%{{{  someone saying something (could be us).
			IPid ! {say, Who, What},
			game_player_run (GMgr, PName, IPid, OTab, InLNum, InLocn);
			%}}}
		{action, Who, What} ->
			%{{{  someone doing something (could be us).
			IPid ! {action, Who, What},
			game_player_run (GMgr, PName, IPid, OTab, InLNum, InLocn);
			%}}}
		{inv_look, Pid} ->
			%{{{  something looking in the player's inventory.
			Pid ! {inventory, ets:foldl (fun ({I,_},L) -> L ++ [I] end, [], OTab)},
			game_player_run (GMgr, PName, IPid, OTab, InLNum, InLocn);
			%}}}
		{do_examine, OName, Pid} ->
			%{{{  examine an object: either held by player (pref) or in the room.  Responds to `Pid'.
			OObjs = ets:lookup (OTab, OName),
			case OObjs of
				[] ->
					% none locally, check room -- this responds directly to the process that asked.
					InLocn ! {examine, OName, Pid};
				[{_,OPid}|_] ->
					% at least one locally, check it -- this responds directly to the process that asked.
					OPid ! {examine, Pid}
			end,
			game_player_run (GMgr, PName, IPid, OTab, InLNum, InLocn);
			%}}}
		{do_pickup, OName, Pid} ->
			%{{{  pickup an object: in the room.  Responds to `Pid'.
			InLocn ! {do_pickup, PName, OName, self ()},
			receive
				{picked_up, ONm, OPid} ->
					% add it
					ets:insert (OTab, {ONm, OPid}),
					Pid ! {picked_up, ONm, OPid};
				{pickup_fail, Msg} ->
					Pid ! {pickup_fail, Msg}
			end,
			game_player_run (GMgr, PName, IPid, OTab, InLNum, InLocn);
			%}}}
		{do_drop, OName, Pid} ->
			%{{{  drop an object in the room.  Responds to `Pid'.
			OObjs = ets:lookup (OTab, OName),
			case OObjs of
				[] ->
					Pid ! {drop_fail, "Not carrying that!"};
				[{ONm,OPid}|_] ->
					ets:delete_object (OTab, {ONm, OPid}),
					InLocn ! {player_drop, PName, ONm, OPid},
					Pid ! {dropped, ONm}
			end,
			game_player_run (GMgr, PName, IPid, OTab, InLNum, InLocn);
			%}}}
		{do_use, OName, Pid} ->
			%{{{  use something (us using it).  Responds to `Pid'.
			InLocn ! {do_use, PName, OName, self ()},
			receive
				{use_ok} -> Pid ! {use_ok};
				{use_noobj, M} -> Pid ! {use_noobj, M};
				{use_fail, M} -> Pid ! {use_fail, M};
				{use_in_player, OPid} ->
					% means the object interacts directly with us.
					OPid ! {use_in_player, PName, self(), self ()},
					receive
						{use_fail, M} -> Pid ! {use_fail, M};
						{use_movetolocn, NewLNum, NewLocn} ->
							% extract from current location
							InLocn ! {leave_noexit, PName},
							% place in new
							NewLocn ! {enter, {PName, self ()}},
							Pid ! {use_ok}
							% Note: when this loops, we'll get an 'entered' for the new location.
					end
			end,
			game_player_run (GMgr, PName, IPid, OTab, InLNum, InLocn);
			%}}}
		{do_say, Who, What} ->
			%{{{  say something (us saying it).
			InLocn ! {do_say, Who, What},
			game_player_run (GMgr, PName, IPid, OTab, InLNum, InLocn);
			%}}}
		{do_action, Who, What} ->
			%{{{  do something (us doing it).
			InLocn ! {do_action, Who, What},
			game_player_run (GMgr, PName, IPid, OTab, InLNum, InLocn);
			%}}}
		{examine, Pid} ->
			%{{{  being examined, responds to `Pid' (could be self!).
			Pid ! {examine_resist, "oi!"},
			game_player_run (GMgr, PName, IPid, OTab, InLNum, InLocn);
			%}}}
		Other ->
			io:format ("game_player_run(): got unhandled message: ~p~n", [Other]),
			game_player_run (GMgr, PName, IPid, OTab, InLNum, InLocn)
	end.


game_player_disc (GMgr, PName, IPid, OTab, InLNum, InLocn) ->
	ets:foldl (fun ({OName, OPid}, _) ->
			InLocn ! {player_drop, PName, OName, OPid},
			true
		end, true, OTab),
	ets:delete (OTab),
	InLocn ! {leave, PName},
	game_player_disc_soak (GMgr, PName, IPid, InLNum, InLocn).


game_player_disc_soak (GMgr, PName, IPid, InLNum, InLocn) ->
	receive
		{left, _} ->
			game_player_disc_done (GMgr, PName, IPid);
		_Other ->
			game_player_disc_soak (GMgr, PName, IPid, InLNum, InLocn)
	end.


game_player_disc_done (GMgr, PName, IPid) ->
	GMgr ! {unregister_player, PName, self ()},
	receive {unregistered_player, GMgr, _} ->
		io:format ("game_player_disc_done(): player (~p) is done.~n", [PName]),
		exit (normal)
	end.

%}}}

