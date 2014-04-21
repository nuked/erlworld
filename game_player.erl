% game_player.erl -- game player processes
% Copyright (C) 2014 Fred Barnes, University of Kent <frmb@kent.ac.uk>
% GPL 2

-module (game_player).
-export ([game_player/6, game_player_create/3, game_player_create/5, game_player_run/7]).
-import (game_util, [maybeadd_attr/2, maybeset_attr/2, lookup_attr/2, maybedel_attr/2]).


%{{{  game_player_create (...): creates a new abstract player process.  Returns {ok, Pid} | {error, Reason}.
% `IPid' is the PID of the implementation (e.g. TCP thing or bot).
% 

game_player_create (GMgr, PName, IPid) ->
	% by default, players end up in location 0.
	game_player_create (GMgr, PName, IPid, 0,
		[{health, 100}, {vitality, 100}]).

game_player_create (GMgr, PName, IPid, ILocn, Attrs) ->
	% create new `game_player' process.
	%io:format ("game_player_create(): creating new player..~n"),

	PlayPid = spawn_link (?MODULE, game_player, [GMgr, PName, IPid, ILocn, self(), checkattrs (Attrs)]),

	% then wait for it to respond.
	receive
		{player_fail, PlayPid} ->
			%io:format ("game_player_create(): failed to create player..~n"),
			{error, "Failed to create player in game"};
		{player_start, PlayPid} ->
			%io:format ("game_player_create(): created player!~n"),
			{ok, PlayPid}
	end.

%}}}
%{{{  checkattrs (Attrs): checks attributes -- removes duff, adds defaults if not present.

checkattrs (Attrs) ->
	A1 = lists:filter (fun valid_attr/1, Attrs),
	A2 = maybeadd_attr (A1, {health, 100}),
	maybeadd_attr (A2, {vitality, 100}).

valid_attr ({health, N}) -> is_integer (N);
valid_attr ({vitality, N}) -> is_integer (N);
valid_attr ({immortal}) -> true;
valid_attr ({wielding, _}) -> true.

%}}}
%{{{  wielding attribute handling.

% 'wielding' attribute value is a pair {ObjName, ObjPid}.

set_wield (Attrs, What) ->
	maybeset_attr (Attrs, {wielding, What}).

set_unwield (Attrs, What) ->
	case lookup_attr (Attrs, wielding) of
		{What,_} -> maybedel_attr (Attrs, wielding);
		_ -> Attrs
	end.

set_unwield (Attrs) ->
	maybedel_attr (Attrs, wielding).

%is_wielding (Attrs, What) ->
%	case lookup_attr (Attrs, wielding) of
%		{What,_} -> true;
%		_ -> false
%	end.

%}}}


%{{{  game_player (GMgr, PName, IPid, ILocn, PPid, Attrs): main game player code.
%
% Note: the object table (OTab) is pairs of {Name, Pid}.

game_player (GMgr, PName, IPid, ILocn, PPid, Attrs) ->
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
	SetLocn ! {enter, 0, {PName, self()}},

	game_player_run (GMgr, PName, IPid, OTab, ILocn, SetLocn, Attrs).


game_player_run (GMgr, PName, IPid, OTab, InLNum, InLocn, Attrs) ->
	receive
		code_switch -> %{{{  switching code implementation
			io:format ("game_player_run(): for ~p, telling impl. to switch and going ourselves..~n", [PName]),
			IPid ! code_switch,
			game_player:game_player_run (GMgr, PName, IPid, OTab, InLNum, InLocn, Attrs);
			%}}}
		{player_quit, IPid} -> %{{{  message from implementation (human or bot) about quitting
			% disconnect from current location and unregister from game.
			game_player_disc (GMgr, PName, IPid, OTab, InLNum, InLocn, Attrs);

			%}}}
		{entered, NewLNum, NewLocn} -> %{{{  message from location saying we entered it (possibly same location).
			IPid ! {entered, NewLNum, NewLocn},
			game_player_run (GMgr, PName, IPid, OTab, NewLNum, NewLocn, Attrs);
			%}}}
		{curlocn, Pid} -> %{{{  something asking where this player is (could be the implementation for a look)
			Pid ! {curlocn, self(), InLNum, InLocn},
			game_player_run (GMgr, PName, IPid, OTab, InLNum, InLocn, Attrs);
			%}}}
		{exit_appearing, Direction} -> %{{{  an exit appearing in a specific direction
			IPid ! {exit_appearing, self(), Direction},
			game_player_run (GMgr, PName, IPid, OTab, InLNum, InLocn, Attrs);
			%}}}
		{exit_vanishing, Direction} -> %{{{  an exit appearing in a specific direction
			IPid ! {exit_vanishing, self(), Direction},
			game_player_run (GMgr, PName, IPid, OTab, InLNum, InLocn, Attrs);
			%}}}
		{exit_changing, Direction} -> %{{{  an exit appearing in a specific direction
			IPid ! {exit_changing, self(), Direction},
			game_player_run (GMgr, PName, IPid, OTab, InLNum, InLocn, Attrs);
			%}}}
		{move, IPid, Direction} -> %{{{  implementation telling us we should move
			InLocn ! {do_move, PName, self (), Direction},
			receive
				{cannot_move, InLocn} ->
					IPid ! {cannot_move, self ()};
				{moving, InLocn} ->
					IPid ! {moving, self ()}
			end,
			game_player_run (GMgr, PName, IPid, OTab, InLNum, InLocn, Attrs);
			%}}}
		{person_entering, OName} -> %{{{  someone entering the room
			IPid ! {person_entering, OName},
			game_player_run (GMgr, PName, IPid, OTab, InLNum, InLocn, Attrs);
			%}}}
		{person_leaving, OName} -> %{{{  someone being deleted from the room
			IPid ! {person_leaving, OName},
			game_player_run (GMgr, PName, IPid, OTab, InLNum, InLocn, Attrs);
			%}}}
		{person_leaving_exit, OName, Direction} -> %{{{  someone leaving the room in a specific direction
			IPid ! {person_leaving_exit, OName, Direction},
			game_player_run (GMgr, PName, IPid, OTab, InLNum, InLocn, Attrs);
			%}}}
		{person_leaving_noexit, OName} -> %{{{  someone being removed from the room
			IPid ! {person_leaving_noexit, OName},
			game_player_run (GMgr, PName, IPid, OTab, InLNum, InLocn, Attrs);
			%}}}
		{person_leaving_death, OName} -> %{{{  someone dying in the room
			IPid ! {person_leaving_death, OName},
			game_player_run (GMgr, PName, IPid, OTab, InLNum, InLocn, Attrs);
			%}}}
		{player_drop_object, Name, OName} -> %{{{  player dropping an object, forward.
			IPid ! {player_drop_object, Name, OName},
			game_player_run (GMgr, PName, IPid, OTab, InLNum, InLocn, Attrs);
			%}}}
		{player_picked_up, Name, OName} -> %{{{  player picking an object up, forward.
			IPid ! {player_picked_up, Name, OName},
			game_player_run (GMgr, PName, IPid, OTab, InLNum, InLocn, Attrs);
			%}}}
		{object_appear, OName} -> %{{{  object appearing (probably spawned).
			IPid ! {object_appear, OName},
			game_player_run (GMgr, PName, IPid, OTab, InLNum, InLocn, Attrs);
			%}}}
		{say, Who, What} -> %{{{  someone saying something (could be us).
			IPid ! {say, Who, What},
			game_player_run (GMgr, PName, IPid, OTab, InLNum, InLocn, Attrs);
			%}}}
		{action, Who, What} -> %{{{  someone doing something (could be us).
			IPid ! {action, Who, What},
			game_player_run (GMgr, PName, IPid, OTab, InLNum, InLocn, Attrs);
			%}}}
		{inv_look, Pid} -> %{{{  something looking in the player's inventory.
			Pid ! {inventory, ets:foldl (fun ({I,_},L) -> L ++ [I] end, [], OTab)},
			game_player_run (GMgr, PName, IPid, OTab, InLNum, InLocn, Attrs);
			%}}}
		{qry_status, Pid} -> %{{{  something querying player status.
			Pid ! {status, lookup_attr (Attrs, health), lookup_attr (Attrs, vitality),
					lookup_attr (Attrs, wielding), lookup_attr (Attrs, immortal)},
			game_player_run (GMgr, PName, IPid, OTab, InLNum, InLocn, Attrs);
		%}}}
		{attack, Who, What, Damage, Pid} -> %{{{  being attacked by `Who' with `What' for `Damage'.  Responds to `Pid'.
			case lookup_attr (Attrs, immortal) of
				true ->
					Pid ! {attack_fail, PName ++ " is immortal!"},
					IPid ! {attack, Who, What, 0},
					game_player_run (GMgr, PName, IPid, OTab, InLNum, InLocn, Attrs);
				undefined ->
					% not immportal, take damage
					Pid ! {attack_ok, io_lib:format ("Attacked ~s using ~s, ~w points damage", [Who, What, Damage]), Damage},
					IPid ! {attack, Who, What, Damage},
					H = lookup_attr (Attrs, health),
					H2 = H - Damage,
					if (H2 < 0) ->
						% died..
						IPid ! {died},
						NewAttrs = maybeset_attr (Attrs, {health, 0}),
						game_player_deathloop (GMgr, PName, IPid, OTab, InLNum, InLocn, NewAttrs);
					true ->
						NewAttrs = maybeset_attr (Attrs, {health, H2}),
						game_player_run (GMgr, PName, IPid, OTab, InLNum, InLocn, NewAttrs)
					end
			end;
			%}}}
		{do_examine, OName, Pid} -> %{{{  examine an object: either held by player (pref) or in the room.  Responds to `Pid'.
			OObjs = ets:lookup (OTab, OName),
			case OObjs of
				[] ->
					% none locally, check room -- this responds directly to the process that asked.
					InLocn ! {examine, OName, Pid};
				[{_,OPid}|_] ->
					% at least one locally, check it -- this responds directly to the process that asked.
					OPid ! {examine, Pid}
			end,
			game_player_run (GMgr, PName, IPid, OTab, InLNum, InLocn, Attrs);
			%}}}
		{do_pickup, OName, Pid} -> %{{{  pickup an object: in the room.  Responds to `Pid'.
			InLocn ! {do_pickup, PName, OName, self ()},
			receive
				{picked_up, ONm, OPid} ->
					% add it
					ets:insert (OTab, {ONm, OPid}),
					Pid ! {picked_up, ONm, OPid};
				{pickup_fail, Msg} ->
					Pid ! {pickup_fail, Msg}
			end,
			game_player_run (GMgr, PName, IPid, OTab, InLNum, InLocn, Attrs);
			%}}}
		{do_drop, OName, Pid} -> %{{{  drop an object in the room.  Responds to `Pid'.
			OObjs = ets:lookup (OTab, OName),
			NewAttrs = case OObjs of
				[] ->
					Pid ! {drop_fail, "Not carrying that!"},
					Attrs;
				[{ONm,OPid}|_] ->
					ets:delete_object (OTab, {ONm, OPid}),
					InLocn ! {player_drop, PName, ONm, OPid},
					Pid ! {dropped, ONm},
					set_unwield (Attrs, OName)
			end,
			game_player_run (GMgr, PName, IPid, OTab, InLNum, InLocn, NewAttrs);
			%}}}
		{do_use, OName, Pid} -> %{{{  use something (us using it).  Responds to `Pid'.
			OObjs = ets:lookup (OTab, OName),
			NewAttrs = case OObjs of
				[] ->
					% none locally, check room and handle response.
					InLocn ! {do_use, PName, OName, self ()},
					receive		% result assigned to 'NewAttrs'.
						{use_ok} -> Pid ! {use_ok}, Attrs;
						{use_noobj, M} -> Pid ! {use_noobj, M}, Attrs;
						{use_fail, M} -> Pid ! {use_fail, M}, Attrs;
						{use_in_player, OPid} ->
							% means the object interacts directly with us.
							OPid ! {use_in_player, PName, InLNum, self(), self ()},
							receive
								{use_fail, M} -> Pid ! {use_fail, M};
								{use_movetolocn, _NewLNum, NewLocn} ->
									% extract from current location
									InLocn ! {leave_noexit, PName},
									% place in new
									NewLocn ! {enter, InLNum, {PName, self ()}},
									Pid ! {use_ok}
									% Note: when this loops, we'll get an 'entered' for the new location.
							end,
							Attrs;
						{use_in_player, OPid, Fcn} ->
							case Fcn (OTab) of
								true ->
									% means the object interacts directly with us.
									OPid ! {use_in_player, PName, InLNum, self(), self ()},
									receive
										{use_fail, M} -> Pid ! {use_fail, M};
										{use_movetolocn, _NewLNum, NewLocn} ->
											% extract from current location
											InLocn ! {leave_noexit, PName},
											% place in new
											NewLocn ! {enter, InLNum, {PName, self ()}},
											Pid ! {use_ok}
											% Note: when this loops, we'll get an
											% 'entered' for the new location.
									end;
								{false, X} ->
									Pid ! {use_fail, "probably need a " ++ X}
							end,
							Attrs;
						{use_ate, _Obj, Health} ->
							% means we have just eaten the object!
							CurHealth = lookup_attr (Attrs, health),
							NewHealth = CurHealth + Health,
							NHealth2 = if (NewHealth > 100) -> 100; true -> NewHealth end,
							Pid ! {use_eated, CurHealth, NHealth2},
							maybeset_attr (Attrs, {health, NHealth2})
					end;
				[{ONm,OPid}|_] ->
					% at least one locally, check it and handle response.
					OPid ! {use_type, self()},
					UType = receive {can_use, OPid, U} -> U end,
					case UType of		% result assigned to 'NewAttrs'.
						use_none ->
							Pid ! {use_fail, "Cannot use that"},
							Attrs;
						use_inlocn ->
							Pid ! {use_fail, "Cannot use that here!"},
							Attrs;
						use_inplayer ->
							% means the object interacts directly with us.
							OPid ! {use_in_player, PName, self(), self ()},
							receive
								{use_fail, M} -> Pid ! {use_fail, M};
								{use_movetolocn, _NewLNum, NewLocn} ->
									% extract from current location
									InLocn ! {leave_noexit, PName},
									% place in new
									NewLocn ! {enter, InLNum, {PName, self ()}},
									Pid ! {use_ok}
									% Note: when this loops, we'll get an 'entered' for the new location.
							end,
							Attrs;
						{use_inplayer, Fcn} ->
							case Fcn (OTab) of
								true ->
									% means the object interacts directly with us.
									OPid ! {use_in_player, PName, self(), self ()},
									receive
										{use_fail, M} -> Pid ! {use_fail, M};
										{use_movetolocn, _NewLNum, NewLocn} ->
											% extract from current location
											InLocn ! {leave_noexit, PName},
											% place in new
											NewLocn ! {enter, InLNum, {PName, self ()}},
											Pid ! {use_ok}
											% Note: when this loops, we'll get an
											% 'entered' for the new location.
									end;
								{false, X} ->
									Pid ! {use_fail, "probably need a " ++ X}
							end,
							Attrs;
						use_eat ->
							% means we can eat this.
							OPid ! {get_attr, health, self ()},
							H = receive {attr, health, HX} -> HX end,
							ets:delete_object (OTab, {ONm, OPid}),
							OPid ! {destroy},			% trash object.

							CurHealth = lookup_attr (Attrs, health),
							NewHealth = CurHealth + H,
							NHealth2 = if (NewHealth > 100) -> 100; true -> NewHealth end,
							Pid ! {use_eated, CurHealth, NHealth2},
							maybeset_attr (Attrs, {health, NHealth2})
					end
				end,
			game_player_run (GMgr, PName, IPid, OTab, InLNum, InLocn, NewAttrs);
			%}}}
		{do_wield, OName, Pid} -> %{{{  wield something (must be holding it).  Responds to `Pid'.
			OObjs = ets:lookup (OTab, OName),
			NewAttrs = case OObjs of
				[] ->
					Pid ! {wield_fail, "Not carrying that!"},
					Attrs;
				[{ONm,OPid}|_] ->
					Pid ! {wield_ok, ONm},
					set_wield (Attrs, {ONm, OPid})
			end,
			game_player_run (GMgr, PName, IPid, OTab, InLNum, InLocn, NewAttrs);
			%}}}
		{do_unwield, Pid} -> %{{{  unwield currently wielded thing.  Responds to `Pid'.
			NewAttrs = case lookup_attr (Attrs, wielding) of
				undefined ->
					Pid ! {unwield_fail, "not wielding anything"},
					Attrs;
				{ONm, _} ->
					Pid ! {unwield_ok, ONm},
					set_unwield (Attrs)
			end,
			game_player_run (GMgr, PName, IPid, OTab, InLNum, InLocn, NewAttrs);
			%}}}
		{do_attack, Who, Pid} -> %{{{  attack someone (us attacking).  Reponds to `Pid'.
			case lookup_attr (Attrs, wielding) of
				undefined ->
					Pid ! {attack_fail, "Not with my bare hands!"};
				{ONm, OPid} ->
					% wielding something
					OPid ! {get_attr, damage, self()},
					Dmg = receive {attr, damage, undefined} -> 0; {attr, damage, D} -> D end,
					% ask location to inflict on our behalf
					InLocn ! {do_attack, PName, Who, ONm, Dmg, self()},
					receive
						X = {attack_fail, _} ->
							Pid ! X;
						Y = {attack_ok, _, _} ->
							Pid ! Y
					end
			end,
			game_player_run (GMgr, PName, IPid, OTab, InLNum, InLocn, Attrs);
			%}}}
		{do_say, Who, What} -> %{{{  say something (us saying it).
			InLocn ! {do_say, Who, What},
			game_player_run (GMgr, PName, IPid, OTab, InLNum, InLocn, Attrs);
			%}}}
		{do_action, Who, What} -> %{{{  do something (us doing it).
			InLocn ! {do_action, Who, What},
			game_player_run (GMgr, PName, IPid, OTab, InLNum, InLocn, Attrs);
			%}}}
		{examine, Pid} -> %{{{  being examined, responds to `Pid' (could be self!).
			Pid ! {examine_resist, "oi!"},
			game_player_run (GMgr, PName, IPid, OTab, InLNum, InLocn, Attrs);
			%}}}
		{message, Msg} -> %{{{  general message sent to player -- forward to implementation.
			IPid ! {message, Msg},
			game_player_run (GMgr, PName, IPid, OTab, InLNum, InLocn, Attrs);
			%}}}
		Other ->
			io:format ("game_player_run(): got unhandled message: ~p~n", [Other]),
			game_player_run (GMgr, PName, IPid, OTab, InLNum, InLocn, Attrs)
	end.


game_player_deathloop (GMgr, PName, IPid, OTab, InLNum, InLocn, Attrs) ->
	% first of all, leave the room (so we don't see any more interactions).
	InLocn ! {leave_death, PName},
	receive
		{left_death, _} -> true
	end,
	timer:sleep (1000),
	IPid ! {message, "Some passing Valkyries scrape you up"},
	timer:sleep (2000),
	IPid ! {message, "relieve you of your belongings"},
	% gone from location, drop objects there.
	ets:foldl (fun ({OName, OPid}, _) ->
			InLocn ! {player_drop, "Valkyries", OName, OPid},
			true
		end, true, OTab),
	ets:delete_all_objects (OTab),
	timer:sleep (2000),
	IPid ! {message, "get you back on your feet and drop you off somewhere"},

	GMgr ! {lookup_locn, 0, self()},
	SetLocn = receive {locn, GMgr, XLocn} -> XLocn end,

	timer:sleep (1000),
	IPid ! {resurrect},
	timer:sleep (1000),

	SetLocn ! {enter, InLNum, {PName, self()}},

	% fixup health, vitality, unwield;  loop back into main player code.
	NewAttrs = maybeset_attr (maybeset_attr (set_unwield (Attrs), {health, 100}), {vitality, 100}),
	game_player_run (GMgr, PName, IPid, OTab, 0, SetLocn, NewAttrs).


game_player_disc (GMgr, PName, IPid, OTab, InLNum, InLocn, _Attrs) ->
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


game_player_disc_done (GMgr, PName, _IPid) ->
	GMgr ! {unregister_player, PName, self ()},
	receive {unregistered_player, GMgr, _} ->
		io:format ("game_player_disc_done(): player (~p) is done.~n", [PName]),
		exit (normal)
	end.

%}}}

