% game_object.erl -- game object stuff.
% Copyright (C) 2014 Fred Barnes, University of Kent <frmb@kent.ac.uk>
% GPL 2

-module (game_object).
-export ([game_object/6, game_object_create/3, game_object_run/5]).

-import (game_util, [lookup_attr/2]).


%{{{  game_object_create (GMgr, Name, Opts): creates a new object (free-standing)
% returns Pid of the newly created object (will register itself with the game, but not a location/player).

game_object_create (GMgr, Name, Opts) ->
	case game_read_ofile (Name) of
		{error, R} ->
			io:format ("game_object_create(~s): ~s~n", [Name, R]),
			false;
		{Desc, Attrs} ->
			OPid = spawn_link (?MODULE, game_object, [GMgr, Name, Desc, Attrs, Opts, self ()]),

			% wait for it to register itself
			receive
				{object_start, OPid} -> OPid;		% result is Pid of new object
				{object_fail, OPid} -> false
			end
	end.

% reads description and attributes given an object name,
% returns: {Desc,Attrs} | {error, ...}.

game_read_ofile (Name) ->
	DFName = "data/" ++ Name ++ ".odesc",

	case filelib:is_file (DFName) of
		true ->
			case file:open (DFName, read) of
				{ok, Han} ->
					R = game_read_odesc (Han, [], []),
					file:close (Han),
					R;
				{error, Reason} ->
					{error, io_lib:format ("failed to open file: ~p", [Reason])}
			end;
		false ->
			{error, io_lib:format ("no such file: ~s", [DFName])}
	end.

% this reads the object description from file.
%
game_read_odesc (FHan, DSoFar, ASoFar) ->
	case io:get_line (FHan, '') of
		eof -> {DSoFar, ASoFar};
		Line ->
			SLine = string:strip (string:strip (Line, both, $\n), both, $ ),
			SLen = length (SLine),

			if (SLen == 0) ->
				% ignore blank lines
				game_read_odesc (FHan, DSoFar, ASoFar);
			true ->
				FC = hd (SLine),
				if ((FC == $+) and (SLen > 3)) ->
					Val = list_to_integer (string:substr (SLine, 4)),
					SC = hd (tl (SLine)),
					if (SC == $h) ->
						game_read_odesc (FHan, DSoFar, ASoFar ++ [{health, Val}]);
					(SC == $d) ->
						game_read_odesc (FHan, DSoFar, ASoFar ++ [{damage, Val}]);
					true ->
						% ignore.
						game_read_odesc (FHan, DSoFar, ASoFar)
					end;
				true ->
					% assume part of the description.
					game_read_odesc (FHan, DSoFar ++ [{high5, SLine}], ASoFar)
				end
			end
	end.

%}}}
%{{{  game_object (GMgr Name, Desc, Attrs, Opts, PPid): game object
% opts is a list of particular options:
%  {dispenser, "GenObjName"}
%  {door, "LocnNum"}
%

game_object (GMgr, Name, Desc, Attrs, Opts, PPid) ->

	% register ourselves with the game.
	GMgr ! {register_object, Name, self (), self ()},
	receive
		{registered_object, GMgr, _} -> true;
		{error, GMgr, Msg} ->
			io:format ("game_object(): failed to register object ~s: ~s~n", [Name, Msg]),
			PPid ! {object_fail, self()},
			exit (normal)
	end,

	% sensible object, tell parent process
	PPid ! {object_start, self()},

	game_object_run (GMgr, Name, Desc, Attrs, Opts).


% assorted handy option processing.
object_use_type ([]) -> use_none;
object_use_type ([{dispenser, _}|_]) -> use_inlocn;
object_use_type ([{door, _}|_]) -> use_inplayer;
object_use_type ([_|Xs]) -> object_use_type (Xs).

object_use_name ([{dispenser, O}|_]) -> O;
object_use_name ([_|Xs]) -> object_use_name (Xs).

object_use_locn ([{door, L}|_]) -> L;
object_use_locn ([_|Xs]) -> object_use_locn (Xs).


% main loop for object.
%
game_object_run (GMgr, Name, Desc, Attrs, Opts) ->
	receive
		code_switch -> %{{{  switch code.
			game_object:game_object_run (GMgr, Name, Desc, Attrs, Opts);
			%}}}
		{examine, Pid} -> %{{{  examine object -- responds with description and attributes.
			Pid ! {examined, Desc, Attrs},
			game_object_run (GMgr, Name, Desc, Attrs, Opts);
			%}}}
		{destroy} -> %{{{  destroy object -- assumes it isn't attached in the game anywhere!
			game_object_shutdown (GMgr, Name);
			%}}}
		{can_pickup, Pid} -> %{{{  determine whether this object can be picked up.
			case object_use_type (Opts) of
				use_none -> Pid ! {can_pickup, self (), true};
				_ -> Pid ! {can_pickup, self (), false}
			end,
			game_object_run (GMgr, Name, Desc, Attrs, Opts);
			%}}}
		{get_attr, Attr, Pid} -> %{{{  looks up an attribute, responds to `Pid'.
			Pid ! {attr, Attr, lookup_attr (Attrs, Attr)},
			game_object_run (GMgr, Name, Desc, Attrs, Opts);
			%}}}
		{use_type, Pid} -> %{{{  returns the type of usage this object supports: use_none, use_inlocn, use_inplayer, use_eat
			Pid ! case lookup_attr (Attrs, health) of
				undefined -> {can_use, self (), object_use_type (Opts)};
				_ -> {can_use, self (), use_eat}
			end,
			game_object_run (GMgr, Name, Desc, Attrs, Opts);
			%}}}
		{use_in_locn, _LNum, LPid, Pid} -> %{{{  using object in a specific location
			case object_use_type (Opts) of
				use_inlocn ->
					% create a new object! (regular thing)
					OName = object_use_name (Opts),
					NewObj = game_object_create (GMgr, OName, []),
					case NewObj of
						false -> Pid ! {use_fail, io_lib:format ("failed to create new object ~s", [OName])};
						ObjPid ->
							% created new object! -- dump in location
							LPid ! {drop, OName, ObjPid},
							Pid ! {use_ok}
					end;
				_ -> Pid ! {use_fail, "cannot use this"}
			end,
			game_object_run (GMgr, Name, Desc, Attrs, Opts);
			%}}}
		{use_in_player, _PName, _PPid, Pid} -> %{{{  someone using an object in a specific way.  query is done by abstract player; respond to `Pid' saying how to handle.
			case object_use_type (Opts) of
				use_inplayer ->
					% this transports the player elsewhere
					LocnNum = object_use_locn (Opts),
					GMgr ! {lookup_locn, LocnNum, self ()},
					NewLocn = receive
							{locn, GMgr, X} -> X;
							{error, GMgr, M} ->
								io:format ("game_object_run(): door object failed to find target ~p: ~p~n",
									[LocnNum, M]),
								false
						end,
					if (NewLocn == false) ->
						Pid ! {use_fail, "cannot use this"};
					true ->
						Pid ! {use_movetolocn, LocnNum, NewLocn}
					end;
				_ -> Pid ! {use_fail, "cannot use this"}
			end,
			game_object_run (GMgr, Name, Desc, Attrs, Opts);
			%}}}
		Other ->
			io:format ("game_object_run(): got unhandled message: ~p~n", [Other]),
			game_object_run (GMgr, Name, Desc, Attrs, Opts)
	end.

game_object_shutdown (GMgr, Name) ->
	% io:format ("game_object_shutdown(): destroying object ~s~n", [Name]),
	GMgr ! {unregister_object, Name, self (), self ()},
	receive
		{unregistered_object, GMgr, _} -> true
	end,
	exit (normal).

%}}}

