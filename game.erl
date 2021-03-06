% game.erl -- compile and run harness for Erlang Adventure Game
% Copyright (C) 2014 Fred Barnes, University of Kent <frmb@kent.ac.uk>
% GPL 2

-module (game).
-export ([game_run/0, game_crun/0, game_compile/0]).


game_run () ->
	game_main:game_run ().


% Slightly more gruesome version that attempts to compile stuff first.
game_docompile () ->
	% modules, in dependency order..
	Mods = [game_util, game_object, game_locn, game_player, game_tcp, game_bots, game_linc, game_main],

	lists:map (fun (M) -> {ok, _} = compile:file (atom_to_list (M) ++ ".erl") end, Mods),
	lists:map (fun (M) -> code:purge (M) end, Mods),
	lists:map (fun (M) -> {module, _} = code:load_file (M) end, Mods),

	true.

game_crun () ->
	io:format ("attempting to compile and run...~n"),
	try
		game_docompile (),
		game_run ()
	catch
		error: X -> io:format ("compile or run failure: ~p~n", [X]), false
	end.

game_compile () ->
	io:format ("attempting to compile...~n"),
	try
		game_docompile ()
	catch
		error: X -> io:format ("compile failure: ~p~n", [X]), false
	end.

