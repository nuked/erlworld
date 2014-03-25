% game_bots.erl -- bots for the adventure game
% Copyright (C) 2014 Fred Barnes, University of Kent  <frmb@kent.ac.uk>
% GPL 2

-module (game_bots).
-export ([game_bot_create/4, game_bot_create/7, game_bot_startup/7, game_bot_rtimer/3, game_bot_rtimer_init/3,
		game_wanderbot_run2/4, game_cleanerbot_run2/4]).

-import (game_util, [game_version/0, game_name/0, valid_exits/2]).
-import (game_player, [game_player_create/4]).




%{{{  game_bots_create (GMgr, ...):
%

game_bot_create (GMgr, BName, BType, BLocn) ->
	game_bot_create (GMgr, BName, BType, BLocn, -1, -1, "").

game_bot_create (GMgr, BName, BType, BLocn, BTLocn, BTSrc, BTName) ->
	spawn_link (?MODULE, game_bot_startup, [GMgr, BName, BType, BLocn, BTLocn, BTSrc, BTName]),
	true.

game_bot_startup (GMgr, BName, BTypeAtom, BILocn, BTLocn, BTSrc, BTName) ->
	Fun = case BTypeAtom of
		wander ->
			fun (PP) -> game_wanderbot_run (GMgr, BName, PP) end;
		cleaner ->
			fun (PP) -> game_cleanerbot_run (GMgr, BName, PP) end;
		Other ->
			io:format ("game_bot_startup(): unknown bot type [~p] ...~n", [Other]),
			undefined
	end,
	if (Fun == undefined) ->
		exit (normal);
	true ->
		case game_player_create (GMgr, BName, self (), BILocn) of
			{ok, PlrPid} ->
				Fun (PlrPid);
			{error, R} ->
				io:format ("game_bot_startup(): failed to create player for bot ~s: ~p~n", [BName, R]),
				exit (normal)
		end
	end.

%}}}
%{{{  game_bot_rtimer (Pid, Base, Range): random timer for driving some bot behaviour.
%

game_bot_rtimer (Pid, Base, Range) ->
	R = random:uniform (Range),
	receive
		code_switch ->
			game_bots:game_bot_rtimer (Pid, Base, Range)
	after
		(Base + R) ->
			Pid ! {bot_timeout},
			game_bot_rtimer (Pid, Base, Range)
	end.

game_bot_rtimer_init (Pid, Base, Range) ->
	random:seed (now ()),
	game_bot_rtimer (Pid, Base, Range).

%}}}

%{{{  game_wanderbot_run (GMgr, BName, PlrPid): randomly wandering bot.
%

game_wanderbot_run (GMgr, BName, PlrPid) ->
	random:seed (now ()),
	T = spawn_link (?MODULE, game_bot_rtimer_init, [self (), 2000, 2500]),
	game_wanderbot_run2 (GMgr, BName, PlrPid, T).

game_wanderbot_run2 (GMgr, BName, PlrPid, TmrPid) ->
	receive
		code_switch ->
			%{{{  switch code;  tell timer process en-route.
			PlrPid ! {do_action, BName, "undergoes a firmware upgrade"},
			TmrPid ! code_switch,
			game_bots:game_wanderbot_run2 (GMgr, BName, PlrPid, TmrPid);
			%}}}
		{bot_timeout} ->
			%{{{  timeout, some local action.
			V = random:uniform (6),
			if (V < 3) ->
				%{{{  see if we can move from here.
				PlrPid ! {curlocn, self ()},
				{LNum, LPid} = receive {curlocn, PlrPid, LN, LP} -> {LN, LP} end,
				LPid ! {look, self ()},
				Exits = receive {looked, _Desc, _Objects, _People, E} -> E end,

				VX = valid_exits (Exits, [north, east, south, west]),
				VXL = length (VX),

				if (VXL == 0) ->
					% not going anywhere..
					true;
				true ->
					XSel = lists:nth (random:uniform (VXL), VX),
					% try and move!
					PlrPid ! {move, self(), XSel},
					receive
						{cannot_move, _} ->
							% well, not..
							true;
						{moving, _} ->
							% yes..
							true
					end
				end,
				%}}}
				true;
			true ->
				%{{{  say or do something (maybe)
				case random:uniform (8) of
					1 -> PlrPid ! {do_say, BName, "hello, world!"};
					2 -> PlrPid ! {do_action, BName, "looks around"};
					3 -> PlrPid ! {do_say, BName, "what a nice place this is"};
					4 -> PlrPid ! {do_say, BName, "sometimes I feel so trapped in here"};
					_ -> true
				end,
				true

				%}}}
			end,

			% wait for another timeout period
			receive {bot_timeout} -> true end,

			game_wanderbot_run2 (GMgr, BName, PlrPid, TmrPid);
			%}}}
		_ ->				% throw the rest away
			game_wanderbot_run2 (GMgr, BName, PlrPid, TmrPid)
	end.

%}}}
%{{{  game_cleanerbot_run (GMgr, BName, PlrPid): randomly wandering bot, does some cleaning.
%

game_cleanerbot_run (GMgr, BName, PlrPid) ->
	random:seed (now ()),
	T = spawn_link (?MODULE, game_bot_rtimer_init, [self (), 2000, 2500]),
	game_cleanerbot_run2 (GMgr, BName, PlrPid, T).

game_cleanerbot_run2 (GMgr, BName, PlrPid, TmrPid) ->
	receive
		code_switch ->
			%{{{  switch code;  tell timer process en-route.
			PlrPid ! {do_action, BName, "undergoes a firmware upgrade"},
			TmrPid ! code_switch,
			game_bots:game_cleanerbot_run2 (GMgr, BName, PlrPid, TmrPid);
			%}}}
		{bot_timeout} ->
			%{{{  timeout, some local action.

			% Get exits and objects.
			PlrPid ! {curlocn, self ()},
			{LNum, LPid} = receive {curlocn, PlrPid, LN, LP} -> {LN, LP} end,
			LPid ! {look, self ()},
			{Exits, Objects} = receive {looked, _Desc, O, _People, E} -> {E, O} end,

			V = random:uniform (6),
			if (V < 3) ->
				%{{{  see if we can move from here.

				VX = valid_exits (Exits, [north, east, south, west]),
				VXL = length (VX),

				if (VXL == 0) ->
					% not going anywhere..
					true;
				true ->
					XSel = lists:nth (random:uniform (VXL), VX),
					% try and move!
					PlrPid ! {move, self(), XSel},
					receive
						{cannot_move, _} ->
							% well, not..
							true;
						{moving, _} ->
							% yes..
							true
					end
				end,
				%}}}
				true;
			true ->
				%{{{  say or do something (maybe)
				case random:uniform (8) of
					1 -> PlrPid ! {do_action, BName, "sweeps up crumbs"};
					2 -> PlrPid ! {do_action, BName, "dusts the decor"};
					3 -> PlrPid ! {do_action, BName, "looks around"};
					4 -> PlrPid ! {do_action, BName, "makes a whirring noise"};
					_ ->
						% see if there's any edibles here
						SelOs = lists:filter (fun (I) ->
									case I of
										"bread" -> true;
										"cookies" -> true;
										"ribena" -> true;
										_ -> false
									end end, Objects),
						if (SelOs == []) ->
							% nothing to eat
							true;
						true ->
							% do the pickup on the location, not via abstract player
							LPid ! {do_pickup, BName, hd (SelOs), self ()},
							receive
								{pickup_fail, _} -> true;		% gone already!
								{picked_up, OName, OPid} ->
									% got it, now consume/destroy!
									OPid ! {destroy},
									true
							end
						end
				end,
				true

				%}}}
			end,

			% wait for another timeout period
			receive {bot_timeout} -> true end,

			game_cleanerbot_run2 (GMgr, BName, PlrPid, TmrPid);
			%}}}
		_ ->				% throw the rest away
			game_cleanerbot_run2 (GMgr, BName, PlrPid, TmrPid)
	end.

%}}}


