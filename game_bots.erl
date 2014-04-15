% game_bots.erl -- bots for the adventure game
% Copyright (C) 2014 Fred Barnes, University of Kent  <frmb@kent.ac.uk>
% GPL 2

-module (game_bots).
-export ([game_bot_create/4, game_bot_create/7, game_bot_startup/7, game_bot_rtimer/3, game_bot_rtimer_init/3,
		game_wanderbot_run2/4, game_cleanerbot_run2/4, game_shopkeeperbot_run2/7, game_bartenderbot_run2/6,
		game_secretarybot_run2/6, game_architectbot_run2/6, game_traderbot_run2/4, game_wizardbot_run2/8]).

-import (game_util, [game_version/0, game_name/0, valid_exits/2, lexit_atom/1]).
-import (game_player, [game_player_create/5]).




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
		shopkeeper ->
			fun (PP) -> game_shopkeeperbot_run (GMgr, BName, PP) end;
		bartender ->
			fun (PP) -> game_bartenderbot_run (GMgr, BName, PP) end;
		secretary ->
			fun (PP) -> game_secretarybot_run (GMgr, BName, PP) end;
		architect ->
			fun (PP) -> game_architectbot_run (GMgr, BName, PP) end;
		wizard ->
			fun (PP) -> game_wizardbot_run (GMgr, BName, PP, BTLocn, BTSrc, BTName) end;
		trader ->
			fun (PP) -> game_traderbot_run (GMgr, BName, PP) end;
		Other ->
			io:format ("game_bot_startup(): unknown bot type [~p] ...~n", [Other]),
			undefined
	end,
	if (Fun == undefined) ->
		exit (normal);
	true ->
		case game_player_create (GMgr, BName, self (), BILocn, [{immortal}]) of
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
			game_bots:game_bot_rtimer (Pid, Base, Range);
		{shutdown} ->
			exit (normal)
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
		{attack, _Who, _What, _Damage} ->
			%{{{  attempted attack (will be zero damage anyway)
			PlrPid ! {do_say, BName, "I'm immortal :)"},
			game_wanderbot_run2 (GMgr, BName, PlrPid, TmrPid);
			%}}}
		{bot_timeout} ->
			%{{{  timeout, some local action.
			V = random:uniform (6),
			if (V < 3) ->
				%{{{  see if we can move from here.
				PlrPid ! {curlocn, self ()},
				{_LNum, LPid} = receive {curlocn, PlrPid, LN, LP} -> {LN, LP} end,
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
		code_switch -> %{{{  switch code;  tell timer process en-route.
			PlrPid ! {do_action, BName, "undergoes a firmware upgrade"},
			TmrPid ! code_switch,
			game_bots:game_cleanerbot_run2 (GMgr, BName, PlrPid, TmrPid);
			%}}}
		{attack, _Who, _What, _Damage} ->
			%{{{  attempted attack (will be zero damage anyway)
			PlrPid ! {do_action, BName, "clinks and clutters"},
			game_wanderbot_run2 (GMgr, BName, PlrPid, TmrPid);
			%}}}
		{bot_timeout} -> %{{{  timeout, some local action.

			% Get exits and objects.
			PlrPid ! {curlocn, self ()},
			{_LNum, LPid} = receive {curlocn, PlrPid, LN, LP} -> {LN, LP} end,
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
								{picked_up, _OName, OPid} ->
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
%{{{  game_shopkeeperbot_run (GMgr, BName, PlrPid): specific shop-keeper bot (doesn't move around).
%

game_shopkeeperbot_run (GMgr, BName, PlrPid) ->
	random:seed (now ()),
	T = spawn_link (?MODULE, game_bot_rtimer_init, [self (), 6000, 5000]),
	PlrPid ! {curlocn, self ()},
	{LNum, LPid} = receive {curlocn, PlrPid, LN, LP} -> {LN, LP} end,
	game_shopkeeperbot_run2 (GMgr, BName, PlrPid, T, LNum, LPid, none).


game_shopkeeperbot_run2 (GMgr, BName, PlrPid, TmrPid, LNum, LPid, Holding) ->
	receive
		code_switch -> %{{{  switch code;  tell timer process en-route.
			PlrPid ! {do_action, BName, "puts on some fresh clothes"},
			TmrPid ! code_switch,
			game_bots:game_shopkeeperbot_run2 (GMgr, BName, PlrPid, TmrPid, LNum, LPid, Holding);
			%}}}
		{player_drop_object, PN, OName} -> %{{{  someone dropping something
			NewHolding = if (PN /= BName) ->
				% try and pick it up!
				LPid ! {do_pickup, BName, OName, self ()},
				receive
					{pickup_fail, _} ->	% gone already!
						PlrPid ! {do_say, BName, io_lib:format ("you dropped ~s?", [OName])},
						Holding;
					{picked_up, ONm, OPid} ->
						% got it!
						case Holding of
							none ->		% create some gold here
								case game_object:game_object_create (GMgr, "gold", []) of
									false ->
										true;		% stuffed..
									Gold ->
										LPid ! {player_drop, BName, "gold", Gold}
								end;
							{HNm, HPid} ->
								LPid ! {player_drop, BName, HNm, HPid}
						end,
						{ONm, OPid}
				end;
			true -> Holding				% we dropped it, so ignore
			end,
			game_shopkeeperbot_run2 (GMgr, BName, PlrPid, TmrPid, LNum, LPid, NewHolding);
			%}}}
		{attack, Who, _What, _Damage} -> %{{{  we are being attacked.
			LPid ! {do_attack, BName, Who, "scythe", 15, self()},
			receive {attack_fail, _} -> true; {attack_ok, _, _} -> true end,
			PlrPid ! {do_say, BName, "don't bother"},
			game_shopkeeperbot_run2 (GMgr, BName, PlrPid, TmrPid, LNum, LPid, Holding);
			%}}}
		{bot_timeout} -> %{{{  timeout, some local action
			case random:uniform (6) of
				1 -> PlrPid ! {do_say, BName, "bovinefaeces"};
				2 -> PlrPid ! {do_action, BName, "floats an air biscuit"};
				3 -> PlrPid ! {do_action, BName, "moves stuff around"};
				4 -> PlrPid ! {do_say, BName, "did you want something?"};
				5 -> PlrPid ! {do_action, BName, "points at the exit"};
				6 -> PlrPid ! {do_say, BName, "if you want some action, try the brothel"}
			end,
			game_shopkeeperbot_run2 (GMgr, BName, PlrPid, TmrPid, LNum, LPid, Holding);
			%}}}
		_ ->				% throw the rest away
			game_shopkeeperbot_run2 (GMgr, BName, PlrPid, TmrPid, LNum, LPid, Holding)
	end.
%}}}
%{{{  game_bartenderbot_run (GMgr, BName, PlrPid): bartender bot (doesn't move around).
%

game_bartenderbot_run (GMgr, BName, PlrPid) ->
	random:seed (now ()),
	T = spawn_link (?MODULE, game_bot_rtimer_init, [self (), 8000, 8000]),
	PlrPid ! {curlocn, self ()},
	{LNum, LPid} = receive {curlocn, PlrPid, LN, LP} -> {LN, LP} end,
	game_bartenderbot_run2 (GMgr, BName, PlrPid, T, LNum, LPid).

game_bartenderbot_run2 (GMgr, BName, PlrPid, TmrPid, LNum, LPid) ->
	receive
		code_switch -> %{{{  switch code;  tell timer process en-coute.
			PlrPid ! {do_action, BName, "puts on a fresh apron"},
			TmrPid ! code_switch,
			game_bots:game_bartenderbot_run2 (GMgr, BName, PlrPid, TmrPid, LNum, LPid);
			%}}}
		{attack, Who, _What, _Damage} -> %{{{  we are being attacked.
			LPid ! {do_attack, BName, Who, "scythe", 15, self()},
			receive {attack_fail, _} -> true; {attack_ok, _, _} -> true end,
			PlrPid ! {do_say, BName, "don't bother"},
			game_bartenderbot_run2 (GMgr, BName, PlrPid, TmrPid, LNum, LPid);
			%}}}
		{bot_timeout} -> %{{{  timeout, some local action
			case random:uniform (2) of
				1 -> PlrPid ! {do_action, BName, "wanders around"};
				2 -> PlrPid ! {do_action, BName, "cleans glasses"}
			end,
			game_bartenderbot_run2 (GMgr, BName, PlrPid, TmrPid, LNum, LPid);
			%}}}
		_ ->				% throw the rest away
			game_bartenderbot_run2 (GMgr, BName, PlrPid, TmrPid, LNum, LPid)
	end.

%}}}
%{{{  game_secretarybot_run (GMgr, BName, PlrPid): secretary bot (doesn't move around).
%

game_secretarybot_run (GMgr, BName, PlrPid) ->
	random:seed (now ()),
	T = spawn_link (?MODULE, game_bot_rtimer_init, [self (), 8000, 8000]),
	PlrPid ! {curlocn, self ()},
	{LNum, LPid} = receive {curlocn, PlrPid, LN, LP} -> {LN, LP} end,
	game_secretarybot_run2 (GMgr, BName, PlrPid, T, LNum, LPid).

game_secretarybot_run2 (GMgr, BName, PlrPid, TmrPid, LNum, LPid) ->
	receive
		code_switch -> %{{{  switch code;  tell timer process en-coute.
			PlrPid ! {do_action, BName, "freshens up"},
			TmrPid ! code_switch,
			game_bots:game_secretarybot_run2 (GMgr, BName, PlrPid, TmrPid, LNum, LPid);
			%}}}
		{attack, Who, _What, _Damage} -> %{{{  we are being attacked.
			LPid ! {do_attack, BName, Who, "pointy stick", 10, self()},
			receive {attack_fail, _} -> true; {attack_ok, _, _} -> true end,
			PlrPid ! {do_say, BName, "don't bother"},
			game_secretarybot_run2 (GMgr, BName, PlrPid, TmrPid, LNum, LPid);
			%}}}
		{bot_timeout} -> %{{{  timeout, some local action
			case random:uniform (3) of
				1 -> PlrPid ! {do_say, BName, "your money or your login"};
				2 -> PlrPid ! {do_say, BName, "printer credits can be obtained from the credit machine"};
				3 -> PlrPid ! {do_say, BName, " <your ad here!> "}
			end,
			game_secretarybot_run2 (GMgr, BName, PlrPid, TmrPid, LNum, LPid);
			%}}}
		_ ->				% throw the rest away
			game_secretarybot_run2 (GMgr, BName, PlrPid, TmrPid, LNum, LPid)
	end.

%}}}
%{{{  game_architectbot_run (GMgr, BName, PlrPid): architect bot (doesn't move around).
%

game_architectbot_run (GMgr, BName, PlrPid) ->
	random:seed (now ()),
	T = spawn_link (?MODULE, game_bot_rtimer_init, [self (), 8000, 8000]),
	PlrPid ! {curlocn, self ()},
	{LNum, LPid} = receive {curlocn, PlrPid, LN, LP} -> {LN, LP} end,
	game_architectbot_run2 (GMgr, BName, PlrPid, T, LNum, LPid).

game_architectbot_run2 (GMgr, BName, PlrPid, TmrPid, LNum, LPid) ->
	receive
		code_switch -> %{{{  switch code;  tell timer process en-coute.
			PlrPid ! {do_action, BName, "goes glowy for a moment"},
			TmrPid ! code_switch,
			game_bots:game_architectbot_run2 (GMgr, BName, PlrPid, TmrPid, LNum, LPid);
			%}}}
		{person_entering, PName} -> %{{{  person entering the room.
			timer:sleep (250),
			PlrPid ! {do_action, BName, "looks at " ++ PName},
			game_architectbot_run2 (GMgr, BName, PlrPid, TmrPid, LNum, LPid);
			%}}}
		{say, Who, What} -> %{{{  someone saying something -- maybe us!
			if (Who == BName) -> true;
			true -> case lists:last (What) of $? ->
					timer:sleep (250),
					PlrPid ! {do_say, BName, "I don't know!"},
					true;
				_ -> true end
			end,
			game_architectbot_run2 (GMgr, BName, PlrPid, TmrPid, LNum, LPid);
			%}}}
		{attack, Who, _What, _Damage} -> %{{{  we are being attacked.
			LPid ! {do_attack, BName, Who, "blunt chisel", 15, self()},
			receive {attack_fail, _} -> true; {attack_ok, _, _} -> true end,
			game_architectbot_run2 (GMgr, BName, PlrPid, TmrPid, LNum, LPid);
			%}}}
		{bot_timeout} -> %{{{  timeout, some local action
			case random:uniform (3) of
				1 -> PlrPid ! {do_action, BName, "looks at the creation, and is happy"};
				2 -> PlrPid ! {do_action, BName, "ponders"};
				3 -> PlrPid ! {do_action, BName, "looks around"}
			end,
			game_architectbot_run2 (GMgr, BName, PlrPid, TmrPid, LNum, LPid);
			%}}}
		_ ->				% throw the rest away
			game_architectbot_run2 (GMgr, BName, PlrPid, TmrPid, LNum, LPid)
	end.

%}}}
%{{{  game_traderbot_run (GMgr, BName, PlrPid): trading bot (wanders around).
%

game_traderbot_run (GMgr, BName, PlrPid) ->
	random:seed (now ()),
	T = spawn_link (?MODULE, game_bot_rtimer_init, [self (), 10000, 3000]),
	game_traderbot_run2 (GMgr, BName, PlrPid, T).

game_traderbot_run2 (GMgr, BName, PlrPid, TmrPid) ->
	receive
		code_switch ->
			%{{{  switch code;  tell timer process en-route.
			PlrPid ! {do_action, BName, "undergoes a firmware upgrade"},
			TmrPid ! code_switch,
			game_bots:game_traderbot_run2 (GMgr, BName, PlrPid, TmrPid);
			%}}}
		{player_drop_object, PN, OName} -> %{{{  someone dropping something
			if (PN /= BName) ->
				% try and pick it up!
				PlrPid ! {curlocn, self ()},
				{_LNum, LPid} = receive {curlocn, PlrPid, LN, LP} -> {LN, LP} end,

				LPid ! {do_pickup, BName, OName, self ()},
				receive
					{pickup_fail, _} -> true;		% gone already!
					{picked_up, _OName, OPid} ->
						% got it, now consume/destroy!
						OPid ! {destroy},
						% spawn some silver
						case game_object:game_object_create (GMgr, "silver", []) of
							false -> true;		% stuffed..
							Silver -> LPid ! {player_drop, BName, "silver", Silver}
						end
				end;
			true -> true				% we dropped it, so ignore
			end,
			game_bots:game_traderbot_run2 (GMgr, BName, PlrPid, TmrPid);
			%}}}
		{bot_timeout} ->
			%{{{  timeout, see if we can move.

			PlrPid ! {curlocn, self ()},
			{_LNum, LPid} = receive {curlocn, PlrPid, LN, LP} -> {LN, LP} end,
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

			game_traderbot_run2 (GMgr, BName, PlrPid, TmrPid);
			%}}}
		_ ->				% throw the rest away
			game_traderbot_run2 (GMgr, BName, PlrPid, TmrPid)
	end.

%}}}
%{{{  game_wizardbot_run (GMgr, BName, PlrPid, BTLocn, BTSrc, BTName): door wizard
%

game_wizardbot_run (GMgr, BName, PlrPid, BTLocn, BTSrc, BTName) ->
	random:seed (now ()),
	game_wizardbot_run2 (GMgr, BName, PlrPid, none, BTLocn, BTSrc, BTName, false).

game_wizardbot_run2 (GMgr, BName, PlrPid, TmrPid, BTLocn, BTSrc, BTName, GateOpen) ->
	receive
		code_switch -> %{{{  switch code;  tell timer process en-route.
			PlrPid ! {do_action, BName, "undergoes a firmware upgrade"},
			TmrPid ! code_switch,
			game_bots:game_wizardbot_run2 (GMgr, BName, PlrPid, TmrPid, BTLocn, BTSrc, BTName, GateOpen);
			%}}}
		{player_drop_object, PN, OName} -> %{{{  someone dropping something
			{NewTmr, NewGateOpen} = if ((PN /= BName) and (OName == BTName)) ->
				% try and pick it up!
				PlrPid ! {curlocn, self ()},
				{_LNum, LPid} = receive {curlocn, PlrPid, LN, LP} -> {LN, LP} end,

				GMgr ! {lookup_locn, BTLocn, self ()},
				BTLocnPid = receive
					{error, _} -> none;
					{locn, GMgr, X} -> X
				end,

				if (BTLocnPid == none) ->
					% do nothing, because we're broken..
					{TmrPid, GateOpen};
				true ->
					LPid ! {do_pickup, BName, OName, self ()},
					receive
						{pickup_fail, _} -> {TmrPid, GateOpen};		% gone already!
						{picked_up, _OName, OPid} ->
							% got it, now consume/destroy!
							OPid ! {destroy},
							case GateOpen of
							true ->
								PlrPid ! {do_say, BName, "The gateway is already open"},
								TmrPid ! code_switch,		% cheap way to reset timer.
								{TmrPid, GateOpen};
							false ->
								T = spawn_link (?MODULE, game_bot_rtimer_init, [self (), 7500, 2500]),
								PlrPid ! {do_say, BName, "Opening the gateway"},
								LPid ! {set_exit, self (), lexit_atom (BTSrc), BTLocnPid},
								{T, true}
							end
					end
				end;
			true -> {TmrPid, GateOpen}		% we dropped it, so ignore
			end,
			game_wizardbot_run2 (GMgr, BName, PlrPid, NewTmr, BTLocn, BTSrc, BTName, NewGateOpen);
			%}}}
		{attack, Who, _What, _Damage} ->
			%{{{  attempted attack (will be zero damage anyway)
			PlrPid ! {do_action, BName, "gets angry"},
			timer:sleep (500),
			PlrPid ! {curlocn, self ()},
			{_LNum, LPid} = receive {curlocn, PlrPid, LN, LP} -> {LN, LP} end,
			LPid ! {do_attack, BName, Who, "lightning", 30, self()},
			receive {attack_fail, _} -> true; {attack_ok, _, _} -> true end,

			game_wizardbot_run2 (GMgr, BName, PlrPid, TmrPid, BTLocn, BTSrc, BTName, GateOpen);
			%}}}
		{bot_timeout} -> %{{{  timeout, shut the gateway
			PlrPid ! {curlocn, self ()},
			{_LNum, LPid} = receive {curlocn, PlrPid, LN, LP} -> {LN, LP} end,

			LPid ! {set_exit, self (), lexit_atom (BTSrc), none},
			TmrPid ! {shutdown},

			game_wizardbot_run2 (GMgr, BName, PlrPid, none, BTLocn, BTSrc, BTName, false);
			%}}}
		_ ->				% throw the rest away
			game_wizardbot_run2 (GMgr, BName, PlrPid, TmrPid, BTLocn, BTSrc, BTName, GateOpen)
	end.

%}}}


