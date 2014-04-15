% game_util.erl -- game utility functions (used by other modules).
% Copyright (C) 2014 Fred Barnes, University of Kent <frmb@kent.ac.uk>
% GPL 2

-module (game_util).
-export ([game_version/0, game_name/0, valid_exits/2,
		maybeadd_attr/2, maybeset_attr/2, lookup_attr/2, maybedel_attr/2, lexit_atom/1,
		purge_mods/1, compile_mods/1, reload_mods/1]).


%{{{  some common settings/constants (encoded as functions).

game_version () -> "0.9.0-Erl".

game_name () -> "The Erlang Adventure".

%}}}

%{{{  valid_exits (Exits, RTerms): returns a list formed from RTerms where the corresponding Exit is valid (not `none').

valid_exits ([], []) -> [];
valid_exits ([none|Es], [_|Ns]) ->
	valid_exits (Es, Ns);
valid_exits ([_|Es], [N|Ns]) ->
	[N | valid_exits (Es, Ns)].


%}}}
%{{{  lexit_atom(N): turns exit number (0-3) into exit atom (north, ...).

lexit_atom(0) -> north;
lexit_atom(1) -> east;
lexit_atom(2) -> south;
lexit_atom(3) -> west.


%}}}
%{{{  attribute handling (for things kept in {tag, value} lists).  Max 3 or 4 things.. (else use ETS)

maybeadd_attr ([], X) -> [X];
maybeadd_attr (L = [{T0}|_], {T0}) -> L;
maybeadd_attr (L = [{T1,_}|_], {T1,_}) -> L;
maybeadd_attr ([A|As], X) -> [A|maybeadd_attr (As, X)].

maybeset_attr ([], X) -> [X];
maybeset_attr (L = [{T0}|_], {T0}) -> L;
maybeset_attr ([{T1,_}|R], H = {T1, _}) -> [H|R];
maybeset_attr ([A|As], X) -> [A|maybeset_attr (As, X)].

lookup_attr ([], _) -> undefined;
lookup_attr ([{T0}|_], T0) -> true;
lookup_attr ([{T1,V}|_], T1) -> V;
lookup_attr ([_|As], X) -> lookup_attr (As, X).

maybedel_attr ([], _) -> [];
maybedel_attr ([{T0}|R], T0) -> R;
maybedel_attr ([{T1,_}|R], T1) -> R;
maybedel_attr ([A|As], X) -> [A|maybedel_attr (As, X)].

%}}}
%{{{  for in-game compiling/loading of modules.

purge_mods (Mods) ->
	lists:foldl (fun (M, StillOk) ->
		case StillOk of
			true ->
				case code:soft_purge (M) of
					true -> true;
					false ->
						io:format ("cannot purge code for ~p, old code still in use :(~n", [M]),
						false
				end;
			false -> false
		end
		end, true, Mods).

compile_mods (Mods) ->
	lists:foldl (fun (M, StillOk) ->
		case StillOk of
			true ->
				try
					case compile:file (atom_to_list (M) ++ ".erl") of
						{ok, _} -> true;
						{ok, _, _} -> true
					end
				catch
					error: X -> io:format ("failed to compile ~p, got error: ~p~n", [M, X]),
					false
				end;
			false -> false
		end
		end, true, Mods).

reload_mods (Mods) ->
	lists:foldl (fun (M, StillOk) ->
		case StillOk of
			true ->
				try
					{module, _} = code:load_file (M),
					true
				catch
					error: X -> io:format ("cannot load ~p, got error: ~p~n", [M, X]),
					false
				end;
			false -> false
		end
		end, true, Mods).
%}}}


