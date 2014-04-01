% game_util.erl -- game utility functions (used by other modules).
% Copyright (C) 2014 Fred Barnes, University of Kent <frmb@kent.ac.uk>
% GPL 2

-module (game_util).
-export ([game_version/0, game_name/0, valid_exits/2,
		maybeadd_attr/2, maybeset_attr/2, lookup_attr/2, maybedel_attr/2]).


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
%{{{  attribute handling (for things kept in {tag, value} lists).  Max 3 or 4 things.. (else use ETS)

maybeadd_attr ([], X) -> [X];
maybeadd_attr (L = [{T0}|_], {T0}) -> L;
maybeadd_attr (L = [{T1,_}|_], {T1,_}) -> L;
maybeadd_attr ([A|As], X) -> [A|maybeadd_attr (As, X)].

maybeset_attr ([], X) -> [X];
maybeset_attr (L = [{T0}|_], {T0}) -> L;
maybeset_attr ([{T1,_}|R], H = {T1, V}) -> [H|R];
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

