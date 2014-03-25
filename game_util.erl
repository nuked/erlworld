% game_util.erl -- game utility functions (used by other modules).
% Copyright (C) 2014 Fred Barnes, University of Kent <frmb@kent.ac.uk>
% GPL 2

-module (game_util).
-export ([game_version/0, game_name/0, valid_exits/2]).


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


