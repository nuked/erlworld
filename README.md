erlworld
========

A silly Erlang adventure game (MUD style).  This is based pretty much on a similar
one created in occam-pi circa 2004-2006.

To run from the Erlang shell (erl):

```
c(game).
game:game_crun().
```

By default the TCP server listens on port 4040 for incoming connections.  This can
be changed inside `game_main.erl` (`game_run/0`).

The current state is that most of what existed in the occam-pi game is here, minus
the LINC system and student bots.


--- 
Fred Barnes, April 2014.

