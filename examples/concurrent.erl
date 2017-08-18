-module(concurrent).
-export([main/0]).

main() -> spawn(fun() -> f() end).

f() ->
  register(p, self()),
  receive 
    Hs -> spawn(fun() -> Hs ! 2, p ! exit end),
          conc()
  end.

conc() ->
  receive
    {_, 2}   -> conc();
    {Hs, Num} -> Hs ! Num
  end,
  receive
    exit -> exit(done)
  end.
