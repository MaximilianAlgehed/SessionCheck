-module(queueInterface).
-export([main/0]).

main() -> spawn(fun() -> f() end).

f() ->
  register(p, self()),
  Q = queue:new(),
  receive 
    _ -> loop(Q)
  end.

getItem() ->
  receive
    {_, I} -> I
  end.

sendItem(Item, PID) ->
  case Item of
    empty      -> PID ! nothing;
    {value, V} -> PID ! {just, V}
  end.

loop(Q) ->
  receive 
    {_, enqueue} -> 
      X = getItem(),
      QPrim = queue:in(X, Q),
      loop(QPrim);
    {PID, pop} ->
      {X, QPrim} = queue:out(Q),
      sendItem(X, PID),
      loop(QPrim);
    {PID, peek} ->
      sendItem(queue:peek(Q), PID),
      loop(Q);
    {_, stop} -> exit(done)
  end.
