-module(bookShop).
-export([main/0]).

main() -> spawn(fun() -> f() end).

f() ->
  register(p, self()),
  receive 
    _ -> loop([])
  end.

loop(Bs) ->
  receive 
    {Hs, checkout}  -> Hs ! Bs, exit(done);
    {_, {order, B}} -> continue(B, Bs)
  end.

continue(B, Bs) ->
  Lst = if
          B < 1  -> Bs;
          true   -> [B|Bs]
        end,
  loop(Lst).
