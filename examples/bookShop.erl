-module(bookShop).
-export([main/0]).

main() -> spawn(fun() -> f() end).

f() ->
  register(p, self()),
  receive 
    _ -> loop([])
  end.

loop(XS) ->
  receive 
    {_, I} -> continue(I, XS)
  end.

continue(I, XS) ->
  Lst = if
          %I < 0 -> XS;
          %length(XS) >= 5 -> XS;
          true  -> [I|XS]
        end,
  receive 
    {_,  "another"} -> loop(Lst);
    {Hs, "request"} -> Hs ! Lst, finish(Lst)
  end.

finish(XS) ->
  receive
    {_, "another"} -> loop(XS);
    {_, "done"}    -> exit(done)
  end.
