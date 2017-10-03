-module(bookShopPaper).
-export([main/0]).

main() -> spawn(fun() -> f() end).

f() ->
  register(p, self()),
  receive 
    _ -> loop([])
  end.

recvAnything() ->
  receive
    V -> V
  end.

loop(Books) ->
  receive
    {Hs, "buy"} ->
      B = recvAnything(),
      Books2 =
      if
        B >= 0 -> [ B || Books ];
        true   -> Books
      end,
      loop(Books2);
    {Hs, "request"} -> Hs ! Books,
                       loop(Books);
    {Hs, "exit"} -> exit(done)
  end.
