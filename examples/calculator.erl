-module(calculator).
-export([main/0]).

main() -> spawn(fun() -> f() end).

f() ->
  register(p, self()),
  receive 
    _ -> loop()
  end.

getInt() ->
  receive
    {_, V} -> V
  end.

loop() ->
  receive 
    {_ , "stop"} -> exit(done);
    {HS, "mul"} ->
          I = getInt(),
          J = getInt(),
          HS ! (I * J);
    {HS, "div"} -> 
          I = getInt(),
          J = getInt(),
          HS ! (I div J)
  end,
  loop().
