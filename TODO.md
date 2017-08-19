# TODO

* Make sure to print the counterexample in the actual type of the message,
  not in the format being transmitted (i.e. Dynamic gives "Output <<Int>>")
* Implement shrinking
* Implement subsumption testing, can a client running protocol A communicate
  with a server using protocol B when A /= B
* Implement mutation testing by MITM

* Think some more about the semantics of threads, having any possible
  interleaving be allowed can be very dangerous
* Think about the need for synchronization between "threads"
* Think about the need for communicating results between "threads"
