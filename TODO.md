# TODO

* Get rid of the IORef in the `Implementation` type, consider using an mvar
  instead
* Make sure to print the counterexample in the actual type of the message,
  not in the format being transmitted (i.e. Dynamic gives "Output <<Int>>")
* Implement shrinking
* Implement coherence testing
* Implement mutation testing by MITM
* Think about the need for synchronization between "threads"
* Think about the need for communicating results between "threads"
