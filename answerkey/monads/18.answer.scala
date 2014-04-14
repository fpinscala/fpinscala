`replicateM` for `State` repeats the same state transition a number of times and returns a list of the results. It's not passing the same starting state many times, but chaining the calls together so that the output state of one is the input state of the next.

`map2` works similarly in that it takes two state transitions and feeds the output state of one to the input of the other. The outputs are not put in a list, but combined with a function `f`.

`sequence` takes an entire list of state transitions and does the same kind of thing as `replicateM`: it feeds the output state of the first state transition to the input state of the next, and so on. The results are accumulated in a list.