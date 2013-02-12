We can state the associative law in terms of `join`:

join(join(x)) == join(map(x)(join))

For `Par`, the `join` combinator means something like "make the outer thread wait for the inner one to finish". What this law is saying is that if you have threads starting threads three levels deep, then joining the inner threads and then the outer ones is the same as joining the outer threads and then the inner ones.

For `Parser`, the `join` combinator simply passes the same input to both the inner and the outer `Parser`. The associative law is saying that if you have `Parser`s three levels deep, we can pass the input to all three and it doesn't matter in what order we do that.