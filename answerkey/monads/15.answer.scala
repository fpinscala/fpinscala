We can state the associative law in terms of `join`:

x.join.join == x.map(_.join).join

For `Par`, the `join` combinator means something like "make the outer thread wait for the inner one to finish." What this law is saying is that if you have threads starting threads three levels deep, then joining the inner threads and then the outer ones is the same as joining the outer threads and then the inner ones.

For `Parser`, the `join` combinator is running the outer parser to produce a `Parser`, then running the inner `Parser` _on the remaining input_. The associative law is saying, roughly, that only the _order_ of nesting matters, since that's what affects the order in which the parsers are run.