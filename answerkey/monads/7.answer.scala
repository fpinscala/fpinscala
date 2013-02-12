Let's expand both implementations of `genOrder` into calls to `map` and `flatMap` to see what's going on. In the former case, the translation is straightforward:
 
Gen.nextString.flatMap(name =>
Gen.nextDouble.flatMap(price =>
Gen.nextInt.map(quantity =>
  Order(Item(name, price), quantity))))

But the second case looks like this (inlining the call to `genItem`):

Gen.nextString.flatMap(name =>
Gen.nextInt.map(price =>
  Item(name, price))).flatMap(item =>
Gen.nextInt.map(quantity =>
  Order(item, quantity)