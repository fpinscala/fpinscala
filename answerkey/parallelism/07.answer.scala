y.map(g).map(f) == y.map(f compose g)                           <1>
y.map(id).map(f compose g) == y.map((f compose g) compose id)   <2>
y.map(id).map(f compose g) == y.map(f compose g)                <3>
y.map(f compose g) == y.map(f compose g)                        <4>

See https://github.com/quchen/articles/blob/master/second_functor_law.md

Also https://gist.github.com/pchiusano/444de1f222f1ceb09596
