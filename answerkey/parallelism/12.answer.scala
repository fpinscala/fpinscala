def choiceMap[K,V](key: Par[K])(choices: Map[K,Par[V]]): Par[V] =
  es => {
    val k = run(es)(key).get
    run(es)(choices(k))
  }
