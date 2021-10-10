// First without tagging

extension (self: Prop) def &&(that: Prop): Prop = 
  (max, n, rng) => self(max, n, rng) match
    case Passed => that(max, n, rng)
    case x => x

extension (self: Prop) def ||(that: Prop): Prop = 
  (max, n, rng) => self(max, n, rng) match
    case Falsified(_, _) => that(max, n, rng)
    case x => x

// Then with tagging

extension (self: Prop) def tag(msg: String): Prop = 
  (max, n, rng) => self(max, n, rng) match
    case Falsified(e, c) => Falsified(FailedCase.fromString(s"$msg($e)"), c)
    case x => x

extension (self: Prop) def &&(that: Prop): Prop = 
  (max, n, rng) => self.tag("and-left")(max, n, rng) match
    case Passed => that.tag("and-right")(max, n, rng)
    case x => x

extension (self: Prop) def ||(that: Prop): Prop =
  (max, n, rng) => self.tag("or-left")(max, n, rng) match
    case Falsified(msg, _) => that.tag("or-right").tag(msg.string)(max, n, rng)
    case x => x