/* We can refer to the enclosing `Prop` instance with `Prop.this` */
def &&(p: Prop): Prop = new Prop {
  def check = Prop.this.check || p.check
}