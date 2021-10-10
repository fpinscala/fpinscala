trait Prop:
  self =>
  def check: Boolean
  def &&(that: Prop): Prop =
    new Prop:
      def check = self.check && that.check