package finalTagless

object FinalEmbedding {

  trait Repr[T]

  case class Eval[T](value: T) extends Repr[T]

  trait ExprSym[R[_]] {
    def literal: Int => R[Int]
    def literalBoolean:Boolean => R[Boolean]
    def negation: R[Int] => R[Int]
    def add: R[Int] => R[Int] => R[Int]
    def mul:R[Int] => R[Int] => R[Int]
    def cond[A]:R[Boolean]=> R[A] => R[A] => R[A]

  }

  implicit object ExprSym_Eval extends ExprSym[Eval] {
    override def literal = Eval(_)
    override def literalBoolean: (Boolean) => Eval[Boolean] = x => Eval(x)
    override def negation = x => Eval(-x.value)
    override def add = x => y => Eval(x.value + y.value)
    override def mul: (Eval[Int]) => (Eval[Int]) => Eval[Int] = x => y => Eval(x.value * y.value)

    override def cond[A]: (Eval[Boolean]) => (Eval[A]) => Eval[A] => Eval[A] =
      x=>y=>z=> if (x.value) Eval(y.value) else Eval(z.value)
  }

  //5 *(7 + -(7 + ("true".length==5)?2:6))
  def expression[R[_]](implicit s1: ExprSym[R]): R[Int] = {
    s1.mul(s1.literal(5))(s1.add(s1.literal(7))(s1.negation(s1.add(s1.literal(7))(s1.cond(s1.literalBoolean("true".length == 5))(s1.literal(2))(s1.literal(6))))))
  }

  // run the Eval interpreter
  def eval[T]: Eval[T] => T = _.value

}
