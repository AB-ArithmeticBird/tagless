package finalTagless

object FinalEmbedding {

  trait Repr[T]

  case class Eval[T](value: T) extends Repr[T]


  trait ExprSym[R[_]] {
    def literal: Int => R[Int]
    def negation: R[Int] => R[Int]
    def add: R[Int] => R[Int] => R[Int]
    def mul:R[Int] => R[Int] => R[Int]
  }

  implicit object ExprSym_Eval extends ExprSym[Eval] {
    def literal = Eval(_)
    def negation = x => Eval(-x.value)
    def add = x => y => Eval(x.value + y.value)
    def mul = x => y => Eval(x.value * y.value)
  }


  def expression[R[_]](implicit s1: ExprSym[R]): R[Int] = {
    s1.mul(s1.literal(5))(s1.add(s1.literal(7))(s1.negation(s1.add(s1.literal(7))(s1.literal(2)))))
  }

  // run the Eval interpreter
  def eval[T]: Eval[T] => T = _.value

}
