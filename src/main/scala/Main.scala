import finalTagless.FinalEmbedding.ExprSym
import finalTagless.NewOperation.DivisonSym

object Main extends App{

  //60 / (5 *(7 + -(7 + ("true".length==5)?2:6)))
  def expression[R[_]](implicit s1: ExprSym[R], s2:DivisonSym[R]): R[Int] = {
   s2.div(s1.literal(60)) (s1.mul(s1.literal(5))(s1.add(s1.literal(7))(s1.negation(s1.add(s1.literal(7))(s1.cond(s1.literalBoolean("true".length == 5))(s1.literal(2))(s1.literal(6)))))))
  }

  println(finalTagless.FinalEmbedding.eval(expression))

}
