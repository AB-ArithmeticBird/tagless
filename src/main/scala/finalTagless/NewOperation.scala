package finalTagless

import finalTagless.FinalEmbedding.Eval

/**
  * Created by arithmeticbird on 1/2/16.
  */
object NewOperation {

  trait DivisonSym[R[_]] {
    def div : R[Int] => R[Int] => R[Int]
  }

  implicit object DivisonSym_Eval extends DivisonSym[Eval] {
    override def div = x => y => Eval(x.value / y.value)
  }

}
