import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import moe.roselia.lisa
import moe.roselia.lisa.Evaluator
import moe.roselia.lisa.LispExp.{NilObj, PrimitiveFunction}

class UtilTests extends AsyncWordSpec with Matchers {
  "Returnable" should {
    import lisa.Util.ReturnControlFlow.Returns
    "return value" in {
      val returner = new Returns[Int]
      returner.returnable {
        returner.returns(2)
        3
      } shouldEqual 2
    }

    "return where invoked" in {
      val ret1 = new Returns[Int]
      val ret2 = new Returns[Int]

      ret1.returnable {
        ret2.returnable {
          ret1.returns(1)
          ret2.returns(2)
        }
      } shouldEqual 1
    }

    "produce an exception out of returnable block" in {
      a[lisa.Util.ReturnControlFlow.ReturnException[Int]] should be thrownBy {
        new Returns[Int].returns(1)
      }
    }

    "not be caught in applications" in {
      val ret = new Returns[Any]
      val retExpr = PrimitiveFunction(_ => ret.returns(NilObj))
      a[lisa.Util.ReturnControlFlow.ReturnException[_]] should be thrownBy {
        Evaluator.applyToEither(retExpr, Nil)
      }
    }
  }
}
