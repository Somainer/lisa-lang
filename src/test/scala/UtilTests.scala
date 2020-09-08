import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import moe.roselia.lisa
import moe.roselia.lisa.Evaluator
import moe.roselia.lisa.LispExp.{NilObj, PrimitiveFunction}
import moe.roselia.lisa.Util.ConsoleColor.RGBColor

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

  "ConsoleColor" should {
    import lisa.Util.ConsoleColor
    import ConsoleColor._
    import ConsoleColor.Implicits._
    import org.scalatest.OptionValues._
    import org.scalatest.PrivateMethodTester._

    "parse color correctly" in {
      val color: RGBColor = "#66ccff"
      color shouldEqual RGBColor(0x66, 0xcc, 0xff)
    }

    "parse short color correctly" in {
      val color: RGBColor = "#01b"
      color shouldEqual RGBColor(0x00, 0x11, 0xbb)
    }

    "handle wrong colors" in {
      RGBColor.fromHexString("#1145141919810") shouldBe None
      an[Exception] shouldBe thrownBy {
        "Payload".foreground("#11451419191810")
      }
    }

    "handle reset arguments" in {
      val cc = "Hello".bold.underline.foreground("#66ccff")
      cc.additionalArguments should contain allOf (ANSIConstants.bold, ANSIConstants.underline)
      val resetArguments = PrivateMethod[List[Int]](Symbol("resetArguments"))
      cc invokePrivate resetArguments() should contain allOf(
        ANSIConstants.reverseFlagOf(ANSIConstants.bold).value,
        ANSIConstants.reverseFlagOf(ANSIConstants.underline).value,
        ANSIConstants.defaultForegroundColor
      )
      val withArgument = PrivateMethod[ConsoleColor.ConsoleColor](Symbol("withArgument"))
      val newColor = cc invokePrivate withArgument(114)
      newColor.invisible invokePrivate resetArguments() should contain only ANSIConstants.reset
    }

    "should yield correct argument string" in {
      val cc = "Hello".background("#66ccff").toString
      val expected = s"\u001b[48;2;${0x66};${0xcc};${0xff}mHello\u001b[${ANSIConstants.defaultBackgroundColor}m"
      cc shouldBe expected
    }
  }
}
