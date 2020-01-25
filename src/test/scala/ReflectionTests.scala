import org.scalatest.wordspec.AsyncWordSpec
import org.scalatest.matchers.should.Matchers
import moe.roselia.lisa
import moe.roselia.lisa.LispExp.Expression

class ReflectionTests extends AsyncWordSpec with Matchers {
  "AccessDot" should {
    import lisa.Reflect.DotAccessor.accessDot
    "get nil-arity function" in {
      class DistinctValue
      val o = new {
        def a = "233"
        val b = new DistinctValue
        var c = new DistinctValue

        lazy val longNameImpl = new DistinctValue
        private def `a-long-name` = longNameImpl
      }
      accessDot("a")(o) shouldEqual o.a
      accessDot("b")(o) shouldEqual o.b
      a[ScalaReflectionException] should be thrownBy {
        accessDot("doesNotExist")(o)
      }
      accessDot("a-long-name")(o) shouldEqual o.longNameImpl
    }
    "access parent value" in {
      class Parent {
        def one = 1
      }
      class Child extends Parent
      val c = new Child
      accessDot("one")(c) shouldEqual c.one
    }
    "access dynamics" in {
      import language.dynamics
      import lisa.LispExp.Implicits._
      import lisa.LispExp.LisaMapRecord
      val result = lisa.LispExp.SString("Whatever")
      val record = LisaMapRecord[Expression](Map(
        "whatever" -> result
      ))
      record.whatever shouldEqual result
      accessDot("whatever")(record) shouldEqual result
    }
  }
  "ApplyDot" should {
    import lisa.Reflect.DotAccessor.applyDot

    "call methods" in {
      val o = new {
        def addOne(x: Int): Int = x + 1
        private def minus(x: Int, y: Int): Int = x - y
      }

      applyDot("addOne")(o)(1) shouldEqual 2
      applyDot("minus")(o)(2, 3) shouldEqual -1
    }

    "call parent methods" in {
      trait P {
        def ?+(x: Int, y: Int) = x + y
      }
      class C extends P
      applyDot("?+")(new C)(2, 3) shouldEqual 5
    }

    "should do type check" in {
      val o = new {
        def idString(s: String): String = s
        def idString(s: lisa.LispExp.SString): String = s.value
      }

      applyDot("idString")(o)("abc") shouldEqual "abc"
      a[ScalaReflectionException] should be thrownBy {
        applyDot("idString")(o)(1)
      }
      a[ScalaReflectionException] should be thrownBy {
        applyDot("idString")(o)("two", "strings")
      }
    }

    "should resolve overloads" in {
      val o = new {
        def handleType(s: String) = "string"
        def handleType(i: Int) = "int"
      }

      applyDot("handleType")(o)("s") shouldEqual "string"
      applyDot("handleType")(o)(233) shouldEqual "int"
      a[ScalaReflectionException] should be thrownBy {
        applyDot("handleType")(o)('a')
      }
    }
  }
}
