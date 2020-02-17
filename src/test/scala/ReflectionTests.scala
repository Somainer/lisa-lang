import org.scalatest.wordspec.AsyncWordSpec
import org.scalatest.matchers.should.Matchers
import moe.roselia.lisa
import moe.roselia.lisa.Annotation.RawLisa
import moe.roselia.lisa.LispExp.{Expression, SInteger, SString}

class ReflectionTests extends AsyncWordSpec with Matchers {
  class Tester {
    @RawLisa
    def lisaString(a: SString): String = a.value
    @RawLisa
    def lisaString(i: SInteger): String = i.value.toString

    def lisaString(fn: Double): String = fn.toString
  }
  class DistinctException extends RuntimeException
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

    "throw what is thrown" in {
      val x = new {
        def o: Nothing = throw new DistinctException
      }
      a [DistinctException] shouldBe thrownBy {
        accessDot("o")(x)
      }
    }
  }
  "ApplyDot" should {
    import lisa.Reflect.DotAccessor.applyDot

    "call methods" in {
      val o = new {
        def addOne(x: Int): Int = x + 1
        private def minus(x: Int, y: Int): Int = x - y
      }

      applyDot("addOne")(o)(1)() shouldEqual 2
      applyDot("minus")(o)(2, 3)() shouldEqual -1
    }

    "call parent methods" in {
      trait P {
        def ?+(x: Int, y: Int) = x + y
      }
      class C extends P
      applyDot("?+")(new C)(2, 3)() shouldEqual 5
    }

    "should do type check" in {
      val o = new {
        def idString(s: String): String = s
        def idString(s: lisa.LispExp.SString): String = s.value
      }

      applyDot("idString")(o)("abc")() shouldEqual "abc"
      a[ScalaReflectionException] should be thrownBy {
        applyDot("idString")(o)(1)()
      }
      a[ScalaReflectionException] should be thrownBy {
        applyDot("idString")(o)("two", "strings")()
      }
    }

    "should resolve overloads" in {
      val o = new {
        def handleType(s: String) = "string"
        def handleType(i: Int) = "int"
      }

      applyDot("handleType")(o)("s")() shouldEqual "string"
      applyDot("handleType")(o)(233)() shouldEqual "int"
      a[ScalaReflectionException] should be thrownBy {
        applyDot("handleType")(o)('a')()
      }
    }

    "should resolve RawLisa annotation" in {
      val tester = new Tester

      applyDot("lisaString")(tester)("native")(SString("lisa")) shouldEqual "lisa"
      applyDot("lisaString")(tester)()(SInteger(233)) shouldEqual "233"
      applyDot("lisaString")(tester)(114.514)() shouldEqual "114.514"
      an[Exception] should be thrownBy {
        applyDot("lisaString")(tester)("no way")()
      }
    }

    "should not catch control flows" in {
      val breaks = new util.control.Breaks
      a[util.control.ControlThrowable] should be thrownBy {
        applyDot("break")(breaks)()()
      }
    }

    "throw what is thrown" in {
      val x = new {
        def f(x: Int) = throw new DistinctException
      }

      a [DistinctException] shouldBe thrownBy {
        applyDot("f")(x)(1)()
      }
    }
  }

  "Reflection Constructor" should {
    import moe.roselia.lisa.Reflect.ConstructorCaller.newInstanceFromClassName

    "construct object from simple names" in {
      val string = newInstanceFromClassName("String", Seq("string"))
      string shouldEqual "string"

      newInstanceFromClassName("Some", Seq(1)) shouldEqual Some(1)
    }
  }

  "Static Field Accessor" should {
    import moe.roselia.lisa.Reflect.StaticFieldAccessor._

    "Access field with names" in {
      getFieldOrNilArityMethod(classOf[Math], "PI") shouldEqual Math.PI
      a[NoSuchFieldException] should be thrownBy {
        getFieldOrNilArityMethod(classOf[Math], "cos")
      }
      a [NoSuchFieldException] should be thrownBy {
        getFieldOrNilArityMethod(classOf[String], "PI")
      }
    }

    "Invoke static methods" in {
      invokeStaticMethod(classOf[String], "valueOf")(114514) shouldEqual "114514"
      invokeStaticMethod(classOf[Math], "max")(1, 2) shouldEqual 2
      a[NoSuchMethodException] should be thrownBy invokeStaticMethod(classOf[Math], "max")(2)
      invokeStaticMethod(classOf[Class[_]], "forName")("java.lang.String") shouldEqual classOf[java.lang.String]
    }

    "Handle Vararg functions" in {
      invokeStaticMethod(classOf[String], "format")("%d%s", 2, "33") shouldEqual "233"
      invokeStaticMethod(classOf[String], "format")("Hello") shouldEqual "Hello"
    }
  }
}
