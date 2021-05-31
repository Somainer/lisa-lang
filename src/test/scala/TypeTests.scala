import moe.roselia.lisa.LispExp.NilObj
import moe.roselia.lisa.Typing
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import moe.roselia.lisa.Typing.{AnyType, ClassType, CompoundType, FunctionType, InstantiatedType, LisaType, NothingType, SequenceType, SequenceTypeConstructor, TupleType, TupleTypeConstructor, TypeVariable, Untyped, ValueType}

class TypeTests extends AsyncWordSpec with Matchers with TypeHelper {
  "class of types" should {
    val stringType = LisaType.of[String]

    "identical type should be equal" in {
      stringType =:= ClassType.of[String] shouldBe true
    }

    "type constructors should work" in {
      LisaType.of[String].asInstanceOf[InstantiatedType].runtimeClass shouldBe classOf[String]
      ValueType.of[Int].runtimeClass shouldBe Integer.TYPE
      LisaType.of[Int] shouldBe ValueType.of[Int]
      ClassType.of[Integer].runtimeClass shouldBe classOf[Integer]
    }

    "bottom type should be assignable to any type" in {
      NothingType <:< stringType shouldBe true
      stringType <:< NothingType shouldBe false
    }

    "value types should be assignable to boxed type" in {
      ValueType.of[Int].boxedType shouldBe ClassType.of[Integer]
      LisaType.of[Int] <:< LisaType.of[Integer] shouldBe true
      LisaType.of[Integer] <:< LisaType.of[Int] shouldBe true
      LisaType.of[Int] <:< LisaType.of[Int] shouldBe true
      LisaType.of[Int] =:= LisaType.of[Integer] shouldBe true
    }

    "partially ordering should work" in {
      LisaType.of[Int] should be <= LisaType.of[Int]
      LisaType.of[Integer] should be <= LisaType.of[Int]
      LisaType.of[String] should be < LisaType.any
    }

    "nil should be assignable to a list" in {
      LisaType.nil <:< SequenceTypeConstructor.construct(List(LisaType.nothing)) shouldBe true
    }

    "sequence like types should be covariant" in {
      TupleTypeConstructor
        .construct(List(LisaType.of[Int], LisaType.of[String])) <:< TupleTypeConstructor
        .construct(List(LisaType.of[Int], LisaType.any)) shouldBe true

      SequenceTypeConstructor.construct(Seq(LisaType.nothing)) <:< SequenceTypeConstructor.construct(Seq(LisaType.nothing)) shouldBe true
    }

    "tuple type should be covariant" in {
      val tupleOfNothing = TupleTypeConstructor.construct(List(LisaType.nothing))
      val tupleOfString = TupleTypeConstructor.construct(List(LisaType.of[String]))

      tupleOfNothing <:< tupleOfString shouldBe true
    }

    "tuple type constructor is a function type" in {
      val fnType = new FunctionType {
        override def arguments: TupleType = TupleTypeConstructor.construct(Seq(SequenceType(LisaType.itself))).asInstanceOf[TupleType]

        override def returnType: LisaType = LisaType.any

        override def construct(typeArgs: Seq[LisaType]): CompoundType = ???
        override def name: String = "fn"
        override def fullName: String = ???
      }

      val anyFnType = new FunctionType {
        override def arguments: TupleType = TupleTypeConstructor.construct(Seq(SequenceType(LisaType.any))).asInstanceOf[TupleType]

        override def returnType: LisaType = LisaType.nothing

        override def construct(typeArgs: Seq[LisaType]): CompoundType = ???
        override def name: String = "fn"
        override def fullName: String = ???
      }

      TupleTypeConstructor <:< fnType shouldBe true
      anyFnType <:< TupleTypeConstructor shouldBe true
    }

    "untyped should not be comparable with any other types" in {
      Untyped.isAssignableFrom(Untyped) shouldBe empty
      val gen = TypeVariable.generator()
      gen.hasNext shouldBe true
      gen.next() should not be gen.next()
      gen.next() <:< gen.next() shouldBe false
    }

    "type should be an expression" in {
      NilObj.lisaType <:< LisaType.nil shouldBe true
      NilObj.lisaType.lisaType <:< LisaType.nothing.lisaType shouldBe true
      NilObj.lisaType.lisaType.lisaType shouldBe LisaType.itself
    }
  }
}
