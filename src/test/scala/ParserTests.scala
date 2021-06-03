import org.scalatest.matchers.should.Matchers
import org.scalatest.OptionValues
import org.scalatest.wordspec.AsyncWordSpec

import moe.lisa.parsing.LisaParser
import moe.lisa.core.expression.Tree._
import moe.lisa.core.expression.UntypedTree
import moe.lisa.core.expression.UntypedTree.NumberKind
import moe.lisa.core.SourceFile

class ParserTests extends AsyncWordSpec, Matchers, OptionValues:
  given SourceFile.SourceFile = SourceFile.NoSourceFile
  val parser = LisaParser.ofSource

  def parse(input: String): Option[Tree[?]] =
    import parser._
    parseAll(sExpressionOrNil, input) match
      case Success(r, _) => Some(r)
      case _ => None

  "number parser" should {
    "match hex numbers" in {
      parse("0x66ccff").value shouldBe UntypedTree.Number("66ccff", NumberKind.Whole(16))
    }
    "match neg hex numbers" in {
      parse("-0x66ccff").value shouldBe UntypedTree.Number("-66ccff", NumberKind.Whole(16))
    }

    "match dec numbers" in {
      parse("114514").value shouldBe UntypedTree.Number("114514", NumberKind.Whole(10))
      parse("114514n").value shouldBe UntypedTree.Number("114514", NumberKind.BigNumber(10))
    }
  }
