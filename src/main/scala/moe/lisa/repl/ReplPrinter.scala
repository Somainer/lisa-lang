package moe.lisa.repl

import scala.util.control.NonFatal
import dotty.tools.dotc.ast.{untpd => scala}
import dotty.tools.dotc.core.Names._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.core.Constants.{Constant => SConstant}
import dotty.tools.dotc.core.Contexts.{Context, atPhase, ctx}
import dotty.tools.dotc.core.Definitions
import dotty.tools.dotc.core.Denotations._
import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.NameKinds
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.NameOps._
import dotty.tools.dotc.core.Phases._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Signature
import dotty.tools.repl.AbstractFileClassLoader
import dotty.tools.dotc.reporting.Diagnostic
import dotty.tools.dotc.report
import scala.{Ident, TypeTree, cpy => scpy}
import moe.lisa.core.expression.Constant
import moe.lisa.core.expression.{Tree => lisa}
import moe.lisa.core.expression.{UntypedTree => lutpd}

class ReplPrinter(parentClassLoader: Option[ClassLoader] = None) {
  private var myClassLoader: ClassLoader = _
  private def classLoader(using context: Context) = {
    if myClassLoader != null then myClassLoader
    else {
      val parent = parentClassLoader.getOrElse {
        val compilerClasspath = ctx.platform.classPath(using ctx).asURLs
        // We can't use the system classloader as a parent because it would
        // pollute the user classpath with everything passed to the JVM
        // `-classpath`. We can't use `null` as a parent either because on Java
        // 9+ that's the bootstrap classloader which doesn't contain modules
        // like `java.sql`, so we use the parent of the system classloader,
        // which should correspond to the platform classloader on Java 9+.
        val baseClassLoader = ClassLoader.getSystemClassLoader.getParent
        new java.net.URLClassLoader(compilerClasspath.toArray, baseClassLoader)
      }

      myClassLoader = new AbstractFileClassLoader(ctx.settings.outputDir.value, parent)
      myClassLoader
    }
  }
  private def newestWrapper(tree: scala.Tree): Name = tree match {
    case scala.PackageDef(_, (obj: scala.ModuleDef) :: Nil) => obj.name.moduleClassName
    case _ => nme.NO_NAME
  }
  def printDefinedValues(lTrees: List[lutpd.Tree], unit: CompilationUnit, state: ReplState): Seq[Diagnostic] = {
    given Context = state.state.context
    val wrapper = newestWrapper(unit.untpdTree)
    atPhase(typerPhase.next) {
      val denots: Option[SingleDenotation] = unit.tpdTree.symbol.info.memberClasses
        .find(_.symbol.name == wrapper.moduleClassName)
      denots.toSeq.flatMap { wrappedModule =>
        val symbol = wrappedModule.symbol
        try
          renderMembers(symbol)
        catch case NonFatal(ex) =>
          Seq(new Diagnostic.StickyError(exceptionToString(ex.getCause), lTrees.head.sourcePos))
      }
    }
  }

  def exceptionToString(ex: Throwable) =
    import java.io.{StringWriter, PrintWriter}
    val sw = new StringWriter
    val pw = new PrintWriter(sw)
    ex.printStackTrace(pw)
    sw.toString

  def renderMembers(symbol: Symbol)(using Context): Seq[Diagnostic] = {
    import Flags._
    val info: Type = symbol.info
    if !info.exists then Seq.empty
    else
      val defs = info.hiBound.finalResultType
        .membersBasedOnFlags(required = Flags.Method, excluded = Accessor | ParamAccessor | Synthetic | Private)
        .filterNot { denot =>
          defn.topClasses.contains(denot.symbol.owner) || denot.symbol.isConstructor
        }
      val values = info.fields.filter(_.symbol.name.is(NameKinds.SimpleNameKind))
      defs.map(renderDefDef) ++ values.flatMap(renderValDef)
  }

  def paramName(sig: Signature.ParamSig): String = sig match {
    case i: Int => i.toString()
    case n: TypeName => n.decode.toString
  }

  def renderDefDef(denot: Denotation)(using Context): Diagnostic =
    val name: Name = denot.symbol.name
    val symbolDef = name.decode.toString
    val tpe: Type = denot.symbol.info
    val msg = tpe match {
      case meth: MethodType =>
        val sig: Signature = tpe.signature
        val returnType = sig.resSig.decode.toString
        s"$symbolDef: (Fn ${sig.paramsSig.map(paramName).appended(returnType).mkString(" ")})"
      case _ =>
        val name = tpe.typeSymbol.name.toString
        s"$symbolDef: $name"
    }

    infoDiagnostic(msg, denot)

  def getValueOf(sym: Symbol)(using Context): Option[Any] =
    val objectName = sym.owner.fullName.encode.toString.stripSuffix("$")
    val resClz: Class[?] = Class.forName(objectName, true, classLoader)
    val valueName = sym.name.encode.toString
    resClz.getDeclaredMethods.find(_.getName == valueName).map(_.invoke(null))

  def renderValDef(denotation: Denotation)(using Context): Option[Diagnostic] =
    getValueOf(denotation.symbol).flatMap { value =>
      val name = denotation.name.decode.toString
      val tpe = denotation.symbol.info
      if tpe == defn.UnitType then None
      else
        val typeName = tpe.typeSymbol.name
        Some(infoDiagnostic(s"$name: $typeName = $value", denotation))
    }

  def infoDiagnostic(msg: String, d: Denotation)(using Context): Diagnostic =
    new Diagnostic.Info(msg, d.symbol.sourcePos)
}
