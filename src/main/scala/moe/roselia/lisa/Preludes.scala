package moe.roselia.lisa

import moe.roselia.lisa.Environments.{CombineEnv, EmptyEnv, Environment, MutableEnv, NameSpacedEnv, SpecialEnv, TransparentLayer}
import moe.roselia.lisa.Evaluator.{EvalFailure, EvalFailureMessage, EvalResult, EvalSuccess}
import moe.roselia.lisa.Import.PackageImporter
import moe.roselia.lisa.LispExp._
import moe.roselia.lisa.Reflect.{ConstructorCaller, PackageAccessor, ToolboxDotAccessor}
import moe.roselia.lisa.Reflect.ScalaBridge.{fromJVMNative, fromScalaNative, toScalaNative}

import scala.util.Try

object Preludes extends LispExp.Implicits {
  type EngineType = javax.script.ScriptEngine with javax.script.Invocable
  private lazy val globalJSEngine = new javax.script.ScriptEngineManager()
    .getEngineByName("ecmascript").asInstanceOf[EngineType]
  private lazy val globalScalaEngine = {
    val engine = new javax.script.ScriptEngineManager()
      .getEngineByName("scala").asInstanceOf[EngineType]
    engine
  }

  type DirtyPromise[T] = () => T
  import scala.language.implicitConversions
  implicit def wrapPromise[T](t: => T): DirtyPromise[T] = () => t
  implicit def unwrapPromise[T](t: DirtyPromise[T]): T = t()

  private lazy val selectablePreludes = Map[String, DirtyPromise[Environment]](
    "javascript" -> javaScriptPlugin,
    "javascript-cross" -> javaScriptEnv,
    "scala" -> scalaPlugin,
    "scala-cross" -> scalaEnv,
    "scala-root" -> PackageAccessor.rootScalaEnv,
    "dot-accessor" -> ToolboxDotAccessor.accessEnv,
    "system" -> Library.System.systemEnv,
    "io-source" -> Library.IOSource.sourceLibrary,
    "logical" -> Logical.LogicalModuleEnvironment,
    "predef" -> preludeEnvironment,
  ).view.mapValues(_.withIdentify("prelude"))

  private lazy val primitiveEnvironment: Environment = EmptyEnv.withValues(Seq(
    "+" -> PrimitiveFunction {
      case ls@x :: _ if x.isInstanceOf[SNumber[_]] =>
        ls.asInstanceOf[List[SNumber[_]]].reduce(SNumber.performComputation(_ + _))
      case xs: List[SString] => SString(xs.map(_.value).reduce(_ + _))
    },
    "-" -> PrimitiveFunction {
      case (x: SNumber[_])::Nil => -x
      case xs: List[SNumber[_]] => xs.reduce(SNumber.performComputation(_ - _))
    },
    "*" -> PrimitiveFunction {
      case xs: List[SNumber[_]] => xs.reduce(SNumber.performComputation(_ * _))
    },
    "/" -> PrimitiveFunction {
      case (lhs: SNumber[_]) :: (rhs: SNumber[_]) :: Nil =>
        SNumber.performComputation(_ / _)(lhs, rhs)
    },
    "<" -> PrimitiveFunction {
      case xs@head::_::_ if head.isInstanceOf[SNumber[_]] =>
        val ls = xs.asInstanceOf[List[SNumber[_]]]
        SBool(ls.zip(ls.tail).view.map(Function.tupled(SNumber.performComputation(_ < _))).forall(identity))
      case xs@head::_::_ if head.isInstanceOf[Comparable[_]] =>
        val ls = xs.asInstanceOf[List[Comparable[Any]]]
        ls.zip(ls.tail).view.map(Function.tupled(_ compareTo _)).forall(_ < 0)
    },
    "<=" -> PrimitiveFunction {
      case xs@head::_::_ if head.isInstanceOf[SNumber[_]] =>
        val ls = xs.asInstanceOf[List[SNumber[_]]]
        SBool(ls.zip(ls.tail).view.map(Function.tupled(SNumber.performComputation(_ <= _))).forall(identity))
      case xs@head::_::_ if head.isInstanceOf[Comparable[_]] =>
        val ls = xs.asInstanceOf[List[Comparable[Any]]]
        ls.zip(ls.tail).view.map(Function.tupled(_ compareTo _)).forall(_ <= 0)
    },
    ">" -> PrimitiveFunction {
      case xs@head::_::_ if head.isInstanceOf[SNumber[_]] =>
        val ls = xs.asInstanceOf[List[SNumber[_]]]
        SBool(ls.zip(ls.tail).view.map(Function.tupled(SNumber.performComputation(_ > _))).forall(identity))
      case xs@head::_::_ if head.isInstanceOf[Comparable[_]] =>
        val ls = xs.asInstanceOf[List[Comparable[Any]]]
        ls.zip(ls.tail).view.map(Function.tupled(_ compareTo _)).forall(_ > 0)
    },
    ">=" -> PrimitiveFunction {
      case xs@head::_::_ if head.isInstanceOf[SNumber[_]] =>
        val ls = xs.asInstanceOf[List[SNumber[_]]]
        SBool(ls.zip(ls.tail).view.map(Function.tupled(SNumber.performComputation(_ >= _))).forall(identity))
      case xs@head::_::_ if head.isInstanceOf[Comparable[_]] =>
        val ls = xs.asInstanceOf[List[Comparable[Any]]]
        ls.zip(ls.tail).view.map(Function.tupled(_ compareTo _)).forall(_ >= 0)
    },
    "=" -> PrimitiveFunction {
      case (lhs: SNumber[_]) :: (rhs: SNumber[_]) :: Nil =>
        SNumber.performComputation(_ equalsTo _)(lhs, rhs)
      case lhs::rhs::Nil => lhs == rhs
    }.withArity(2),
    "print!" -> PrimitiveFunction {
      case x::Nil =>
        print(x)
        NilObj
      case x =>
        print(s"${x.mkString(" ")}")
        NilObj
    },
    "println!" -> PrimitiveFunction {
      case x::Nil =>
        println(x)
      case x =>
        println(s"${x.mkString(" ")}")
    },
    "input" -> PrimitiveFunction {
      x =>
        SString(scala.io.StdIn.readLine(x.mkString(" ")))
    },
    "int" -> PrimitiveFunction {
      case v::Nil => SInteger {
        v match {
          case SString(x) => BigInt(x)
          case SInteger(x) => x
          case SFloat(f) => f.toBigInt
          case SBool(b) => if(b) 1 else 0
          case num: SNumber[_] => num.toIntNumber.number
        }
      }
    }.withArity(1),
    "eval" -> SideEffectFunction {
      case (x::Nil, env) => Evaluator.eval(Evaluator.unQuoteList(x), env) match {
        case EvalSuccess(expression, newEnv) => expression -> newEnv
        case EvalFailureMessage(msg) => Failure("Eval Failure", msg) -> env
      }
    },
    "truthy?" -> PrimitiveFunction {
      case ex::Nil => LispExp.SBool {
        ex match {
          case LispExp.NilObj => false
          case LispExp.SInteger(n) => n != 0
          case LispExp.SFloat(n) => n != 0.0
          case LispExp.SBool(b) => b
          case LispExp.SString(s) => s.nonEmpty
          case _ => true
        }
      }
      case _ => LispExp.Failure("Runtime Error", "truthy? can only apply to one value")
    }.withArity(1),
    "import-env!" -> PrimitiveMacro {
      case (Symbol(sym)::Nil, env) =>
        if (selectablePreludes.contains(sym))
          (NilObj, CombineEnv(Seq(env, selectablePreludes(sym))))
        else (Failure("Import Error", s"Environment $sym not found"), env)
      case (Symbol(sym)::Symbol(ns)::Nil, env) =>
        if (selectablePreludes.contains(sym))
          (NilObj, CombineEnv(Seq(env, NameSpacedEnv(ns, selectablePreludes(sym)))))
        else (Failure("Import Error", s"Environment $sym not found"), env)
      case s => (Failure("Import Error", s"Cannot import ${s._1}"), s._2)
    },
    "wrap-scala" -> PrimitiveFunction {
      x => WrappedScalaObject(toScalaNative(x(0)))
    }.withArity(1),
    "wrap" -> PrimitiveFunction { x => WrappedScalaObject(x.head) }.withArity(1),
    "set!" -> new PrimitiveMacro({
      case (Symbol(x)::va::Nil, e) =>
        if (e.isMutable(x))
          Evaluator.eval(va, e) match {
            case EvalSuccess(expression, _) => (NilObj, e.forceUpdated(x, expression))
            case EvalFailureMessage(message) => (Failure("Eval Failure", message), e)
          }
        else (Failure("set! Error", s"Can not assign an immutable value $x."), e)
      case (other, e) => (Failure("Arity Error", s"set! only accepts a symbol and an expression, but $other found."), e)
    }) with WithFreeValues {
      override def collectEnvDependency(defined: Set[String], env: Environment, context: List[Expression]): (Set[String], Set[String]) =
        context match {
          case Symbol(x)::_::Nil => (if(defined contains x) Set.empty else Set(x), defined)
          case _ => throw new IllegalArgumentException(s"set! only accepts a symbol and an expression, but $context found.")
        }
    }.withArity(2),
    "define-mutable!" -> new PrimitiveMacro({
      case (Symbol(x)::Nil, e) =>
        (NilObj, TransparentLayer(MutableEnv.createEmpty.addValue(x, e.getValueOption(x).getOrElse(NilObj)), e))
      case (_, e) => (Failure("Define Failure", "define-mutable! only accepts one symbol."), e)
    }) with WithFreeValues {
      override def collectEnvDependency(defined: Set[String], env: Environment, context: List[Expression]): (Set[String], Set[String]) =
        context match {
          case Symbol(x)::Nil => (Set.empty, defined + x)
          case _ => throw new IllegalArgumentException(s"Can not define mutable value: $context")
        }
    }.withArity(1),
    "group!" -> PrimitiveMacro {
      case (xs, e) => xs.foldLeft[EvalResult](EvalSuccess(NilObj, e)) {
        case (EvalSuccess(_, env), x) => Evaluator.eval(x, env)
        case (f, _) => f
      } match {
        case EvalSuccess(expression, env) => expression -> env
        case EvalFailureMessage(msg) => Failure("Group Code Execution Failure", msg) -> e
      }
    },
    "block" -> PrimitiveMacro {
      case (xs, e) => xs.foldLeft[EvalResult](EvalSuccess(NilObj, e.newFrame)) {
        case (EvalSuccess(_, env), x) => Evaluator.eval(x, env)
        case (f, _) => f
      } match {
        case EvalSuccess(expression, _) => expression -> e
        case EvalFailureMessage(msg) => Failure("Block Code Execution Failure", msg) -> e
      }
    },
    "while" -> PrimitiveMacro {
      case (predicate::body::Nil, e) =>
        @annotation.tailrec
        def repeat(env: Environment): EvalResult = Evaluator.eval(predicate, env) match {
          case EvalSuccess(SBool(true), _) =>
            Evaluator.eval(body, env) match {
              case EvalSuccess(_, ne) => repeat(ne)
              case f => f
            }
          case f@EvalSuccess(SBool(false), _) => f
          case EvalSuccess(expression, _) => EvalFailure(s"While predicate $expression is not a Bool.")
          case f => f
        }
        repeat(e) match {
          case EvalSuccess(_, en) => (NilObj, en)
          case EvalFailureMessage(msg) => Failure("While execution failure", msg) -> e
        }
    }.withArity(2),
    "help" -> PrimitiveFunction {
      case exp::Nil =>
        val docString = exp.docString
        if(docString.nonEmpty) docString else exp.code
      case _ => Failure("Help error", "You can only get help from one object.")
    }.withDocString("help :: Any => String\nGet document string from a object.").withArity(1),
    "panic!" -> PrimitiveFunction {
      exp => throw new RuntimeException(exp.mkString(" "))
    },
    "apply" -> PrimitiveFunction {
      case fn::WrappedScalaObject(s: Seq[Expression])::Nil =>
        Evaluator.applyToEither(fn, s.toList).fold(Failure("Apply Failure", _), x => x)
      case fn :: LisaList(s) :: Nil =>
        Evaluator.applyToEither(fn, s).fold(Failure("Apply Failure", _), x => x)
      case _ => Failure("Apply Failure", "Apply expects 2 arguments.")
    }.withArity(2),
    "limit-arity" -> PrimitiveFunction {
      case SInteger(n)::fn::Nil =>
        val argList = 0.until(n.toInt).map(i => s"arg$i").map(PlainSymbol).toList
        Closure(argList, Apply(fn, argList), EmptyEnv).copyDocString(fn)
    }.withArity(2).withDocString("Limit va-arg function to accept n arguments"),
    "get-doc" -> PrimitiveFunction {
      case f1::Nil => f1.docString
    }.withArity(1),
    "set-doc" -> PrimitiveFunction {
      case f1::SString(s)::Nil => f1.withDocString(s)
    }.withArity(2),
    "define-phrase" -> PrimitiveMacro {(x, e) =>
      def defineHelper(toBeDefined: SimpleMacroClosure) = {
        val previous = e.getValueOption(PHRASE_VAR)
        if (previous.exists(_.isInstanceOf[PolymorphicExpression]))
          NilObj -> e.withValue(PHRASE_VAR, previous.get.asInstanceOf[PolymorphicExpression].withExpression(toBeDefined))
        else Define(Symbol(PHRASE_VAR), toBeDefined) -> e
      }
      (x, e) match {
        case (Apply(head, tail) :: body, _) =>
          val toBeDefined = SimpleMacroClosure((head :: tail).map(_.toRawList), body.last, body.init, e)
          defineHelper(toBeDefined)
        case (Symbol(defined)::Nil, _)
          if e.getValueOption(defined).exists(_.isInstanceOf[SimpleMacroClosure]) =>
          defineHelper(e.getValueOption(defined).get.asInstanceOf[SimpleMacroClosure])
      }
    },
    "try-option" -> PrimitiveMacro {
      case (expr::Nil, e) => Evaluator.eval(expr, e) match {
        case EvalSuccess(obj, _) => WrappedScalaObject(Some(obj)) -> e
        case _ => WrappedScalaObject(None) -> e
      }
      case (_, e) => Failure("Arity Error", "try-option only accepts one argument.") -> e
    }.withArity(1),
    "string->symbol" -> PrimitiveFunction {
      case SString(sym)::Nil => Symbol(sym)
      case _ => Failure("Contract Violation", "only accept a string")
    }.withArity(1),
    "string->atom" -> PrimitiveFunction.withArityChecked(1) {
      case SString(sym) :: Nil => SAtom(sym)
      case _ => Failure("Contract Violation", "only accept a string")
    },
    "string" -> PrimitiveFunction { xs =>
      xs.mkString
    }.withArity(1),
    "string/interpolate" -> PrimitiveFunction.withArityChecked(2) {
      case LisaList(part :: parts) :: LisaList(arguments) :: Nil =>
        (part :: arguments.zip(parts).flatten(x => List(x._1, x._2))).mkString
    },
    "returnable" -> PrimitiveFunction {
      case fn :: Nil =>
        val returnable = new Util.ReturnControlFlow.Returns
        val returnFn = PrimitiveFunction {
          case x :: Nil => returnable.returns(x)
          case Nil => returnable.returns(NilObj)
          case _ => throw new IllegalArgumentException("Too many arguments.")
        }
        returnable.returnable {
          Evaluator.eval(Apply(fn, returnFn :: Nil), EmptyEnv) match {
            case EvalSuccess(exp, _) => exp
            case EvalFailureMessage(message) =>
              throw new RuntimeException(s"Returnable: $message")
          }
        }
    },
    "expand-macro" -> SideEffectFunction {
      case (LisaList(m :: args) :: Nil, env) =>
        Evaluator.eval(m, env) match {
          case EvalSuccess(mac: SimpleMacroClosure, _) =>
            Quote(Evaluator.expandMacro(mac, args, env)) -> env
          case EvalSuccess(pm@PolymorphicExpression(_, _, _, true), _) =>
            pm.findMatch(args, env).map {
              case ((mac: SimpleMacroClosure), _) =>
                Quote(Evaluator.expandMacro(mac, args, env)) -> env
            }.get
        }
    },
    "gen-sym" -> PrimitiveFunction {
      case Nil => Util.SymGenerator.nextSym
      case _ => throw new IllegalArgumentException("gen-sym does not accept arguments.")
    }.withArity(0),
    "write" -> PrimitiveFunction {
      case x :: Nil => x.code
    }.withArity(1),
    "ast-of" -> PrimitiveFunction {
      case x :: Nil => Evaluator.unQuoteList(x)
    }.withArity(1),
    "prelude-environment" -> PrimitiveFunction.withArityChecked(0) {
      case _ =>
        LisaMapRecord(preludeEnvironment.
          collectDefinedValues.map(key => key -> preludeEnvironment.getValueOption(key).get).toMap)
      case _ =>
        throw new IllegalArgumentException(s"prelude-environment do not expect arguments")
    },
    "freeze-environment" -> SideEffectFunction { case (Nil, env) =>
      new LisaRecordWithMap[Expression] {
        override def selectDynamic(key: String): Expression = env.getValueOption(key).getOrElse(throw new NoSuchElementException(key))

        override def containsKey(key: String): Boolean = env.has(key)

        override def getOrElse[EV >: Expression](key: String, otherwise: => EV): EV = env.getValueOption(key).getOrElse(otherwise)

        override def recordTypeName: String = s"frozen-environment"

        override def indented(bySpace: Int, level: Int): String = toString

        override def toString: String = s"#$recordTypeName [not-computed]"

        override def tpe: LisaType = NameOnlyType("LisaRecord")

        override def updated[B >: Expression <: Expression](key: String, value: B): LisaRecordWithMap[B] =
          LisaMapRecord(record.updated(key, value), recordTypeName)

        override def record: Map[String, Expression] = env.flattenToMap
      } -> env
      case  _ => throw new IllegalArgumentException(s"freeze-environment do not expect arguments")
    },
    "read-string" -> PrimitiveFunction.withArityChecked(1) {
      case SString(s) :: Nil =>
        import SExpressionParser._
        parseAll(sExpression, s).map(Evaluator.compileToList) match {
          case Success(result, _) => result
          case f@NoSuccess(_, _) => throw new IllegalArgumentException(s"Bad syntax: $f")
        }
    },
    "read-many-from-string" -> PrimitiveFunction.withArityChecked(1) {
      case SString(s) :: Nil =>
        import SExpressionParser._
        parseAll(rep(sExpression), s).map(_.map(Evaluator.compileToList)) match {
          case Success(result, _) => LisaList(result)
          case f@NoSuccess(_, _) => throw new IllegalArgumentException(s"Bad syntax: $f")
        }
    },
    "read" -> PrimitiveFunction { xs =>
      import SExpressionParser._
      parseAll(sExpression, io.StdIn.readLine(xs.mkString(" "))).map(Evaluator.compileToList) match {
        case Success(result, _) => result
        case f@NoSuccess(_, _) => throw new IllegalArgumentException(s"Bad syntax: $f")
      }
    },
    "read-many" -> PrimitiveFunction { xs =>
      import SExpressionParser._
      print(xs.mkString(" "))
      val input = Iterator.continually(io.StdIn.readLine).takeWhile(_ ne null).mkString
      parseAll(rep(sExpression), input).map(_.map(Evaluator.compileToList)) match {
        case Success(result, _) => LisaList(result)
        case f@NoSuccess(_, _) => throw new IllegalArgumentException(s"Bad syntax: $f")
      }
    },
    "declare" -> PrimitiveMacro { case (declares, env) =>
      val mutableEnv = env match {
        case mutable: MutableEnv => mutable
        case e => e.newMutableFrame
      }
      require(declares.forall(_.isInstanceOf[Symbol]))
      declares.foreach(sym => mutableEnv.addValue(sym.asInstanceOf[Symbol].value, PlaceHolder))
      NilObj -> mutableEnv
    },
    "from-java" -> PrimitiveFunction.withArityChecked(1) {
      case WrappedScalaObject(x) :: Nil => fromJVMNative(x)
      case x :: Nil => x
    },
    "quoted" -> PrimitiveFunction.withArityChecked(1) {
      case x :: Nil => Quote(x)
    },
    "value-exists?" -> SideEffectFunction {
      case (Symbol(s) :: Nil, env) => (env.has(s), env)
      case (SString(s) :: Nil, env) => (env.has(s), env)
    }.withDocString("Test if such value exists in calling context."),
    "import!" -> PackageImporter.importMacro,
    "import" -> PackageImporter.importFunction,
    "require" -> PackageImporter.requireFunction,
    "get-static-method" -> PrimitiveFunction.withArityChecked(2) {
      case WrappedScalaObject(clazz: Class[_]) :: SString(name) :: Nil =>
        Reflect.StaticFieldAccessor.convertStaticMethodToLisa(clazz, name)
    },
    "thunk" -> PrimitiveFunction.withArityChecked(1) {
      case (e: Procedure) :: Nil => LisaThunk(e)
    }
  ))

  private lazy val (scalaPlugin, scalaEnv) = makeEnvironment(globalScalaEngine, "scala")

  private lazy val (javaScriptPlugin, javaScriptEnv) = makeEnvironment(globalJSEngine, "js")

  private def makeEnvironment(engine: => EngineType, prefix: String) = {
    if (engine == null) throw new NullPointerException("The engine is not found")
    val environment = new SpecialEnv {
      override def getValueOption(key: String): Option[Expression] =
        Try(engine.getContext.getAttribute(key, javax.script.ScriptContext.ENGINE_SCOPE))
          .filter(x => x != null).map(Reflect.ScalaBridge.fromScalaNative).toOption
    }
    (EmptyEnv.withValues(Seq(
      prefix -> PrimitiveFunction {
        case SString(s)::Nil => Reflect.ScalaBridge.fromScalaNative(
          engine.eval(s)
        )
      },
      s"set-$prefix!" -> PrimitiveMacro {
        case (Symbol(sym)::exp::Nil, e) =>
          engine.getContext.setAttribute(sym, toScalaNative(exp), javax.script.ScriptContext.ENGINE_SCOPE)
          (NilObj, e)
      },
      s"get-$prefix" -> PrimitiveMacro {
        case (Symbol(sym)::Nil, e) =>
          environment.getValueOption(sym).getOrElse(Failure("Lookup Error", s"Value $sym not found in $prefix env.")) -> e
        case (Quote(Symbol(sym))::alt::Nil, e) =>
          environment.getValueOption(sym).getOrElse(alt) -> e
        case (other, e) => Failure("Argument Error", s"Invalid argument: $other") -> e
      }

    )), environment)
  }

  private lazy val collectionEnvironment = EmptyEnv.withValues(Seq(
    "cons" -> PrimitiveFunction.withArityChecked(2) {
      case x :: (ll: LisaListLike[Expression]) :: Nil => LisaList(x :: ll.list)
      case x :: WrappedScalaObject(wl: Seq[_]) :: Nil => WrappedScalaObject(x +: wl)
      case x :: y :: Nil => LisaList.fromExpression(x, y)
    },
    "car" -> PrimitiveFunction.withArityChecked(1) {
      case LisaList(x :: _) :: Nil => x
      case WrappedScalaObject(ls: Seq[_]) :: Nil => fromScalaNative(ls.head)
      case SString(s) :: Nil => s.head.toString
      case x :: Nil => throw new UnsupportedOperationException(s"car on $x")
    },
    "cdr" -> PrimitiveFunction.withArityChecked(1) {
      case LisaList(_ :: t) :: Nil => LisaList(t)
      case WrappedScalaObject(ls: Seq[_]) :: Nil => WrappedScalaObject(ls.tail)
      case SString(s) :: Nil => s.tail
      case x :: Nil => throw new UnsupportedOperationException(s"cdr on $x")
    },
    "map" -> PrimitiveFunction.withArityChecked(2) {
      case WrappedScalaObject(ls: Iterable[Any])::fn::Nil => fn match {
        case WrappedScalaObject(obj) =>
          WrappedScalaObject(ls.map(x =>
            obj.asInstanceOf[{def apply(a: Any): Any}].apply(x)))
        case x => WrappedScalaObject(Util.CollectionHelper.generalMap(ls, x).toIndexedSeq)
      }
      case WrappedScalaObject(els)::fn::Nil => fromScalaNative(els.asInstanceOf[{
        def map(a: Any): Any
      }].map(toScalaNative(fn)))
      case LisaList(ll) :: fn :: Nil =>
        LisaList(Util.CollectionHelper.generalMap(ll, fn).toList)
      case SString(s) :: fn :: Nil =>
        LisaList(Util.CollectionHelper.generalMap(s.toIterable, fn).toList)
      case (it: Iterable[_]) :: fn :: Nil =>
        LisaList(Util.CollectionHelper.generalMap(it, fn).toList)
      case _ => Failure("Arity Error", "map only accepts 2 arguments, a seq-like and a function-like.")
    },
    "filter" -> PrimitiveFunction {
      case WrappedScalaObject(ls: Iterable[Expression])::fn::Nil => {
        WrappedScalaObject(Util.CollectionHelper.generalFilter(ls, fn).toIndexedSeq)
      }
      case LisaList(ll) :: fn :: Nil =>
        LisaList(Util.CollectionHelper.generalFilter(ll, fn).toList)
      case (it: Iterable[Expression]) :: fn :: Nil =>
        LisaList(Util.CollectionHelper.generalFilter(it, fn).toList)
      case SString(s) :: fn :: Nil =>
        Util.CollectionHelper.generalFilter(s.toIterable.map(_.toString).map(SString), fn).mkString
      case _ => Failure("Arity Error", "filter only accepts 2 arguments, a seq-like and a function-like.")
    }.withArity(2),
    "iter" -> PrimitiveFunction {
      case x::Nil => x match {
        case SString(s) => LisaList(s.split("").map(SString).toList)
        case WrappedScalaObject(xs: Iterable[Any]) => LisaList(xs.map(fromScalaNative).toList)
        case ll: LisaListLike[_] => ll
        case _ => Failure("iter Error", s"$x is not iterable.")
      }
      case _ => Failure("Arity Error", "iter only accepts 1 argument, an iterable.")
    }.withArity(1),
    "length" -> PrimitiveFunction {
      case arg::Nil => arg match {
        case mha: MayHaveArity if mha.arity.isDefined => mha.arity.get
        case SString(s) => s.length
        case WrappedScalaObject(ls: Seq[Any]) => ls.length
        case WrappedScalaObject(other) => ToolboxDotAccessor.accessDot("length")(other).asInstanceOf[Int]
        case ll: LisaListLike[_] => ll.length
        case other => Failure("Runtime Error", s"Can not get length for $other.")
      }
      case other => Failure("Arity Error", s"length only accepts one argument but ${other.length} found.")
    }.withArity(1),
    "list" -> PrimitiveFunction (LisaList(_)),
    "seq" -> PrimitiveFunction (xs => WrappedScalaObject(xs.toIndexedSeq)),
    "nth" -> PrimitiveFunction { arguments =>
      require(arguments.length == 2 || arguments.length == 3, "nth expects 2 or 3 arguments")
      def getDefault(i: Int): Expression =
        if (arguments.length == 3) arguments.last else throw new IndexOutOfBoundsException(i)
      arguments(1) match {
        case SInteger(n) =>
          val key = n.toInt
          Util.CollectionHelper.generalGetElementOfSeqLike(arguments.head, key, getDefault(key))
        case ex =>
          throw new IllegalArgumentException(s"Contract violation: key of seq-like must be an integer, but ${ex.tpe.name} found.")
      }
    },
    "get" -> PrimitiveFunction { arguments =>
      require(arguments.length == 2 || arguments.length == 3, "get expects 2 or 3 arguments")
      def getDefault: Expression = if (arguments.length == 3) arguments.last else NilObj
      val keyExpr = arguments(1)

      arguments.head match {
        case lr: LisaRecord[Expression] =>
          keyExpr match {
            case SString(key) => lr.getOrElse(key, getDefault)
            case ex =>
              throw new IllegalArgumentException(s"Contract violation: key of Record must be a string, but ${ex.tpe.name} found.")
          }
        case coll =>
          keyExpr match {
            case SInteger(n) =>
              val key = n.toInt
              Util.CollectionHelper.generalGetElementOfSeqLike(coll, key, getDefault)
            case invalidKey =>
              throw new IllegalArgumentException(s"Contract violation: key of seq-like must be an integer, but ${invalidKey.tpe.name} found.")
          }
      }
    },
    "match-list" -> PrimitiveFunction.withArityChecked(2) {
      case (LisaList(pattern)) :: (LisaList(arguments)) :: Nil =>
        Evaluator.matchArgument(pattern, arguments).map(LisaMapRecord(_)).getOrElse(NilObj)
    },
    "->record" -> PrimitiveFunction.withArityChecked(1) {
      case WrappedScalaObject(m: Map[_, _]) :: Nil =>
        LisaMapRecord(m.map {
          case (k, v) => k.toString -> fromScalaNative(v)
        })
      case WrappedScalaObject(jm: java.util.Map[_, _]) :: Nil =>
        import scala.jdk.CollectionConverters._
        LisaMapRecord(jm.asScala.map {
          case (k, v) => k.toString -> fromScalaNative(v)
        }.toMap)
    },
    "range" -> PrimitiveFunction {
      case (from: SInteger) :: (to: SInteger) :: Nil =>
        WrappedScalaObject(scala.collection.immutable.NumericRange(from, to, SInteger(1)))
      case (from: SInteger) :: (to: SInteger) :: (step: SInteger) :: Nil =>
        WrappedScalaObject(scala.collection.immutable.NumericRange(from, to, step))
    },
    "range/inclusive" -> PrimitiveFunction {
      case (from: SInteger) :: (to: SInteger) :: Nil =>
        WrappedScalaObject(scala.collection.immutable.NumericRange.inclusive(from, to, SInteger(1)))
      case (from: SInteger) :: (to: SInteger) :: (step: SInteger) :: Nil =>
        WrappedScalaObject(scala.collection.immutable.NumericRange.inclusive(from, to, step))
    },
    "sort" -> PrimitiveFunction {
      case coll :: Nil => Util.CollectionHelper.generalSortWith(coll, primitiveEnvironment.get("<"))
      case coll :: fn :: Nil =>
        Util.CollectionHelper.generalSortWith(coll, fn)
    }
  ))

  private lazy val testerEnvironment = EmptyEnv.withValues(Seq(
    "nil?" -> PrimitiveFunction.withArityChecked(1) {
      case NilObj :: Nil => true
      case _ :: Nil => false
    }.withDocString("Test if an expression is an empty list."),
    "type-of" -> PrimitiveFunction.withArityChecked(1) {
      case x :: Nil => WrappedScalaObject(x.tpe)
    },
    "typename-of" -> PrimitiveFunction.withArityChecked(1) {
      case x :: Nil => x.tpe.name
    },
    "class-of" -> PrimitiveFunction.withArityChecked(1) {
      case WrappedScalaObject(o) :: Nil => WrappedScalaObject(o.getClass)
      case x :: Nil => WrappedScalaObject(x.getClass)
    },
    "integer?" -> PrimitiveFunction.withArityChecked(1) {
      case SInteger(_) :: Nil => true
      case _ :: Nil => false
    },
    "number?" -> PrimitiveFunction.withArityChecked(1) {
      case (_: SNumber[_]) :: Nil => true
      case _ :: Nil => false
    },
    "rational?" -> PrimitiveFunction.withArityChecked(1) {
      case SRational(_) :: Nil => true
      case _ => false
    },
    "float?" -> PrimitiveFunction.withArityChecked(1) {
      case SFloat(_) :: Nil => true
      case _ => false
    },
    "string?" -> PrimitiveFunction.withArityChecked(1) {
      case SString(_) :: Nil => true
      case _ => false
    },
    "symbol?" -> PrimitiveFunction.withArityChecked(1) {
      case Symbol(_) :: Nil => true
      case _ => false
    },
    "atom?" -> PrimitiveFunction.withArityChecked(1) {
      case SAtom(_) :: Nil => true
      case _ => false
    },
    "procedure?" -> PrimitiveFunction.withArityChecked(1) {
      case (_: Procedure | PolymorphicExpression(_, _, _, false) |
            _: PrimitiveFunction | _: SideEffectFunction) :: Nil => true
      case _ => false
    },
    "macro?" -> PrimitiveFunction.withArityChecked(1) {
      case (_: SimpleMacro | _: PrimitiveMacro | PolymorphicExpression(_, _, _, true) | _: SimpleMacroClosure) :: Nil => true
      case _ => false
    },
    "callable?" -> PrimitiveFunction.withArityChecked(1) {
      case c :: Nil => c.isInstanceOf[Procedure]
    },
    "list?" -> PrimitiveFunction.withArityChecked(1) {
      case (_: LisaListLike[_]) :: Nil => true
      case _ => false
    },
    "record?" -> PrimitiveFunction.withArityChecked(1) {
      case (_: LisaRecord[_]) :: Nil => true
      case _ => false
    },
    "wrapped?" -> PrimitiveFunction.withArityChecked(1) {
      case (_: WrappedScalaObject[_]) :: Nil => true
      case _ => false
    },
    "iterable?" -> PrimitiveFunction.withArityChecked(1) {
      case (_: LisaRecord[_] | WrappedScalaObject(_: Iterable[_]) | SString(_)) :: Nil => true
      case _ => false
    },
    "quoted?" -> PrimitiveFunction.withArityChecked(1) {
      case Quote(_) :: Nil => true
      case _ => false
    },
    "same-reference?" -> PrimitiveFunction.withArityChecked(2) {
      case x :: y :: Nil => x eq y
    },
    "not" -> PrimitiveFunction.withArityChecked(1) {
      case SBool(bool) :: Nil => SBool(!bool)
    },
    "and" -> PrimitiveMacro { case (args, env) =>
      assert(args.length == 2, s"and accepts 2 arguments but got ${args.length}.")
      val lhs :: rhs :: Nil = args
      val andSymbol = Symbol("&&the symbol for and&&")
      Apply(LambdaExpression(SIfElse(andSymbol, rhs, andSymbol), andSymbol :: Nil), lhs :: Nil) -> env
    },
    "or" -> PrimitiveMacro { case (args, env) =>
      assert(args.length == 2, s"or accepts 2 arguments but got ${args.length}.")
      val lhs :: rhs :: Nil = args
      val orSymbol = Symbol("&&the symbol for or&&")
      Apply(LambdaExpression(SIfElse(orSymbol, orSymbol, rhs), orSymbol :: Nil), lhs :: Nil) -> env
    }
  ))

  lazy val preludeEnvironment: CombineEnv = CombineEnv(Seq(
    primitiveEnvironment,
    collectionEnvironment,
    testerEnvironment,
    ConstructorCaller.ConstructorEnvironment))
}
