package moe.roselia.lisa.Reflect

import java.lang.invoke.{LambdaMetafactory, MethodHandleProxies, MethodHandles, MethodType}
import java.lang.reflect.{Method, Modifier}

import moe.roselia.lisa.LispExp.{Expression, Procedure}

import scala.reflect.{ClassTag, classTag}
import scala.reflect.runtime.{universe => ru}

object FunctionalInterfaceAdapter {
  trait AnyArgHandler {
    def apply(xs: Array[Any]): Any
  }

  def isAbstract(clazz: Class[_]): Boolean = Modifier.isAbstract(clazz.getModifiers)
  def isAbstract(method: Method): Boolean = Modifier.isAbstract(method.getModifiers)

  def findFunctionalMethod(clazz: Class[_]): Method = {
    require(isAbstract(clazz), "The class must be abstract")
    val abstractMethods = clazz.getDeclaredMethods.filter(isAbstract)
      .ensuring(_.length == 1, "Should only have one abstract method.")
    abstractMethods.head
  }

  def isFunctional(clazz: Class[_]): Boolean = {
    isAbstract(clazz) && clazz.getDeclaredMethods.count(isAbstract) == 1
  }

  def isFunctional[T: ClassTag]: Boolean = isFunctional(classTag.runtimeClass)

  def isFunctional(typ: ru.Type): Boolean = {
    typ.typeSymbol.isAbstract && typ.decls.count(_.isAbstract) == 1
  }

  def createFunctionInvoker[T](clazz: Class[T], function: Procedure): T = {
    val cl = ScalaBridge.evalClosure(function)(_)
    val aah = new AnyArgHandler {
      override def apply(xs: Array[Any]): Any = cl(xs.toSeq)
    }

    val handle = MethodHandles.lookup()
      .unreflect(classOf[AnyArgHandler].getDeclaredMethod("apply", classOf[Array[Any]]))
      .bindTo(aah)
      .asVarargsCollector(classOf[Array[Any]])
    MethodHandleProxies.asInterfaceInstance(
      clazz,
      handle
    )
  }

  def createFunctionInvoker[T: ClassTag](function: Procedure): T =
    createFunctionInvoker(classTag.runtimeClass.asInstanceOf[Class[T]], function)

  def performSAMTransform(args: Seq[Any], parameterTypes: Seq[Class[_]]): Seq[Any] = {
    if (args.length != parameterTypes.length) args
    else {
      args.indices.map { i =>
        val arg = args(i)
        val parameterType = parameterTypes(i)
        arg match {
          case procedure: Procedure if isFunctional(parameterType) =>
            createFunctionInvoker(parameterType, procedure)
          case _ => arg
        }
      }
    }
  }
}
