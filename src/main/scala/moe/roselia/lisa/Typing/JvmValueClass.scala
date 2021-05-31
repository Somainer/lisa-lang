package moe.roselia.lisa.Typing

object JvmValueClass {
  val ScalaByte: Class[Byte] = Class.forName("scala.Byte").asInstanceOf[Class[Byte]]
  val ScalaShort: Class[Short] = Class.forName("scala.Short").asInstanceOf[Class[Short]]
  val ScalaInt: Class[Int] = Class.forName("scala.Int").asInstanceOf[Class[Int]]
  val ScalaLong: Class[Long] = Class.forName("scala.Long").asInstanceOf[Class[Long]]
  val ScalaFloat: Class[Float] = Class.forName("scala.Float").asInstanceOf[Class[Float]]
  val ScalaDouble: Class[Double] = Class.forName("scala.Double").asInstanceOf[Class[Double]]
  val ScalaChar: Class[Char] = Class.forName("scala.Char").asInstanceOf[Class[Char]]
  val ScalaBoolean: Class[Boolean] = Class.forName("scala.Boolean").asInstanceOf[Class[Boolean]]

  val JavaByte: Class[java.lang.Byte] = classOf[java.lang.Byte]
  val JavaShort: Class[java.lang.Short] = classOf[java.lang.Short]
  val JavaInt: Class[java.lang.Integer] = classOf[java.lang.Integer]
  val JavaLong: Class[java.lang.Long] = classOf[java.lang.Long]
  val JavaFloat: Class[java.lang.Float] = classOf[java.lang.Float]
  val JavaDouble: Class[java.lang.Double] = classOf[java.lang.Double]
  val JavaChar: Class[java.lang.Character] = classOf[java.lang.Character]
  val JavaBoolean: Class[java.lang.Boolean] = classOf[java.lang.Boolean]

  val NativeByte: Class[java.lang.Byte] = java.lang.Byte.TYPE
  val NativeShort: Class[java.lang.Short] = java.lang.Short.TYPE
  val NativeInt: Class[java.lang.Integer] = java.lang.Integer.TYPE
  val NativeLong: Class[java.lang.Long] = java.lang.Long.TYPE
  val NativeFloat: Class[java.lang.Float] = java.lang.Float.TYPE
  val NativeDouble: Class[java.lang.Double] = java.lang.Double.TYPE
  val NativeChar: Class[java.lang.Character] = java.lang.Character.TYPE
  val NativeBoolean: Class[java.lang.Boolean] = java.lang.Boolean.TYPE
}
