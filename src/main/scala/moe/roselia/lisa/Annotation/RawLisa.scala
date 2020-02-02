package moe.roselia.lisa.Annotation

/**
 * The annotation to indicate a module or object is a lisa module,
 * the interpreter will not convert [[moe.roselia.lisa.LispExp.Expression]] to [[Any]]
 * when calling functions of this module.
 * */
class RawLisa extends scala.annotation.StaticAnnotation
