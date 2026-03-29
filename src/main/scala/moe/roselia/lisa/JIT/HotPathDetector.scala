package moe.roselia.lisa.JIT

import moe.roselia.lisa.LispExp.{Closure, Expression, Procedure}
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicLong

/**
 * Tracks how many times each closure is called and triggers JIT compilation
 * when a threshold is reached.
 */
object HotPathDetector {
  private val callCounts = new ConcurrentHashMap[String, AtomicLong]()
  private val jitCompiled = new ConcurrentHashMap[String, Expression]()

  /** The number of calls before JIT compilation is triggered */
  val CompilationThreshold: Long = 50

  /** Record a call to a procedure and return a JIT-compiled version if available */
  def recordCall(procedure: Procedure): Option[Expression] = {
    val key = procedureKey(procedure)
    if (key.isEmpty) return None

    val k = key.get

    // Check if already JIT compiled
    val compiled = jitCompiled.get(k)
    if (compiled != null) return Some(compiled)

    // Increment call count
    val counter = callCounts.computeIfAbsent(k, _ => new AtomicLong(0))
    val count = counter.incrementAndGet()

    if (count == CompilationThreshold) {
      // Try to JIT compile
      procedure match {
        case closure: Closure =>
          JITCompiler.tryCompile(closure).foreach { compiled =>
            jitCompiled.put(k, compiled)
            return Some(compiled)
          }
        case _ =>
      }
    } else if (count > CompilationThreshold) {
      // Already attempted compilation, just return None (interpreted path)
    }

    None
  }

  private def procedureKey(procedure: Procedure): Option[String] = procedure match {
    case c: Closure => Some(s"closure:${c.body.code}:${c.boundVariable.map(_.code).mkString(",")}")
    case _ => None
  }

  /** Reset all JIT state */
  def reset(): Unit = {
    callCounts.clear()
    jitCompiled.clear()
  }

  /** Get statistics about JIT compilation */
  def stats: Map[String, Any] = Map(
    "trackedClosures" -> callCounts.size(),
    "compiledClosures" -> jitCompiled.size(),
    "threshold" -> CompilationThreshold
  )
}
