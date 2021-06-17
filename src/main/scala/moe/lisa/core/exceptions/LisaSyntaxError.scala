package moe.lisa.core.exceptions

import moe.lisa.core.expression.Tree._

trait LisaSyntaxError[T >: Untyped](sourceTree: Tree[T]) extends Exception:
  def source = sourceTree.source
  def position = sourceTree.source.atSpan(sourceTree.span)
  def showSourceLocation: String =
    val name = sourceTree.source.name
    val pos = position
    s"$name: ${pos.startLine}:${pos.startColumn}"
  def show: String
  def message: String

case class MessageLisaSyntaxError[T >: Untyped](message: String, tree: Tree[T]) extends LisaSyntaxError(tree):
  override def show: String = s"$showSourceLocation\n$message"
