package specific.graph

import scala.collection.mutable.Buffer

/**
 * @author maring
 */
trait Graphable[T] {
  def write[T](node: Option[Int], value: T): Option[Int]
  def read[T](node: Int): Option[T]
}

class GraphBuffer[A](implicit graphable: Graphable[A]) extends Buffer[A] {
  def +=(elem: A): this.type = ???
  def +=:(elem: A): this.type = ???
  def apply(n: Int): A = ???
  def clear(): Unit = ???
  def insertAll(n: Int, elems: Traversable[A]): Unit = ???
  def iterator: Iterator[A] = ???
  def length: Int = ???
  def remove(n: Int): A = ???
  def update(n: Int, newelem: A): Unit = ???
} 