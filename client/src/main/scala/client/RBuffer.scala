package client

import org.scalajs.dom._
import scala.collection.mutable
import Util._

trait RBuffer[T] extends collection.mutable.Buffer[T] {
  def elem: Seq[Node]
}

object RBuffer {
  def apply[T](root: Seq[Node], render: T => (Seq[Node], (Seq[Node]) => Unit)): RBuffer[T] = {
    val buffer = mutable.Buffer.empty[T]
    val beforeHead = HTML("<!-- RBuffer Start -->").head
    val afterLast = HTML("<!-- RBuffer End -->").head
    root.append(beforeHead)
    root.append(afterLast)
    val spacers = mutable.Buffer[Node](beforeHead, afterLast)
    def before(n: Int) = spacers(n)
    def after(n: Int) = spacers(n+1)
    def removeAfter(n: Int) = {
      val s = spacers.remove(n+1)
      s.parentNode.removeChild(s)
    }
    def insertBeforeSpacer(n: Int, ns: Seq[Node]) = {
      for {
        root <- root
        node <- ns
      } root.insertBefore(node, after(n+1))
    }
    new RBuffer[T] {
      def elem = root

      override def clear(): Unit = {
        var current = beforeHead.nextSibling
        while (current != afterLast) {
          val rem = current
          current = current.nextSibling
          rem.parentNode.removeChild(rem)
        }
        spacers.remove(1,spacers.length - 2)
        buffer.clear()
      }

      override def apply(n: Int): T = buffer.apply(n)

      override def update(n: Int, newelem: T): Unit = {
        remove(n)
        insert(n,newelem)
      }

      override def length: Int = buffer.length

      override def remove(n: Int): T = {
        var x = before(n).nextSibling
        while (x != null && x != after(n)) {
          val m = x
          x = x.nextSibling
          m.parentNode.removeChild(m)
        }
        if (after(n) != afterLast) removeAfter(n)
        buffer.remove(n)
      }

      override def +=:(elem: T): this.type = {
        insertAll(0,Seq(elem))
        this
      }

      override def +=(elem: T): this.type = {
        insertAll(length,Seq(elem))
        this
      }

      override def insertAll(n: Int, elems: Traversable[T]): Unit = {
        buffer.insertAll(n, elems)
        var x = n
        val a = after(math.max(n - 1, 0))
        elems.foreach { elem =>
          if (x > 0) {
            val b = HTML("<!-- RBuffer Spacer -->")
            root.foreach(r => r.insertBefore(b.head, a))
            spacers.insert(x, b.head)
          }
          val (node,init) = render(elem)
          for {
            r <- root
            n <- node
          } r.insertBefore(n, a)
          init(node)
          x += 1
        }
      }

      override def iterator: Iterator[T] = buffer.iterator

    }
  }
}