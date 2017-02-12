package client

import org.scalajs.dom.console
import org.scalajs.dom.document
import org.scalajs.dom.Element
import org.scalajs.dom.raw.{HTMLElement, SVGElement, SVGPathElement}
import Util._

import scala.util.Random

/**
  * @author Martin Ring <martin.ring@dfki.de>
  */
object UML {
  lazy val svg = document.getElementById("uml").asInstanceOf[SVGElement]
  class Association(fromElemR: Element, toElemR: Element, className: String, label: Option[String]) {
    def fromElem = fromElemR.getBoundingClientRect()
    def toElem = toElemR.getBoundingClientRect()
    val path = document.createElementNS("http://www.w3.org/2000/svg","path").asInstanceOf[SVGPathElement]
    path.classList.add("association")
    path.classList.add(className)
    path.classList.add("add");
    path.once(Event.Animation.End) { _ =>
      path.classList.remove("add")
    }
    val from = path.createSVGPathSegMovetoAbs(0,0)
    val to = path.createSVGPathSegLinetoAbs(0,0)
    path.pathSegList.appendItem(from)
    path.pathSegList.appendItem(to)
    val id = Random.nextLong().toHexString
    path.id = id
    def update() = {
      from.y = fromElem.top + fromElem.height / 2
      to.y = toElem.top + toElem.height / 2
      if (fromElem.left + fromElem.width < toElem.left) {
        from.x = fromElem.left + fromElem.width
        to.x = toElem.left
      } else if (fromElem.left > toElem.left + toElem.width) {
        from.x = fromElem.left
        to.x = toElem.left + toElem.width
      } else {
        from.x = fromElem.left + fromElem.width / 2
        to.x = toElem.left + toElem.width / 2
        if (fromElem.top + fromElem.height< toElem.top) {
          from.y = fromElem.top + fromElem.height
          to.y = toElem.top
        } else if (fromElem.top > toElem.top + toElem.height) {
          from.y = fromElem.top
          to.y = toElem.top + toElem.height
        }
      }
    }
    svg.appendChild(path)
    update()
    val labelNode = label.map { text =>
      val textNode = document.createElementNS("http://www.w3.org/2000/svg","text");
      val textPath = document.createElementNS("http://www.w3.org/2000/svg","textPath")
      textPath.setAttribute("href","#" + id)
      textPath.setAttribute("startOffset","50%")
      textPath.innerHTML = text
      textNode.setAttribute("text-anchor","middle")
      textNode.classList.add("add")
      textNode.once(Event.Animation.End) { _ =>
        path.classList.remove("add")
      }
      textNode.appendChild(textPath)
      svg.appendChild(textNode)
      textNode
    }
    def remove() = {
      path.classList.add("remove")
      labelNode.foreach(_.classList.add("remove"))
      path.once(Event.Animation.End) { _ =>
        svg.removeChild(path)
        labelNode.foreach(svg.removeChild)
      }
    }
  }
}
