package client

import org.scalajs.dom.console
import org.scalajs.dom.document
import org.scalajs.dom.Element
import org.scalajs.dom.raw.{HTMLElement, SVGElement, SVGPathElement}
import Util._

/**
  * @author Martin Ring <martin.ring@dfki.de>
  */
object UML {
  lazy val svg = document.getElementById("uml").asInstanceOf[SVGElement]
  class Association(fromElemR: Element, toElemR: Element, className: String) {
    def fromElem = fromElemR.getBoundingClientRect()
    def toElem = toElemR.getBoundingClientRect()
    val offset = 10
    val path = document.createElementNS("http://www.w3.org/2000/svg","path").asInstanceOf[SVGPathElement]
    path.classList.add("association")
    path.classList.add(className)
    path.classList.add("add");
    path.once(Event.Animation.End) { _ =>
      path.classList.remove("add");
    }
    val from = path.createSVGPathSegMovetoAbs(0,0)
    val to = path.createSVGPathSegLinetoAbs(0,0)
    path.pathSegList.appendItem(from)
    path.pathSegList.appendItem(to)
    def update() = {
      from.y = fromElem.top + fromElem.height / 2
      to.y = toElem.top + toElem.height / 2
      if (fromElem.left + fromElem.width + offset < toElem.left) {
        from.x = fromElem.left + fromElem.width
        to.x = toElem.left - offset
      } else if (fromElem.left > toElem.left + toElem.width + offset) {
        from.x = fromElem.left
        to.x = toElem.left + toElem.width + offset
      } else {
        from.x = fromElem.left + fromElem.width / 2
        to.x = toElem.left + toElem.width / 2
        if (fromElem.top + fromElem.height + offset< toElem.top) {
          from.y = fromElem.top + fromElem.height
          to.y = toElem.top - offset
        } else if (fromElem.top > toElem.top + toElem.height + offset * 2) {
          from.y = fromElem.top
          to.y = toElem.top + toElem.height + offset
        }
      }
      console.log(path,from,to)
    }
    svg.appendChild(path)
    update()
    def remove() = {
      path.classList.add("remove")
      path.once(Event.Animation.End) { _ =>
        svg.removeChild(path)
      }
    }
  }
}
