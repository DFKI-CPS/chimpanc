package client

import net.flatmap.codemirror.{CodeMirror, LineWidget, TextMarker, TextMarkerOptions}
import net.flatmap.materialize.{jQuery => $}
import org.scalajs.dom._
import org.scalajs.dom.raw.HTMLElement
import specific._
import Util._

import scala.util.control.NonFatal

class EntityElement(val elem: Seq[Node], layer: Layer, val entity: LayerObject, editor: CodeMirror, marker: TextMarker) {
  val error = RVar(false)
  error.react { v =>
    if (v) {
      elem.classes += "error"
    } else {
      elem.classes -= "error"
    }
  }

  def clear() = {
    marker.clear()
    lineWidget.foreach(_.clear())
  }

  val warn = RVar(false)
  warn.react { v =>
    if (v) {
      elem.classes += "warning"
    } else {
      elem.classes -= "warning"
    }
  }

  private var lineWidget = Option.empty[LineWidget]
  val widget = RVar(Option.empty[HTMLElement])
  widget.react { w =>
    lineWidget.foreach(l => try { l.clear() } catch { case NonFatal(_) => () })
    lineWidget = w.map { w => val x = editor.addLineWidget(marker.find().to.line, w); schedule(500)(x.changed()); x }
  }

  val gutter = RVar(Option.empty[HTMLElement])
  gutter.react {
    case None =>
      editor.setGutterMarker(marker.find().from.line, "issues", null)
    case Some(w) =>
      editor.setGutterMarker(marker.find().from.line, "issues", w)
  }

  val lineClass = RVar(Option.empty[String])
  lineClass.onChange { case (o,n) =>
    o.foreach { c =>
      editor.removeLineClass(marker.find().to.line, "gutter", c)
    }
    n.foreach { c =>
      editor.addLineClass(marker.find().to.line, "gutter", c)
    }
  }

  {
    var expanded = false
    def expand() = {
      expanded = true
      elem.tabIndex = 1
      elem.focus()
      val i = Main.layers.indexOf(layer)
      if (matchedElements.isEmpty) {
        val ignore = HTML( s"""<a class="inlineOption">&nbsp;ignore</a>""")
        elem.append(ignore)
        Main.layers(i + 1).addMapping := true
        Main.mappingModel = Some((layer.name,entity))
        elem.on(Event.Blur) { _ =>
          ignore.remove()
          expanded = false
          schedule(250) {
            Main.mappingModel = None
          }
          Main.layers(i + 1).addMapping := false
          elem.elements.foreach(_.removeAttribute("tabindex"))
        }
        ignore.on(Event.Mouse.Click) { _ =>
          Main.send(IgnoreModel(layer.name, entity, org.scalajs.dom.window.prompt(s"Why should the model '${entity.name}' be ignored?")))
          ignore.blur()
        }
      } else {
        val remove = HTML( s"""<a class="inlineOption">&nbsp;remove mappings</a>""")
        elem.append(remove)
        Main.layers(i + 1).addMapping := true
        Main.mappingModel = Some((layer.name,entity))
        elem.on(Event.Blur) { _ =>
          remove.remove()
          expanded = false
          elem.elements.foreach(_.removeAttribute("tabindex"))
          schedule(250) {
            Main.mappingModel = None
          }
          Main.layers(i + 1).addMapping := false
          elem.elements.foreach(_.removeAttribute("tabindex"))
        }
        remove.on(Event.Mouse.Click) { _ =>
          Main.send(RemoveMappings(layer.name, entity))
          remove.blur()
        }
      }
    }
    elem.query(".name").on(Event.Mouse.Click) { _ =>
      if (!expanded && Main.mappingModel.isEmpty && error()) expand()
      else Main.mappingModel.foreach {
        case (tl,te) => Main.send(AddMapping(layer.name, entity, tl, te))
      }
    }
    elem.query(".name").on(Event.Mouse.DoubleClick) { _ => if (!expanded) expand() }
  }

  private var obligationWidget = false
  private var obligationHTML = Option.empty[Seq[Node]]

  private def renderPO(po: OCLProofObligation): Seq[Node] = HTML("<a class='collection-item'>" + po.implication + s"<i class='secondary-content material-icons ${if (po.proven) "green" else "red"}-text'>"+
    s"${if (po.proven) "done" else "close"}</i></a>")

  private var poMark = Option.empty[TextMarker]

  val proofObligations = RVar(Set.empty[OCLProofObligation])
  proofObligations.react { case pos =>
    obligationHTML.foreach(_.remove())
    if (pos.nonEmpty) {
      val obligations = HTML(s"<span class='proofObligations'>" +
        (if (pos.exists(!_.proven)) s"<span class='new red white-text'>${pos.count(!_.proven)}</span>" else "") +
        (if (pos.exists(_.proven)) s"<span class='done green white-text'>${pos.count(_.proven)}</span>" else "") +
        s"</span>")
      val tooltip = pos.count(!_.proven) match {
        case 0 => "no proof obligations remaining"
        case 1 => "one remaining proof obligation"
        case n => s"$n proof obligations remaining"
      }
      obligations.data("tooltip") = Some(tooltip)
      obligations.data("position") = Some("right")
      obligations.classes += "tooltipped"
      schedule(200)($(".tooltipped").tooltip())
      obligationHTML = Some(HTML("&nbsp") ++ obligations)
      obligationHTML.to
      elem.append(obligationHTML.get)
      def drawWidget = {
        val obls = HTML("<div class='collection proofObligationList'></div>")
        pos.foreach { po => if (!po.proven) {
          val e = renderPO(po)
          obls.append(e)
          e.on(Event.Mouse.Enter) { e =>
            poMark.foreach(_.clear())
            val options = TextMarkerOptions()
            options.css = "background-color: lightgoldenrodyellow"
            val startPos = CodeMirror.Pos(po.cLine - 1, po.cColumn - 1)
            poMark = Some(
              editor.getDoc().markText(
                startPos,
                editor.getDoc().posFromIndex(editor.getDoc().indexFromPos(startPos) + po.cLength),
                options
              )
            )
          }
          e.on(Event.Mouse.Leave) { e =>
            poMark.foreach(_.clear())
            poMark = None
          }
          e.onclick {
            Main.send(Proven(po.layer,po.implication))
          }
        }}
        widget := Some(obls.head.asInstanceOf[HTMLElement])
        obligationWidget = true
      }
      if (obligationWidget) {
        widget := None
        if (pos.exists(!_.proven)) drawWidget
        else obligationWidget = false
      }
      obligations.onclick {
        if (obligationWidget) {
          obligationWidget = false
          widget := None
        } else if (pos.exists(!_.proven)) {
          drawWidget
        }
      }
    } else {
      obligationHTML.foreach(_.remove())
    }
  }

  val hover = RVar(false)
  hover.react { v =>
    if (v) {
      elem.classes += "hoverMatch"
    } else {
      elem.classes -= "hoverMatch"
    }
  }

  val ignored = RVar(Option.empty[String])
  ignored.react {
    case None =>
      tooltip := None
      elem.classes -= "ignored"
    case Some(reason) =>
      tooltip := Some("Ignored because: " + reason)
      elem.classes += "ignored"
  }

  val showTransitiveHull = RVar(false)
  elem.on(Event.Mouse.Click) { _ =>
    showTransitiveHull.modify(!_)
  }
  showTransitiveHull.react {
    case true =>
      elem.classes += "transitiveMatch"
      show()
    case false =>
      elem.classes -= "transitiveMatch"
  }

  val removedModel = RVar(Option.empty[LayerObject])
  removedModel.react {
    case None =>
      tooltip := None
      elem.classes -= "removedModel"
    case Some(model) =>
      tooltip := Some(s"Model '${model.name}' has been removed")
      elem.classes += "removedModel"
  }

  val removedImpl = RVar(Option.empty[LayerObject])
  removedImpl.react {
    case None =>
      tooltip := None
      elem.classes -= "removedImpl"
    case Some(model) =>
      tooltip := Some(s"Implementation '${model.name}' has been removed")
      elem.classes += "removedImpl"
  }

  val modified = RVar(false)
  modified.react {
    case false =>
      elem.classes -= "removedImpl"
    case true =>
      elem.classes += "removedImpl"
  }

  val modifiedClient = RVar(false)
  modifiedClient.react {
    case false =>
      tooltip := None
      elem.classes -= "removedImpl"
    case true =>
      tooltip := Some(s"A Client has been modified")
      elem.classes += "removedImpl"
  }

  val modifiedSupplier = RVar(false)
  modifiedSupplier.react {
    case false =>
      tooltip := None
      elem.classes -= "removedImpl"
    case true =>
      tooltip := Some(s"A supplier has been modified")
      elem.classes += "removedImpl"
  }

  val tooltip = RVar(Option.empty[String])
  tooltip.react {
    case None =>
      elem.query(".name").classes -= "tooltipped"
      schedule(200)($(".tooltipped").tooltip())
    case Some(t) =>
      elem.query(".name").data("tooltip") = Some(t)
      elem.query(".name").data("position") = Some("right")
      elem.query(".name").classes += "tooltipped"
      schedule(200)($(".tooltipped").tooltip())
  }

  private val matchedElements = collection.mutable.Map.empty[(Layer, LayerObject), Subscription]

  object matches {
    def += (layer: Layer, entity: LayerObject, morph: Option[String], stereotype: String) = {
      tooltip.modify(morph orElse _)
      val label = None
      if (!matchedElements.contains((layer,entity))) {
        var assoc = Option.empty[UML.Association]
        setInterval(() => assoc.foreach(_.update()), 10);
        val e1 = elem.query(".name").take(1).on(Event.Mouse.Enter) { e =>
          layer.entityElements.get(entity.path).foreach { e =>
            e.hover := true
            e.show()
            if (assoc.isEmpty) assoc = for {
              from <- e.elem.elements.headOption
              to <- elem.elements.headOption
            } yield {
              new UML.Association(from,to,stereotype,label)
            }
          }
        }
        val e2 = elem.query(".name").take(1).on(Event.Mouse.Leave) { e =>
          layer.entityElements.get(entity.path).foreach { e =>
            e.hover := false
            if (!showTransitiveHull()) {
              assoc.foreach(_.remove())
              assoc = None
            }
          }
        }
        layer.entityElements.get(entity.path).foreach { e =>
          showTransitiveHull.react { v =>
            e.showTransitiveHull := v
            if (v) {
              if (assoc.isEmpty) assoc = for {
                from <- e.elem.elements.headOption
                to <- elem.elements.headOption
              } yield {
                new UML.Association(from,to,stereotype,label)
              }
            } else {
              assoc.foreach(_.remove())
              assoc = None
            }
          }
          e.showTransitiveHull.react {
            showTransitiveHull.:=(_)
          }
        }
        val e = e1 + e2
        elem.classes += "match"
        matchedElements += (layer,entity) -> e
      }
    }

    def -= (layer: Layer, entity: LayerObject) = {
      matchedElements.remove((layer,entity)).foreach { subscription =>
        subscription.cancel()
        if (matchedElements.isEmpty)
          elem.classes -= "match"
      }
    }
  }

  def show() = editor.scrollIntoView(marker.find())
}