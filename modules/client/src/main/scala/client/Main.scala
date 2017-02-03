package client

import scala.scalajs.js
import specific._
import org.scalajs.dom.raw.{HTMLDivElement, HTMLElement}
import scala.util.matching.Regex
import org.scalajs.dom._
import net.flatmap.codemirror._
import js.JSStringOps.enableJSStringOps
import Util._

object Main extends SocketApp[Message,Message](s"ws://${window.location.host}/session")(Message.read,Message.write) {
  var nlEditor: Option[CodeMirror] = None

  case class FSLEditors(emf: CodeMirror, ocl: CodeMirror)

  var fslEditors: Seq[FSLEditors] = Seq.empty

  var eslEditor: Option[CodeMirror] = None

  var mappingModel: Option[(String,LayerObject)] = None

  val overlay = RVar(false)

  overlay.react {
    case true => document.body.classList.add("overlay")
    case false => document.body.classList.remove("overlay")
  }

  lazy val tabs: RBuffer[Layer] = {
    var activeLayer: Option[Layer] = None
    RBuffer[Layer](query("#layer-tabs"), { (layer: Layer) =>
      (HTML(s"<li class='tab'><a id='${layer.id}-tab'>${layer.name}</a></li>"), (nodes) => {
        query("#layer-tabs").$.tabs()
        layer.active.react { (active) => if (active)
          activeLayer.foreach(_.active := false)
          activeLayer = Some(layer)
          document.getElementById(s"${layer.id}-tab").asInstanceOf[HTMLElement].click()
        }
        document.getElementById(layer.id + "-tab").addEventListener("click", (e: raw.Event) => {
          if (!activeLayer.contains(layer)) {
            layer.active := true
          }
          setCenterColumn(tabs.indexOf(layer))
        })
      })
    })
  }

  def layer(name: String): Layer = layers.find(_.name == name).get

  lazy val layers: RBuffer[Layer] = RBuffer[Layer](query("#main"), { (layer: Layer) =>
    layer.content() match {
      case SysML(layerName, sysml) =>
        (HTML(
          s"""<div class="layer sysml" id="${layer.id}
-container">
                 |  <div class="content">
                 |  <div class="card">
                 |    <div class="card-content">
                 |      <span id="${layer.id}-title" class="card-title truncate">${layer.name.toUpperCase}
: Formal Specification Level</span>
                 |      <div id="${layer.id}"></div>
                 |    </div>
                 |  </div>
                 |  </div>
                 |</div>""".stripMargin),(nodes) => {
          layer.active.react {
            case true => query(s"#${layer.id}-container").classes += "active"
            case false => query(s"#${layer.id}-container").classes -= "active"
          }
          val sysmlEditor = CodeMirror(document.getElementById(layer.id))
          sysmlEditor.setOption("mode", "sysml")
          sysmlEditor.setOption("readOnly", true)
          sysmlEditor.setOption("autoRefresh",true)
          sysmlEditor.getDoc().setValue(sysml)

          sysmlEditor.setOption("gutters",js.Array("issues"))
          def update(e: CodeMirror, ev: raw.Event): Unit = {
            query(e.getWrapperElement()).

              query(".tooltipped").$.tooltip()
          }
          sysmlEditor.on("viewportChange", update _)
          query(s"#commit-${layer.id}").on(Event.Mouse.Click) { _ =>
            send(Commit)
          }
          layer.addMapping.react {
            case true =>
              sysmlEditor.getWrapperElement().classList.add("add-mapping")
            case false =>
              sysmlEditor.getWrapperElement().classList.remove("add-mapping")
          }
          layer.entities.react { e =>
            sysmlEditor.getDoc().getAllMarks().foreach(_.clear())
            layer.entityElements.values.foreach(_.clear())
            layer.entityElements.clear()
            e.foreach { e =>
              val elem = HTML(s"<span class='entity'><span class='name'>${e.name}</span></span>").head.asInstanceOf[HTMLElement]
              val o = TextMarkerOptions()
              o.replacedWith = elem
              o.addToHistory = false
              mark(sysmlEditor, e, o).map { m =>
                layer.entityElements += e.path -> new EntityElement(Seq(elem), layer, e, sysmlEditor, m)
              }
            }
          }
          this.eslEditor = Some(sysmlEditor)
        })
  }})


  val rules: List[String] = List(
    "Define one requirement at a time",
    "Avoid conjunctions",
    "Use simple direct sentences",
    "Each requirement must contain a subject and a predicate",
    "Avoid let-out clauses",
    "Avoid expressing suggestions of possibilities",
    "Avoid weak phrases and undefined terms",
    "Do not speculate",
    "Avoid wishful thinking",
    "Define veifiable criteria"
  )

  def mark(regex: Regex, editor: CodeMirror, options: TextMarkerOptions, offset: Option[(Int,Int)] = None): Seq[TextMarker] = {
    val doc = editor.getDoc()
    val position = offset match {
      case None =>
        regex.findFirstMatchIn(doc.getValue()).map { res =>
          (res.start(1), res.end(1))
        }
      case Some((length,offset)) =>
        regex.findFirstMatchIn(doc.getValue()).map { res =>
          (res.end - length - offset, res.end - offset)
        }
    }
    position.map { case (from,to) =>
      doc.markText(doc.posFromIndex(from),
                   doc.posFromIndex(to),
                   options)
    }.toSeq
  }

  def mark(editor: CodeMirror, entity: LayerObject, options: TextMarkerOptions): Seq[TextMarker] = {
    val doc = editor.getDoc()
    val line = entity.line
    val column = entity.column
    Seq(doc.markText(CodeMirror.Pos(line -1 ,column -1),CodeMirror.Pos(line-1,column-1 + entity.name.length), options))
  }

    /*
  def markESL(eslEditor: CodeMirror, entity: LayerObject, options: TextMarkerOptions): Seq[TextMarker] = {
    entity match {
      case Clazz(clazz,_) =>
        mark(r"SC_MODULE\(($clazz)\)", eslEditor, options, Some((clazz.length,1))) ++
        mark(r"typedef[^\n]*($clazz);", eslEditor, options, Some((clazz.length,1)))
      case Attribute(clazz, member, _) =>
        mark(r"(?:SC_MODULE\()$clazz\)\s*\{(?:.|\n)*?($member)(?:\(|;|\[|=)", eslEditor, options, Some((member.length,1)))
      case Reference(clazz, member, t) =>
        mark(r"(?:SC_MODULE\()$clazz\)\s*\{(?:.|\n)*?($member)(?:\(|;|\[)", eslEditor, options, Some((member.length,1)))
      case Operation(clazz, member, t, params) =>
        if (clazz == member) mark(r"SC_MODULE\($clazz\)\s*\{(?:.|\n)*?SC_CTOR\(($member)\)", eslEditor, options, Some((member.length,1)))
        else mark(r"(?:SC_MODULE\()$clazz\)\s*\{(?:.|\n)*?[^\n\s]\s($member)(?:\(|;|\[)", eslEditor, options, Some((member.length + 1,0)))
      case Parameter(clazz,op,name,tpe) =>
        if (name == "this") Seq.empty
        else mark(r"(?:SC_MODULE\()$clazz\)\s*\{(?:.|\n)*?[^\n\s]\s($op)\([^\)]*\s+($name)(?:\)|,)", eslEditor, options, Some((name.length,1)))
    }
  }*/

  def fslUnimplemented(emf: Layer)(clazz: String): Unit = {
    val e = emf.entityElements(clazz).error := true
  }

  def fslUnimplemented(emf: Layer, clazz: String, member: String): Unit = {
    val e = emf.entityElements(clazz + "." + member).error := true
  }

  def markMapping(mapping: Mapping) = for {
    to <- layer(mapping.toLayer).entityElements.get(mapping.to)
    from <- layer(mapping.fromLayer).entityElements.get(mapping.from)
  } {
    to.matches += (layer(mapping.fromLayer), from.entity)
  }

  def removeMapping(mapping: Mapping) = for {
    to <- layer(mapping.toLayer).entityElements.get(mapping.to)
    from <- layer(mapping.fromLayer).entityElements.get(mapping.from)
  } {
    to.matches -= (layer(mapping.fromLayer), from.entity)
  }

  var issues: Set[SemanticIssue] = Set.empty
  var mappings: Set[Mapping] = Set.empty

  class Task(val name: String) {
    def id = "task-"+name.hashCode.toString
    val progress = RVar(Option.empty[Int])
    progress.react {
      case None =>
        query(s"#$id .progress").classes += "hide"
        query(s"#$id .progress div").classes += "indeterminate"
        query(s"#$id .progress div").classes -= "determinate"
      case Some(value) =>
        query(s"#$id .progress").classes -= "hide"
        query(s"#$id .progress div").classes -= "indeterminate"
        query(s"#$id .progress div").classes += "determinate"
        query(s"#$id .progress .determinate").elements.foreach { p =>
          p.asInstanceOf[HTMLElement].style.width = (value / 10) + "%"
        }
    }
    val done = RVar(false)
    done.react {
      case true =>
        query(s"#$id").classes += "done"
      case false =>
        query(s"#$id").classes -= "done"
    }
    def info(message: String) =
      query(s"#$id").append(HTML(s"<li class='collection-item info'>$message<i class='secondary-content material-icons green-text'>done</i></li>"))
    def error(message: String, stackTrace: String) = {
      val lasti = query(s"#$id li .secondary-content").elements.last
      lasti.innerHTML = "close"
      lasti.classList.remove("green-text")
      lasti.classList.add("red-text")
      query(s"#$id").append(HTML(s"<li class='collection-item red white-text'><span>$message: $stackTrace</span>" +
        s"<a href='#!' id='$id-dismiss' class='secondary-content'><i class='material-icons white-text'>play_arrow</i></a>" +
        s"</li>"))
      query(s"#$id-dismiss").onclick {
        val i = tasks.indexWhere(_.name == name)
        if (i >= 0) tasks.remove(i)
        if (tasks.count(!_.done()) == 0) overlay := false
      }
    }
    val failed = RVar(false)
    failed.react {
      case true =>
        query(s"#$id").classes += "failed"
        query(s"#$id").append(HTML(""))
      case false =>
        query(s"#$id").classes += "failed"
    }
  }

  lazy val tasks = RBuffer[Task](query("#tasks"), { (task: Task) =>
    (HTML(
      s"""<ul id='${task.id}' class='task collection with-header'>
         |  <li class='collection-header'>
         |    <h5>${task.name}</h5>
         |    <div class="progress">
         |      <div class="indeterminate" style="width: 0%"></div>
         |    </div>
         |  </li>
         |</ul>""".stripMargin), (nodes) => ())
  })

  var initalized = false

  def receive: PartialFunction[Message,Unit] = {
    case InitSpecs(Specs(ls)) if !initalized =>
      window.on(Event.Key.Press) { k =>
        if (k.key == "r") send(CommitAll)
      }
      val layers = ls.map(new Layer(_))
      tabs ++= layers
      this.layers ++= layers
      layers.headOption.foreach(_.active := true)
      initalized = true
    case InitSpecs(Specs(ls)) if initalized =>
      ls.foreach { layer =>
        this.layers.find(_.name == layer.name).foreach(_.content := layer)
      }
    case StartTask(name) =>
      val old = tasks.indexWhere(_.name == name)
      if (old >= 0) tasks.remove(old)
      overlay := true
      tasks.append(new Task(name))
    case EndTask(name) =>
      tasks.find(_.name == name).foreach {
        _.done := true
      }
      if (!tasks.exists(!_.done())) {
        overlay := false
      }
      schedule(300) {
        val old = tasks.indexWhere(t => t.name == name && t.done())
        if (old >= 0) tasks.remove(old)
      }
    case Info(task, message) =>
      console.info(s"[info][$task] $message")
      tasks.find(_.name == task).foreach { _.info(message) }
    case Error(task, message, stackTrace) =>
      console.error(s"[error][$task] $message: $stackTrace")
      tasks.find(_.name == task).foreach { task =>
        task.error(message, stackTrace)
        task.failed := true
      }
    case Progress(task, value) =>
      tasks.find(_.name == task).foreach { task =>
        task.progress := Some(value)
      }
    case Debug(task, message) =>
      console.log(s"[debug][$task] $message")
    case Warn(task, message) =>
      console.warn(s"[warn][$task] $message")
    case Entities(layer, entities) =>
      layers.find(_.name == layer).foreach { layer =>
        layer.entities := entities
      }
    case MappingsList(mappings) =>
      val removed = this.mappings -- mappings
      this.mappings = mappings
      removed.foreach { removeMapping }
      mappings.foreach { markMapping }

    case IssueList(issues) =>
      val removed = this.issues -- issues
      this.issues = issues
      removed.foreach {
        case UnimplementedModel(layer, model) =>
          this.layer(layer).entityElements.get(model.path).foreach { elem =>
            elem.error := false
            elem.tooltip := None
          }
        case IgnoredModel(layer, model, reason) =>
          this.layer(layer).entityElements.get(model.path).foreach { elem =>
            elem.ignored := None
          }
        case RemovedModel(tl, model, fl, impl) =>
          this.layer(fl).entityElements.get(impl.path).foreach { elem =>
            elem.removedModel := None
          }
        case RemovedImplementation(tl, model, fl, impl) =>
          this.layer(tl).entityElements.get(model.path).foreach { elem =>
            elem.removedImpl := None
          }
        case ModifiedImplementation(tl, model, fl, impl) =>
          this.layer(tl).entityElements.get(model.path).foreach { elem =>
            elem.modifiedImpl := None
          }
        case RequirementEvaluation(layer, requirement, results) =>
          this.layer(layer).entityElements.get(requirement).foreach { elem =>
            elem.tooltip := None
            elem.warn := false
          }
        case MaltypedMapping(layer, model, actual, expected) =>
          this.layer(layer).entityElements.get(model.path).foreach { elem =>
            elem.error := false
            elem.tooltip := None
          }
        case MismatchingBounds(layer,model,_,_) =>
          this.layer(layer).entityElements.get(model.path).foreach { elem =>
            elem.error := false
            elem.tooltip := None
          }
        case obl: OCLProofObligation =>
          this.layer(obl.layer).entityElements.get(obl.owner).foreach { elem =>
            elem.proofObligations.modify(_ - obl)
          }
      }
      issues.foreach {
        case UnimplementedModel(layer, model) =>
          this.layer(layer).entityElements.get(model.path).foreach { elem =>
            elem.error := true
            elem.tooltip := Some(s"'${model.name}' doesn't have an implementation")
          }
        case IgnoredModel(layer, model, reason) =>
          this.layer(layer).entityElements.get(model.path).foreach { elem =>
            elem.ignored := Some(reason)
          }
        case RemovedModel(tl, model, fl, impl) =>
          this.layer(fl).entityElements.get(impl.path).foreach { elem =>
            elem.removedModel := Some(model)
          }
        case RemovedImplementation(tl, model, fl, impl) =>
          this.layer(tl).entityElements.get(model.path).foreach { elem =>
            elem.removedImpl := Some(impl)
          }
        case ModifiedImplementation(tl, model, fl, impl) =>
          this.layer(tl).entityElements.get(model.path).foreach { elem =>
            elem.modifiedImpl := Some(impl)
          }
        case MismatchingBounds(tl, model, _, _) =>
          this.layer(tl).entityElements.get(model.path).foreach { elem =>
            elem.error := true
            elem.tooltip := Some(s"Mismatching bounds")
          }
        case RequirementEvaluation(layer, requirement, results) =>
          val out = results.zipWithIndex.collect {
            case (Some(false),i) => rules(i)
            case (None,i) => "(" + rules(i) + ")"
          }.mkString("; ")
          this.layer(layer).entityElements(requirement).warn := true
          this.layer(layer).entityElements(requirement).tooltip := Some(out)
        case MaltypedMapping(layer, model, expected, actual) =>
          this.layer(layer).entityElements(model.path).error := true
          this.layer(layer).entityElements(model.path).tooltip := Some(s"Maltyped implementation: expected: $expected, actual: $actual")
        case obl: OCLProofObligation =>
          this.layer(obl.layer).entityElements.get(obl.owner).foreach { elem =>
            elem.proofObligations.modify(_ + obl)
          }
      }
  }

  var centerColumn = -1
  var triple = false
  var nlPinned = false

  def setCenterColumn(n: Int) = {
    $"#main".classes += "transition"

    if (n == centerColumn) triple = !triple
    centerColumn = n

    if (triple) {
      val x = math.min(tabs.length - 2, math.max(1,n))
      $"body".classes += "triple-view" -= "double-view"
      $"#main".head.asInstanceOf[HTMLDivElement].style.transform = s"translate(calc(-1 * ${x-1} * 100vw / 3),0)"
      $"#main".head.asInstanceOf[HTMLDivElement].style.setProperty("-webkit-transform", s"translate(calc(-1 * ${x-1} * 100vw / 3),0)")
      if (nlPinned) {
        $"#main .layer.nl".head.asInstanceOf[HTMLDivElement].style.transform = s"translate(calc(${x - 1} * 100vw / 3),0)"
        $"#main .layer.nl".head.asInstanceOf[HTMLDivElement].style.setProperty("-webkit-transform", s"translate(calc(${x - 1} * 100vw / 3),0)")
      } else {
        $"#main .layer.nl".head.asInstanceOf[HTMLDivElement].style.removeProperty("transform")
        $"#main .layer.nl".head.asInstanceOf[HTMLDivElement].style.removeProperty("-webkit-transform")
      }
    } else {
      val x = math.max(n, 1)
      $"body".classes -= "triple-view" += "double-view"
      $"#main".head.asInstanceOf[HTMLDivElement].style.transform = s"translate(calc(-1 * ${x-1} * 100vw / 2),0)"
      $"#main".head.asInstanceOf[HTMLDivElement].style.setProperty("-webkit-transform", s"translate(calc(-1 * ${x-1} * 100vw / 2),0)")
      if (nlPinned) {
        $"#main .layer.nl".head.asInstanceOf[HTMLDivElement].style.transform = s"translate(calc(${x - 1} * 100vw / 2),0)"
        $"#main .layer.nl".head.asInstanceOf[HTMLDivElement].style.setProperty("-webkit-transform", s"translate(calc(${x - 1} * 100vw / 2),0)")
      }
      else {
        $"#main .layer.nl".head.asInstanceOf[HTMLDivElement].style.removeProperty("transform")
        $"#main .layer.nl".head.asInstanceOf[HTMLDivElement].style.removeProperty("-webkit-transform")
      }
    }

    $"#main".once(Event.Transition.End) { e =>
      $"#main".classes -= "transition"
      $".tooltipped".$.tooltip()
    }
  }
}