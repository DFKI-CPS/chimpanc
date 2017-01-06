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
      case NL(layerName, nl) =>
        (HTML(s"""<div class="layer nl" id="${layer.id}-container">
                 |  <div class="content">
                 |  <div class="card">
                 |    <div class="card-content">
                 |      <span id="${layer.id}-title" class="card-title truncate">${layer.name.toUpperCase}: Natural Language</span>
                 |      <div id="${layer.id}"></div>
                 |    </div>
                 |    <div class="card-action">
                 |      <button class="waves-effect waves-light btn blue" id="check-${layer.id}">Check</button>
                 |      <button class="waves-effect waves-light btn blue" id="commit-${layer.id}" disabled>Commit</button>
                 |      <a id="lock-nl" class='btn-floating blue right' href='#'><i class='material-icons'>lock_open</i></a>
                 |    </div>
                 |  </div>
                 |  </div>
                 |</div>""".stripMargin),(nodes) => {
            layer.active.react {
              case true =>
                query(s"#${layer.id}-container").classes += "active"
              case false =>
                query(s"#${layer.id}-container").classes -= "active"
            }
            $"#lock-nl".onclick {
              nlPinned = !nlPinned
              if (nlPinned) {
                $"#lock-nl i".elements.head.innerHTML = "lock"
                $"#${layer.id}-container".classes += "locked"
              }
              else {
                $"#lock-nl i".elements.head.innerHTML = "lock_open"
                $"#${layer.id}-container".classes -= "locked"
              }
              triple = !triple
              setCenterColumn(centerColumn)
            }
            val nlpEditor = CodeMirror(document.getElementById(layer.id))
            nlpEditor.setOption("mode", null)
            nlpEditor.setOption("autoRefresh",true)
            nlpEditor.setOption("lineWrapping",true)
            nlpEditor.setOption("gutters",js.Array("issues"))
            nlpEditor.getDoc().setValue(nl)
            def update(e: CodeMirror, ev: raw.Event): Unit = {
              query(nlpEditor.getWrapperElement()).query(".tooltipped").$.tooltip()
            }
            nlpEditor.on("viewportChange", update _)
            query(s"#check-${layer.id}").on(Event.Mouse.Click) { _ =>
              val doc = nlEditor.get.getDoc()
              send(Evaluate(NL(layer.name, doc.getValue())))
              nlEditor.get.refresh()
              $"#commit-${layer.id}".head.asInstanceOf[html.Button].disabled = false;
            }
            $"#commit-${layer.id}".on(Event.Mouse.Click) { _ =>
              val doc = nlEditor.get.getDoc()
              println("COMMIT NL")
              send(Commit(NL(layer.name, doc.getValue())))
            }
            layer.entities.react { e =>
              nlpEditor.getDoc().getAllMarks().foreach(_.clear())
              layer.entityElements.values.foreach(_.clear())
              layer.entityElements.clear()
              e.foreach { e =>
                val elem = HTML(s"<span class='entity'><span class='name'>${e.name}</span></span>").head.asInstanceOf[HTMLElement]
                if (e.isInstanceOf[Clazz])
                  elem.classList.add("class")
                else if (e.isInstanceOf[Parameter])
                  elem.classList.add("param")
                else
                  elem.classList.add("member")
                val o = TextMarkerOptions()
                o.replacedWith = elem
                o.addToHistory = false
                markNL(nlpEditor, e, o).map { m =>
                  layer.entityElements += e.qName -> new EntityElement(Seq(elem), layer, e, nlpEditor, m)
                }
              }
            }
            this.nlEditor = Some(nlpEditor)
          })
      case FSL(layerName, emf,ocl) =>
        (HTML(s"""<div class="layer fsl" id="${layer.id}-container">
                 |  <div class="content">
                 |  <div class="card">
                 |    <div class="card-content">
                 |      <span class="card-title truncate right"><a id="${layer.id}-emf-link">EMF</a> | <a id="${layer.id}-ocl-link">OCL</a></span>
                 |      <span id="${layer.id}-title" class="card-title truncate">${layer.name.toUpperCase}: Formal Specification Level</span>
                 |      <div id="${layer.id}-emf"></div>
                 |      <div id="${layer.id}-ocl"></div>
                 |    </div>
                 |    <div class="card-action">
                 |      <button class="waves-effect waves-light btn blue" id="commit-${layer.id}">Commit</button>
                 |      <!--div class="right">
                 |        <a id="${layer.id}-abstract" class="waves-effect waves-teal btn-flat"><i class="material-icons left">chevron_left</i>Abstract</a>
                 |        <a id="${layer.id}-refine" class="waves-effect waves-teal btn-flat">Refine<i class="material-icons right">chevron_right</i></a>
                 |      </div-->
                 |    </div>
                 |  </div>
                 |  </div>
                 |</div>""".stripMargin),(nodes) => {
          layer.active.react {
            case true => query(s"#${layer.id}-container").classes += "active"
            case false => query(s"#${layer.id}-container").classes -= "active"
          }
          val emfEditor = CodeMirror(document.getElementById(layer.id + "-emf"))
          val oclEditor = CodeMirror(document.getElementById(layer.id + "-ocl"))
          emfEditor.setOption("mode", "text/x-csrc")
          emfEditor.setOption("autoRefresh",true)
          emfEditor.getDoc().setValue(emf)
          emfEditor.setOption("gutters",js.Array("issues"))
          oclEditor.setOption("mode", "text/x-csrc")
          oclEditor.setOption("autoRefresh",true)
          oclEditor.getDoc().setValue(ocl)
          oclEditor.setOption("gutters",js.Array("issues"))
          oclEditor.getWrapperElement().classList.add("hide")
          def update(e: CodeMirror, ev: raw.Event): Unit = {
            query(e.getWrapperElement()).query(".tooltipped").$.tooltip()
          }
          emfEditor.on("viewportChange", update _)
          oclEditor.on("viewportChange", update _)
          layer.addMapping.react {
            case true =>
              oclEditor.getWrapperElement().classList.add("add-mapping")
              emfEditor.getWrapperElement().classList.add("add-mapping")
            case false =>
              oclEditor.getWrapperElement().classList.remove("add-mapping")
              emfEditor.getWrapperElement().classList.remove("add-mapping")
          }

          $"#commit-${layer.id}".onclick {
            send(Commit(FSL(layer.name,emfEditor.getDoc().getValue(), oclEditor.getDoc().getValue())))
          }
          $"#${layer.id}-emf-link".onclick {
            oclEditor.getWrapperElement().classList.add("hide")
            emfEditor.getWrapperElement().classList.remove("hide")
            emfEditor.refresh()
          }
          $"#${layer.id}-ocl-link".onclick {
            emfEditor.getWrapperElement().classList.add("hide")
            oclEditor.getWrapperElement().classList.remove("hide")
            oclEditor.refresh()
          }
          $"#${layer.id}-abstract".onclick {
            val i: Int = layers.indexOf(layer)
            val newLayer = new Layer(layer.content())
            tabs.insert(i, newLayer)
            layers.insert(i, newLayer)
            document.getElementById(newLayer.id + "-tab").asInstanceOf[html.Anchor].click()
          }
          $"#${layer.id}-refine".onclick {
            val i: Int = layers.indexOf(layer) + 1
            val newLayer = new Layer(layer.content())
            tabs.insert(i, newLayer)
            layers.insert(i, newLayer)
            document.getElementById(newLayer.id + "-tab").asInstanceOf[html.Anchor].click()
          }
          layer.entities.react { e =>
            emfEditor.getDoc().getAllMarks().foreach(_.clear())
            oclEditor.getDoc().getAllMarks().foreach(_.clear())
            layer.entityElements.values.foreach(_.clear())
            layer.entityElements.clear()
            e.foreach { e =>
              val elem = HTML(s"<span class='entity'><span class='name'>${e.name}</span></span>").head.asInstanceOf[HTMLElement]
              if (e.isInstanceOf[Clazz])
                elem.classList.add("class")
              else
                elem.classList.add("member")
              val o = TextMarkerOptions()
              o.replacedWith = elem
              o.addToHistory = false
              if (e.isInstanceOf[ConstraintInfo]) markFSL(oclEditor, e, o).map { m =>
                layer.entityElements += e.qName -> new EntityElement(Seq(elem), layer, e, oclEditor, m)
              } else
              markFSL(emfEditor, e, o).map { m =>
                layer.entityElements += e.qName -> new EntityElement(Seq(elem), layer, e, emfEditor, m)
              }
            }
          }
          this.fslEditors = Seq(FSLEditors(emfEditor, oclEditor))
        })
      case ESL(layerName, esl) =>
        (HTML(s"""<div class="layer esl" id="${layer.id}-container">
                 |  <div class="content">
                 |  <div class="card">
                 |    <div class="card-content">
                 |      <span id="${layer.id}-title" class="card-title truncate">${layer.name.toUpperCase}: Electronic System Level</span>
                 |      <div id="${layer.id}"></div>
                 |    </div>
                 |    <div class="card-action">
                 |      <button class="waves-effect waves-light btn blue" id="commit-${layer.id}">Commit</button>
                 |    </div>
                 |  </div>
                 |  </div>
                 |</div>""".stripMargin),(nodes) => {
          layer.active.react {
            case true => query(s"#${layer.id}-container").classes += "active"
            case false => query(s"#${layer.id}-container").classes -= "active"
          }
          val eslEditor = CodeMirror(document.getElementById(layer.id))
          eslEditor.setOption("mode", "text/x-c++src")
          eslEditor.setOption("autoRefresh",true)
          eslEditor.getDoc().setValue(esl)
          eslEditor.setOption("gutters",js.Array("issues"))
          def update(e: CodeMirror, ev: raw.Event): Unit = {
            query(e.getWrapperElement()).query(".tooltipped").$.tooltip()
          }
          eslEditor.on("viewportChange", update _)
          query(s"#commit-${layer.id}").on(Event.Mouse.Click) { _ =>
            send(Commit(ESL(layer.name,eslEditor.getDoc().getValue())))
          }
          layer.addMapping.react {
            case true =>
              eslEditor.getWrapperElement().classList.add("add-mapping")
            case false =>
              eslEditor.getWrapperElement().classList.remove("add-mapping")
          }
          layer.entities.react { e =>
            eslEditor.getDoc().getAllMarks().foreach(_.clear())
            layer.entityElements.values.foreach(_.clear())
            layer.entityElements.clear()
            e.foreach { e =>
              val elem = HTML(s"<span class='entity'><span class='name'>${e.name}</span></span>").head.asInstanceOf[HTMLElement]
              if (e.isInstanceOf[Clazz])
                elem.classList.add("class")
              else
                elem.classList.add("member")
              val o = TextMarkerOptions()
              o.replacedWith = elem
              o.addToHistory = false
              markESL(eslEditor, e, o).map { m =>
                layer.entityElements += e.qName -> new EntityElement(Seq(elem), layer, e, eslEditor, m)
              }
            }
          }
          this.eslEditor = Some(eslEditor)
        })
    }
  })


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

  def markNL(editor: CodeMirror, entity: LayerObject, options: TextMarkerOptions): Seq[TextMarker] = {
    entity match {
      case Requirement(name, content) =>
        mark(r"($name):\s+(?:$content)", editor, options)
    }
  }

  def markFSL(editor: CodeMirror, entity: LayerObject, options: TextMarkerOptions): Seq[TextMarker] = {
    entity match {
      case Clazz(clazz,_) =>
        mark(r"(?:class|enum)\s+($clazz)", editor, options, Some((clazz.length,0)))
      case Attribute(clazz, member, _) =>
        mark(r"(?:class|enum)\s+(?:$clazz)\s+\{[^\}]*(?:val|attr)[^\n]+\s($member);", editor, options, Some((member.length,1)))
      case Reference(clazz, member, t) =>
        mark(r"(?:class|enum)\s+(?:$clazz)\s+\{[^\}]*(?:ref|val)[^\n]+\s($member);", editor, options, Some((member.length,1)))
      case Operation(clazz, member, t, params) =>
        mark(r"(?:class|enum)\s+(?:$clazz)\s+\{[^\}]*op[^\n]+\s($member)(?:\()", editor, options, Some((member.length + 1,0)))
      case Parameter(clazz, member, "this", t) =>
        mark(r"(?:class|enum)\s+(?:$clazz)\s+\{[^\}]*op[^\n]+\s(?:$member)(\()", editor, options, Some((0,0)))
      case Parameter(clazz, member, name, t) =>
        mark(r"(?:class|enum)\s+(?:$clazz)\s+\{[^\}]*op[^\n]+\s(?:$member)\([^\)]*(?:$t)\s+($name)(?:\)|,)", editor, options, Some((name.length,1)))
      case Invariant(clazz, name) =>
        mark(r"(?:inv\s+)($name)\s*\:", editor, options, Some((name.length,1)))
      case Precondition(op, name) =>
        mark(r"(?:pre\s+)($name)\s*\:", editor, options, Some((name.length,1)))
      case Postcondition(op, name) =>
        mark(r"(?:post\s+)($name)\s*\:", editor, options, Some((name.length,1)))
    }
  }

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
  }

  def fslUnimplemented(emf: Layer)(clazz: String): Unit = {
    val e = emf.entityElements(clazz).error := true
  }

  def fslUnimplemented(emf: Layer, clazz: String, member: String): Unit = {
    val e = emf.entityElements(clazz + "." + member).error := true
  }

  def markMapping(mapping: Mapping) = for {
    to <- layer(mapping.toLayer).entityElements.get(mapping.to.qName)
  } {
    //from.matches += (layer(mapping.toLayer), mapping.to)
    to.matches += (layer(mapping.fromLayer), mapping.from)
  }

  def removeMapping(mapping: Mapping) = for {
    to <- layer(mapping.toLayer).entityElements.get(mapping.to.qName)
  } {
    //from.matches -= (layer(mapping.toLayer), mapping.to)
    to.matches -= (layer(mapping.fromLayer), mapping.from)
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

  def receive: PartialFunction[Message,Unit] = {
    case InitSpecs(Specs(nl,fsls,esl)) =>
      val layers = nl.map(new Layer(_)) ++ fsls.map(new Layer(_)) ++ esl.map(new Layer(_))
      tabs ++= layers
      this.layers ++= layers
      layers.headOption.foreach(_.active := true)
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
          this.layer(layer).entityElements.get(model.qName).foreach { elem =>
            elem.error := false
            elem.tooltip := None
          }
        case IgnoredModel(layer, model, reason) =>
          this.layer(layer).entityElements.get(model.qName).foreach { elem =>
            elem.ignored := None
          }
        case RemovedModel(tl, model, fl, impl) =>
          this.layer(fl).entityElements.get(impl.qName).foreach { elem =>
            elem.removedModel := None
          }
        case RemovedImplementation(tl, model, fl, impl) =>
          this.layer(tl).entityElements.get(model.qName).foreach { elem =>
            elem.removedImpl := None
          }
        case ModifiedImplementation(tl, model, fl, impl) =>
          this.layer(tl).entityElements.get(model.qName).foreach { elem =>
            elem.modifiedImpl := None
          }
        case RequirementEvaluation(layer, requirement, results) =>
          this.layer(layer).entityElements.get(requirement.qName).foreach { elem =>
            elem.tooltip := None
            elem.warn := false
          }
        case MaltypedMapping(layer, model, actual, expected) =>
          this.layer(layer).entityElements.get(model.qName).foreach { elem =>
            elem.error := false
            elem.tooltip := None
          }
        case MismatchingBounds(layer,model,_,_) =>
          this.layer(layer).entityElements.get(model.qName).foreach { elem =>
            elem.error := false
            elem.tooltip := None
          }
        case obl: OCLProofObligation =>
          this.layer(obl.layer).entityElements.get(obl.clazz).foreach { elem =>
            elem.proofObligations.modify(_ - obl)
          }
      }
      issues.foreach {
        case UnimplementedModel(layer, model) =>
          this.layer(layer).entityElements.get(model.qName).foreach { elem =>
            elem.error := true
            elem.tooltip := Some(s"'${model.qName}' doesn't have an implementation")
          }
        case IgnoredModel(layer, model, reason) =>
          this.layer(layer).entityElements.get(model.qName).foreach { elem =>
            elem.ignored := Some(reason)
          }
        case RemovedModel(tl, model, fl, impl) =>
          this.layer(fl).entityElements.get(impl.qName).foreach { elem =>
            elem.removedModel := Some(model)
          }
        case RemovedImplementation(tl, model, fl, impl) =>
          this.layer(tl).entityElements.get(model.qName).foreach { elem =>
            elem.removedImpl := Some(impl)
          }
        case ModifiedImplementation(tl, model, fl, impl) =>
          this.layer(tl).entityElements.get(model.qName).foreach { elem =>
            elem.modifiedImpl := Some(impl)
          }
        case MismatchingBounds(tl, model, _, _) =>
          this.layer(tl).entityElements.get(model.qName).foreach { elem =>
            elem.error := true
            elem.tooltip := Some(s"Mismatching bounds")
          }
        case RequirementEvaluation(layer, requirement, results) =>
          val out = results.zipWithIndex.collect {
            case (Some(false),i) => rules(i)
            case (None,i) => "(" + rules(i) + ")"
          }.mkString("; ")
          this.layer(layer).entityElements(requirement.qName).warn := true
          this.layer(layer).entityElements(requirement.qName).tooltip := Some(out)
        case MaltypedMapping(layer, model, expected, actual) =>
          this.layer(layer).entityElements(model.qName).error := true
          this.layer(layer).entityElements(model.qName).tooltip := Some(s"Maltyped implementation: expected: $expected, actual: $actual")
        case obl: OCLProofObligation =>
          this.layer(obl.layer).entityElements.get(obl.clazz).foreach { elem =>
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