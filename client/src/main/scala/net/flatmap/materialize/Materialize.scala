package net.flatmap.materialize

import net.flatmap.codemirror.{Mode, CodeMirrorConfiguration}
import org.scalajs.dom.raw.{HTMLTextAreaElement, Element}

import scala.scalajs.js
import scala.scalajs.js.annotation.JSName

/**
 * Created by martin on 16.09.15.
 */
@JSName("CodeMirror")
object Materialize extends js.Object {
  def toast(content: String, duration: Int): Unit = js.native
  /*def apply(place: Element): CodeMirror = js.native
  def version: String = js.native
  def fromTextArea(host: HTMLTextAreaElement, options: CodeMirrorConfiguration = js.native): CodeMirror with FromTextArea = js.native
  def defaults: CodeMirrorConfiguration = js.native
  def defineExtension(name: String, value: js.Any): Unit = js.native
  def defineDocExtension(name: String, value: js.Any): Unit = js.native
  def defineOption(name: String, default: js.Any, updateFunc: js.Function): Unit = js.native
  def defineInitHook(func: js.Function1[CodeMirror,Unit]): Unit = js.native
  def registerHelper(typ: String, name: String, value: js.Any): Unit = js.native
  def registerGlobalHelper(typ: String, name: String, predicate: js.Function2[String,CodeMirror,Boolean], value: js.Any): Unit = js.native
  def Pos(line: Int, ch: Int = js.native): Position = js.native
  def changeEnd(change: js.Any): Position = js.native
  def copyState[S](mode: Mode[S], state: S): S = js.native

  def Pass: Nothing = js.native
  def defineMode[S](name: String, constructor: js.Function2[CodeMirrorConfiguration, Any, Mode[S]]): Unit = js.native
  def defineMIME(mime: String, modeSpec: String): Unit = js.native
  def defineMIME[S](mime: String, modeSpec: Mode[S]): Unit = js.native
  def extendMode[S](mode: String, extensions: Mode[S]): Unit = js.native

  def commands: CodeMirrorCommands = js.native*/
}

@JSName("$")
object jQuery extends js.Object {
  def apply(selector: String, context: Element = js.native): jQuery = js.native
  def apply(elems: js.Array[Element]): jQuery = js.native
  def apply(elem: Element): jQuery = js.native
}

trait jQuery extends js.Object {
  def tooltip(): Unit = js.native
  def tabs(): Unit = js.native
  def collapsible(): Unit = js.native
}