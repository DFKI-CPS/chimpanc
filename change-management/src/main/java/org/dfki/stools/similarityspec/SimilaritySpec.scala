/*
 * Copyright (c) Serge Autexier, Dominik Dietrich, DFKI GmbH 2010.
 */
package org.dfki.stools.similarityspec

import org.antlr.runtime._
import org.antlr.runtime.tree.Tree
import org.dfki.PersonalNamespaceContext
import org.dfki.stools.SimilaritySpecLexer
import org.dfki.stools.SimilaritySpecParser
import org.dfki.utils.Collectionxx
import org.dfki.utils.Stringxx
import javax.xml.XMLConstants
import java.io.File
import java.io.IOException
import java.util._
import org.dfki.stools.SimilaritySpecLexer._

/**
 * Created by IntelliJ IDEA.
 * User: autexier
 * Date: Nov 4, 2010
 * Time: 9:53:14 AM
 * To change this template use File | Settings | File Templates.
 */
object SimilaritySpec {
  def fromString(s: String): List[SimilaritySpec] = {
    val input: ANTLRStringStream = new ANTLRStringStream(s)
    fromCharStream(input)
  }

  def fromFile(f: File): List[SimilaritySpec] = {
    var input: ANTLRFileStream = null
    try {
      input = new ANTLRFileStream(f.getCanonicalPath)
      return fromCharStream(input)
    }
    catch {
      case e: IOException =>
        e.printStackTrace()
    }
    new ArrayList[SimilaritySpec]
  }

  private def fromCharStream(input: CharStream): List[SimilaritySpec] = {
    val lex: SimilaritySpecLexer = new SimilaritySpecLexer(input)
    val tokens: CommonTokenStream = new CommonTokenStream(lex)
    val parser: SimilaritySpecParser = new SimilaritySpecParser(tokens)
    val result: List[SimilaritySpec] = new ArrayList[SimilaritySpec]
    try {
      val res: SimilaritySpecParser.equivspecs_return = parser.equivspecs
      if (res != null && (res.getTree.asInstanceOf[Tree]).getType == EQUIVSPECS) {
        val equivspecs: Tree = res.getTree.asInstanceOf[Tree]
        var i: Int = 0
        while (i < equivspecs.getChildCount) {
          {
            val child: Tree = equivspecs.getChild(i).asInstanceOf[Tree]
            result.add(new SimilaritySpec(child))
          }
          {
            i += 1; i - 1
          }
        }
      }
    }
    catch {
      case e: RecognitionException =>
        e.printStackTrace()
    }
    result
  }
}

class SimilaritySpec(private[similarityspec] var name: String = "", tree: Option[Tree] = None) {
  private[similarityspec] val entries: Map[String, List[ElementSimilaritySpec]] = new HashMap[String, List[ElementSimilaritySpec]]
  private[similarityspec] var defaultSpec: List[ElementSimilaritySpec] = new ArrayList[ElementSimilaritySpec]
  private[similarityspec] var namespaceContext: PersonalNamespaceContext = new PersonalNamespaceContext(new HashMap[String, String])
  private[similarityspec] var parents: Set[String] = new HashSet[String]
  private[similarityspec] var limit: ElementSpecs = new ElementSpecs

  def this(tree: Tree) = this("", Some(tree))

  tree.foreach { tree =>
      var i: Int = 0
      while (i < tree.getChildCount) {
        {
          val child: Tree = tree.getChild(i)
          child.getType match {
            case NCNAME =>
              name = child.getText
            case EXTENDS => {
              var c: Int = 0
              while (c < child.getChildCount) {
                {
                  val extendschild: Tree = child.getChild(c)
                  parents.add(extendschild.getText)
                }
                {
                  c += 1; c - 1
                }
              }
            }
            case XMLNS =>
              addDefaultNameSpace(child.getChild(0).getText)
            case XMLNSPREFIX =>
              addDeclaredNameSpace(child.getChild(0).getText, child.getChild(1).getText)
            case ELEMENT =>
              val e: ElementSimilaritySpec = new ElementSimilaritySpec(child, this)
              val temp: List[ElementSimilaritySpec] = new ArrayList[ElementSimilaritySpec]
              temp.add(e)
              entries.put(e.tagname, temp)
            case LIMIT => {
              var c: Int = 0
              while (c < child.getChildCount) {
                {
                  limit.add(SubElementSimilaritySpec.parseElementSpec(child.getChild(c), this))
                }
                {
                  c += 1; c - 1
                }
              }
            }
            case IGNORE =>
            case DEFAULTSPEC =>
              defaultSpec.add(new ElementSimilaritySpec(child, this))
            case _ =>
              System.err.println("Unrecognized child " + child)
          }
        }
        {
          i += 1; i - 1
        }
      }
  }

  def addParent(name: String) {
    parents.add(name)
  }

  def getParents: Set[String] = parents

  def setParents(parents: Set[String]) {
    this.parents = parents
  }

  def getName: String = name

  def getLimit: ElementSpecs = limit

  def addLimit(spec: ElementSpec) {
    limit.add(spec)
  }

  def addDefaultSimilaritySpec(d: ArrayList[ElementSimilaritySpec]) {
    if (defaultSpec == null) defaultSpec = d
    else defaultSpec.addAll(d)
  }

  def addDefaultNameSpace(url: String) {
    namespaceContext.namespaces.put("", url)
  }

  def addDeclaredNameSpace(prefix: String, url: String) {
    namespaceContext.namespaces.put(prefix, url)
  }

  def addElementSimilaritySpec(name: String, spec: ElementSimilaritySpec) {
    var n = name
    if (n.contains(":")) {
      val temp: Array[String] = n.split(":")
      if (!(namespaceContext.getNamespaceURI(temp(0)) == XMLConstants.NULL_NS_URI)) {
        n = namespaceContext.getNamespaceURI(temp(0)) + ":" + n.substring(temp(0).length + 1, n.length)
      }
    }
    else if (!(namespaceContext.getNamespaceURI("") == XMLConstants.NULL_NS_URI)) {
      n = namespaceContext.getNamespaceURI("") + ":" + n
    }
    if (entries.containsKey(n)) {
      entries.get(n).add(spec)
    }
    else {
      val temp: ArrayList[ElementSimilaritySpec] = new ArrayList[ElementSimilaritySpec]
      temp.add(spec)
      entries.put(n, temp)
    }
  }

  def addElementSimilaritySpec(name: String, specs: List[ElementSimilaritySpec]) {
    var n = name
    if (n.contains(":")) {
      val temp: Array[String] = n.split(":")
      if (!(namespaceContext.getNamespaceURI(temp(0)) == XMLConstants.NULL_NS_URI)) {
        n = namespaceContext.getNamespaceURI(temp(0)) + ":" + n.substring(temp(0).length + 1, n.length)
      }
    }
    else if (!(namespaceContext.getNamespaceURI("") == XMLConstants.NULL_NS_URI)) {
      n = namespaceContext.getNamespaceURI("") + ":" + n
    }
    if (entries.containsKey(n)) {
      entries.get(n).addAll(specs)
    }
    else {
      val temp: ArrayList[ElementSimilaritySpec] = new ArrayList[ElementSimilaritySpec]
      temp.addAll(specs)
      entries.put(n, temp)
    }
  }

  def getElementSimilaritySpecs(namespaceURI: String, name: String): List[ElementSimilaritySpec] = {
    var n = name
    if (namespaceURI != null && !(namespaceURI == XMLConstants.NULL_NS_URI)) {
      n = namespaceURI + ":" + n
    }
    if (entries.containsKey(n)) {
      return entries.get(n)
    }
    else if (defaultSpec.isEmpty) {
      System.out.println("Creating defaultspec for " + n + " spec=" + this.getName)
      val aspec: AnnotationSimilaritySpec = new AnnotationSimilaritySpec
      aspec.checkannotations.add(new AllButAnnotationSpec(new ArrayList[AnnotationNameSpec](0)))
      val espec: SubElementSimilaritySpec = new SubElementSimilaritySpec
      espec.ordered.add(new AllButElementSpec(new ArrayList[ElementNameSpec](0)))
      defaultSpec = Collectionxx.newList(new ElementSimilaritySpec(null, null, aspec, espec))
    }
    val especinst: List[ElementSimilaritySpec] = new ArrayList[ElementSimilaritySpec]
    import scala.collection.JavaConversions._
    for (espec <- defaultSpec) {
      especinst.add(new ElementSimilaritySpec(namespaceURI, n, espec.annotationSimSpec, espec.subelementSimSpec))
    }
    return especinst
  }

  private def similarityEntriesToString(es: ElementSpecs): String = {
    val es1: List[ElementSimilaritySpec] = new ArrayList[ElementSimilaritySpec](es.size)
    es1.addAll(es1)
    return similarityEntriesToString(es1)
  }

  private def similarityEntriesToString(es: List[ElementSimilaritySpec]): String = {
    var res: String = ""
    es.size match {
      case 0 =>
      case 1 =>
        res = res + "\n" + Stringxx.tabulate(es.get(0).toString, 4)
        res = res + "  "
      case _ =>
        res = res + "\n  alternatives {\n"
        import scala.collection.JavaConversions._
        for (e <- es) {
          res = res + "  {\n" + Stringxx.tabulate(e.toString, 4)
          res = res + "  }\n"
        }
        res = res + "  }\n"
    }
    res
  }

  override def toString: String = {
    var res: String = "equivspec " + name
    if (!(namespaceContext.getNamespaceURI("") == XMLConstants.NULL_NS_URI)) res = res + " xmlns " + namespaceContext.getNamespaceURI("")
    if (!parents.isEmpty) {
      res = res + " extends "
      import scala.collection.JavaConversions._
      for (p <- parents) res = res + p + " "
    }
    res = res + " {\n"
    import scala.collection.JavaConversions._
    for (prefix <- namespaceContext.namespaces.keySet) {
      if (!(prefix == "")) {
        res = res + "xmlns:" + prefix + "=" + namespaceContext.getNamespaceURI(prefix) + "\n"
      }
    }
    val keys: ArrayList[String] = new ArrayList[String]
    keys.addAll(entries.keySet)
    Collections.sort(keys)
    import scala.collection.JavaConversions._
    for (tagname <- keys) {
      val temp: String = "element " + tagname + " {"
      res = res + temp
      res = res + similarityEntriesToString(entries.get(tagname))
      res = res + "}\n"
    }
    if (!limit.isEmpty) {
      res = res + "limit {" + similarityEntriesToString(limit) + "}\n"
    }
    if (defaultSpec != null) {
      res = res + ":default {"
      res = res + similarityEntriesToString(defaultSpec)
      res = res + "}\n"
    }
    res = res + "}"
    res
  }

  def includeSimilaritySpec(spec: SimilaritySpec) {
    import scala.collection.JavaConversions._
    for (name <- spec.entries.keySet) {
      if (!entries.containsKey(name)) {
        val es: List[ElementSimilaritySpec] = Collectionxx.copy(spec.entries.get(name))
        if (!spec.limit.isEmpty) {
          import scala.collection.JavaConversions._
          for (e <- es) {
            if (e.subelementSimSpec.limit.isEmpty) e.subelementSimSpec.limit.addAll(spec.limit)
          }
        }
        entries.put(name, es)
      }
    }
    if (!defaultSpec.isEmpty) defaultSpec = Collectionxx.copy(spec.defaultSpec)
    import scala.collection.JavaConversions._
    for (prefix <- spec.namespaceContext.namespaces.keySet) {
      if (!namespaceContext.namespaces.containsKey(prefix)) {
        namespaceContext.namespaces.put(prefix, spec.namespaceContext.namespaces.get(prefix))
      }
    }
  }
}