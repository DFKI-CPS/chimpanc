package specific.nlp

import scala.collection.JavaConversions._
import java.lang.Object._
import edu.stanford.nlp.ling.CoreAnnotations._
import edu.stanford.nlp.pipeline.Annotation
import edu.stanford.nlp.semgraph.SemanticGraphCoreAnnotations._
import edu.stanford.nlp.trees.TreeCoreAnnotations._
import edu.stanford.nlp.util.CoreMap
import java.io._
object NLPUtils {
  val filePath = "resume.html";

  /* Annotation: sentence */
  def getSentences(document: Annotation) = document.get(classOf[SentencesAnnotation])

  /* Annotation: tokens */
  def getTokens(sentence: CoreMap) = sentence.get(classOf[TokensAnnotation])

  /* Annotation: tree */
  def getTree(sentence: CoreMap) = sentence.get(classOf[TreeAnnotation])

  /* Annotation: dependency graph */
  def getDependencyGraph(sentence: CoreMap) = sentence.get(classOf[CollapsedCCProcessedDependenciesAnnotation])

  def getBasicDependencyGraph(sentence: CoreMap) = sentence.get(classOf[BasicDependenciesAnnotation])

  /* Returns the number of sentences in a document */
  def numberOfSentences(document: Annotation) = getSentences(document).size

  /* Returns the first sentence in a document */
  def firstSentence(document: Annotation): CoreMap = getSentences(document).get(0)

  /* Returns the number of sub-sentences in a document */
  def numberOfSubsentences(sentence: CoreMap) = getTree(sentence).iterator.count(n => n.value == "S" && // parent is connective component
    n.numChildren() > 1)

  def listContains(ls: List[String], value: String): Boolean = {
    for (x <- ls) {
      if (x.equalsIgnoreCase(value))
        return true
    }
    return false
  }

  /* Returns whether the sentence contains a token */
  def hasToken(sentence: CoreMap, wordList: List[String]) =
    wordList.exists(n => sentence.toString.toLowerCase().contains(n.toLowerCase()))

  def hasCondition(sentence: CoreMap, wordList: List[String]) = {
    val tree = getTree(sentence)
    (tree.iterator.count(n => n.label().value().equals("SBAR")) > 0 // siblings of parent are subsentences
      || wordList.exists(n => sentence.toString.toLowerCase().contains(n.toLowerCase())))

  }
  /* Returns whether the sentence has two subjects */
  def multipleSubjects(sentence: CoreMap) = {

    val b = getBasicDependencyGraph(sentence).getEdgeSet
    val r1 = b.filter(n => (n.getRelation.getShortName == "nsubj")).map(n => n.getDependent)
    val r2 = b.filter(n => (n.getRelation.getShortName == "nsubjpass")).map(n => n.getDependent)
    val r3 = b.filter(n => (n.getRelation.getShortName == "conj")).map(n => n.getGovernor)
    (r1 & r3 nonEmpty) || (r2 & r3 nonEmpty)
  }

  /* Returns whether the sentence has multiple verbs */
  def multipleVerbs(sentence: CoreMap) = {
      val tree = getTree(sentence)
      tree.iterator.count(n => List("for", "nor", "and", "but", "or", "yet", "so").contains(n.value.toString.toLowerCase()) && // connective node
      n.parent(tree).value == "CC" && // parent is connective component
      n.parent(tree).siblings(tree).count(x => (x.value.startsWith("S") || x.value.startsWith("V") || x.value.startsWith("MD"))) > 1 // siblings of parent are subsentences
      ) != 0
  }

  /* Returns whether the sentence respect the format of a requirement <subj aux predicate> */
  def respectFormat(sentence: CoreMap) = {
    val graph = getBasicDependencyGraph(sentence)
    val b = graph.getEdgeSet
    val r1 = b.filter(n => (n.getRelation.getShortName == "nsubj")).map(n => n.getGovernor)
    val r2 = b.filter(n => (n.getRelation.getShortName == "nsubjpass")).map(n => n.getGovernor)
    val r3 = b.filter(n => (n.getRelation.getShortName == "aux")).map(n => n.getGovernor)
    ((r1 & r3) contains (graph.getFirstRoot())) || ((r2 & r3) contains (graph.getFirstRoot()))
  }

 /* Returns whether the sentence is passive or not*/
  def hasPaasiveVoice(sentence: CoreMap) = {
    val graph = getBasicDependencyGraph(sentence)
    val b = graph.getEdgeSet
    val r1 = b.filter(n => (n.getRelation.getShortName == "nsubj")).map(n => n.getGovernor)
    val r2 = b.filter(n => (n.getRelation.getShortName == "nsubjpass")).map(n => n.getGovernor)
    val r3 = b.filter(n => (n.getRelation.getShortName == "auxpass")).map(n => n.getGovernor)
     val r5 = b.filter(n => (n.getRelation.getShortName == "neg")).map(n => n.getGovernor)
    ((r1 & r3) contains (graph.getFirstRoot())) || ((r2 & r3) contains (graph.getFirstRoot())) || (r5 contains (graph.getFirstRoot()))

  }

  /* Returns whether the sentence has a subsentence connective */
  def hasConnective(sentence: CoreMap) = {
    val tree = getTree(sentence)
    tree.iterator.count(n => List("for", "nor", "and", "but", "or", "yet", "so").contains(n.value) && // connective node
      n.parent(tree).value == "CC" && // parent is connective component
      n.parent(tree).siblings(tree).count(n => (n.value.startsWith("S")) || (n.value.startsWith("N")) || (n.value.startsWith("V") || (n.value.startsWith("MD")))) > 1 // siblings of parent are subsentences
      ) != 0

  }

  /* Returns whether the dependency graph of a sentence has some given relations */
  def hasRelations(sentence: CoreMap, relations: List[String]) =
    getBasicDependencyGraph(sentence).getEdgeSet.exists(n => (relations contains n.getRelation.getShortName))

  /* Returns whether the sentence can be verifiable */
  def verifiable(sentence: CoreMap) = {
    val pattern = new scala.util.matching.Regex("[><=]\\d")
    (pattern findFirstIn sentence.get(classOf[TextAnnotation])) nonEmpty
  }

  /* Methods used for generating html file */

  def using[A <: { def close(): Unit }, B](resource: A)(f: A => B): B =
    try f(resource) finally resource.close()

  def writeToFile(data: String): Unit =
    using(new FileWriter(filePath))(_.write(data))

  def appendToFile(data: String): Unit =
    using(new PrintWriter(new FileWriter(filePath, true)))(_.println(data))

  def addHeader() = {
    var i = 0
    writeToFile("<!DOCTYPE html>\n<html>\n<head>\n<style>\n table,th,td\n{\n border:1px solid black;\n border-collapse:collapse;\npadding:5px;}\n</style>\n</head>\n<body>\n<table>\n")
    appendToFile("\t<tr>\n\t\t<th  width=\"5%\"> ID </th>")
    appendToFile("\t\t<th  width=\"45%\" style=\"word-wrap:break-word\">  Requirement  </th>")
    for (i <- 0 to 9) {
      appendToFile("\t\t<th  width=\"5%\" align=\"center\"> R" + i.toString + " </th>")
     }
    appendToFile("\t</tr>")
  }

  def addFooter() = {
    appendToFile("</table>\n</body>\n</html>")
  }

  def addCell(str: String, width: Int, color: Int) = {
    if (width == 0)
      appendToFile("\t\t<td  width=\"45%\" styleword-wrap:break-word\"> " + str + " </td>")
    else if (color == 0)
      appendToFile("\t\t<td  width=\"5%\" align=\"center\">" + str + " </td>")
    else if (color == 1)
      appendToFile("\t\t<td  width=\"5%\" align=\"center\" bgcolor=\"#00CC66\"> " + str + " </td>")
    else if (color == 2)
      appendToFile("\t\t<td  width=\"5%\" align=\"center\" bgcolor=\"#FF0000\"> " + str + " </td>")
    else
      appendToFile("\t\t<td  width=\"5%\" align=\"center\" bgcolor=\"#BDF2FF\"> " + str + " </td>")
  }

  def init(line: String, i : Int) = {
      appendToFile("\t<tr>")
      addCell(i.toString, 1, 0)
      addCell(line, 0, 0)
  }
}
