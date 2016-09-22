package specific.nlp

import scala.io.Source

import Rules._
import Pipeline._
import NLPUtils._
import Array._

object Main extends App {
  var i: Int = 0
  addHeader()
  for (line <- Source.fromFile("req_list.dat").getLines) {

    if (line.nonEmpty && !line.contains("Requirement List") && !line.contains("---")) {
      init(line, i)
 
      ruleIsOneRequirement(line)
      ruleHasNoConjunctions(line)
      ruleIsDirectSentence(line)
      ruleIsRequirementFormat(line)
      ruleAvoidLetOutClauses(line)
      ruleAvoidExpressingSuggestions(line)
      ruleAvoidWeakPhrases(line)
      ruleAvoidSpeculation(line)
      ruleAvoidWishfulThinking(line)
      ruleVerifiableCriteria(line)  

      appendToFile("\t</tr>")
      i = i + 1
    }
  }
  addFooter()
}
