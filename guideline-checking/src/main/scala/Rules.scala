package specific.nlp

import scala.collection.JavaConversions._
import scala.util.matching.Regex
import edu.stanford.nlp.pipeline.Annotation

import Pipeline._
import NLPUtils._

object Rules {
  var condition = true;
  /* Rule 1 */
  def ruleIsOneRequirement(document: Annotation): Option[Boolean] = {
    val numbSentences = numberOfSentences(document)
    val numSubsentences = numberOfSubsentences(firstSentence(document))
   if (numSubsentences == 2 && !getSentences(document).exists(hasToken(_, List("As a precondition", "As preconditions", "As a postcondition", "As postconditions", "In case","supposing","once", "until","till","in order that", "when", "if","before","whether","where","whenever", "after")))) {
      condition = false
      addCell("F", 1, 2)
      Some(false)
    }
    else if (numbSentences > 1 || numSubsentences > 2 || getSentences(document).exists(multipleVerbs(_)) || getSentences(document).exists(multipleSubjects(_)))
    { 
      addCell("F", 1, 2)
      Some(false)
    }
    else 
    {    
      addCell("T", 1, 1)
      Some(true)
    }
  }

  /* Rule 2 */
  def ruleHasNoConjunctions(document: Annotation): Option[Boolean] = {
    if (getSentences(document).exists(multipleVerbs(_)) || getSentences(document).exists(multipleSubjects(_)))
    { 
      addCell("F", 1, 2)
      Some(false)
    }
    else 
    {    
      addCell("T", 1, 1)
      Some(true)
    }
  }

  /* Rule 3 */
  def ruleIsDirectSentence(document: Annotation): Option[Boolean] = {
    if (getSentences(document).exists(hasPaasiveVoice(_)))
    { 
      addCell("F", 1, 2)
      Some(false)
    }
    else 
    {    
      addCell("T", 1, 1)
      Some(true)
    }
  }

  /* Rule 4 */
  def ruleIsRequirementFormat(document: Annotation): Option[Boolean] = {
    if (getSentences(document).exists(!respectFormat(_)))
    { 
      addCell("F", 1, 2)
      Some(false)
    }
    else 
    {    
      addCell("T", 1, 1)
      Some(true)
    }
  }

  /* Rule 5 */
  def ruleAvoidLetOutClauses(document: Annotation): Option[Boolean] = {
    if ((getSentences(document).exists(hasToken(_, List("unless", "except", "if necessary", "but", "when", "unless", "although")))) && !condition)
    { 
      addCell("F", 1, 2)
      Some(false)
    }
    else 
    {    
      addCell("T", 1, 1)
      Some(true)
    }
  }

  /* Rule 6 */
  def ruleAvoidExpressingSuggestions(document: Annotation): Option[Boolean] = {
    if ((getSentences(document).exists(hasToken(_, List("might", "may", "could", "ought", "should", "perhaps", "probably", "can be")))))
    { 
      addCell("F", 1, 2)
      Some(false)
    }
    else 
    {    
      addCell("T", 1, 1)
      Some(true)
    }
  }

  /* Rule 7 */
  def ruleAvoidWeakPhrases(document: Annotation): Option[Boolean] ={
    if ((getSentences(document).exists(hasToken(_, List("adequate", "as possible", "as a minimum", "as applicable", "easy", "as appropriate", "be able to", "is able to", "are able to",
      "be capable", "is capable", "are capable", "but not limited to", "capability of", "capability to", "effective", "if practical", "normal", "provide for", "timely", "tbd", "user-friendly",
      "versatile", "robust", "approximately", "minimal impact", "etc", "and so on", "flexible", "to the maximum extent", "as much as possible", "minimal impact", "in one whack",
      "different", "various", "many", "some of", "diverse", "immediately", "relative", "several", "helpful", "appropriate", "to ease", "necessary")))))
    { 
      addCell("F", 1, 2)
      Some(false)
    }
    else 
    {    
      addCell("T", 1, 1)
      Some(true)
    }
  }

  /* Rule 8 */
  def ruleAvoidSpeculation(document: Annotation): Option[Boolean] = {
    if ((getSentences(document).exists(hasToken(_, List("usually", "generally", "as if", "often", "normally", "typically", "typical")))))
    { 
      addCell("F", 1, 2)
      Some(false)
    }
    else 
    {    
      addCell("T", 1, 1)
      Some(true)
    }
  }

  /* Rule 9 */
  def ruleAvoidWishfulThinking(document: Annotation): Option[Boolean] = {
    if ((getSentences(document).exists(hasToken(_, List("100% reliable", "safe system", "handle all failures", "fully upgradeable", "run on all platforms")))))
    { 
      addCell("F", 1, 2)
      Some(false)
    }
    else 
    {    
      addCell("T", 1, 1)
      Some(true)
    }
  }

  /* Rule 10 */
  def ruleVerifiableCriteria(document: Annotation): Option[Boolean] = {
    val a = getSentences(document).exists(verifiable(_))
    val b = getSentences(document).exists(hasToken(_, List("minimum", "minimal", "maximum", "maximal", "approximately", "different", "equal",
      "big", "bigger", "small", "smaller", "bigger", "less", "cheaper", "lower", "more", "much")))
    val c = getSentences(document).exists(hasToken(_, List("high", "low", "immediately")))

    if (a && b)
    { 
      addCell("U", 1, 3)
      None
    }
    else if (a || c)
    {    
      addCell("T", 1, 1)
      Some(true)
    }
    else if (b)
    {    
      addCell("F", 1, 2)
      Some(false)
    }
    else
    {    
      addCell("U", 1, 3)
      None
    }
  }    
  
}

