import collection.mutable.Stack
import org.scalatest._

import Pipeline._
import Rules._

class RuleTest extends FlatSpec with Matchers {

  "Rule 1" should "work correctly" in {
    ruleIsOneRequirement( "The machine is fast." ) should be ( None )
    ruleIsOneRequirement( "The machine is loud. The machine is clean." ) should be ( Some(false) )
    ruleIsOneRequirement( "The machine is lound and the machine is clean." ) should be ( Some(false) )
  }

  "Rule 2" should "work correctly" in {
    ruleHasNoConjunctions( "The machine is lound and the machine is clean." ) should be ( Some(false) )
    ruleHasNoConjunctions( "The machine is loud and clean." ) should be ( Some(true) )
  }

  "Rule 4" should "work correctly" in {
    ruleIsDirectSentence( "I operate the machine." ) should be ( None )
    ruleIsDirectSentence( "The machine is operated by me." ) should be ( Some(false) )
  }

  "Rule 6" should "work correctly" in {
    ruleAvoidExpressingSuggestions( "The machine may hold unexpected." ) should be ( Some(false) )
    ruleAvoidExpressingSuggestions( "The machine shouldn't stop." ) should be ( Some(false) )
    ruleAvoidExpressingSuggestions( "The machine is loud." ) should be ( Some(true) )
  }

}
