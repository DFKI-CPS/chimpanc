package specific.nlp

import edu.stanford.nlp.pipeline.Annotation
import edu.stanford.nlp.pipeline.StanfordCoreNLP

import Conversions._

object Pipeline {

  /* The Stanford Core NLP pipeline to work with */
  lazy val pipeline = new StanfordCoreNLP( Map( "annotators" -> "tokenize, ssplit, parse" ) )

  /* Creates an annotation from a text and runs it through the pipeline */
  implicit def annotate( text: String ): Annotation = {
    val document = new Annotation( text )
    pipeline annotate document
    document
  }

}
