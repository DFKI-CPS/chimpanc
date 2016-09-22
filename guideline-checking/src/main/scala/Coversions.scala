package specific.nlp

import java.util.Properties

object Conversions {
  implicit def toProperties( m: Map[String,String] ) = {
    val properties = new Properties
    for ( (k, v) <- m ) {
      properties.put( k, v )
    }
    properties
  }
}
