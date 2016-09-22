package specific.graph

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.eclipse.emf.common.util.URI
import org.eclipse.uml2.uml.resources.util.UMLResourcesUtil
import org.eclipse.emf.ecore.xmi.impl.EcoreResourceFactoryImpl
import org.eclipse.papyrus.sysml.blocks.BlocksPackage

/**
 * @author Martin Ring <martin.ring@dfki.de>
 * @since  Nov 21, 2014
 */
class HybridResourceSetSpec extends FlatSpec with Matchers {
  lazy val resourceSet = {
    val rs = new HybridResourceSet
    UMLResourcesUtil.init(rs)
    rs.getPackageRegistry put ("http://www.eclipse.org/papyrus/0.7.0/SysML/Blocks", BlocksPackage.eINSTANCE)
    rs.getResourceFactoryRegistry().getExtensionToFactoryMap().put( 
      "ecore", new EcoreResourceFactoryImpl()); 
    rs
  }
  
  "a hybrid resource set" should "load file resources" in {
    val fileResource = resourceSet.getResource(URI.createURI(getClass.getResource("/model.uml").getPath), true)
    fileResource should not be (null)
  }
  
  it should "persist a file resource to the graph" in {   
    val fileResource = resourceSet.getResource(URI.createURI(getClass.getResource("/model.uml").getPath), true)    
    resourceSet.writeResource(fileResource, "test", "UML") should equal (true)
  }
  
  it should "load resources from the graph" in {
    val graphResource = resourceSet.getGraphResource("test", true)
    graphResource should not be (null)
  }
}