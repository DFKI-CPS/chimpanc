package specific.graph

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.eclipse.emf.common.util.URI
import org.eclipse.uml2.uml.resources.util.UMLResourcesUtil
import org.eclipse.emf.ecore.xmi.impl.EcoreResourceFactoryImpl
import org.eclipse.papyrus.sysml.blocks.BlocksPackage
import org.dfki.stools.STools
import java.io.File
import specific.secore.SEcore._

/**
 * @author Martin Ring <martin.ring@dfki.de>
 * @since  Nov 24, 2014
 */
class SemanticsSpec extends FlatSpec with Matchers with ECoreToGraph {
  "the semantics module" should "find all unimplemented models" in {
    lazy val resourceSet = {
      val rs = new HybridResourceSet
      UMLResourcesUtil.init(rs)
      rs.getPackageRegistry put ("http://www.eclipse.org/papyrus/0.7.0/SysML/Blocks", BlocksPackage.eINSTANCE)
      rs.getResourceFactoryRegistry().getExtensionToFactoryMap().put(
        "ecore", new EcoreResourceFactoryImpl());
      rs
    }
    
    resourceSet.clearGraph()
    
    val model = resourceSet.getResource(URI.createURI(getClass.getResource("/model.uml").getPath), true)
    val systemc = resourceSet.getResource(URI.createURI(getClass.getResource("/systemc.ecore").getPath), true)
    
    writeResource(model, "model", "UML")  
    writeResource(systemc, "systemc", "SystemC")
  
    val graphModel = resourceSet.getGraphResource("model")
    val graphImpl = resourceSet.getGraphResource("systemc")
  
    Semantics.propagateFSL(graphModel)
    Semantics.propagateESL(graphImpl)
    Semantics.mapFSL2ESL(graphModel,graphImpl)  
    
    Semantics.findProblems() should contain allOf (
        UnimplementedModel("System"),
        UnimplementedModel("EnoughTires"),
        UnimplementedModel("Tire.pressure"))
    
    /*Semantics.resetSemantics(graphModel)
  
    val stools = new STools(new File(getClass.getResource("/ecore.simeq").getFile))
    val model2 = resourceSet.getResource(URI.createURI(getClass.getResource("/model2.uml").getPath), true)
    
    val diff = stools.getSTool("ecore").sdiff(new SResource(graphModel), new SResource(model2))
    
    applyDiff(diff)
      
    Semantics.propagateFSL(graphModel)
    
    println()
    println("* Problems after diff application:")
    
    Semantics.findProblems().foreach { println }*/
  }
  
  it should "find dangling implementations after diff application" in {
    lazy val resourceSet = {
      val rs = new HybridResourceSet
      UMLResourcesUtil.init(rs)
      rs.getPackageRegistry put ("http://www.eclipse.org/papyrus/0.7.0/SysML/Blocks", BlocksPackage.eINSTANCE)
      rs.getResourceFactoryRegistry().getExtensionToFactoryMap().put(
        "ecore", new EcoreResourceFactoryImpl());
      rs
    }
    
    resourceSet.clearGraph()
    
    val model = resourceSet.getResource(URI.createURI(getClass.getResource("/model.uml").getPath), true)
    val systemc = resourceSet.getResource(URI.createURI(getClass.getResource("/systemc.ecore").getPath), true)
    
    writeResource(model, "model", "UML")  
    writeResource(systemc, "systemc", "SystemC")
  
    val graphModel = resourceSet.getGraphResource("model")
    val graphImpl = resourceSet.getGraphResource("systemc")
  
    Semantics.propagateFSL(graphModel)
    Semantics.propagateESL(graphImpl)
    Semantics.mapFSL2ESL(graphModel,graphImpl)  
        
    Semantics.resetSemantics(graphModel)
  
    val stools = new STools(new File(getClass.getResource("/ecore.simeq").getFile))
    val model2 = resourceSet.getResource(URI.createURI(getClass.getResource("/model2.uml").getPath), true)
    
    val diff = stools.getSTool("ecore").sdiff(new SResource(graphModel), new SResource(model2))
    
    applyDiff(diff)
      
    Semantics.propagateFSL(graphModel)
        
    Semantics.findProblems() should contain allOf (
        RemovedModel("Tire","Tire"),
        RemovedModel("Car.tire","tire"))    
  }
}