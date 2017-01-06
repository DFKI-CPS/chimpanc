/**
 */
package specific.emf.cpp;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Namespace</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link specific.emf.cpp.CppNamespace#getTypedefs <em>Typedefs</em>}</li>
 *   <li>{@link specific.emf.cpp.CppNamespace#getOperations <em>Operations</em>}</li>
 *   <li>{@link specific.emf.cpp.CppNamespace#getVariables <em>Variables</em>}</li>
 * </ul>
 * </p>
 *
 * @see specific.emf.cpp.CppPackage#getCppNamespace()
 * @model
 * @generated
 */
public interface CppNamespace extends EObject, EPackage {
	/**
	 * Returns the value of the '<em><b>Typedefs</b></em>' containment reference list.
	 * The list contents are of type {@link specific.emf.cpp.CppTypedef}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Typedefs</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Typedefs</em>' containment reference list.
	 * @see specific.emf.cpp.CppPackage#getCppNamespace_Typedefs()
	 * @model containment="true" keys="name"
	 *        extendedMetaData="namespace=''"
	 * @generated
	 */
	EList<CppTypedef> getTypedefs();

	/**
	 * Returns the value of the '<em><b>Operations</b></em>' containment reference list.
	 * The list contents are of type {@link specific.emf.cpp.CppOperation}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Operations</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Operations</em>' containment reference list.
	 * @see specific.emf.cpp.CppPackage#getCppNamespace_Operations()
	 * @model containment="true"
	 * @generated
	 */
	EList<CppOperation> getOperations();

	/**
	 * Returns the value of the '<em><b>Variables</b></em>' containment reference list.
	 * The list contents are of type {@link specific.emf.cpp.CppAttribute}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Variables</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Variables</em>' containment reference list.
	 * @see specific.emf.cpp.CppPackage#getCppNamespace_Variables()
	 * @model containment="true"
	 * @generated
	 */
	EList<CppAttribute> getVariables();

} // CppNamespace
