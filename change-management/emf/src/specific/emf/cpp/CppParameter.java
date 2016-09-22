/**
 */
package specific.emf.cpp;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EParameter;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Parameter</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link specific.emf.cpp.CppParameter#getTypedef <em>Typedef</em>}</li>
 * </ul>
 * </p>
 *
 * @see specific.emf.cpp.CppPackage#getCppParameter()
 * @model extendedMetaData="name='CPPParameter'"
 * @generated
 */
public interface CppParameter extends EObject, EParameter {
	/**
	 * Returns the value of the '<em><b>Typedef</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Typedef</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Typedef</em>' reference.
	 * @see #setTypedef(CppTypedef)
	 * @see specific.emf.cpp.CppPackage#getCppParameter_Typedef()
	 * @model extendedMetaData="name='typedef' namespace=''"
	 * @generated
	 */
	CppTypedef getTypedef();

	/**
	 * Sets the value of the '{@link specific.emf.cpp.CppParameter#getTypedef <em>Typedef</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Typedef</em>' reference.
	 * @see #getTypedef()
	 * @generated
	 */
	void setTypedef(CppTypedef value);

} // CppParameter
