/**
 */
package specific.emf.cpp;

import org.eclipse.emf.ecore.EGenericType;
import org.eclipse.emf.ecore.ENamedElement;
import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Typedef</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link specific.emf.cpp.CppTypedef#getVisibility <em>Visibility</em>}</li>
 * </ul>
 * </p>
 *
 * @see specific.emf.cpp.CppPackage#getCppTypedef()
 * @model
 * @generated
 */
public interface CppTypedef extends EObject, EGenericType, ENamedElement {
	/**
	 * Returns the value of the '<em><b>Visibility</b></em>' attribute.
	 * The default value is <code>"default"</code>.
	 * The literals are from the enumeration {@link specific.emf.cpp.CppVisibility}.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Visibility</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Visibility</em>' attribute.
	 * @see specific.emf.cpp.CppVisibility
	 * @see #setVisibility(CppVisibility)
	 * @see specific.emf.cpp.CppPackage#getCppTypedef_Visibility()
	 * @model default="default"
	 * @generated
	 */
	CppVisibility getVisibility();

	/**
	 * Sets the value of the '{@link specific.emf.cpp.CppTypedef#getVisibility <em>Visibility</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Visibility</em>' attribute.
	 * @see specific.emf.cpp.CppVisibility
	 * @see #getVisibility()
	 * @generated
	 */
	void setVisibility(CppVisibility value);

} // CppTypedef
