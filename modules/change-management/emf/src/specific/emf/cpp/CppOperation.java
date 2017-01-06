/**
 */
package specific.emf.cpp;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EOperation;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Operation</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link specific.emf.cpp.CppOperation#getVisibility <em>Visibility</em>}</li>
 *   <li>{@link specific.emf.cpp.CppOperation#isStatic <em>Static</em>}</li>
 *   <li>{@link specific.emf.cpp.CppOperation#getTypedef <em>Typedef</em>}</li>
 * </ul>
 * </p>
 *
 * @see specific.emf.cpp.CppPackage#getCppOperation()
 * @model
 * @generated
 */
public interface CppOperation extends EObject, EOperation {
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
	 * @see specific.emf.cpp.CppPackage#getCppOperation_Visibility()
	 * @model default="default"
	 * @generated
	 */
	CppVisibility getVisibility();

	/**
	 * Sets the value of the '{@link specific.emf.cpp.CppOperation#getVisibility <em>Visibility</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Visibility</em>' attribute.
	 * @see specific.emf.cpp.CppVisibility
	 * @see #getVisibility()
	 * @generated
	 */
	void setVisibility(CppVisibility value);

	/**
	 * Returns the value of the '<em><b>Static</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Static</em>' attribute isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Static</em>' attribute.
	 * @see #setStatic(boolean)
	 * @see specific.emf.cpp.CppPackage#getCppOperation_Static()
	 * @model
	 * @generated
	 */
	boolean isStatic();

	/**
	 * Sets the value of the '{@link specific.emf.cpp.CppOperation#isStatic <em>Static</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Static</em>' attribute.
	 * @see #isStatic()
	 * @generated
	 */
	void setStatic(boolean value);

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
	 * @see specific.emf.cpp.CppPackage#getCppOperation_Typedef()
	 * @model extendedMetaData="name='typedef' namespace=''"
	 * @generated
	 */
	CppTypedef getTypedef();

	/**
	 * Sets the value of the '{@link specific.emf.cpp.CppOperation#getTypedef <em>Typedef</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Typedef</em>' reference.
	 * @see #getTypedef()
	 * @generated
	 */
	void setTypedef(CppTypedef value);

} // CppOperation
