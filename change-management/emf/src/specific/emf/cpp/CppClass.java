/**
 */
package specific.emf.cpp;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;

/**
 * <!-- begin-user-doc -->
 * A representation of the model object '<em><b>Class</b></em>'.
 * <!-- end-user-doc -->
 *
 * <p>
 * The following features are supported:
 * <ul>
 *   <li>{@link specific.emf.cpp.CppClass#getTypedefs <em>Typedefs</em>}</li>
 *   <li>{@link specific.emf.cpp.CppClass#getNestedClasses <em>Nested Classes</em>}</li>
 *   <li>{@link specific.emf.cpp.CppClass#getParentClass <em>Parent Class</em>}</li>
 *   <li>{@link specific.emf.cpp.CppClass#getNamespace <em>Namespace</em>}</li>
 *   <li>{@link specific.emf.cpp.CppClass#getVisibility <em>Visibility</em>}</li>
 * </ul>
 * </p>
 *
 * @see specific.emf.cpp.CppPackage#getCppClass()
 * @model
 * @generated
 */
public interface CppClass extends EObject, EClass {
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
	 * @see specific.emf.cpp.CppPackage#getCppClass_Typedefs()
	 * @model containment="true" ordered="false"
	 * @generated
	 */
	EList<CppTypedef> getTypedefs();

	/**
	 * Returns the value of the '<em><b>Nested Classes</b></em>' containment reference list.
	 * The list contents are of type {@link specific.emf.cpp.CppClass}.
	 * It is bidirectional and its opposite is '{@link specific.emf.cpp.CppClass#getParentClass <em>Parent Class</em>}'.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Nested Classes</em>' containment reference list isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Nested Classes</em>' containment reference list.
	 * @see specific.emf.cpp.CppPackage#getCppClass_NestedClasses()
	 * @see specific.emf.cpp.CppClass#getParentClass
	 * @model opposite="parentClass" containment="true"
	 * @generated
	 */
	EList<CppClass> getNestedClasses();

	/**
	 * Returns the value of the '<em><b>Parent Class</b></em>' container reference.
	 * It is bidirectional and its opposite is '{@link specific.emf.cpp.CppClass#getNestedClasses <em>Nested Classes</em>}'.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Parent Class</em>' container reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Parent Class</em>' container reference.
	 * @see #setParentClass(CppClass)
	 * @see specific.emf.cpp.CppPackage#getCppClass_ParentClass()
	 * @see specific.emf.cpp.CppClass#getNestedClasses
	 * @model opposite="nestedClasses" transient="false"
	 * @generated
	 */
	CppClass getParentClass();

	/**
	 * Sets the value of the '{@link specific.emf.cpp.CppClass#getParentClass <em>Parent Class</em>}' container reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Parent Class</em>' container reference.
	 * @see #getParentClass()
	 * @generated
	 */
	void setParentClass(CppClass value);

	/**
	 * Returns the value of the '<em><b>Namespace</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of the '<em>Namespace</em>' reference isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @return the value of the '<em>Namespace</em>' reference.
	 * @see specific.emf.cpp.CppPackage#getCppClass_Namespace()
	 * @model transient="true" changeable="false" volatile="true" derived="true"
	 * @generated
	 */
	CppNamespace getNamespace();

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
	 * @see specific.emf.cpp.CppPackage#getCppClass_Visibility()
	 * @model default="default"
	 * @generated
	 */
	CppVisibility getVisibility();

	/**
	 * Sets the value of the '{@link specific.emf.cpp.CppClass#getVisibility <em>Visibility</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @param value the new value of the '<em>Visibility</em>' attribute.
	 * @see specific.emf.cpp.CppVisibility
	 * @see #getVisibility()
	 * @generated
	 */
	void setVisibility(CppVisibility value);

} // CppClass
