/**
 */
package specific.emf.cpp.impl;

import java.util.Collection;

import org.eclipse.emf.common.notify.Notification;
import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.EClassImpl;
import org.eclipse.emf.ecore.impl.ENotificationImpl;

import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.EObjectContainmentWithInverseEList;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.emf.ecore.util.InternalEList;

import specific.emf.cpp.CppClass;
import specific.emf.cpp.CppNamespace;
import specific.emf.cpp.CppPackage;
import specific.emf.cpp.CppTypedef;
import specific.emf.cpp.CppVisibility;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Class</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link specific.emf.cpp.impl.CppClassImpl#getTypedefs <em>Typedefs</em>}</li>
 *   <li>{@link specific.emf.cpp.impl.CppClassImpl#getNestedClasses <em>Nested Classes</em>}</li>
 *   <li>{@link specific.emf.cpp.impl.CppClassImpl#getParentClass <em>Parent Class</em>}</li>
 *   <li>{@link specific.emf.cpp.impl.CppClassImpl#getNamespace <em>Namespace</em>}</li>
 *   <li>{@link specific.emf.cpp.impl.CppClassImpl#getVisibility <em>Visibility</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class CppClassImpl extends EClassImpl implements CppClass {
	/**
	 * The cached value of the '{@link #getTypedefs() <em>Typedefs</em>}' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getTypedefs()
	 * @generated
	 * @ordered
	 */
	protected EList<CppTypedef> typedefs;

	/**
	 * The cached value of the '{@link #getNestedClasses() <em>Nested Classes</em>}' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getNestedClasses()
	 * @generated
	 * @ordered
	 */
	protected EList<CppClass> nestedClasses;

	/**
	 * The default value of the '{@link #getVisibility() <em>Visibility</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getVisibility()
	 * @generated
	 * @ordered
	 */
	protected static final CppVisibility VISIBILITY_EDEFAULT = CppVisibility.DEFAULT;

	/**
	 * The cached value of the '{@link #getVisibility() <em>Visibility</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getVisibility()
	 * @generated
	 * @ordered
	 */
	protected CppVisibility visibility = VISIBILITY_EDEFAULT;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected CppClassImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return CppPackage.Literals.CPP_CLASS;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<CppTypedef> getTypedefs() {
		if (typedefs == null) {
			typedefs = new EObjectContainmentEList<CppTypedef>(CppTypedef.class, this, CppPackage.CPP_CLASS__TYPEDEFS);
		}
		return typedefs;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<CppClass> getNestedClasses() {
		if (nestedClasses == null) {
			nestedClasses = new EObjectContainmentWithInverseEList<CppClass>(CppClass.class, this, CppPackage.CPP_CLASS__NESTED_CLASSES, CppPackage.CPP_CLASS__PARENT_CLASS);
		}
		return nestedClasses;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public CppClass getParentClass() {
		if (eContainerFeatureID() != CppPackage.CPP_CLASS__PARENT_CLASS) return null;
		return (CppClass)eInternalContainer();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public NotificationChain basicSetParentClass(CppClass newParentClass, NotificationChain msgs) {
		msgs = eBasicSetContainer((InternalEObject)newParentClass, CppPackage.CPP_CLASS__PARENT_CLASS, msgs);
		return msgs;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setParentClass(CppClass newParentClass) {
		if (newParentClass != eInternalContainer() || (eContainerFeatureID() != CppPackage.CPP_CLASS__PARENT_CLASS && newParentClass != null)) {
			if (EcoreUtil.isAncestor(this, newParentClass))
				throw new IllegalArgumentException("Recursive containment not allowed for " + toString());
			NotificationChain msgs = null;
			if (eInternalContainer() != null)
				msgs = eBasicRemoveFromContainer(msgs);
			if (newParentClass != null)
				msgs = ((InternalEObject)newParentClass).eInverseAdd(this, CppPackage.CPP_CLASS__NESTED_CLASSES, CppClass.class, msgs);
			msgs = basicSetParentClass(newParentClass, msgs);
			if (msgs != null) msgs.dispatch();
		}
		else if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, CppPackage.CPP_CLASS__PARENT_CLASS, newParentClass, newParentClass));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public CppNamespace getNamespace() {
		CppNamespace namespace = basicGetNamespace();
		return namespace != null && namespace.eIsProxy() ? (CppNamespace)eResolveProxy((InternalEObject)namespace) : namespace;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated NOT
	 */
	public CppNamespace basicGetNamespace() {
		if (getParentClass() != null) 
			return getParentClass().getNamespace();
		else if (getEPackage() != null && getEPackage() instanceof CppNamespace)
			return (CppNamespace)getEPackage();
		else return null;		
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public CppVisibility getVisibility() {
		return visibility;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setVisibility(CppVisibility newVisibility) {
		CppVisibility oldVisibility = visibility;
		visibility = newVisibility == null ? VISIBILITY_EDEFAULT : newVisibility;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, CppPackage.CPP_CLASS__VISIBILITY, oldVisibility, visibility));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@SuppressWarnings("unchecked")
	@Override
	public NotificationChain eInverseAdd(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case CppPackage.CPP_CLASS__NESTED_CLASSES:
				return ((InternalEList<InternalEObject>)(InternalEList<?>)getNestedClasses()).basicAdd(otherEnd, msgs);
			case CppPackage.CPP_CLASS__PARENT_CLASS:
				if (eInternalContainer() != null)
					msgs = eBasicRemoveFromContainer(msgs);
				return basicSetParentClass((CppClass)otherEnd, msgs);
		}
		return super.eInverseAdd(otherEnd, featureID, msgs);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case CppPackage.CPP_CLASS__TYPEDEFS:
				return ((InternalEList<?>)getTypedefs()).basicRemove(otherEnd, msgs);
			case CppPackage.CPP_CLASS__NESTED_CLASSES:
				return ((InternalEList<?>)getNestedClasses()).basicRemove(otherEnd, msgs);
			case CppPackage.CPP_CLASS__PARENT_CLASS:
				return basicSetParentClass(null, msgs);
		}
		return super.eInverseRemove(otherEnd, featureID, msgs);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public NotificationChain eBasicRemoveFromContainerFeature(NotificationChain msgs) {
		switch (eContainerFeatureID()) {
			case CppPackage.CPP_CLASS__PARENT_CLASS:
				return eInternalContainer().eInverseRemove(this, CppPackage.CPP_CLASS__NESTED_CLASSES, CppClass.class, msgs);
		}
		return super.eBasicRemoveFromContainerFeature(msgs);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case CppPackage.CPP_CLASS__TYPEDEFS:
				return getTypedefs();
			case CppPackage.CPP_CLASS__NESTED_CLASSES:
				return getNestedClasses();
			case CppPackage.CPP_CLASS__PARENT_CLASS:
				return getParentClass();
			case CppPackage.CPP_CLASS__NAMESPACE:
				if (resolve) return getNamespace();
				return basicGetNamespace();
			case CppPackage.CPP_CLASS__VISIBILITY:
				return getVisibility();
		}
		return super.eGet(featureID, resolve, coreType);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@SuppressWarnings("unchecked")
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case CppPackage.CPP_CLASS__TYPEDEFS:
				getTypedefs().clear();
				getTypedefs().addAll((Collection<? extends CppTypedef>)newValue);
				return;
			case CppPackage.CPP_CLASS__NESTED_CLASSES:
				getNestedClasses().clear();
				getNestedClasses().addAll((Collection<? extends CppClass>)newValue);
				return;
			case CppPackage.CPP_CLASS__PARENT_CLASS:
				setParentClass((CppClass)newValue);
				return;
			case CppPackage.CPP_CLASS__VISIBILITY:
				setVisibility((CppVisibility)newValue);
				return;
		}
		super.eSet(featureID, newValue);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void eUnset(int featureID) {
		switch (featureID) {
			case CppPackage.CPP_CLASS__TYPEDEFS:
				getTypedefs().clear();
				return;
			case CppPackage.CPP_CLASS__NESTED_CLASSES:
				getNestedClasses().clear();
				return;
			case CppPackage.CPP_CLASS__PARENT_CLASS:
				setParentClass((CppClass)null);
				return;
			case CppPackage.CPP_CLASS__VISIBILITY:
				setVisibility(VISIBILITY_EDEFAULT);
				return;
		}
		super.eUnset(featureID);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public boolean eIsSet(int featureID) {
		switch (featureID) {
			case CppPackage.CPP_CLASS__TYPEDEFS:
				return typedefs != null && !typedefs.isEmpty();
			case CppPackage.CPP_CLASS__NESTED_CLASSES:
				return nestedClasses != null && !nestedClasses.isEmpty();
			case CppPackage.CPP_CLASS__PARENT_CLASS:
				return getParentClass() != null;
			case CppPackage.CPP_CLASS__NAMESPACE:
				return basicGetNamespace() != null;
			case CppPackage.CPP_CLASS__VISIBILITY:
				return visibility != VISIBILITY_EDEFAULT;
		}
		return super.eIsSet(featureID);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public String toString() {
		if (eIsProxy()) return super.toString();

		StringBuffer result = new StringBuffer(super.toString());
		result.append(" (visibility: ");
		result.append(visibility);
		result.append(')');
		return result.toString();
	}

} //CppClassImpl
