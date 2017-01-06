/**
 */
package specific.emf.cpp.impl;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.EGenericTypeImpl;
import org.eclipse.emf.ecore.impl.ENotificationImpl;

import specific.emf.cpp.CppPackage;
import specific.emf.cpp.CppTypedef;
import specific.emf.cpp.CppTypedefRef;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Typedef Ref</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link specific.emf.cpp.impl.CppTypedefRefImpl#getTypedef <em>Typedef</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class CppTypedefRefImpl extends EGenericTypeImpl implements CppTypedefRef {
	/**
	 * The cached value of the '{@link #getTypedef() <em>Typedef</em>}' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getTypedef()
	 * @generated
	 * @ordered
	 */
	protected CppTypedef typedef;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected CppTypedefRefImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return CppPackage.Literals.CPP_TYPEDEF_REF;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public CppTypedef getTypedef() {
		if (typedef != null && typedef.eIsProxy()) {
			InternalEObject oldTypedef = (InternalEObject)typedef;
			typedef = (CppTypedef)eResolveProxy(oldTypedef);
			if (typedef != oldTypedef) {
				if (eNotificationRequired())
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, CppPackage.CPP_TYPEDEF_REF__TYPEDEF, oldTypedef, typedef));
			}
		}
		return typedef;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public CppTypedef basicGetTypedef() {
		return typedef;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setTypedef(CppTypedef newTypedef) {
		CppTypedef oldTypedef = typedef;
		typedef = newTypedef;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, CppPackage.CPP_TYPEDEF_REF__TYPEDEF, oldTypedef, typedef));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case CppPackage.CPP_TYPEDEF_REF__TYPEDEF:
				if (resolve) return getTypedef();
				return basicGetTypedef();
		}
		return super.eGet(featureID, resolve, coreType);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public void eSet(int featureID, Object newValue) {
		switch (featureID) {
			case CppPackage.CPP_TYPEDEF_REF__TYPEDEF:
				setTypedef((CppTypedef)newValue);
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
			case CppPackage.CPP_TYPEDEF_REF__TYPEDEF:
				setTypedef((CppTypedef)null);
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
			case CppPackage.CPP_TYPEDEF_REF__TYPEDEF:
				return typedef != null;
		}
		return super.eIsSet(featureID);
	}

} //CppTypedefRefImpl
