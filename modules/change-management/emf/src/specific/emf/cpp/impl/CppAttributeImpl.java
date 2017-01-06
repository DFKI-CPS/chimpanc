/**
 */
package specific.emf.cpp.impl;

import org.eclipse.emf.common.notify.Notification;

import org.eclipse.emf.ecore.EClass;

import org.eclipse.emf.ecore.InternalEObject;
import org.eclipse.emf.ecore.impl.EAttributeImpl;
import org.eclipse.emf.ecore.impl.ENotificationImpl;

import specific.emf.cpp.CppAttribute;
import specific.emf.cpp.CppPackage;
import specific.emf.cpp.CppTypedef;
import specific.emf.cpp.CppVisibility;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Attribute</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link specific.emf.cpp.impl.CppAttributeImpl#getVisibility <em>Visibility</em>}</li>
 *   <li>{@link specific.emf.cpp.impl.CppAttributeImpl#isStatic <em>Static</em>}</li>
 *   <li>{@link specific.emf.cpp.impl.CppAttributeImpl#getTypedef <em>Typedef</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class CppAttributeImpl extends EAttributeImpl implements CppAttribute {
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
	 * The default value of the '{@link #isStatic() <em>Static</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isStatic()
	 * @generated
	 * @ordered
	 */
	protected static final boolean STATIC_EDEFAULT = false;

	/**
	 * The cached value of the '{@link #isStatic() <em>Static</em>}' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #isStatic()
	 * @generated
	 * @ordered
	 */
	protected boolean static_ = STATIC_EDEFAULT;

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
	protected CppAttributeImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return CppPackage.Literals.CPP_ATTRIBUTE;
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
			eNotify(new ENotificationImpl(this, Notification.SET, CppPackage.CPP_ATTRIBUTE__VISIBILITY, oldVisibility, visibility));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public boolean isStatic() {
		return static_;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void setStatic(boolean newStatic) {
		boolean oldStatic = static_;
		static_ = newStatic;
		if (eNotificationRequired())
			eNotify(new ENotificationImpl(this, Notification.SET, CppPackage.CPP_ATTRIBUTE__STATIC, oldStatic, static_));
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
					eNotify(new ENotificationImpl(this, Notification.RESOLVE, CppPackage.CPP_ATTRIBUTE__TYPEDEF, oldTypedef, typedef));
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
			eNotify(new ENotificationImpl(this, Notification.SET, CppPackage.CPP_ATTRIBUTE__TYPEDEF, oldTypedef, typedef));
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case CppPackage.CPP_ATTRIBUTE__VISIBILITY:
				return getVisibility();
			case CppPackage.CPP_ATTRIBUTE__STATIC:
				return isStatic();
			case CppPackage.CPP_ATTRIBUTE__TYPEDEF:
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
			case CppPackage.CPP_ATTRIBUTE__VISIBILITY:
				setVisibility((CppVisibility)newValue);
				return;
			case CppPackage.CPP_ATTRIBUTE__STATIC:
				setStatic((Boolean)newValue);
				return;
			case CppPackage.CPP_ATTRIBUTE__TYPEDEF:
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
			case CppPackage.CPP_ATTRIBUTE__VISIBILITY:
				setVisibility(VISIBILITY_EDEFAULT);
				return;
			case CppPackage.CPP_ATTRIBUTE__STATIC:
				setStatic(STATIC_EDEFAULT);
				return;
			case CppPackage.CPP_ATTRIBUTE__TYPEDEF:
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
			case CppPackage.CPP_ATTRIBUTE__VISIBILITY:
				return visibility != VISIBILITY_EDEFAULT;
			case CppPackage.CPP_ATTRIBUTE__STATIC:
				return static_ != STATIC_EDEFAULT;
			case CppPackage.CPP_ATTRIBUTE__TYPEDEF:
				return typedef != null;
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
		result.append(", static: ");
		result.append(static_);
		result.append(')');
		return result.toString();
	}

} //CppAttributeImpl
