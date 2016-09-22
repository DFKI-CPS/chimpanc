/**
 */
package specific.emf.cpp.impl;

import java.util.Collection;

import org.eclipse.emf.common.notify.NotificationChain;

import org.eclipse.emf.common.util.EList;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.InternalEObject;

import org.eclipse.emf.ecore.impl.EPackageImpl;

import org.eclipse.emf.ecore.util.EObjectContainmentEList;
import org.eclipse.emf.ecore.util.InternalEList;

import specific.emf.cpp.CppAttribute;
import specific.emf.cpp.CppNamespace;
import specific.emf.cpp.CppOperation;
import specific.emf.cpp.CppPackage;
import specific.emf.cpp.CppTypedef;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model object '<em><b>Namespace</b></em>'.
 * <!-- end-user-doc -->
 * <p>
 * The following features are implemented:
 * <ul>
 *   <li>{@link specific.emf.cpp.impl.CppNamespaceImpl#getTypedefs <em>Typedefs</em>}</li>
 *   <li>{@link specific.emf.cpp.impl.CppNamespaceImpl#getOperations <em>Operations</em>}</li>
 *   <li>{@link specific.emf.cpp.impl.CppNamespaceImpl#getVariables <em>Variables</em>}</li>
 * </ul>
 * </p>
 *
 * @generated
 */
public class CppNamespaceImpl extends EPackageImpl implements CppNamespace {
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
	 * The cached value of the '{@link #getOperations() <em>Operations</em>}' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getOperations()
	 * @generated
	 * @ordered
	 */
	protected EList<CppOperation> operations;

	/**
	 * The cached value of the '{@link #getVariables() <em>Variables</em>}' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #getVariables()
	 * @generated
	 * @ordered
	 */
	protected EList<CppAttribute> variables;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected CppNamespaceImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return CppPackage.Literals.CPP_NAMESPACE;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<CppTypedef> getTypedefs() {
		if (typedefs == null) {
			typedefs = new EObjectContainmentEList<CppTypedef>(CppTypedef.class, this, CppPackage.CPP_NAMESPACE__TYPEDEFS);
		}
		return typedefs;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<CppOperation> getOperations() {
		if (operations == null) {
			operations = new EObjectContainmentEList<CppOperation>(CppOperation.class, this, CppPackage.CPP_NAMESPACE__OPERATIONS);
		}
		return operations;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EList<CppAttribute> getVariables() {
		if (variables == null) {
			variables = new EObjectContainmentEList<CppAttribute>(CppAttribute.class, this, CppPackage.CPP_NAMESPACE__VARIABLES);
		}
		return variables;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public NotificationChain eInverseRemove(InternalEObject otherEnd, int featureID, NotificationChain msgs) {
		switch (featureID) {
			case CppPackage.CPP_NAMESPACE__TYPEDEFS:
				return ((InternalEList<?>)getTypedefs()).basicRemove(otherEnd, msgs);
			case CppPackage.CPP_NAMESPACE__OPERATIONS:
				return ((InternalEList<?>)getOperations()).basicRemove(otherEnd, msgs);
			case CppPackage.CPP_NAMESPACE__VARIABLES:
				return ((InternalEList<?>)getVariables()).basicRemove(otherEnd, msgs);
		}
		return super.eInverseRemove(otherEnd, featureID, msgs);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object eGet(int featureID, boolean resolve, boolean coreType) {
		switch (featureID) {
			case CppPackage.CPP_NAMESPACE__TYPEDEFS:
				return getTypedefs();
			case CppPackage.CPP_NAMESPACE__OPERATIONS:
				return getOperations();
			case CppPackage.CPP_NAMESPACE__VARIABLES:
				return getVariables();
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
			case CppPackage.CPP_NAMESPACE__TYPEDEFS:
				getTypedefs().clear();
				getTypedefs().addAll((Collection<? extends CppTypedef>)newValue);
				return;
			case CppPackage.CPP_NAMESPACE__OPERATIONS:
				getOperations().clear();
				getOperations().addAll((Collection<? extends CppOperation>)newValue);
				return;
			case CppPackage.CPP_NAMESPACE__VARIABLES:
				getVariables().clear();
				getVariables().addAll((Collection<? extends CppAttribute>)newValue);
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
			case CppPackage.CPP_NAMESPACE__TYPEDEFS:
				getTypedefs().clear();
				return;
			case CppPackage.CPP_NAMESPACE__OPERATIONS:
				getOperations().clear();
				return;
			case CppPackage.CPP_NAMESPACE__VARIABLES:
				getVariables().clear();
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
			case CppPackage.CPP_NAMESPACE__TYPEDEFS:
				return typedefs != null && !typedefs.isEmpty();
			case CppPackage.CPP_NAMESPACE__OPERATIONS:
				return operations != null && !operations.isEmpty();
			case CppPackage.CPP_NAMESPACE__VARIABLES:
				return variables != null && !variables.isEmpty();
		}
		return super.eIsSet(featureID);
	}

} //CppNamespaceImpl
