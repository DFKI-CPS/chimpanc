/**
 */
package specific.emf.cpp.impl;

import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EDataType;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;

import org.eclipse.emf.ecore.impl.EFactoryImpl;

import org.eclipse.emf.ecore.plugin.EcorePlugin;

import specific.emf.cpp.*;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model <b>Factory</b>.
 * <!-- end-user-doc -->
 * @generated
 */
public class CppFactoryImpl extends EFactoryImpl implements CppFactory {
	/**
	 * Creates the default factory implementation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static CppFactory init() {
		try {
			CppFactory theCppFactory = (CppFactory)EPackage.Registry.INSTANCE.getEFactory(CppPackage.eNS_URI);
			if (theCppFactory != null) {
				return theCppFactory;
			}
		}
		catch (Exception exception) {
			EcorePlugin.INSTANCE.log(exception);
		}
		return new CppFactoryImpl();
	}

	/**
	 * Creates an instance of the factory.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public CppFactoryImpl() {
		super();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public EObject create(EClass eClass) {
		switch (eClass.getClassifierID()) {
			case CppPackage.CPP_NAMESPACE: return createCppNamespace();
			case CppPackage.CPP_CLASS: return createCppClass();
			case CppPackage.CPP_ATTRIBUTE: return createCppAttribute();
			case CppPackage.CPP_OPERATION: return createCppOperation();
			case CppPackage.CPP_TYPEDEF: return createCppTypedef();
			case CppPackage.CPP_PARAMETER: return createCppParameter();
			case CppPackage.CPP_TYPEDEF_REF: return createCppTypedefRef();
			default:
				throw new IllegalArgumentException("The class '" + eClass.getName() + "' is not a valid classifier");
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public Object createFromString(EDataType eDataType, String initialValue) {
		switch (eDataType.getClassifierID()) {
			case CppPackage.CPP_VISIBILITY:
				return createCppVisibilityFromString(eDataType, initialValue);
			default:
				throw new IllegalArgumentException("The datatype '" + eDataType.getName() + "' is not a valid classifier");
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	@Override
	public String convertToString(EDataType eDataType, Object instanceValue) {
		switch (eDataType.getClassifierID()) {
			case CppPackage.CPP_VISIBILITY:
				return convertCppVisibilityToString(eDataType, instanceValue);
			default:
				throw new IllegalArgumentException("The datatype '" + eDataType.getName() + "' is not a valid classifier");
		}
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public CppNamespace createCppNamespace() {
		CppNamespaceImpl cppNamespace = new CppNamespaceImpl();
		return cppNamespace;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public CppClass createCppClass() {
		CppClassImpl cppClass = new CppClassImpl();
		return cppClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public CppAttribute createCppAttribute() {
		CppAttributeImpl cppAttribute = new CppAttributeImpl();
		return cppAttribute;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public CppOperation createCppOperation() {
		CppOperationImpl cppOperation = new CppOperationImpl();
		return cppOperation;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public CppTypedef createCppTypedef() {
		CppTypedefImpl cppTypedef = new CppTypedefImpl();
		return cppTypedef;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public CppParameter createCppParameter() {
		CppParameterImpl cppParameter = new CppParameterImpl();
		return cppParameter;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public CppTypedefRef createCppTypedefRef() {
		CppTypedefRefImpl cppTypedefRef = new CppTypedefRefImpl();
		return cppTypedefRef;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public CppVisibility createCppVisibilityFromString(EDataType eDataType, String initialValue) {
		CppVisibility result = CppVisibility.get(initialValue);
		if (result == null) throw new IllegalArgumentException("The value '" + initialValue + "' is not a valid enumerator of '" + eDataType.getName() + "'");
		return result;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public String convertCppVisibilityToString(EDataType eDataType, Object instanceValue) {
		return instanceValue == null ? null : instanceValue.toString();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public CppPackage getCppPackage() {
		return (CppPackage)getEPackage();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @deprecated
	 * @generated
	 */
	@Deprecated
	public static CppPackage getPackage() {
		return CppPackage.eINSTANCE;
	}

} //CppFactoryImpl
