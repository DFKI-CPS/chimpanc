/**
 */
package specific.emf.cpp.impl;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EEnum;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.impl.EPackageImpl;
import specific.emf.cpp.CppAttribute;
import specific.emf.cpp.CppClass;
import specific.emf.cpp.CppFactory;
import specific.emf.cpp.CppNamespace;
import specific.emf.cpp.CppOperation;
import specific.emf.cpp.CppPackage;
import specific.emf.cpp.CppParameter;
import specific.emf.cpp.CppTypedef;
import specific.emf.cpp.CppTypedefRef;
import specific.emf.cpp.CppVisibility;

/**
 * <!-- begin-user-doc -->
 * An implementation of the model <b>Package</b>.
 * <!-- end-user-doc -->
 * @generated
 */
public class CppPackageImpl extends EPackageImpl implements CppPackage {
	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass cppNamespaceEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass cppClassEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass cppAttributeEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass cppOperationEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass cppTypedefEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass cppParameterEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EClass cppTypedefRefEClass = null;

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private EEnum cppVisibilityEEnum = null;

	/**
	 * Creates an instance of the model <b>Package</b>, registered with
	 * {@link org.eclipse.emf.ecore.EPackage.Registry EPackage.Registry} by the package
	 * package URI value.
	 * <p>Note: the correct way to create the package is via the static
	 * factory method {@link #init init()}, which also performs
	 * initialization of the package, or returns the registered package,
	 * if one already exists.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see org.eclipse.emf.ecore.EPackage.Registry
	 * @see specific.emf.cpp.CppPackage#eNS_URI
	 * @see #init()
	 * @generated
	 */
	private CppPackageImpl() {
		super(eNS_URI, CppFactory.eINSTANCE);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private static boolean isInited = false;

	/**
	 * Creates, registers, and initializes the <b>Package</b> for this model, and for any others upon which it depends.
	 * 
	 * <p>This method is used to initialize {@link CppPackage#eINSTANCE} when that field is accessed.
	 * Clients should not invoke it directly. Instead, they should simply access that field to obtain the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #eNS_URI
	 * @see #createPackageContents()
	 * @see #initializePackageContents()
	 * @generated
	 */
	public static CppPackage init() {
		if (isInited) return (CppPackage)EPackage.Registry.INSTANCE.getEPackage(CppPackage.eNS_URI);

		// Obtain or create and register package
		CppPackageImpl theCppPackage = (CppPackageImpl)(EPackage.Registry.INSTANCE.get(eNS_URI) instanceof CppPackageImpl ? EPackage.Registry.INSTANCE.get(eNS_URI) : new CppPackageImpl());

		isInited = true;

		// Create package meta-data objects
		theCppPackage.createPackageContents();

		// Initialize created meta-data
		theCppPackage.initializePackageContents();

		// Mark meta-data to indicate it can't be changed
		theCppPackage.freeze();

  
		// Update the registry and return the package
		EPackage.Registry.INSTANCE.put(CppPackage.eNS_URI, theCppPackage);
		return theCppPackage;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getCppNamespace() {
		return cppNamespaceEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getCppNamespace_Typedefs() {
		return (EReference)cppNamespaceEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getCppNamespace_Operations() {
		return (EReference)cppNamespaceEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getCppNamespace_Variables() {
		return (EReference)cppNamespaceEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getCppClass() {
		return cppClassEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getCppClass_Typedefs() {
		return (EReference)cppClassEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getCppClass_NestedClasses() {
		return (EReference)cppClassEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getCppClass_ParentClass() {
		return (EReference)cppClassEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getCppClass_Namespace() {
		return (EReference)cppClassEClass.getEStructuralFeatures().get(3);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getCppClass_Visibility() {
		return (EAttribute)cppClassEClass.getEStructuralFeatures().get(4);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getCppAttribute() {
		return cppAttributeEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getCppAttribute_Visibility() {
		return (EAttribute)cppAttributeEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getCppAttribute_Static() {
		return (EAttribute)cppAttributeEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getCppAttribute_Typedef() {
		return (EReference)cppAttributeEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getCppOperation() {
		return cppOperationEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getCppOperation_Visibility() {
		return (EAttribute)cppOperationEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getCppOperation_Static() {
		return (EAttribute)cppOperationEClass.getEStructuralFeatures().get(1);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getCppOperation_Typedef() {
		return (EReference)cppOperationEClass.getEStructuralFeatures().get(2);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getCppTypedef() {
		return cppTypedefEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EAttribute getCppTypedef_Visibility() {
		return (EAttribute)cppTypedefEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getCppParameter() {
		return cppParameterEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getCppParameter_Typedef() {
		return (EReference)cppParameterEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EClass getCppTypedefRef() {
		return cppTypedefRefEClass;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EReference getCppTypedefRef_Typedef() {
		return (EReference)cppTypedefRefEClass.getEStructuralFeatures().get(0);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public EEnum getCppVisibility() {
		return cppVisibilityEEnum;
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public CppFactory getCppFactory() {
		return (CppFactory)getEFactoryInstance();
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private boolean isCreated = false;

	/**
	 * Creates the meta-model objects for the package.  This method is
	 * guarded to have no affect on any invocation but its first.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void createPackageContents() {
		if (isCreated) return;
		isCreated = true;

		// Create classes and their features
		cppNamespaceEClass = createEClass(CPP_NAMESPACE);
		createEReference(cppNamespaceEClass, CPP_NAMESPACE__TYPEDEFS);
		createEReference(cppNamespaceEClass, CPP_NAMESPACE__OPERATIONS);
		createEReference(cppNamespaceEClass, CPP_NAMESPACE__VARIABLES);

		cppClassEClass = createEClass(CPP_CLASS);
		createEReference(cppClassEClass, CPP_CLASS__TYPEDEFS);
		createEReference(cppClassEClass, CPP_CLASS__NESTED_CLASSES);
		createEReference(cppClassEClass, CPP_CLASS__PARENT_CLASS);
		createEReference(cppClassEClass, CPP_CLASS__NAMESPACE);
		createEAttribute(cppClassEClass, CPP_CLASS__VISIBILITY);

		cppAttributeEClass = createEClass(CPP_ATTRIBUTE);
		createEAttribute(cppAttributeEClass, CPP_ATTRIBUTE__VISIBILITY);
		createEAttribute(cppAttributeEClass, CPP_ATTRIBUTE__STATIC);
		createEReference(cppAttributeEClass, CPP_ATTRIBUTE__TYPEDEF);

		cppOperationEClass = createEClass(CPP_OPERATION);
		createEAttribute(cppOperationEClass, CPP_OPERATION__VISIBILITY);
		createEAttribute(cppOperationEClass, CPP_OPERATION__STATIC);
		createEReference(cppOperationEClass, CPP_OPERATION__TYPEDEF);

		cppTypedefEClass = createEClass(CPP_TYPEDEF);
		createEAttribute(cppTypedefEClass, CPP_TYPEDEF__VISIBILITY);

		cppParameterEClass = createEClass(CPP_PARAMETER);
		createEReference(cppParameterEClass, CPP_PARAMETER__TYPEDEF);

		cppTypedefRefEClass = createEClass(CPP_TYPEDEF_REF);
		createEReference(cppTypedefRefEClass, CPP_TYPEDEF_REF__TYPEDEF);

		// Create enums
		cppVisibilityEEnum = createEEnum(CPP_VISIBILITY);
	}

	/**
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private boolean isInitialized = false;

	/**
	 * Complete the initialization of the package and its meta-model.  This
	 * method is guarded to have no affect on any invocation but its first.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public void initializePackageContents() {
		if (isInitialized) return;
		isInitialized = true;

		// Initialize package
		setName(eNAME);
		setNsPrefix(eNS_PREFIX);
		setNsURI(eNS_URI);

		// Create type parameters

		// Set bounds for type parameters

		// Add supertypes to classes
		cppNamespaceEClass.getESuperTypes().add(ecorePackage.getEPackage());
		cppClassEClass.getESuperTypes().add(ecorePackage.getEClass());
		cppAttributeEClass.getESuperTypes().add(ecorePackage.getEAttribute());
		cppOperationEClass.getESuperTypes().add(ecorePackage.getEOperation());
		cppTypedefEClass.getESuperTypes().add(ecorePackage.getEGenericType());
		cppTypedefEClass.getESuperTypes().add(ecorePackage.getENamedElement());
		cppParameterEClass.getESuperTypes().add(ecorePackage.getEParameter());
		cppTypedefRefEClass.getESuperTypes().add(ecorePackage.getEGenericType());

		// Initialize classes, features, and operations; add parameters
		initEClass(cppNamespaceEClass, CppNamespace.class, "CppNamespace", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getCppNamespace_Typedefs(), this.getCppTypedef(), null, "typedefs", null, 0, -1, CppNamespace.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		getCppNamespace_Typedefs().getEKeys().add(ecorePackage.getENamedElement_Name());
		initEReference(getCppNamespace_Operations(), this.getCppOperation(), null, "operations", null, 0, -1, CppNamespace.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getCppNamespace_Variables(), this.getCppAttribute(), null, "variables", null, 0, -1, CppNamespace.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(cppClassEClass, CppClass.class, "CppClass", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getCppClass_Typedefs(), this.getCppTypedef(), null, "typedefs", null, 0, -1, CppClass.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, !IS_ORDERED);
		initEReference(getCppClass_NestedClasses(), this.getCppClass(), this.getCppClass_ParentClass(), "nestedClasses", null, 0, -1, CppClass.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getCppClass_ParentClass(), this.getCppClass(), this.getCppClass_NestedClasses(), "parentClass", null, 0, 1, CppClass.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, !IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getCppClass_Namespace(), this.getCppNamespace(), null, "namespace", null, 0, 1, CppClass.class, IS_TRANSIENT, IS_VOLATILE, !IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, IS_DERIVED, IS_ORDERED);
		initEAttribute(getCppClass_Visibility(), this.getCppVisibility(), "visibility", "default", 0, 1, CppClass.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(cppAttributeEClass, CppAttribute.class, "CppAttribute", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getCppAttribute_Visibility(), this.getCppVisibility(), "visibility", "default", 0, 1, CppAttribute.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getCppAttribute_Static(), ecorePackage.getEBoolean(), "static", null, 0, 1, CppAttribute.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getCppAttribute_Typedef(), this.getCppTypedef(), null, "typedef", null, 0, 1, CppAttribute.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(cppOperationEClass, CppOperation.class, "CppOperation", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getCppOperation_Visibility(), this.getCppVisibility(), "visibility", "default", 0, 1, CppOperation.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEAttribute(getCppOperation_Static(), ecorePackage.getEBoolean(), "static", null, 0, 1, CppOperation.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);
		initEReference(getCppOperation_Typedef(), this.getCppTypedef(), null, "typedef", null, 0, 1, CppOperation.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(cppTypedefEClass, CppTypedef.class, "CppTypedef", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEAttribute(getCppTypedef_Visibility(), this.getCppVisibility(), "visibility", "default", 0, 1, CppTypedef.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_UNSETTABLE, !IS_ID, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(cppParameterEClass, CppParameter.class, "CppParameter", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getCppParameter_Typedef(), this.getCppTypedef(), null, "typedef", null, 0, 1, CppParameter.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		initEClass(cppTypedefRefEClass, CppTypedefRef.class, "CppTypedefRef", !IS_ABSTRACT, !IS_INTERFACE, IS_GENERATED_INSTANCE_CLASS);
		initEReference(getCppTypedefRef_Typedef(), this.getCppTypedef(), null, "typedef", null, 0, 1, CppTypedefRef.class, !IS_TRANSIENT, !IS_VOLATILE, IS_CHANGEABLE, !IS_COMPOSITE, IS_RESOLVE_PROXIES, !IS_UNSETTABLE, IS_UNIQUE, !IS_DERIVED, IS_ORDERED);

		// Initialize enums and add enum literals
		initEEnum(cppVisibilityEEnum, CppVisibility.class, "CppVisibility");
		addEEnumLiteral(cppVisibilityEEnum, CppVisibility.DEFAULT);
		addEEnumLiteral(cppVisibilityEEnum, CppVisibility.PRIVATE);
		addEEnumLiteral(cppVisibilityEEnum, CppVisibility.PUBLIC);

		// Create resource
		createResource(eNS_URI);

		// Create annotations
		// http:///org/eclipse/emf/ecore/util/ExtendedMetaData
		createExtendedMetaDataAnnotations();
	}

	/**
	 * Initializes the annotations for <b>http:///org/eclipse/emf/ecore/util/ExtendedMetaData</b>.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	protected void createExtendedMetaDataAnnotations() {
		String source = "http:///org/eclipse/emf/ecore/util/ExtendedMetaData";	
		addAnnotation
		  (getCppNamespace_Typedefs(), 
		   source, 
		   new String[] {
			 "namespace", ""
		   });	
		addAnnotation
		  (getCppAttribute_Typedef(), 
		   source, 
		   new String[] {
			 "name", "typedef",
			 "namespace", ""
		   });	
		addAnnotation
		  (getCppOperation_Typedef(), 
		   source, 
		   new String[] {
			 "name", "typedef",
			 "namespace", ""
		   });	
		addAnnotation
		  (cppParameterEClass, 
		   source, 
		   new String[] {
			 "name", "CPPParameter"
		   });	
		addAnnotation
		  (getCppParameter_Typedef(), 
		   source, 
		   new String[] {
			 "name", "typedef",
			 "namespace", ""
		   });	
		addAnnotation
		  (getCppTypedefRef_Typedef(), 
		   source, 
		   new String[] {
			 "name", "typedef",
			 "namespace", ""
		   });
	}

} //CppPackageImpl
