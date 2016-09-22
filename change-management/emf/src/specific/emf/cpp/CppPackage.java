/**
 */
package specific.emf.cpp;

import org.eclipse.emf.ecore.EAttribute;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EEnum;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.EReference;
import org.eclipse.emf.ecore.EcorePackage;

/**
 * <!-- begin-user-doc -->
 * The <b>Package</b> for the model.
 * It contains accessors for the meta objects to represent
 * <ul>
 *   <li>each class,</li>
 *   <li>each feature of each class,</li>
 *   <li>each operation of each class,</li>
 *   <li>each enum,</li>
 *   <li>and each data type</li>
 * </ul>
 * <!-- end-user-doc -->
 * @see specific.emf.cpp.CppFactory
 * @model kind="package"
 * @generated
 */
public interface CppPackage extends EPackage {
	/**
	 * The package name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNAME = "cpp";

	/**
	 * The package namespace URI.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_URI = "http://www.dfki.de/SPECifIC/emf/2015/cpp";

	/**
	 * The package namespace name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	String eNS_PREFIX = "cpp";

	/**
	 * The singleton instance of the package.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	CppPackage eINSTANCE = specific.emf.cpp.impl.CppPackageImpl.init();

	/**
	 * The meta object id for the '{@link specific.emf.cpp.impl.CppNamespaceImpl <em>Namespace</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see specific.emf.cpp.impl.CppNamespaceImpl
	 * @see specific.emf.cpp.impl.CppPackageImpl#getCppNamespace()
	 * @generated
	 */
	int CPP_NAMESPACE = 0;

	/**
	 * The feature id for the '<em><b>EAnnotations</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_NAMESPACE__EANNOTATIONS = EcorePackage.EPACKAGE__EANNOTATIONS;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_NAMESPACE__NAME = EcorePackage.EPACKAGE__NAME;

	/**
	 * The feature id for the '<em><b>Ns URI</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_NAMESPACE__NS_URI = EcorePackage.EPACKAGE__NS_URI;

	/**
	 * The feature id for the '<em><b>Ns Prefix</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_NAMESPACE__NS_PREFIX = EcorePackage.EPACKAGE__NS_PREFIX;

	/**
	 * The feature id for the '<em><b>EFactory Instance</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_NAMESPACE__EFACTORY_INSTANCE = EcorePackage.EPACKAGE__EFACTORY_INSTANCE;

	/**
	 * The feature id for the '<em><b>EClassifiers</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_NAMESPACE__ECLASSIFIERS = EcorePackage.EPACKAGE__ECLASSIFIERS;

	/**
	 * The feature id for the '<em><b>ESubpackages</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_NAMESPACE__ESUBPACKAGES = EcorePackage.EPACKAGE__ESUBPACKAGES;

	/**
	 * The feature id for the '<em><b>ESuper Package</b></em>' container reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_NAMESPACE__ESUPER_PACKAGE = EcorePackage.EPACKAGE__ESUPER_PACKAGE;

	/**
	 * The feature id for the '<em><b>Typedefs</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_NAMESPACE__TYPEDEFS = EcorePackage.EPACKAGE_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Operations</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_NAMESPACE__OPERATIONS = EcorePackage.EPACKAGE_FEATURE_COUNT + 1;

	/**
	 * The feature id for the '<em><b>Variables</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_NAMESPACE__VARIABLES = EcorePackage.EPACKAGE_FEATURE_COUNT + 2;

	/**
	 * The number of structural features of the '<em>Namespace</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_NAMESPACE_FEATURE_COUNT = EcorePackage.EPACKAGE_FEATURE_COUNT + 3;

	/**
	 * The operation id for the '<em>Get EAnnotation</em>' operation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_NAMESPACE___GET_EANNOTATION__STRING = EcorePackage.EPACKAGE___GET_EANNOTATION__STRING;

	/**
	 * The operation id for the '<em>Get EClassifier</em>' operation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_NAMESPACE___GET_ECLASSIFIER__STRING = EcorePackage.EPACKAGE___GET_ECLASSIFIER__STRING;

	/**
	 * The number of operations of the '<em>Namespace</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_NAMESPACE_OPERATION_COUNT = EcorePackage.EPACKAGE_OPERATION_COUNT + 0;

	/**
	 * The meta object id for the '{@link specific.emf.cpp.impl.CppClassImpl <em>Class</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see specific.emf.cpp.impl.CppClassImpl
	 * @see specific.emf.cpp.impl.CppPackageImpl#getCppClass()
	 * @generated
	 */
	int CPP_CLASS = 1;

	/**
	 * The feature id for the '<em><b>EAnnotations</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_CLASS__EANNOTATIONS = EcorePackage.ECLASS__EANNOTATIONS;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_CLASS__NAME = EcorePackage.ECLASS__NAME;

	/**
	 * The feature id for the '<em><b>Instance Class Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_CLASS__INSTANCE_CLASS_NAME = EcorePackage.ECLASS__INSTANCE_CLASS_NAME;

	/**
	 * The feature id for the '<em><b>Instance Class</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_CLASS__INSTANCE_CLASS = EcorePackage.ECLASS__INSTANCE_CLASS;

	/**
	 * The feature id for the '<em><b>Default Value</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_CLASS__DEFAULT_VALUE = EcorePackage.ECLASS__DEFAULT_VALUE;

	/**
	 * The feature id for the '<em><b>Instance Type Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_CLASS__INSTANCE_TYPE_NAME = EcorePackage.ECLASS__INSTANCE_TYPE_NAME;

	/**
	 * The feature id for the '<em><b>EPackage</b></em>' container reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_CLASS__EPACKAGE = EcorePackage.ECLASS__EPACKAGE;

	/**
	 * The feature id for the '<em><b>EType Parameters</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_CLASS__ETYPE_PARAMETERS = EcorePackage.ECLASS__ETYPE_PARAMETERS;

	/**
	 * The feature id for the '<em><b>Abstract</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_CLASS__ABSTRACT = EcorePackage.ECLASS__ABSTRACT;

	/**
	 * The feature id for the '<em><b>Interface</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_CLASS__INTERFACE = EcorePackage.ECLASS__INTERFACE;

	/**
	 * The feature id for the '<em><b>ESuper Types</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_CLASS__ESUPER_TYPES = EcorePackage.ECLASS__ESUPER_TYPES;

	/**
	 * The feature id for the '<em><b>EOperations</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_CLASS__EOPERATIONS = EcorePackage.ECLASS__EOPERATIONS;

	/**
	 * The feature id for the '<em><b>EAll Attributes</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_CLASS__EALL_ATTRIBUTES = EcorePackage.ECLASS__EALL_ATTRIBUTES;

	/**
	 * The feature id for the '<em><b>EAll References</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_CLASS__EALL_REFERENCES = EcorePackage.ECLASS__EALL_REFERENCES;

	/**
	 * The feature id for the '<em><b>EReferences</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_CLASS__EREFERENCES = EcorePackage.ECLASS__EREFERENCES;

	/**
	 * The feature id for the '<em><b>EAttributes</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_CLASS__EATTRIBUTES = EcorePackage.ECLASS__EATTRIBUTES;

	/**
	 * The feature id for the '<em><b>EAll Containments</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_CLASS__EALL_CONTAINMENTS = EcorePackage.ECLASS__EALL_CONTAINMENTS;

	/**
	 * The feature id for the '<em><b>EAll Operations</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_CLASS__EALL_OPERATIONS = EcorePackage.ECLASS__EALL_OPERATIONS;

	/**
	 * The feature id for the '<em><b>EAll Structural Features</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_CLASS__EALL_STRUCTURAL_FEATURES = EcorePackage.ECLASS__EALL_STRUCTURAL_FEATURES;

	/**
	 * The feature id for the '<em><b>EAll Super Types</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_CLASS__EALL_SUPER_TYPES = EcorePackage.ECLASS__EALL_SUPER_TYPES;

	/**
	 * The feature id for the '<em><b>EID Attribute</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_CLASS__EID_ATTRIBUTE = EcorePackage.ECLASS__EID_ATTRIBUTE;

	/**
	 * The feature id for the '<em><b>EStructural Features</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_CLASS__ESTRUCTURAL_FEATURES = EcorePackage.ECLASS__ESTRUCTURAL_FEATURES;

	/**
	 * The feature id for the '<em><b>EGeneric Super Types</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_CLASS__EGENERIC_SUPER_TYPES = EcorePackage.ECLASS__EGENERIC_SUPER_TYPES;

	/**
	 * The feature id for the '<em><b>EAll Generic Super Types</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_CLASS__EALL_GENERIC_SUPER_TYPES = EcorePackage.ECLASS__EALL_GENERIC_SUPER_TYPES;

	/**
	 * The feature id for the '<em><b>Typedefs</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_CLASS__TYPEDEFS = EcorePackage.ECLASS_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Nested Classes</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_CLASS__NESTED_CLASSES = EcorePackage.ECLASS_FEATURE_COUNT + 1;

	/**
	 * The feature id for the '<em><b>Parent Class</b></em>' container reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_CLASS__PARENT_CLASS = EcorePackage.ECLASS_FEATURE_COUNT + 2;

	/**
	 * The feature id for the '<em><b>Namespace</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_CLASS__NAMESPACE = EcorePackage.ECLASS_FEATURE_COUNT + 3;

	/**
	 * The feature id for the '<em><b>Visibility</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_CLASS__VISIBILITY = EcorePackage.ECLASS_FEATURE_COUNT + 4;

	/**
	 * The number of structural features of the '<em>Class</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_CLASS_FEATURE_COUNT = EcorePackage.ECLASS_FEATURE_COUNT + 5;

	/**
	 * The operation id for the '<em>Get EAnnotation</em>' operation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_CLASS___GET_EANNOTATION__STRING = EcorePackage.ECLASS___GET_EANNOTATION__STRING;

	/**
	 * The operation id for the '<em>Is Instance</em>' operation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_CLASS___IS_INSTANCE__OBJECT = EcorePackage.ECLASS___IS_INSTANCE__OBJECT;

	/**
	 * The operation id for the '<em>Get Classifier ID</em>' operation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_CLASS___GET_CLASSIFIER_ID = EcorePackage.ECLASS___GET_CLASSIFIER_ID;

	/**
	 * The operation id for the '<em>Is Super Type Of</em>' operation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_CLASS___IS_SUPER_TYPE_OF__ECLASS = EcorePackage.ECLASS___IS_SUPER_TYPE_OF__ECLASS;

	/**
	 * The operation id for the '<em>Get Feature Count</em>' operation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_CLASS___GET_FEATURE_COUNT = EcorePackage.ECLASS___GET_FEATURE_COUNT;

	/**
	 * The operation id for the '<em>Get EStructural Feature</em>' operation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_CLASS___GET_ESTRUCTURAL_FEATURE__INT = EcorePackage.ECLASS___GET_ESTRUCTURAL_FEATURE__INT;

	/**
	 * The operation id for the '<em>Get Feature ID</em>' operation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_CLASS___GET_FEATURE_ID__ESTRUCTURALFEATURE = EcorePackage.ECLASS___GET_FEATURE_ID__ESTRUCTURALFEATURE;

	/**
	 * The operation id for the '<em>Get EStructural Feature</em>' operation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_CLASS___GET_ESTRUCTURAL_FEATURE__STRING = EcorePackage.ECLASS___GET_ESTRUCTURAL_FEATURE__STRING;

	/**
	 * The operation id for the '<em>Get Operation Count</em>' operation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_CLASS___GET_OPERATION_COUNT = EcorePackage.ECLASS___GET_OPERATION_COUNT;

	/**
	 * The operation id for the '<em>Get EOperation</em>' operation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_CLASS___GET_EOPERATION__INT = EcorePackage.ECLASS___GET_EOPERATION__INT;

	/**
	 * The operation id for the '<em>Get Operation ID</em>' operation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_CLASS___GET_OPERATION_ID__EOPERATION = EcorePackage.ECLASS___GET_OPERATION_ID__EOPERATION;

	/**
	 * The operation id for the '<em>Get Override</em>' operation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_CLASS___GET_OVERRIDE__EOPERATION = EcorePackage.ECLASS___GET_OVERRIDE__EOPERATION;

	/**
	 * The operation id for the '<em>Get Feature Type</em>' operation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_CLASS___GET_FEATURE_TYPE__ESTRUCTURALFEATURE = EcorePackage.ECLASS___GET_FEATURE_TYPE__ESTRUCTURALFEATURE;

	/**
	 * The number of operations of the '<em>Class</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_CLASS_OPERATION_COUNT = EcorePackage.ECLASS_OPERATION_COUNT + 0;

	/**
	 * The meta object id for the '{@link specific.emf.cpp.impl.CppAttributeImpl <em>Attribute</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see specific.emf.cpp.impl.CppAttributeImpl
	 * @see specific.emf.cpp.impl.CppPackageImpl#getCppAttribute()
	 * @generated
	 */
	int CPP_ATTRIBUTE = 2;

	/**
	 * The feature id for the '<em><b>EAnnotations</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_ATTRIBUTE__EANNOTATIONS = EcorePackage.EATTRIBUTE__EANNOTATIONS;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_ATTRIBUTE__NAME = EcorePackage.EATTRIBUTE__NAME;

	/**
	 * The feature id for the '<em><b>Ordered</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_ATTRIBUTE__ORDERED = EcorePackage.EATTRIBUTE__ORDERED;

	/**
	 * The feature id for the '<em><b>Unique</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_ATTRIBUTE__UNIQUE = EcorePackage.EATTRIBUTE__UNIQUE;

	/**
	 * The feature id for the '<em><b>Lower Bound</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_ATTRIBUTE__LOWER_BOUND = EcorePackage.EATTRIBUTE__LOWER_BOUND;

	/**
	 * The feature id for the '<em><b>Upper Bound</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_ATTRIBUTE__UPPER_BOUND = EcorePackage.EATTRIBUTE__UPPER_BOUND;

	/**
	 * The feature id for the '<em><b>Many</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_ATTRIBUTE__MANY = EcorePackage.EATTRIBUTE__MANY;

	/**
	 * The feature id for the '<em><b>Required</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_ATTRIBUTE__REQUIRED = EcorePackage.EATTRIBUTE__REQUIRED;

	/**
	 * The feature id for the '<em><b>EType</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_ATTRIBUTE__ETYPE = EcorePackage.EATTRIBUTE__ETYPE;

	/**
	 * The feature id for the '<em><b>EGeneric Type</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_ATTRIBUTE__EGENERIC_TYPE = EcorePackage.EATTRIBUTE__EGENERIC_TYPE;

	/**
	 * The feature id for the '<em><b>Changeable</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_ATTRIBUTE__CHANGEABLE = EcorePackage.EATTRIBUTE__CHANGEABLE;

	/**
	 * The feature id for the '<em><b>Volatile</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_ATTRIBUTE__VOLATILE = EcorePackage.EATTRIBUTE__VOLATILE;

	/**
	 * The feature id for the '<em><b>Transient</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_ATTRIBUTE__TRANSIENT = EcorePackage.EATTRIBUTE__TRANSIENT;

	/**
	 * The feature id for the '<em><b>Default Value Literal</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_ATTRIBUTE__DEFAULT_VALUE_LITERAL = EcorePackage.EATTRIBUTE__DEFAULT_VALUE_LITERAL;

	/**
	 * The feature id for the '<em><b>Default Value</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_ATTRIBUTE__DEFAULT_VALUE = EcorePackage.EATTRIBUTE__DEFAULT_VALUE;

	/**
	 * The feature id for the '<em><b>Unsettable</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_ATTRIBUTE__UNSETTABLE = EcorePackage.EATTRIBUTE__UNSETTABLE;

	/**
	 * The feature id for the '<em><b>Derived</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_ATTRIBUTE__DERIVED = EcorePackage.EATTRIBUTE__DERIVED;

	/**
	 * The feature id for the '<em><b>EContaining Class</b></em>' container reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_ATTRIBUTE__ECONTAINING_CLASS = EcorePackage.EATTRIBUTE__ECONTAINING_CLASS;

	/**
	 * The feature id for the '<em><b>ID</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_ATTRIBUTE__ID = EcorePackage.EATTRIBUTE__ID;

	/**
	 * The feature id for the '<em><b>EAttribute Type</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_ATTRIBUTE__EATTRIBUTE_TYPE = EcorePackage.EATTRIBUTE__EATTRIBUTE_TYPE;

	/**
	 * The feature id for the '<em><b>Visibility</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_ATTRIBUTE__VISIBILITY = EcorePackage.EATTRIBUTE_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Static</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_ATTRIBUTE__STATIC = EcorePackage.EATTRIBUTE_FEATURE_COUNT + 1;

	/**
	 * The feature id for the '<em><b>Typedef</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_ATTRIBUTE__TYPEDEF = EcorePackage.EATTRIBUTE_FEATURE_COUNT + 2;

	/**
	 * The number of structural features of the '<em>Attribute</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_ATTRIBUTE_FEATURE_COUNT = EcorePackage.EATTRIBUTE_FEATURE_COUNT + 3;

	/**
	 * The operation id for the '<em>Get EAnnotation</em>' operation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_ATTRIBUTE___GET_EANNOTATION__STRING = EcorePackage.EATTRIBUTE___GET_EANNOTATION__STRING;

	/**
	 * The operation id for the '<em>Get Feature ID</em>' operation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_ATTRIBUTE___GET_FEATURE_ID = EcorePackage.EATTRIBUTE___GET_FEATURE_ID;

	/**
	 * The operation id for the '<em>Get Container Class</em>' operation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_ATTRIBUTE___GET_CONTAINER_CLASS = EcorePackage.EATTRIBUTE___GET_CONTAINER_CLASS;

	/**
	 * The number of operations of the '<em>Attribute</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_ATTRIBUTE_OPERATION_COUNT = EcorePackage.EATTRIBUTE_OPERATION_COUNT + 0;

	/**
	 * The meta object id for the '{@link specific.emf.cpp.impl.CppOperationImpl <em>Operation</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see specific.emf.cpp.impl.CppOperationImpl
	 * @see specific.emf.cpp.impl.CppPackageImpl#getCppOperation()
	 * @generated
	 */
	int CPP_OPERATION = 3;

	/**
	 * The feature id for the '<em><b>EAnnotations</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_OPERATION__EANNOTATIONS = EcorePackage.EOPERATION__EANNOTATIONS;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_OPERATION__NAME = EcorePackage.EOPERATION__NAME;

	/**
	 * The feature id for the '<em><b>Ordered</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_OPERATION__ORDERED = EcorePackage.EOPERATION__ORDERED;

	/**
	 * The feature id for the '<em><b>Unique</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_OPERATION__UNIQUE = EcorePackage.EOPERATION__UNIQUE;

	/**
	 * The feature id for the '<em><b>Lower Bound</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_OPERATION__LOWER_BOUND = EcorePackage.EOPERATION__LOWER_BOUND;

	/**
	 * The feature id for the '<em><b>Upper Bound</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_OPERATION__UPPER_BOUND = EcorePackage.EOPERATION__UPPER_BOUND;

	/**
	 * The feature id for the '<em><b>Many</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_OPERATION__MANY = EcorePackage.EOPERATION__MANY;

	/**
	 * The feature id for the '<em><b>Required</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_OPERATION__REQUIRED = EcorePackage.EOPERATION__REQUIRED;

	/**
	 * The feature id for the '<em><b>EType</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_OPERATION__ETYPE = EcorePackage.EOPERATION__ETYPE;

	/**
	 * The feature id for the '<em><b>EGeneric Type</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_OPERATION__EGENERIC_TYPE = EcorePackage.EOPERATION__EGENERIC_TYPE;

	/**
	 * The feature id for the '<em><b>EContaining Class</b></em>' container reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_OPERATION__ECONTAINING_CLASS = EcorePackage.EOPERATION__ECONTAINING_CLASS;

	/**
	 * The feature id for the '<em><b>EType Parameters</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_OPERATION__ETYPE_PARAMETERS = EcorePackage.EOPERATION__ETYPE_PARAMETERS;

	/**
	 * The feature id for the '<em><b>EParameters</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_OPERATION__EPARAMETERS = EcorePackage.EOPERATION__EPARAMETERS;

	/**
	 * The feature id for the '<em><b>EExceptions</b></em>' reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_OPERATION__EEXCEPTIONS = EcorePackage.EOPERATION__EEXCEPTIONS;

	/**
	 * The feature id for the '<em><b>EGeneric Exceptions</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_OPERATION__EGENERIC_EXCEPTIONS = EcorePackage.EOPERATION__EGENERIC_EXCEPTIONS;

	/**
	 * The feature id for the '<em><b>Visibility</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_OPERATION__VISIBILITY = EcorePackage.EOPERATION_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Static</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_OPERATION__STATIC = EcorePackage.EOPERATION_FEATURE_COUNT + 1;

	/**
	 * The feature id for the '<em><b>Typedef</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_OPERATION__TYPEDEF = EcorePackage.EOPERATION_FEATURE_COUNT + 2;

	/**
	 * The number of structural features of the '<em>Operation</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_OPERATION_FEATURE_COUNT = EcorePackage.EOPERATION_FEATURE_COUNT + 3;

	/**
	 * The operation id for the '<em>Get EAnnotation</em>' operation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_OPERATION___GET_EANNOTATION__STRING = EcorePackage.EOPERATION___GET_EANNOTATION__STRING;

	/**
	 * The operation id for the '<em>Get Operation ID</em>' operation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_OPERATION___GET_OPERATION_ID = EcorePackage.EOPERATION___GET_OPERATION_ID;

	/**
	 * The operation id for the '<em>Is Override Of</em>' operation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_OPERATION___IS_OVERRIDE_OF__EOPERATION = EcorePackage.EOPERATION___IS_OVERRIDE_OF__EOPERATION;

	/**
	 * The number of operations of the '<em>Operation</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_OPERATION_OPERATION_COUNT = EcorePackage.EOPERATION_OPERATION_COUNT + 0;

	/**
	 * The meta object id for the '{@link specific.emf.cpp.impl.CppTypedefImpl <em>Typedef</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see specific.emf.cpp.impl.CppTypedefImpl
	 * @see specific.emf.cpp.impl.CppPackageImpl#getCppTypedef()
	 * @generated
	 */
	int CPP_TYPEDEF = 4;

	/**
	 * The feature id for the '<em><b>EUpper Bound</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_TYPEDEF__EUPPER_BOUND = EcorePackage.EGENERIC_TYPE__EUPPER_BOUND;

	/**
	 * The feature id for the '<em><b>EType Arguments</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_TYPEDEF__ETYPE_ARGUMENTS = EcorePackage.EGENERIC_TYPE__ETYPE_ARGUMENTS;

	/**
	 * The feature id for the '<em><b>ERaw Type</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_TYPEDEF__ERAW_TYPE = EcorePackage.EGENERIC_TYPE__ERAW_TYPE;

	/**
	 * The feature id for the '<em><b>ELower Bound</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_TYPEDEF__ELOWER_BOUND = EcorePackage.EGENERIC_TYPE__ELOWER_BOUND;

	/**
	 * The feature id for the '<em><b>EType Parameter</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_TYPEDEF__ETYPE_PARAMETER = EcorePackage.EGENERIC_TYPE__ETYPE_PARAMETER;

	/**
	 * The feature id for the '<em><b>EClassifier</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_TYPEDEF__ECLASSIFIER = EcorePackage.EGENERIC_TYPE__ECLASSIFIER;

	/**
	 * The feature id for the '<em><b>EAnnotations</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_TYPEDEF__EANNOTATIONS = EcorePackage.EGENERIC_TYPE_FEATURE_COUNT + 0;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_TYPEDEF__NAME = EcorePackage.EGENERIC_TYPE_FEATURE_COUNT + 1;

	/**
	 * The feature id for the '<em><b>Visibility</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_TYPEDEF__VISIBILITY = EcorePackage.EGENERIC_TYPE_FEATURE_COUNT + 2;

	/**
	 * The number of structural features of the '<em>Typedef</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_TYPEDEF_FEATURE_COUNT = EcorePackage.EGENERIC_TYPE_FEATURE_COUNT + 3;

	/**
	 * The operation id for the '<em>Is Instance</em>' operation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_TYPEDEF___IS_INSTANCE__OBJECT = EcorePackage.EGENERIC_TYPE___IS_INSTANCE__OBJECT;

	/**
	 * The operation id for the '<em>Get EAnnotation</em>' operation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_TYPEDEF___GET_EANNOTATION__STRING = EcorePackage.EGENERIC_TYPE_OPERATION_COUNT + 0;

	/**
	 * The number of operations of the '<em>Typedef</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_TYPEDEF_OPERATION_COUNT = EcorePackage.EGENERIC_TYPE_OPERATION_COUNT + 1;

	/**
	 * The meta object id for the '{@link specific.emf.cpp.impl.CppParameterImpl <em>Parameter</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see specific.emf.cpp.impl.CppParameterImpl
	 * @see specific.emf.cpp.impl.CppPackageImpl#getCppParameter()
	 * @generated
	 */
	int CPP_PARAMETER = 5;

	/**
	 * The feature id for the '<em><b>EAnnotations</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_PARAMETER__EANNOTATIONS = EcorePackage.EPARAMETER__EANNOTATIONS;

	/**
	 * The feature id for the '<em><b>Name</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_PARAMETER__NAME = EcorePackage.EPARAMETER__NAME;

	/**
	 * The feature id for the '<em><b>Ordered</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_PARAMETER__ORDERED = EcorePackage.EPARAMETER__ORDERED;

	/**
	 * The feature id for the '<em><b>Unique</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_PARAMETER__UNIQUE = EcorePackage.EPARAMETER__UNIQUE;

	/**
	 * The feature id for the '<em><b>Lower Bound</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_PARAMETER__LOWER_BOUND = EcorePackage.EPARAMETER__LOWER_BOUND;

	/**
	 * The feature id for the '<em><b>Upper Bound</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_PARAMETER__UPPER_BOUND = EcorePackage.EPARAMETER__UPPER_BOUND;

	/**
	 * The feature id for the '<em><b>Many</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_PARAMETER__MANY = EcorePackage.EPARAMETER__MANY;

	/**
	 * The feature id for the '<em><b>Required</b></em>' attribute.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_PARAMETER__REQUIRED = EcorePackage.EPARAMETER__REQUIRED;

	/**
	 * The feature id for the '<em><b>EType</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_PARAMETER__ETYPE = EcorePackage.EPARAMETER__ETYPE;

	/**
	 * The feature id for the '<em><b>EGeneric Type</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_PARAMETER__EGENERIC_TYPE = EcorePackage.EPARAMETER__EGENERIC_TYPE;

	/**
	 * The feature id for the '<em><b>EOperation</b></em>' container reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_PARAMETER__EOPERATION = EcorePackage.EPARAMETER__EOPERATION;

	/**
	 * The feature id for the '<em><b>Typedef</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_PARAMETER__TYPEDEF = EcorePackage.EPARAMETER_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Parameter</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_PARAMETER_FEATURE_COUNT = EcorePackage.EPARAMETER_FEATURE_COUNT + 1;

	/**
	 * The operation id for the '<em>Get EAnnotation</em>' operation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_PARAMETER___GET_EANNOTATION__STRING = EcorePackage.EPARAMETER___GET_EANNOTATION__STRING;

	/**
	 * The number of operations of the '<em>Parameter</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_PARAMETER_OPERATION_COUNT = EcorePackage.EPARAMETER_OPERATION_COUNT + 0;

	/**
	 * The meta object id for the '{@link specific.emf.cpp.impl.CppTypedefRefImpl <em>Typedef Ref</em>}' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see specific.emf.cpp.impl.CppTypedefRefImpl
	 * @see specific.emf.cpp.impl.CppPackageImpl#getCppTypedefRef()
	 * @generated
	 */
	int CPP_TYPEDEF_REF = 6;

	/**
	 * The feature id for the '<em><b>EUpper Bound</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_TYPEDEF_REF__EUPPER_BOUND = EcorePackage.EGENERIC_TYPE__EUPPER_BOUND;

	/**
	 * The feature id for the '<em><b>EType Arguments</b></em>' containment reference list.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_TYPEDEF_REF__ETYPE_ARGUMENTS = EcorePackage.EGENERIC_TYPE__ETYPE_ARGUMENTS;

	/**
	 * The feature id for the '<em><b>ERaw Type</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_TYPEDEF_REF__ERAW_TYPE = EcorePackage.EGENERIC_TYPE__ERAW_TYPE;

	/**
	 * The feature id for the '<em><b>ELower Bound</b></em>' containment reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_TYPEDEF_REF__ELOWER_BOUND = EcorePackage.EGENERIC_TYPE__ELOWER_BOUND;

	/**
	 * The feature id for the '<em><b>EType Parameter</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_TYPEDEF_REF__ETYPE_PARAMETER = EcorePackage.EGENERIC_TYPE__ETYPE_PARAMETER;

	/**
	 * The feature id for the '<em><b>EClassifier</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_TYPEDEF_REF__ECLASSIFIER = EcorePackage.EGENERIC_TYPE__ECLASSIFIER;

	/**
	 * The feature id for the '<em><b>Typedef</b></em>' reference.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_TYPEDEF_REF__TYPEDEF = EcorePackage.EGENERIC_TYPE_FEATURE_COUNT + 0;

	/**
	 * The number of structural features of the '<em>Typedef Ref</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_TYPEDEF_REF_FEATURE_COUNT = EcorePackage.EGENERIC_TYPE_FEATURE_COUNT + 1;

	/**
	 * The operation id for the '<em>Is Instance</em>' operation.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_TYPEDEF_REF___IS_INSTANCE__OBJECT = EcorePackage.EGENERIC_TYPE___IS_INSTANCE__OBJECT;

	/**
	 * The number of operations of the '<em>Typedef Ref</em>' class.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 * @ordered
	 */
	int CPP_TYPEDEF_REF_OPERATION_COUNT = EcorePackage.EGENERIC_TYPE_OPERATION_COUNT + 0;

	/**
	 * The meta object id for the '{@link specific.emf.cpp.CppVisibility <em>Visibility</em>}' enum.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see specific.emf.cpp.CppVisibility
	 * @see specific.emf.cpp.impl.CppPackageImpl#getCppVisibility()
	 * @generated
	 */
	int CPP_VISIBILITY = 7;


	/**
	 * Returns the meta object for class '{@link specific.emf.cpp.CppNamespace <em>Namespace</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Namespace</em>'.
	 * @see specific.emf.cpp.CppNamespace
	 * @generated
	 */
	EClass getCppNamespace();

	/**
	 * Returns the meta object for the containment reference list '{@link specific.emf.cpp.CppNamespace#getTypedefs <em>Typedefs</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference list '<em>Typedefs</em>'.
	 * @see specific.emf.cpp.CppNamespace#getTypedefs()
	 * @see #getCppNamespace()
	 * @generated
	 */
	EReference getCppNamespace_Typedefs();

	/**
	 * Returns the meta object for the containment reference list '{@link specific.emf.cpp.CppNamespace#getOperations <em>Operations</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference list '<em>Operations</em>'.
	 * @see specific.emf.cpp.CppNamespace#getOperations()
	 * @see #getCppNamespace()
	 * @generated
	 */
	EReference getCppNamespace_Operations();

	/**
	 * Returns the meta object for the containment reference list '{@link specific.emf.cpp.CppNamespace#getVariables <em>Variables</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference list '<em>Variables</em>'.
	 * @see specific.emf.cpp.CppNamespace#getVariables()
	 * @see #getCppNamespace()
	 * @generated
	 */
	EReference getCppNamespace_Variables();

	/**
	 * Returns the meta object for class '{@link specific.emf.cpp.CppClass <em>Class</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Class</em>'.
	 * @see specific.emf.cpp.CppClass
	 * @generated
	 */
	EClass getCppClass();

	/**
	 * Returns the meta object for the containment reference list '{@link specific.emf.cpp.CppClass#getTypedefs <em>Typedefs</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference list '<em>Typedefs</em>'.
	 * @see specific.emf.cpp.CppClass#getTypedefs()
	 * @see #getCppClass()
	 * @generated
	 */
	EReference getCppClass_Typedefs();

	/**
	 * Returns the meta object for the containment reference list '{@link specific.emf.cpp.CppClass#getNestedClasses <em>Nested Classes</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the containment reference list '<em>Nested Classes</em>'.
	 * @see specific.emf.cpp.CppClass#getNestedClasses()
	 * @see #getCppClass()
	 * @generated
	 */
	EReference getCppClass_NestedClasses();

	/**
	 * Returns the meta object for the container reference '{@link specific.emf.cpp.CppClass#getParentClass <em>Parent Class</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the container reference '<em>Parent Class</em>'.
	 * @see specific.emf.cpp.CppClass#getParentClass()
	 * @see #getCppClass()
	 * @generated
	 */
	EReference getCppClass_ParentClass();

	/**
	 * Returns the meta object for the reference '{@link specific.emf.cpp.CppClass#getNamespace <em>Namespace</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Namespace</em>'.
	 * @see specific.emf.cpp.CppClass#getNamespace()
	 * @see #getCppClass()
	 * @generated
	 */
	EReference getCppClass_Namespace();

	/**
	 * Returns the meta object for the attribute '{@link specific.emf.cpp.CppClass#getVisibility <em>Visibility</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Visibility</em>'.
	 * @see specific.emf.cpp.CppClass#getVisibility()
	 * @see #getCppClass()
	 * @generated
	 */
	EAttribute getCppClass_Visibility();

	/**
	 * Returns the meta object for class '{@link specific.emf.cpp.CppAttribute <em>Attribute</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Attribute</em>'.
	 * @see specific.emf.cpp.CppAttribute
	 * @generated
	 */
	EClass getCppAttribute();

	/**
	 * Returns the meta object for the attribute '{@link specific.emf.cpp.CppAttribute#getVisibility <em>Visibility</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Visibility</em>'.
	 * @see specific.emf.cpp.CppAttribute#getVisibility()
	 * @see #getCppAttribute()
	 * @generated
	 */
	EAttribute getCppAttribute_Visibility();

	/**
	 * Returns the meta object for the attribute '{@link specific.emf.cpp.CppAttribute#isStatic <em>Static</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Static</em>'.
	 * @see specific.emf.cpp.CppAttribute#isStatic()
	 * @see #getCppAttribute()
	 * @generated
	 */
	EAttribute getCppAttribute_Static();

	/**
	 * Returns the meta object for the reference '{@link specific.emf.cpp.CppAttribute#getTypedef <em>Typedef</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Typedef</em>'.
	 * @see specific.emf.cpp.CppAttribute#getTypedef()
	 * @see #getCppAttribute()
	 * @generated
	 */
	EReference getCppAttribute_Typedef();

	/**
	 * Returns the meta object for class '{@link specific.emf.cpp.CppOperation <em>Operation</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Operation</em>'.
	 * @see specific.emf.cpp.CppOperation
	 * @generated
	 */
	EClass getCppOperation();

	/**
	 * Returns the meta object for the attribute '{@link specific.emf.cpp.CppOperation#getVisibility <em>Visibility</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Visibility</em>'.
	 * @see specific.emf.cpp.CppOperation#getVisibility()
	 * @see #getCppOperation()
	 * @generated
	 */
	EAttribute getCppOperation_Visibility();

	/**
	 * Returns the meta object for the attribute '{@link specific.emf.cpp.CppOperation#isStatic <em>Static</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Static</em>'.
	 * @see specific.emf.cpp.CppOperation#isStatic()
	 * @see #getCppOperation()
	 * @generated
	 */
	EAttribute getCppOperation_Static();

	/**
	 * Returns the meta object for the reference '{@link specific.emf.cpp.CppOperation#getTypedef <em>Typedef</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Typedef</em>'.
	 * @see specific.emf.cpp.CppOperation#getTypedef()
	 * @see #getCppOperation()
	 * @generated
	 */
	EReference getCppOperation_Typedef();

	/**
	 * Returns the meta object for class '{@link specific.emf.cpp.CppTypedef <em>Typedef</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Typedef</em>'.
	 * @see specific.emf.cpp.CppTypedef
	 * @generated
	 */
	EClass getCppTypedef();

	/**
	 * Returns the meta object for the attribute '{@link specific.emf.cpp.CppTypedef#getVisibility <em>Visibility</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the attribute '<em>Visibility</em>'.
	 * @see specific.emf.cpp.CppTypedef#getVisibility()
	 * @see #getCppTypedef()
	 * @generated
	 */
	EAttribute getCppTypedef_Visibility();

	/**
	 * Returns the meta object for class '{@link specific.emf.cpp.CppParameter <em>Parameter</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Parameter</em>'.
	 * @see specific.emf.cpp.CppParameter
	 * @generated
	 */
	EClass getCppParameter();

	/**
	 * Returns the meta object for the reference '{@link specific.emf.cpp.CppParameter#getTypedef <em>Typedef</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Typedef</em>'.
	 * @see specific.emf.cpp.CppParameter#getTypedef()
	 * @see #getCppParameter()
	 * @generated
	 */
	EReference getCppParameter_Typedef();

	/**
	 * Returns the meta object for class '{@link specific.emf.cpp.CppTypedefRef <em>Typedef Ref</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for class '<em>Typedef Ref</em>'.
	 * @see specific.emf.cpp.CppTypedefRef
	 * @generated
	 */
	EClass getCppTypedefRef();

	/**
	 * Returns the meta object for the reference '{@link specific.emf.cpp.CppTypedefRef#getTypedef <em>Typedef</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for the reference '<em>Typedef</em>'.
	 * @see specific.emf.cpp.CppTypedefRef#getTypedef()
	 * @see #getCppTypedefRef()
	 * @generated
	 */
	EReference getCppTypedefRef_Typedef();

	/**
	 * Returns the meta object for enum '{@link specific.emf.cpp.CppVisibility <em>Visibility</em>}'.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the meta object for enum '<em>Visibility</em>'.
	 * @see specific.emf.cpp.CppVisibility
	 * @generated
	 */
	EEnum getCppVisibility();

	/**
	 * Returns the factory that creates the instances of the model.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @return the factory that creates the instances of the model.
	 * @generated
	 */
	CppFactory getCppFactory();

	/**
	 * <!-- begin-user-doc -->
	 * Defines literals for the meta objects that represent
	 * <ul>
	 *   <li>each class,</li>
	 *   <li>each feature of each class,</li>
	 *   <li>each operation of each class,</li>
	 *   <li>each enum,</li>
	 *   <li>and each data type</li>
	 * </ul>
	 * <!-- end-user-doc -->
	 * @generated
	 */
	interface Literals {
		/**
		 * The meta object literal for the '{@link specific.emf.cpp.impl.CppNamespaceImpl <em>Namespace</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see specific.emf.cpp.impl.CppNamespaceImpl
		 * @see specific.emf.cpp.impl.CppPackageImpl#getCppNamespace()
		 * @generated
		 */
		EClass CPP_NAMESPACE = eINSTANCE.getCppNamespace();

		/**
		 * The meta object literal for the '<em><b>Typedefs</b></em>' containment reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference CPP_NAMESPACE__TYPEDEFS = eINSTANCE.getCppNamespace_Typedefs();

		/**
		 * The meta object literal for the '<em><b>Operations</b></em>' containment reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference CPP_NAMESPACE__OPERATIONS = eINSTANCE.getCppNamespace_Operations();

		/**
		 * The meta object literal for the '<em><b>Variables</b></em>' containment reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference CPP_NAMESPACE__VARIABLES = eINSTANCE.getCppNamespace_Variables();

		/**
		 * The meta object literal for the '{@link specific.emf.cpp.impl.CppClassImpl <em>Class</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see specific.emf.cpp.impl.CppClassImpl
		 * @see specific.emf.cpp.impl.CppPackageImpl#getCppClass()
		 * @generated
		 */
		EClass CPP_CLASS = eINSTANCE.getCppClass();

		/**
		 * The meta object literal for the '<em><b>Typedefs</b></em>' containment reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference CPP_CLASS__TYPEDEFS = eINSTANCE.getCppClass_Typedefs();

		/**
		 * The meta object literal for the '<em><b>Nested Classes</b></em>' containment reference list feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference CPP_CLASS__NESTED_CLASSES = eINSTANCE.getCppClass_NestedClasses();

		/**
		 * The meta object literal for the '<em><b>Parent Class</b></em>' container reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference CPP_CLASS__PARENT_CLASS = eINSTANCE.getCppClass_ParentClass();

		/**
		 * The meta object literal for the '<em><b>Namespace</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference CPP_CLASS__NAMESPACE = eINSTANCE.getCppClass_Namespace();

		/**
		 * The meta object literal for the '<em><b>Visibility</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute CPP_CLASS__VISIBILITY = eINSTANCE.getCppClass_Visibility();

		/**
		 * The meta object literal for the '{@link specific.emf.cpp.impl.CppAttributeImpl <em>Attribute</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see specific.emf.cpp.impl.CppAttributeImpl
		 * @see specific.emf.cpp.impl.CppPackageImpl#getCppAttribute()
		 * @generated
		 */
		EClass CPP_ATTRIBUTE = eINSTANCE.getCppAttribute();

		/**
		 * The meta object literal for the '<em><b>Visibility</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute CPP_ATTRIBUTE__VISIBILITY = eINSTANCE.getCppAttribute_Visibility();

		/**
		 * The meta object literal for the '<em><b>Static</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute CPP_ATTRIBUTE__STATIC = eINSTANCE.getCppAttribute_Static();

		/**
		 * The meta object literal for the '<em><b>Typedef</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference CPP_ATTRIBUTE__TYPEDEF = eINSTANCE.getCppAttribute_Typedef();

		/**
		 * The meta object literal for the '{@link specific.emf.cpp.impl.CppOperationImpl <em>Operation</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see specific.emf.cpp.impl.CppOperationImpl
		 * @see specific.emf.cpp.impl.CppPackageImpl#getCppOperation()
		 * @generated
		 */
		EClass CPP_OPERATION = eINSTANCE.getCppOperation();

		/**
		 * The meta object literal for the '<em><b>Visibility</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute CPP_OPERATION__VISIBILITY = eINSTANCE.getCppOperation_Visibility();

		/**
		 * The meta object literal for the '<em><b>Static</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute CPP_OPERATION__STATIC = eINSTANCE.getCppOperation_Static();

		/**
		 * The meta object literal for the '<em><b>Typedef</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference CPP_OPERATION__TYPEDEF = eINSTANCE.getCppOperation_Typedef();

		/**
		 * The meta object literal for the '{@link specific.emf.cpp.impl.CppTypedefImpl <em>Typedef</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see specific.emf.cpp.impl.CppTypedefImpl
		 * @see specific.emf.cpp.impl.CppPackageImpl#getCppTypedef()
		 * @generated
		 */
		EClass CPP_TYPEDEF = eINSTANCE.getCppTypedef();

		/**
		 * The meta object literal for the '<em><b>Visibility</b></em>' attribute feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EAttribute CPP_TYPEDEF__VISIBILITY = eINSTANCE.getCppTypedef_Visibility();

		/**
		 * The meta object literal for the '{@link specific.emf.cpp.impl.CppParameterImpl <em>Parameter</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see specific.emf.cpp.impl.CppParameterImpl
		 * @see specific.emf.cpp.impl.CppPackageImpl#getCppParameter()
		 * @generated
		 */
		EClass CPP_PARAMETER = eINSTANCE.getCppParameter();

		/**
		 * The meta object literal for the '<em><b>Typedef</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference CPP_PARAMETER__TYPEDEF = eINSTANCE.getCppParameter_Typedef();

		/**
		 * The meta object literal for the '{@link specific.emf.cpp.impl.CppTypedefRefImpl <em>Typedef Ref</em>}' class.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see specific.emf.cpp.impl.CppTypedefRefImpl
		 * @see specific.emf.cpp.impl.CppPackageImpl#getCppTypedefRef()
		 * @generated
		 */
		EClass CPP_TYPEDEF_REF = eINSTANCE.getCppTypedefRef();

		/**
		 * The meta object literal for the '<em><b>Typedef</b></em>' reference feature.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @generated
		 */
		EReference CPP_TYPEDEF_REF__TYPEDEF = eINSTANCE.getCppTypedefRef_Typedef();

		/**
		 * The meta object literal for the '{@link specific.emf.cpp.CppVisibility <em>Visibility</em>}' enum.
		 * <!-- begin-user-doc -->
		 * <!-- end-user-doc -->
		 * @see specific.emf.cpp.CppVisibility
		 * @see specific.emf.cpp.impl.CppPackageImpl#getCppVisibility()
		 * @generated
		 */
		EEnum CPP_VISIBILITY = eINSTANCE.getCppVisibility();

	}

} //CppPackage
