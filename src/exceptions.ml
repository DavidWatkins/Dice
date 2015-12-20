(* Dice Exceptions *)
exception InvalidNumberCompilerArguments of int
exception InvalidCompilerArgument of string
exception NoFileArgument

(* Processor Exceptions *)
exception MissingEOF

(* Scanner Exceptions *)
exception IllegalCharacter of string * char * int
exception UnmatchedQuotation of int
exception IllegalToken of string

(* Analyzer Exceptions *)
exception IncorrectNumberOfArgumentsException
exception ConstructorNotFound of string
exception DuplicateClassName of string
exception DuplicateField
exception DuplicateFunction of string
exception DuplicateConstructor
exception DuplicateLocal of string
exception UndefinedClass of string
exception UnknownIdentifier of string
exception InvalidBinopExpression of string
exception InvalidIfStatementType
exception InvalidForStatementType
exception ReturnTypeMismatch
exception MainNotDefined
exception MultipleMainsDefined
exception InvalidWhileStatementType
exception LocalAssignTypeMismatch of string * string
exception InvalidUnaryOperation
exception AssignmentTypeMismatch of string * string
exception FunctionNotFound of string
exception UndefinedID of string
exception InvalidAccessLHS of string
exception LHSofRootAccessMustBeIDorFunc of string
exception ObjAccessMustHaveObjectType of string
exception UnknownIdentifierForClass of string * string
exception CannotUseReservedFuncName of string
exception InvalidArrayPrimitiveConsecutiveTypes of string * string
exception InvalidArrayPrimitiveType of string
exception MustPassIntegerTypeToArrayCreate
exception ArrayInitTypeInvalid of string
exception MustPassIntegerTypeToArrayAccess
exception ArrayAccessInvalidParamLength of string * string
exception ArrayAccessExpressionNotArray of string
exception CanOnlyAccessLengthOfArray
exception CanOnlyDeleteObjectsOrArrays
exception CannotAccessLengthOfCharArray
exception AllNonVoidFunctionsMustEndWithReturn of string
exception CyclicalDependencyBetween of string * string

(* Codegen Exceptions *)
exception InvalidTypePassedToPrintf
exception InvalidBinaryOperator
exception UnknownVariable of string
exception AssignLHSMustBeAssignable
exception CannotCastTypeException of string * string
exception InvalidBinopEvaluationType
exception FloatOpNotSupported
exception IntOpNotSupported
exception LLVMFunctionNotFound of string
exception InvalidStructType of string
exception UnableToCallFunctionWithoutParent of string
exception CannotAssignParam of string
exception InvalidUnopEvaluationType
exception UnopNotSupported
exception ArrayLargerThan1Unsupported
exception CanOnlyCompareObjectsWithNull of string * string
exception ObjOpNotSupported of string
exception CanOnlyCompareArraysWithNull of string * string