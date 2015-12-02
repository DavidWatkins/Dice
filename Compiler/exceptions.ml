(* Dice Exceptions *)
exception InvalidNumberCompilerArguments of int
exception InvalidCompilerArgument of string

(* Processor Exceptions *)
exception MissingEOF

(* Scanner Exceptions *)
exception IllegalCharacter of string * char * int
exception UnmatchedQuotation of int
exception IllegalToken of string

(* Analyzer Exceptions *)
exception IncorrectNumberOfArgumentsException
exception ConstructorNotFound
exception DuplicateClassName
exception DuplicateField
exception DuplicateFunction
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
exception LocalTypeMismatch
exception InvalidUnaryOperation
exception AssignmentTypeMismatch

(* Codegen Exceptions *)
exception InvalidTypePassedToPrintf
exception InvalidBinaryOperator
exception UnknownVariable
exception AssignLHSMustBeAssignable
exception CannotCastTypeException
exception InvalidBinopEvaluationType
exception FloatOpNotSupported
exception IntOpNotSupported