(* Processor Exceptions *)
exception MissingEOF

(* Scanner Exceptions *)
exception IllegalCharacter of char * int
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
