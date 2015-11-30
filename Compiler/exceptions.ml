(* Processor Exceptions *)
exception MissingEOF

(* Scanner Exceptions *)
exception IllegalCharacter of char * int
exception UnmatchedQuotation of int
exception IllegalToken of string

(* Analyzer Exceptions *)
exception IncorrectNumberOfArgumentsException
exception DuplicateClassName
exception DuplicateField
exception DuplicateFunction
exception DuplicateConstructor
exception InvalidBinopExpression
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