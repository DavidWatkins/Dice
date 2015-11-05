(* Scanner Exceptions *)
exception IllegalCharacter of char * int
exception UnmatchedQuotation of int
exception IllegalToken of string