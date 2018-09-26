type matched = { Start: int, Len: int, Groups : list string }
type testChar = char -> bool

datatype pattern =
	 Literal of string
       | OneOf of testChar
       | FromStart of pattern
       | Seq of list pattern
       | OneOrMoreOf of testChar
       | OptOf of testChar
       | Group of pattern

val match :  string -> pattern -> bool -> option matched

type lnRes = { Line:string, Result: option matched }

val test : unit -> transaction page
		   
val testNRe : unit -> transaction (option (list lnRes))
