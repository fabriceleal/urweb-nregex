
datatype pattern t =
	 Literal of string
       | OneOf of list char
       | FromStart of pattern t
       | Seq of list (pattern t)
       | OneOrMoreOf of list char
       (* optOf isnt particularly smart if its pattern it's the same as the next pattern of the parent Seq *)
       (* using Eith and providing 2 possible paths is a way of dodging this issue*)
       | OptOf of pattern t
       | Group of (pattern t) * t
       | Eith of list (pattern t)

		 
type matched t = { Start: int, Len: int, Groups : list (string * t) }

val splitChs : string -> list char

val match : t ::: Type -> string -> pattern t -> bool -> option (matched t)

val matchAll : t ::: Type -> string -> pattern t -> list (matched t)

val splitAllLines : string -> list string

val startsWith : string -> string -> bool
		 
val optToList : t ::: Type -> option t -> list t
					  
