open Nregex

datatype mkTag =
	 Hash
       | Word
       | Whitespace
       | BoldDel
       | ItalicDel
       | UrlSpec

val openbr = splitChs "["
val closebr = splitChs "]"
val openp = splitChs "("
val closep = splitChs ")"
val italicDel = splitChs "_"
val boldDel = splitChs "*"
val hash = splitChs "#"
val whitespace = splitChs " "
val anyChar = splitChs "ABCDEFGHIKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
val anyCharUrl = List.append anyChar (splitChs ":/.")

val matchLinkToken : pattern mkTag = 
    (Seq ((OneOf openbr) :: (OneOrMoreOf anyChar) :: (OneOf closebr) ::
			 (OneOf openp) :: (OneOrMoreOf anyCharUrl) :: (OneOf closep) :: []))
    
val matchLineToken : pattern mkTag =
    Eith ((Group ((OneOrMoreOf hash), Hash)) ::
	(Group ((OneOrMoreOf whitespace), Whitespace)) ::
	(Group ((OneOrMoreOf anyChar), Word)) ::
	(Group ((OneOf boldDel), BoldDel)) ::
	(Group ((OneOf italicDel), ItalicDel)) ::
	(Group (matchLinkToken, UrlSpec)) ::
	[])

fun decomposeMkL lines =
    case lines of
	[] => []
      | h :: t =>
	let
	    val matches = matchAll h matchLineToken
	    val grps = List.foldr List.append []
				  (List.mp (fn e => e.Groups) matches)
	in
	    grps :: (decomposeMkL t)
	end
	 
fun decomposeMk str =
    decomposeMkL (splitAllLines str)
    
