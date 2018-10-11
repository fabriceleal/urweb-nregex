open Nregex

datatype mkTag =
	 Hash
       | Word
       | Hr
       | Whitespace
       | BoldDel
       | ItalicDel
       | UrlSpec

datatype mkLinkPart =
	 UrlSpecLink
       | UrlSpecText

val mk_link_part_eq = mkEq (fn a b =>
			       case (a, b) of
				   (UrlSpecLink, UrlSpecLink) => True
				 | (UrlSpecText, UrlSpecText) => True
				 | _ => False)
		      
val openbr = splitChs "["
val closebr = splitChs "]"
val openp = splitChs "("
val closep = splitChs ")"
val italicDel = splitChs "_"
val dash = splitChs "-"
val boldDel = splitChs "*"
val hash = splitChs "#"
val whitespace = splitChs " "
val anyChar = splitChs "ABCDEFGHIKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
val anyCharUrl = List.append anyChar (splitChs ":/.")

(* FIXME fix this when we can use anonymous functions in a algebraic datatype. we want to return at once a single group,
							 the tag of which has the 2 elements of a link (text + url) *)
		 
val matchLinkToken : pattern mkTag = 
    (Seq ((OneOf openbr) :: (OneOrMoreOf anyChar) :: (OneOf closebr) ::
			 (OneOf openp) :: (OneOrMoreOf anyCharUrl) :: (OneOf closep) :: []))

val matchLinkParts : pattern mkLinkPart =
    (Seq ((OneOf openbr) :: (Group ((OneOrMoreOf anyChar), UrlSpecText)) :: (OneOf closebr) ::
			 (OneOf openp) :: (Group ((OneOrMoreOf anyCharUrl), UrlSpecLink)) :: (OneOf closep) :: []))    
    
val matchLineToken : pattern mkTag =
    Eith ((Group ((OneOrMoreOf hash), Hash)) ::
	(Group ((OneOrMoreOf dash), Hr)) ::
	(Group ((OneOrMoreOf whitespace), Whitespace)) ::
	(Group ((OneOrMoreOf anyChar), Word)) ::
	(Group ((OneOf boldDel), BoldDel)) ::
	(Group ((OneOf italicDel), ItalicDel)) ::
	(Group (matchLinkToken, UrlSpec)) ::
	[])

fun decomposeUrlRaw raw =
    case (match raw matchLinkParts True) of
	None => None
      | Some m' => Some m'.Groups

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
    
