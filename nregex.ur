
datatype charset =
IsAlNum
| IsAlpha
| IsBlank
| IsCntrl
| IsDigit
| IsGraph
| IsLower
| IsPrint
| IsPunct
| IsSpace
| IsUpper
| IsXDigit
| CLiteral of char
| CAnd of list charset
| COr of list charset
| CNot of charset
	  
fun testInSet s c =
    case s of
	IsAlNum => isalnum c
      | IsAlpha => isalpha c
      | IsBlank => isblank c
      | IsCntrl => iscntrl c
      | IsDigit => isdigit c
      | IsGraph => isgraph c
      | IsLower => islower c
      | IsPrint => isprint c
      | IsPunct => ispunct c
      | IsSpace => isspace c
      | IsUpper => isupper c
      | IsXDigit => isxdigit c
      | CLiteral c' => c = c'
      | CAnd l =>
	let
	    fun eval' l =
		case l of
		    [] => True
		  | h :: t =>
		    if (testInSet h c) then
			eval' t
		    else
			False

	    fun eval l =
		case l of
		    [] => False
		  | _ => eval' l
	in
	    eval l
	end
      | COr l =>
	let
	    fun eval l =
		case l of
		    [] => False
		  | h :: t =>
		    if (testInSet h c) then
			True
		    else
			eval t
		    
	in
	    eval l
	end
      | CNot s => not (testInSet s c)
		
datatype pattern t =
	 Literal of string
       | OneOf of charset
       | FromStart of pattern t
       | Seq of list (pattern t)
       | OneOrMoreOf of charset
       (* optOf isnt particularly smart if its pattern it's the same as the next pattern of the parent Seq *)
       (* using Eith and providing 2 possible paths is a way of dodging this issue*)
       | OptOf of pattern t
       | Group of (pattern t) * t
       | Eith of list (pattern t)

(*
fun isOneOf opts test =
    case opts of
	[] => False
      | h :: t  =>
	if h = test then
	    True
	else
	    isOneOf t test *)

fun splitChs s =
    let
	val l = strlen s
    in
	if l = 0 then
	    []
	else
	    (strsub s 0) :: (splitChs (substring s 1 (l - (1))))
    end

fun joinChs cs =
    case cs of
	[] => ""
      | h :: t => (show h) ^ (joinChs t)

fun charsToSet l =
    COr (List.mp (fn e => CLiteral e) l)

type matched t = { Start: int, Len: int, Groups : list (string * t) }


(* ugly as sin *)
(* TODO improve this *)
fun startsWith str seq =
    case String.sindex {Haystack=str, Needle=seq} of
	None => False
      | Some i => i = 0

		  
fun seekOneOf (str : string) (testFn : charset) (accidx : int) : option int =
    let
	val l = strlen str
    in
	if (l = 0) then
	    None
	else
	    case (testInSet testFn (strsub str 0)) of
		True => Some accidx
	      | _ =>
		seekOneOf (substring str 1 (l - (1))) testFn (accidx + 1)
    end

fun match [t] (str : string) (pat : pattern t) (zeroReq : bool) : option (matched t) =
    case pat of
	Literal s =>
	if zeroReq then
	    (if startsWith str s then
		 Some {Start=0, Len=strlen s, Groups = []}
	     else
		 None)
	else
	    (case String.sindex {Haystack=str, Needle=s} of
		 None => None
	       | Some i => Some { Start = i, Len =strlen s, Groups = [] })
      | OneOf fnCh =>
	(case (seekOneOf str fnCh 0) of
	    None => None
	  | Some i =>
	    if zeroReq then
		(if i = 0 then
		    Some {Start=i,Len=1, Groups = []}
		else
		    None)
	    else
		Some {Start=i,Len=1, Groups = []})
      | OptOf pat =>
	(case (match str pat False) of
	     None => Some {Start=0, Len=0, Groups = []}
	   | Some m =>
	     if zeroReq then
		 (if m.Start = 0 then
		      Some m
		  else
		      Some {Start=0, Len=0, Groups = []})
	     else
		 Some m)
      | OneOrMoreOf fnCh =>
	(case (match str (OneOf fnCh : pattern t) zeroReq) of
	    None => None
	  | Some m =>
	    let
		val l = strlen str
			
		fun match' str accStart accLen =
		    case (match str (OptOf (OneOf fnCh) : pattern t) True) of
			None => Some {Start=accStart, Len=accLen, Groups = []} (*optOf actually always matches *)
		      | Some m' =>
			let
			    val idx = m'.Start + m'.Len
			in
			    if m'.Len = 0 then
				Some {Start=accStart, Len=accLen, Groups = []}
			    else
				match' (substring str idx ((strlen str) - idx)) accStart (accLen + m'.Len)
			end
	    in
		match' (substring str 1 (l - (1))) m.Start m.Len
	    end
	)
      | FromStart pat =>
	(match str pat True)
      | Seq lsPat =>
	let
	    fun match' (str : string) (t : list (pattern t)) (accStart : int) (accLen : int) (accGroups : list (string * t)) =
		case t of
		    [] => Some { Start = accStart, Len = accLen, Groups = accGroups }
		  | h :: t' =>
		    case (match str h True) of
			None => None
		      | Some m' =>
			let
			    val idx = m'.Start + m'.Len
			in
			    match' (substring str idx ((strlen str) - idx)) t' accStart (accLen + m'.Len) (List.append m'.Groups accGroups)
			end 
	in
	    case lsPat of
		[] => None
	      | h :: t =>
		(case (match str h zeroReq) of
		     None => None
		   | Some m =>
		     let
			 val idx = m.Start + m.Len
		     in
			 match' (substring str idx ((strlen str) - idx)) t m.Start m.Len m.Groups
		     end)
	end
      | Eith lsPat =>
      (* scan string 1 char at a time and find best match *)
	let
	    fun match' str lsPat' accIdx =
		(case lsPat' of
		    [] =>
		    if zeroReq then
			None
		    else
			let
			    val l = strlen str
			in
			    (if l = 0 then
				None
			    else
				match' (substring str 1 (l - (1))) lsPat (accIdx + 1)) 
			end
		    
		  | pat :: r =>
		    let
			val m' = match str pat True
		    in
			case m' of
			    None => match' str r accIdx
			  | Some m'' => Some {Start = accIdx, Len = m''.Len, Groups = m''.Groups }
		    end)
			
	in
	    match' str lsPat 0
	end

	
      | Group (pat, tag) =>
	let
	    val m = match str pat zeroReq
	in
	    case m of
		None => None
	      | Some m' => 
		Some {Start = m'.Start, Len = m'.Len, Groups = (substring str m'.Start m'.Len, tag) :: m'.Groups}
	end
      | _ => None

fun optToList [t] (o : option t) : list t =
    case o of
	None => []
      | Some a =>  a :: []

fun matchAll [t] (str : string) (pat: pattern t) : list (matched t) =
    case (match str pat False) of
	None => []
      | Some m' =>
	let
	    val len = strlen str
	    val idx = m'.Start + m'.Len
	in
	    m' :: (matchAll (substring str idx (len - idx)) pat)
	end

fun splitAllLines str =
    case (String.ssplit {Haystack=str,Needle= "\n"}) of
	None => str :: []
      | Some (h, t) =>
	h :: (splitAllLines t)

	
