
type testChar = char -> bool

datatype pattern =
	 Literal of string
       | OneOf of testChar
       | FromStart of pattern
       | Seq of list pattern
       | OneOrMoreOf of testChar
       | OptOf of testChar
       | Group of pattern

val testPgn = "[Event \"Reykjavik Open\"]
[Site \"Reykjavik, Iceland\"]
[Date \"????.??.??\"]
[Round \"8.1\"]
[White \"Adhiban, Baskaran\"]
[Black \"Rapport, Richard\"]
[Result \"1-0\"]
[ECO \"D15\"]
[PlyCount \"54\"]

1. d4 {[%emt 0:00:27]} d5 {[%emt 0:01:11]} 2. c4 {[%emt 0:00:42]} c6 {[%emt 0:
00:06]} 3. Nf3 {[%emt 0:02:33]} Nf6 {[%emt 0:00:30]} 4. Nc3 {[%emt 0:00:05]}
dxc4 {[%emt 0:00:05]} 5. e4 {[%emt 0:02:39]} b5 {[%emt 0:00:26]} 6. Be2 {
[%emt 0:00:01]} e6 {[%emt 0:01:31]} 7. O-O {[%emt 0:00:46]} Be7 {[%emt 0:03:04]
} 8. a4 {[%emt 0:03:34]} b4 {[%emt 0:00:44]} 9. e5 {[%emt 0:00:08]} bxc3 {
[%emt 0:02:06]} 10. exf6 {[%emt 0:00:05]} Bxf6 {[%emt 0:00:14]} 11. bxc3 {
[%emt 0:00:07]} Ba6 {[%emt 0:00:37]} 12. Ne5 {[%emt 0:17:30]} Bxe5 {[%emt 0:04:
41]} 13. dxe5 {[%emt 0:00:02]} Qxd1 {[%emt 0:00:25]} 14. Rxd1 {[%emt 0:00:02]}
Nd7 {[%emt 0:00:07]} 15. f4 {[%emt 0:04:04]} Nb6 {[%emt 0:04:54]} 16. Rd6 {
[%emt 0:02:24]} O-O {[%emt 0:13:59]} 17. Bf3 {[%emt 0:08:12]} Nc8 {[%emt 0:07:
22]} 18. Rxc6 {[%emt 0:02:30]} Bb7 {[%emt 0:00:29]} 19. Rxc8 {[%emt 0:00:02]}
Raxc8 {[%emt 0:00:05]} 20. Bxb7 {[%emt 0:00:05]} Rb8 {[%emt 0:00:12]} 21. Ba6 {
[%emt 0:02:05]} Rb3 {[%emt 0:00:15]} 22. Ba3 {[%emt 0:00:48]} Rd8 {[%emt 0:00:
11]} 23. Bb4 {[%emt 0:00:47]} Rd2 {[%emt 0:00:39]} 24. Bxc4 {[%emt 0:00:11]}
Rbb2 {[%emt 0:00:02]} 25. Bf1 {[%emt 0:00:30]} h5 {[%emt 0:00:10]} 26. a5 {
[%emt 0:02:24]} a6 {[%emt 0:00:16]} 27. Bc5 {[%emt 0:03:33]} g6 {[%emt 0:00:52]
} 1-0
"
fun isOneOf (opts : list char) (test : char) =
    case opts of
	[] => False
      | h :: t  =>
	if h = test then
	    True
	else
	    isOneOf t test

fun splitChs s =
    let
	val l = strlen s
    in
	if l = 0 then
	    []
	else
	    (strsub s 0) :: (splitChs (substring s 1 (l - (1))))
    end

(*
val anyLett = isalpha 
val whitespace = isspace 
val anyLettAndWs = fn c => (anyLett c) || (whitespace c) 
val quote = fn c => c = #"\"" 
val leftb = fn c => c = #"[" 
val rightb = fn c => c = #"]"
*)
(*

    
val anyLett = isOneOf (splitChs "abcdefghijhklmnopqrstuwyxzABCDEFGHIJHKLMNOPQRSTUVWXYZ,")
val whitespace = isOneOf (splitChs " ")
val anyLettAndWs = isOneOf (splitChs "abcdefghijhklmnopqrstuwyxzABCDEFGHIJHKLMNOPQRSTUVWXYZ, ")
val quote = isOneOf (splitChs "\"")
val leftb =  isOneOf (splitChs "[")
val rightb =  isOneOf (splitChs "]")
 *)


(* 
fun anyLett c = isOneOf (splitChs "abcdefghijhklmnopqrstuwyxzABCDEFGHIJHKLMNOPQRSTUVWXYZ,") c
fun whitespace c = isOneOf (splitChs " ") c
fun anyLettAndWs c = isOneOf (splitChs "abcdefghijhklmnopqrstuwyxzABCDEFGHIJHKLMNOPQRSTUVWXYZ, ") c
fun quote c = isOneOf (splitChs "\"") c
fun leftb c =  isOneOf (splitChs "[") c
fun rightb c =  isOneOf (splitChs "]") c
 *)
    
(**)
fun anyLett _ = False
fun whitespace _ = False
fun anyLettAndWs _ = False
fun quote _ = False
fun leftb _ = False
fun rightb _ = False

val matchEventTag =
(*    (Group (OneOrMoreOf anyLett)) *)
    FromStart
	(Seq ((OneOf leftb) ::
	 (Group (OneOrMoreOf anyLett)) ::
	 (OneOrMoreOf whitespace) ::
	 (OneOf quote) :: (Group (OneOrMoreOf anyLettAndWs)) ::
	 (OneOf quote) :: (OneOf rightb) :: [])) 
	    
type matched = { Start: int, Len: int, Groups : list string }

(* ugly as sin *)
fun startsWith str seq =
    case String.sindex {Haystack=str, Needle=seq} of
	None => False
      | Some i => i = 0

		  
fun seekOneOf str testFn accidx =
    let
	val l = strlen str
    in
	if (l = 0) then
	    None
	else
	    case (testFn (strsub str 0)) of
		True => Some accidx
	      | _ =>
		seekOneOf (substring str 1 (l - (1))) testFn (accidx + 1)
    end

fun match (str : string) (pat : pattern) (zeroReq : bool) : option matched =
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
      | OptOf fnCh =>
	(case (seekOneOf str fnCh 0) of
	     None => Some {Start=0, Len=0, Groups = []}
	   | Some i =>
	     if zeroReq then
		 (if i = 0 then
		      Some {Start=i,Len=1, Groups = []}
		  else
		      None)
	     else
		 Some {Start=i,Len=1, Groups = []})
      | OneOrMoreOf fnCh =>
	(case (match str (OneOf fnCh) zeroReq) of
	    None => None
	  | Some m =>
	    let
		val l = strlen str
			
		fun match' str accStart accLen =
		    case (match str (OptOf fnCh) True) of
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
	    fun match' (str : string) (t : list pattern) (accStart : int) (accLen : int) (accGroups : list string): option matched =
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
      | Group pat =>
	let
	    val m = match str pat zeroReq
	in
	    case m of
		None => None
	      | Some m' => 
		Some {Start = m'.Start, Len = m'.Len, Groups = (substring str m'.Start m'.Len) :: m'.Groups}
	end
      | _ => None

fun matchForStr str =
   { Line=str, Result= match str matchEventTag False}

fun splitAllLines str =
    case (String.ssplit {Haystack=str,Needle= "\n"}) of
	None => str :: []
      | Some (h, t) =>
	h :: (splitAllLines t)

fun test () =    
    return <xml>
      <body>
	{List.foldr (fn i acc => <xml>{acc} <div>{[show i.Line]} </div></xml>) <xml></xml> (List.mp matchForStr (splitAllLines testPgn))}
      </body>
    </xml>
