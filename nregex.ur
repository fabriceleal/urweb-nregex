
type testChar = char -> bool

datatype pattern =
	 OneOf of (char -> bool)

val testPgn = "a"

val anyLett = isalpha 

type matched = { Start: int, Len: int, Groups : list string }
		  
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

fun match (str : string) (pat : pattern) : option matched =
    case pat of
	OneOf fnCh =>
	(case (seekOneOf str fnCh 0) of
	     None => None
	   | Some i =>	     
	     Some {Start=i,Len=1, Groups = []})
	
fun matchForStr str =
   { Line=str, Result= (match str (OneOf isalpha))}

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
