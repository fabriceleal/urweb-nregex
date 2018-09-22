open Nregex

datatype pgnTag =
	 Castle
       | LongCastle
       | Piece
       | PieceDesamb
       | PawnMove
       | MoveNbr
       | Comment
       | HeaderKey
       | HeaderValue

val show_pgn_tag = mkShow (fn tag => case tag of
					 Castle => "Castle"
				       | LongCastle => "LongCastle"
				       | Piece => "Piece" 
				       | PieceDesamb => "PieceDesamb"
				       | PawnMove => "PawnMove"
				       | MoveNbr => "MoveNbr"
				       | Comment => "Comment"
				       | HeaderKey => "HeaderKey"
				       | HeaderValue => "HeaderValue")

type patternPgn = pattern pgnTag

type matchedPgn = matched pgnTag
		  
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


val anyLett = splitChs "WhiteBlackAdhibanBaskaranResult1-0PlyCount4ECOD15DateSiteEventAdhibanBaskaran8.1?ReykjavikIcelandOpenPlyCount"
val anyLettAndWs = splitChs "WhiteBlackAdhiban, BaskaranResult1-0PlyCount4ECOD15DateSiteEventAdhibanBaskaran8.1?Reykjavik,IcelandOpenPlyCount "
val whitespace = splitChs " "
val quote = splitChs "\""
val leftb = splitChs "["
val rightb = splitChs "]"

val matchHeader : patternPgn =
    FromStart
	(Seq ((OneOf leftb) ::
	 (Group ((OneOrMoreOf anyLett), HeaderKey)) ::
	 (OneOrMoreOf whitespace) ::
	 (OneOf quote) :: (Group ((OneOrMoreOf anyLettAndWs), HeaderValue)) ::
	 (OneOf quote) :: (OneOf rightb) :: [])) 

val file = splitChs "abcdefgh"
val digit = splitChs "0123456789"
val rank = splitChs "12345678"
val piece = splitChs "KQRNB"
val takes = splitChs "x"
val anything = splitChs "[]%:-/abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUWVXYZ "

(* TODO promotions *)
(* TODO check / mates *)
(* TODO final result? *)
(* TODO variations *)
	       
val matchMoveTokens : patternPgn =
    Eith (
          (* 1. *)
          (Group (Seq ((OneOrMoreOf digit) :: (Literal "." :: [])), MoveNbr)) ::
	  (* Raa8 or Raxa8*)
          (Group (Seq (((OneOf piece) :: (OneOf file) :: (OptOf (Literal "x")) :: (OneOf file) :: (OneOf rank) :: [])), PieceDesamb)) ::
	  (* Nf3 or Nxf3 *)
          (Group (Seq (((OneOf piece) :: (OptOf (Literal "x")) :: (OneOf file) :: (OneOf rank) :: [])), Piece)) ::
	  (* d4 or dxe5 *)
          (Group (Seq (((OneOf file) :: (OptOf (Seq ((OneOf takes) :: (OneOf file) :: []))) :: (OneOf rank) :: [])), PawnMove)) ::
	  (* castling *)
	  (Group (Literal "O-O-O", LongCastle)) ::
	  (Group (Literal "O-O", Castle)) ::
	  (* a comment *)
	  (Group (Seq (((Literal "{") :: (OneOrMoreOf anything) :: (Literal "}") :: [])), Comment)) :: 
          [] )

    

fun matchForStr str =
    { Line=str,
(*      Result = match str matchMoveTokens False, *)
      Result = matchAll str matchMoveTokens
    } (* EventTag *)

    
fun decomposePgn pgn =
    let
	val lines = splitAllLines pgn

	fun matchMoves lines' =
	    let
		val line = List.foldr (fn i acc => i ^ acc) "" lines'
	    in
		List.mp (fn e => e.Groups) (matchAll line matchMoveTokens)
	    end

	fun matchHdrs lines' =
	    case lines' of
		[] => []
	      | h :: t =>
		(case (match h matchHeader False) of
		     None => matchMoves t
		   | Some m => (List.rev m.Groups) :: (matchHdrs t))     
    in
	matchHdrs lines
    end
	
   
fun test () =
    let
	val decomposed = decomposePgn testPgn
	
(*	val lines = List.rev (splitAllLines testPgn)
	val lines2 = List.mp matchForStr lines 

	fun resLToXml r =
	    case r of
		[] => <xml></xml>
	      | m :: t =>
		<xml>
		  <div>
		    { List.foldr (fn i acc => <xml>{acc} <div>{[show i]}</div></xml> ) <xml></xml> m.Groups }		    
		  </div>
		  { resLToXml t }
		</xml>
*)
    (*
	fun resToXml (r : option matched)   =
	    case r of
		    None => <xml>none</xml>
		  | Some m =>
		    <xml>
		      <div>
			{ List.foldr (fn i acc => <xml>{acc} <div>{[show i]}</div></xml> ) <xml></xml> m.Groups }
		      </div>
		      </xml> *)
	fun dispL i =
	    List.foldr (fn e acc2 => <xml>{[case e of
						(str, tag) => str ^ " (" ^ (show tag) ^ ") "]} {acc2}</xml>) <xml></xml> i
    in
    return <xml>
      <body>
	<span>
	  (* {
	   List.foldr (fn i acc => <xml>{acc} <div>{[show i.Line]} = {resLToXml i.Result} </div></xml>) <xml></xml> lines2
	   } *)

{	  List.foldr (fn i acc => <xml><div>{dispL i}</div> {acc}</xml>) <xml></xml> decomposed }
	      
	</span>
      </body>
    </xml>
    end

