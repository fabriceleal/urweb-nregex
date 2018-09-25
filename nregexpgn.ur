open Nregex

datatype pgnTag =
	 Castle
       | LongCastle
       | Piece
       | PieceDesamb
       | PawnMove
       | MoveNbr
       | Result
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
				       | HeaderValue => "HeaderValue"
				       | Result => "Result")

type patternPgn = pattern pgnTag

type matchedPgn = matched pgnTag
		  
val testPgn = "[Event \"Reykjavik Open\"]
[Site \"Reykjavik, Iceland\"]
[Date \"????.??.??\"]
[Round \"8.3\"]
[White \"Sarin, Nihal\"]
[Black \"Vaibhav, Suri\"]
[Result \"1/2-1/2\"]
[ECO \"A29\"]
[PlyCount \"38\"]

1. c4 {[%emt 0:00:09]} Nf6 {[%emt 0:00:26]} 2. Nc3 {[%emt 0:00:05]} e5 {
[%emt 0:00:10]} 3. Nf3 {[%emt 0:00:24]} Nc6 {[%emt 0:00:06]} 4. g3 {[%emt 0:00:
04]} d5 {[%emt 0:00:09]} 5. cxd5 {[%emt 0:00:02]} Nxd5 {[%emt 0:00:07]} 6. Bg2
Nb6 {[%emt 0:00:21]} 7. O-O {[%emt 0:00:06]} Be7 {[%emt 0:00:05]} 8. a3 {
[%emt 0:00:15]} O-O {[%emt 0:00:11]} 9. b4 {[%emt 0:00:07]} Be6 {[%emt 0:00:27]
} 10. Rb1 {[%emt 0:00:12]} f6 {[%emt 0:00:24]} 11. b5 {[%emt 0:00:05]} Nd4 {
[%emt 0:00:04]} 12. e3 {[%emt 0:00:05]} Nxf3+ {[%emt 0:00:06]} 13. Bxf3 {
[%emt 0:00:02]} Qc8 {[%emt 0:00:19]} 14. Qc2 {[%emt 0:01:41]} Rd8 {[%emt 0:00:
10]} 15. d4 {[%emt 0:00:05]} Bf5 {[%emt 0:11:21]} 16. Qb3+ {[%emt 0:01:08]} Be6
{[%emt 0:00:39]} 17. Qc2 {[%emt 0:00:19]} Bf5 {[%emt 0:00:09]} 18. Qb3+ {
[%emt 0:00:01]} Be6 {[%emt 0:00:16]} 19. Qc2 {[%emt 0:00:02]} Bf5 {[%emt 0:02:
44]} 1/2-1/2
	"


(* any char that can be used in a header key *)
val anyLett = splitChs "abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUWVXYZ"
(* any char that can be used in a header value *)
val anyLettAndWs = splitChs "abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUWVXYZ?,.- "
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
	  (* result *)
	  (Group (Seq ((Eith ((Literal "1/2") :: (Literal "1") :: (Literal "0") :: [] )) ::
		              (Literal "-") ::
		       (Eith ((Literal "1/2") :: (Literal "1") :: (Literal "0") :: [] )) :: []), Result)) ::
          [] )

    

fun matchForStr str =
    { Line=str,
(*      Result = match str matchMoveTokens False, *)
      Result = matchAll str matchMoveTokens
    } (* EventTag *)


fun decomposePgnL lines = 
    let
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

    
fun decomposePgn pgn =
    decomposePgnL (splitAllLines pgn)
	
   
fun test () =
    let
	val decomposed = decomposePgn testPgn
	val merged = List.foldr List.append [] decomposed
	
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

(*		      </xml> *)
	fun dispL i =
	    List.foldr (fn e acc2 => <xml>{[case e of
						(str, tag) => str ^ " (" ^ (show tag) ^ ") "]} {acc2}</xml>) <xml></xml> i
*)
	fun disp e =
	    <xml>{[case e of (str, tag) => str ^ " (" ^ (show tag) ^ ") "]} </xml>
    in
    return <xml>
      <body>
	<span>
	  (* {
	   List.foldr (fn i acc => <xml>{acc} <div>{[show i.Line]} = {resLToXml i.Result} </div></xml>) <xml></xml> lines2
	   } *)

{	  List.foldr (fn i acc => <xml><div>{disp i}</div> {acc}</xml>) <xml></xml> merged }
	      
	</span>
      </body>
    </xml>
    end

