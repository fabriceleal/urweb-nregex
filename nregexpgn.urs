
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

val decomposePgn : string -> list (list (string * pgnTag))

val test : unit -> transaction page
