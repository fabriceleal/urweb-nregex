
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

val decomposePgn : string -> list (list (string * pgnTag))

val test : unit -> transaction page
