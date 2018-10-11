
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

val mk_link_part_eq : eq mkLinkPart

val decomposeMk : string -> list (list (string * mkTag))

val decomposeUrlRaw : string -> option (list (string * mkLinkPart))
