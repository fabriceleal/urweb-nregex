
fun splitChs s =
    let
	val l = strlen s
    in
	if l = 0 then
	    []
	else
	    (strsub s 0) :: (splitChs (substring s 1 (l - 1)))
    end

fun joinChs cs =
    case cs of
	[] => ""
      | h :: t => (show h) ^ (joinChs t)

val lit = "á"
val l = splitChs lit
val j = joinChs l
	
fun index () =
return <xml>
  <body>
    <div>Original: <br />{[lit]}</div>
    <div>Joined back together: <br />{[j]}</div>
    <table>
      <tr>
	<th>show</th>
	<th>str1</th>
	<th>ord</th>
	<th>isalnum</th>
(*	<th>isalpha</th>
	<th>isdigit</th>
	<th>isblank</th>
	<th>isprint</th>
	<th>isgraph</th>
	<th>iscntrl</th>
	<th>islower</th>
	<th>isupper</th>
	<th>isspace</th>	
	<th>ispunct</th>
	<th>isxdigit</th>*)
      </tr>

      {List.foldr (fn e acc => <xml>
	<tr>
	  <td>{[show e]}</td>
	  <td>{[str1 e]}</td>
	  <td>{[show (ord e)]}</td>
	  <td>{[show (isalnum e)]}</td>
(*	  <td>{[show (isalpha e)]}</td>
	  <td>{[show (isdigit e)]}</td>
	  <td>{[show (isblank e)]}</td>
	  <td>{[show (isprint e)]}</td>
	  <td>{[show (isgraph e)]}</td>
	  <td>{[show (iscntrl e)]}</td>
	  <td>{[show (islower e)]}</td>
	  <td>{[show (isupper e)]}</td>
	  <td>{[show (isspace e)]}</td>
	  <td>{[show (ispunct e)]}</td>
	  <td>{[show (isxdigit e)]}</td>*)
      </tr>{acc}</xml>) <xml></xml> l}
      
    </table>
  </body>
  </xml>
