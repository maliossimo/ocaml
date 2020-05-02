type tree = Empty | Leaf of char | Node of char * tree list ;;

let stringToCharList str =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (str.[i] :: l) in
  exp (String.length str - 1) []
;;

let rec charListToNodes charList =
  match charList with
    | [] -> Empty
    | h::[] -> Leaf h
    | h::t -> Node(h, charListToNodes t :: [])
;;

let rec addCharToNode c children =
match children with
  | [] -> Leaf c :: []
  | h::t -> match h with
    | Leaf ch -> if c == ch then children else addCharToNode c t
    | _ -> addCharToNode c t;;
;;

let rec addCharListToTree charList tree =
match (charList, tree) with
  | (_, Node(' ', children)) -> Node(' ', addCharListToNodes charList children)
  | ([], _) -> tree
  | (h::[], Empty) -> Leaf h
  | (h::[], Leaf c) -> Node(c, (Leaf h)::[])
  | (h::[], Node(c, children)) -> Node(c, addCharToNode h children)
  | (h::t, Empty) -> charListToNodes charList
  | (h::t, Leaf c) -> Node(c, (charListToNodes charList)::[])
  | (h::t, Node(c, children)) -> Node(c, addCharListToNodes charList children)
  and addCharListToNodes charList nodesList =
    match (charList, nodesList) with
      | (h::t, []) -> let sd = Leaf h in let el = addCharListToTree t sd in el :: []
      | (h::t, n :: dq) -> begin
        match n with
          | Node(c, ch) -> if h == c then addCharListToTree t n :: dq else n :: addCharListToNodes charList dq
          | _ -> addCharListToNodes charList dq
        end
      | (_, _) -> nodesList
;;

let rec buildTree stringList = 
  List.fold_left(fun acc -> fun elem -> addCharListToTree (stringToCharList elem) acc) (Node (' ', [])) stringList ;;
;;

let read_file filename = 
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines 
;;

let stringList = ["gat"; "tt"; "aca"; "ga"] ;;
let tree = buildTree stringList ;;