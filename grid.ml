open Node

module Grid = struct
  type t
  let rows = 100
  let cols = 100
  let grid = failwith "Unimplamented"
  let print = failwith "Unimplamented"


  type cell = {
    typ : string;
    position : (int * int); (*Row, Col*)
  }

  (**[init_grid row col] is a 2d array of nodes with [col] nodes in a row
      and [row] rows in the array *)
  let init_grid row col = 
    let rec init_row r c acc : node list = 
    match c with 
    | -1 -> acc
    | _ -> init_row r (c-1) acc @ [{row=r;col=c;typ="none";symbol="#"}]
    in
    let rec init_cols r c acc : node list list = 
    match r with
    | -1 -> acc
    | _ -> init_cols (r-1) c acc @ [init_row r c []]
    in init_cols (row - 1) (col - 1) []
  let print_grid (grid : node list list) = 
    let rec concat_row (elems : node list) (acc : string) : string = 
      match elems with
      | [] -> acc ^ "\n"
      | h::t -> concat_row t (acc ^ h.symbol)
    in
    let rec concat_matrix (rows : node list list) (acc : string) : string =
      match rows with
      | [] -> acc
      | h::t -> concat_matrix t (acc ^ concat_row h "")
    in print_string (concat_matrix grid "")

    exception PathFailure of string


  (**[init_score_assoc grid] is the association list initialized for every 
      node in [grid] with the score being a large integer (infinity)*)
  let init_score_assoc (grid : node list list) : (node*int) list = 
    let assoc_list = ref [] in
    for i = 0 to List.length grid - 1 do
      let temp_row = ref [] in
      for j = 0 to List.length (List.nth grid i) - 1 do
        let n = List.nth (List.nth grid i) j in 
        temp_row := !temp_row @ [(n,1000000)];
      done;
      assoc_list := !assoc_list @ !temp_row;
    done;
    !assoc_list

  let add_assoc (n : node) (v : 'a) (al : (node*'a) list) : (node*'a) list = 
    let rec add_help = function
    | [] -> al @ [(n,v)]
    | (tn,_)::t when tn.row = n.row && tn.col = n.col -> (n,v)::t
    | h::t -> add_help t @ [h] in
    add_help al

  (** [contains elem lst] is a boolean indicating whether [elem] is in [lst]*)
  let rec contains elem = function
  | [] -> false
  | h::_ when h = elem -> true
  | _::t -> contains elem t

  (**[remove elem lst] is [lst] without the element [elem]*)
  let rec remove elem = function
  | [] -> []
  | h::t when h = elem -> t
  | h::t -> remove elem t @ [h]

  (**[reconstruct_path came_from e] is an ordered list of nodes from end [e]
    using association list [came_from]*)
  let reconstruct_path came_from (s : node) (e : node) : node list = 
    let rec reconstruct_help cur_node acc = 
      match cur_node with
      | n when n = s -> acc
      | _ -> 
        let next = List.assoc cur_node came_from in
        reconstruct_help next (next::acc) in
    reconstruct_help e [e]

  (**[astar s e] is an ordered list of nodes that represents the shortest 
      path from start node [s] to end node [e]*)
  let astar (s : node) (e : node) (grid : node list list) : node list = 
    let open_set = ref [s] in
    let came_from = ref [(s,s)] in
    let g_score = ref (init_score_assoc grid |> add_assoc s 0) in
    let f_score = ref (init_score_assoc grid |> add_assoc s (h_score s e)) in

    let rec loop cur =
      match cur with
      | cur when cur = e ->
        reconstruct_path !came_from s e
      | _ ->  
        open_set := remove cur !open_set;
        let neighbors = get_neighbors cur grid in 
        for i = 0 to List.length neighbors - 1 do
          let n = List.nth neighbors i in    
          let temp_g_score = (List.assoc cur !g_score) + 1 in
          if temp_g_score < List.assoc n !g_score then
            came_from := add_assoc n cur !came_from;
            g_score := add_assoc n temp_g_score !g_score;
            f_score := add_assoc n (temp_g_score + h_score n e) !f_score;
            if not (contains n !open_set) then open_set := !open_set @ [n];
        done;
        if List.length !open_set < 1 then raise (PathFailure "No path found");
        loop (get_min_node !open_set !f_score) in
    loop (get_min_node !open_set !f_score)

  end