module Node = struct
  
  type node = {
    row : int;
    col : int;
    typ :string; (*typ will be like "wall" "item" "open"*)
    symbol : string;
    path : node list;
    f_score : int;
  }

  (**[get_neighbors n grid] is a list of viable nodes in [grid] that the astar 
    algo is able to traverse to from node [n]*)
  let get_neighbors (n : node) (grid : node list list) : node list = 
    let neighbors = ref [] in
    if n.row > 0 then 
      neighbors := !neighbors @ [List.nth (List.nth grid (n.row - 1)) n.col];
    if n.col > 0 then 
      neighbors := !neighbors @ [List.nth (List.nth grid n.row) (n.col - 1)];
    if n.row < List.length grid - 1 then 
      neighbors := !neighbors @ [List.nth (List.nth grid (n.row + 1)) n.col];
    if n.col < List.length (List.nth grid 0) - 1 then 
      neighbors := !neighbors @ [List.nth (List.nth grid n.row) (n.col + 1)];
    !neighbors

  (** [get_min_node open_set f_score] is the node in [open_set]
  that has the smallest score in [f_score]*)
  let get_min_node (open_set : node list) (f_score : (node * int) list) : node = 
    let rec get_min_node_help os cur_min cur_min_score : node = 
      match os with
      | [] -> cur_min
      | v::t when List.assoc v f_score  < cur_min_score 
                      -> get_min_node_help t v (List.assoc v f_score)
      | _::t -> get_min_node_help t cur_min cur_min_score in
    get_min_node_help open_set (List.nth open_set 0) 100000000

  (** [h_score s e] is the manhatten distance from start node [s] to end node [e]*)
  let h_score (s : node) (e : node) : int = 
    if s.col - e.col < 0 && s.row - e.row < 0 then 
      e.col - s.col + e.row - s.row
    else if s.col - e.col < 0 then s.row - e.row + e.col - s.col
    else s.row - e.row + s.col - e.col

end