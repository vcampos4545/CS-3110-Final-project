module type Grid = sig

    type g
    type node
    
    val rows : int
    val cols : int
    val grid : node list list (*List of string lists, each string is the state of the cell*)
    val print : unit

    val init_grid : int -> int -> node list list
  
    val init_score_assoc : node list list -> (node * int) list
    
    val add_assoc : node -> 'a -> (node * 'a) list -> (node * 'a) list
    
    val h_score : node -> node -> int
    
    val contains : 'a -> 'a list -> bool
    
    val remove : 'a -> 'a list -> 'a list
    
    val reconstruct_path : (node * node) list -> node -> node -> node list
    
    val get_neighbors : node -> node list list -> node list
    
    val get_min_node : node list -> (node * int) list -> node
    
    val astar : node -> node -> node list list -> node list
end