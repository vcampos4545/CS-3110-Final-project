open OUnit2

type node = {
  row : int;
  col : int;
  typ :string; (*typ will be like "wall" "item" "open"*)
  symbol : string;
}

exception PathFailure of string

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

let simple_grid = init_grid 10 10 


let suite =
  "test suite for A2"
  >::: List.flatten [ (*add suites here *) ]

let _ = run_test_tt_main suite
