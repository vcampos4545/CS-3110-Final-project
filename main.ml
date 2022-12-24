open Grid

let grid = init_grid 4 4

let start = List.nth (List.nth grid 0) 0

let en = List.nth (List.nth grid 3) 3

let path = astar start en grid