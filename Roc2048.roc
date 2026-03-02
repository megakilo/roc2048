module [Board, Direction, check_board, set_value, move, empty_cells, empty_board, max_tile]

Cell : I32
Board : List (List Cell)
Direction : [NoOp, Quit, Restart, L, D, U, R]
Coordinate : (U64, U64)
BoardState : [HasMove, GameOver]

# Game Constants
dimension = 4

empty_board : Board
empty_board = List.repeat(List.repeat(0, dimension), dimension)

max_tile : Board -> Cell
max_tile = |board| List.walk(board, 0, |acc, row| List.walk(row, acc, Num.max))

empty_cells : Board -> List Coordinate
empty_cells = |board|
    List.walk_with_index(board, [], |acc, row, i|
        List.walk_with_index(row, acc, |inner, cell, j|
            if cell == 0 then List.append(inner, (i, j)) else inner
        )
    )

set_value : Board, Coordinate, Cell -> Board
set_value = |board, (x, y), val|
    List.update(board, x, |row| List.update(row, y, |_| val))

transpose : Board -> Board
transpose = |board|
    List.walk(board, List.repeat([], dimension), |acc, row|
        List.map2(acc, row, |col, cell| List.append(col, cell))
    )

check_board : Board -> BoardState
check_board = |board|
    if List.any([L, R, U, D], |d| (move(board, d)).board != board) then HasMove else GameOver

move : Board, Direction -> { board : Board, score : U64 }
move = |board, direction|
    when direction is
        L -> move_helper(board, |x| x, |x| x)
        R -> move_helper(board, |b| List.map(b, List.reverse), |b| List.map(b, List.reverse))
        U -> move_helper(board, transpose, transpose)
        D -> move_helper(board, |b| transpose(b) |> List.map(List.reverse), |b| List.map(b, List.reverse) |> transpose)
        _ -> { board, score: 0 }

move_helper = |board, transform, inverse|
    transformed = transform(board)
    results = List.map(transformed, move_row)
    {
        board: inverse(List.map(results, .row)),
        score: List.map(results, .score) |> List.sum,
    }

move_row = |row|
    { list, score } = row |> List.keep_if(|x| x != 0) |> merge_row(0)
    { row: List.concat(list, List.repeat(0, dimension - List.len(list))), score }

merge_row = |list, score|
    when list is
        [x, y, .. as rest] if x == y ->
            { list: next, score: s } = merge_row(rest, score + Num.to_u64(2 * x))
            { list: List.prepend(next, 2 * x), score: s }
        [x, y, .. as rest] ->
            { list: next, score: s } = merge_row(List.prepend(rest, y), score)
            { list: List.prepend(next, x), score: s }
        _ -> { list, score }

# Tests
expect
    afterMove = set_value(empty_board, (1, 2), 2)
    transpose(afterMove) == [[0, 0, 0, 0], [0, 0, 0, 0], [0, 2, 0, 0], [0, 0, 0, 0]]

expect
    board = [[2, 0, 4, 4], [2, 2, 0, 4], [0, 2, 2, 4], [2, 4, 4, 0]]
    r = empty_cells(board)
    r == [(0, 1), (1, 2), (2, 0), (3, 3)]

expect
    board = [[0, 0, 0, 2], [0, 0, 0, 2], [0, 0, 0, 0], [0, 0, 0, 0]]
    r = move(board, D)
    r.board == [[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 4]]

expect
    board = [[2, 2, 0, 0], [4, 4, 0, 0], [8, 8, 8, 8], [0, 0, 0, 0]]
    r = move(board, L)
    r.score == 4 + 8 + 16 + 16
