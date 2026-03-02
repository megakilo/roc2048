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

# Functions

check_board : Board -> BoardState
check_board = |board|
    if List.any([L, R, U, D], |d| (move(board, d)).board != board) then
        HasMove
    else
        GameOver

set_value : Board, Coordinate, Cell -> Board
set_value = |board, (x, y), t|
    List.update(board, x, |row| List.update(row, y, |_| t))

transpose : Board -> Board
transpose = |list_of_lists|
    init = List.repeat([], dimension)
    List.walk(
        list_of_lists,
        init,
        |state, row|
            List.map2(state, row, |col, e| List.append(col, e)),
    )

move : Board, Direction -> { board : Board, score : U64 }
move = |board, d|
    when d is
        NoOp | Quit | Restart -> { board, score: 0 }
        L ->
            fill_empty = |xs| List.concat(xs, List.repeat(0, (dimension - List.len(xs))))
            merge_left = |input, score|
                when input is
                    [x, y, .. as xs] ->
                        if x == y then
                            res = merge_left(xs, score + Num.to_u64(2 * x))
                            { list: List.prepend(res.list, 2 * x), score: res.score }
                        else
                            res = merge_left(List.prepend(xs, y), score)
                            { list: List.prepend(res.list, x), score: res.score }

                    _ -> { list: input, score: score }

            List.walk(
                board,
                { board: [], score: 0 },
                |state, row|
                    res = List.keep_if(row, |x| x != 0) |> merge_left(0)
                    { board: List.append(state.board, fill_empty(res.list)), score: state.score + res.score },
            )

        R ->
            res = board |> List.map(List.reverse) |> move(L)
            { board: res.board |> List.map(List.reverse), score: res.score }

        U ->
            res = board |> transpose |> move(L)
            { board: res.board |> transpose, score: res.score }

        D ->
            res = board |> transpose |> move(R)
            { board: res.board |> transpose, score: res.score }

empty_cells : Board -> List Coordinate
empty_cells = |board|
    List.walk_with_index(
        board,
        [],
        |result, row, i|
            List.walk_with_index(
                row,
                result,
                |filtered, t, j|
                    if t == 0 then
                        List.append(filtered, (i, j))
                    else
                        filtered,
            ),
    )

max_tile : Board -> Cell
max_tile = |board|
    List.walk(
        board,
        0,
        |best, row|
            List.walk(
                row,
                best,
                Num.max,
            ),
    )

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
