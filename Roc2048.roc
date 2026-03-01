module [Board, draw_board, check_board, set_value, move, get_direction, empty_cells, empty_board]

Cell : I32
Board : List (List Cell)
Direction : [NoOp, Quit, L, D, U, R]
Coordinate : (U64, U64)
BoardState : [HasMove, HitGoal, GameOver]
LineType : [Top, Middle, Bottom]

# Game Constants
goal = 2048
dimension = 4
cell_width = 2 + Str.count_utf8_bytes(Num.to_str(goal))

draw_line : LineType -> Str
draw_line = |line_type|
    (left_end, connector, right_end) =
        when line_type is
            Top -> ("┌", "┬", "┐")
            Middle -> ("├", "┼", "┤")
            Bottom -> ("└", "┴", "┘")
    body = Str.repeat("─", cell_width) |> List.repeat(dimension) |> List.intersperse(connector) |> Str.join_with("")
    "${left_end}${body}${right_end}\r\n"

empty_board : Board
empty_board = List.repeat(List.repeat(0, dimension), dimension)

# Functions
draw_cell : Cell -> Str
draw_cell = |cell|
    if cell == 0 then
        Str.repeat(" ", cell_width)
    else
        len = Str.count_utf8_bytes(Num.to_str(cell))
        pre = (cell_width + 1 - len) // 2
        post = cell_width - pre - len
        spaces = |n| Str.repeat(" ", n)
        "${spaces(pre)}${Num.to_str(cell)}${spaces(post)}"

draw_board : Board -> Str
draw_board = |board|
    body =
        board
        |> List.map(
            |row|
                List.map(row, draw_cell) |> List.intersperse("│") |> Str.join_with(""),
        )
        |> List.intersperse("│\r\n${draw_line(Middle)}│")
        |> Str.join_with("")
    "${draw_line(Top)}│${body}│\r\n${draw_line(Bottom)}"

check_board : Board -> BoardState
check_board = |board|
    if List.any(board, |row| List.any(row, |x| x == goal)) then
        HitGoal
    else if List.any([L, R, U, D], |d| move(board, d) != board) then
        HasMove
    else
        GameOver

set_value : Board, Coordinate, Cell -> Board
set_value = |board, (x, y), t|
    List.update(board, x, |row| List.update(row, y, |_| t))

get_direction : U8 -> Direction
get_direction = |x|
    when x is
        'a' -> L
        's' -> D
        'w' -> U
        'd' -> R
        'q' -> Quit
        _ -> NoOp

transpose : Board -> Board
transpose = |list_of_lists|
    init = List.repeat([], dimension)
    List.walk(
        list_of_lists,
        init,
        |state, row|
            List.map2(state, row, |col, e| List.append(col, e)),
    )

move : Board, Direction -> Board
move = |board, d|
    when d is
        NoOp | Quit -> board
        L ->
            fill_empty = |xs| List.concat(xs, List.repeat(0, (dimension - List.len(xs))))
            merge_left = |input|
                when input is
                    [x, y, .. as xs] ->
                        if x == y then
                            merge_left(xs) |> List.prepend(2 * x)
                        else
                            merge_left(List.prepend(xs, y)) |> List.prepend(x)

                    _ -> input
            List.map(board, |row| List.keep_if(row, |x| x != 0) |> merge_left |> fill_empty)

        R -> board |> List.map List.reverse |> move L |> List.map List.reverse
        U -> board |> transpose |> move(L) |> transpose
        D -> board |> transpose |> move(R) |> transpose

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
    r == [[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 4]]
