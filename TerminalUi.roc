module [draw_board, draw_status]

import Roc2048 exposing [Board, empty_cells, max_tile]

LineType : [Top, Middle, Bottom]

draw_line : U64, U64 -> (LineType -> Str)
draw_line = |cell_width, dimension|
    |line_type|
        (left, mid, right) =
            when line_type is
                Top -> ("┌", "┬", "┐")
                Middle -> ("├", "┼", "┤")
                Bottom -> ("└", "┴", "┘")
        body = Str.repeat("─", cell_width) |> List.repeat(dimension) |> List.intersperse(mid) |> Str.join_with("")
        "${left}${body}${right}\r\n"

get_color = |cell|
    if cell <= 4 then "\u(001b)[37m"
    else if cell <= 16 then "\u(001b)[33m"
    else if cell <= 64 then "\u(001b)[38;5;208m"
    else if cell <= 256 then "\u(001b)[31m"
    else if cell <= 1024 then "\u(001b)[38;5;198m"
    else "\u(001b)[32m"

draw_cell : U64 -> (I32 -> Str)
draw_cell = |width|
    |cell|
        if cell == 0 then Str.repeat(" ", width)
        else
            s = Num.to_str(cell)
            len = Str.count_utf8_bytes(s)
            pre = (width + 1 - len) // 2
            post = width - pre - len
            "${Str.repeat(" ", pre)}${get_color(cell)}${s}\u(001b)[0m${Str.repeat(" ", post)}"

draw_board : Board -> Str
draw_board = |board|
    dim = List.len(board)
    width = 2 + Str.count_utf8_bytes(Num.to_str(Num.max(max_tile(board), 2048)))
    render_cell = draw_cell(width)
    line = draw_line(width, dim)

    rows =
        board
        |> List.map(|row| List.map(row, render_cell) |> List.intersperse("│") |> Str.join_with(""))
        |> List.intersperse("│\r\n${line(Middle)}│")
        |> Str.join_with("")

    "${line(Top)}│${rows}│\r\n${line(Bottom)}"

draw_status : U64, U64, Board -> Str
draw_status = |score, moves, board|
    "Score: ${Num.to_str(score)}  Moves: ${Num.to_str(moves)}  Max: ${Num.to_str(max_tile(board))}  Open: ${Num.to_str(List.len(empty_cells(board)))}"
