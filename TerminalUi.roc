module [draw_board, draw_status]

import Roc2048 exposing [Board, empty_cells, max_tile]

LineType : [Top, Middle, Bottom]

draw_line : U64, U64 -> (LineType -> Str)
draw_line = |cell_width, dimension|
    |line_type|
        (left_end, connector, right_end) =
            when line_type is
                Top -> ("┌", "┬", "┐")
                Middle -> ("├", "┼", "┤")
                Bottom -> ("└", "┴", "┘")

        body = Str.repeat("─", cell_width) |> List.repeat(dimension) |> List.intersperse(connector) |> Str.join_with("")
        "${left_end}${body}${right_end}\r\n"

draw_cell : U64 -> (I32 -> Str)
draw_cell = |cell_width|
    |cell|
        if cell == 0 then
            Str.repeat(" ", cell_width)
        else
            len = Str.count_utf8_bytes(Num.to_str(cell))
            pre = (cell_width + 1 - len) // 2
            post = cell_width - pre - len
            spaces = |n| Str.repeat(" ", n)

            color_code =
                if cell <= 4 then
                    "\u(001b)[37m"
                else if cell <= 16 then
                    "\u(001b)[33m"
                else if cell <= 64 then
                    "\u(001b)[38;5;208m"
                else if cell <= 256 then
                    "\u(001b)[31m"
                else if cell <= 1024 then
                    "\u(001b)[38;5;198m"
                else
                    "\u(001b)[32m"
            reset = "\u(001b)[0m"

            "${spaces(pre)}${color_code}${Num.to_str(cell)}${reset}${spaces(post)}"

draw_board : Board -> Str
draw_board = |board|
    dimension = List.len(board)
    cell_width = 2 + Str.count_utf8_bytes(Num.to_str(Num.max(max_tile(board), 2048)))
    draw_cell_fn = draw_cell(cell_width)
    draw_line_fn = draw_line(cell_width, dimension)

    body =
        board
        |> List.map(
            |row|
                List.map(row, draw_cell_fn) |> List.intersperse("│") |> Str.join_with(""),
        )
        |> List.intersperse("│\r\n${draw_line_fn(Middle)}│")
        |> Str.join_with("")

    "${draw_line_fn(Top)}│${body}│\r\n${draw_line_fn(Bottom)}"

draw_status : U64, U64, Board -> Str
draw_status = |score, moves, board|
    open_cells = List.len(empty_cells(board))
    top_tile = max_tile(board)
    "Score: ${Num.to_str(score)}  Moves: ${Num.to_str(moves)}  Max: ${Num.to_str(top_tile)}  Open: ${Num.to_str(open_cells)}"
