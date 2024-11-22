module [Board, drawBoard, checkBoard, setValue, move, getDirection, emptyCells, emptyBoard]

Cell : I32
Board : List (List Cell)
Direction : [NoOp, Quit, L, D, U, R]
Coordinate : (U64, U64)
BoardState : [HasMove, HitGoal, GameOver]
LineType : [Top, Middle, Bottom]

# Game Constants
goal = 2048
dimension = 4
cellWidth = 2 + Str.countUtf8Bytes (Num.toStr goal)

drawLine : LineType -> Str
drawLine = \lineType ->
    (leftEnd, connector, rightEnd) =
        when lineType is
            Top -> ("┌", "┬", "┐")
            Middle -> ("├", "┼", "┤")
            Bottom -> ("└", "┴", "┘")
    body = Str.repeat "─" cellWidth |> List.repeat dimension |> List.intersperse connector |> Str.joinWith ""
    "$(leftEnd)$(body)$(rightEnd)\n"

emptyBoard : Board
emptyBoard = 0 |> List.repeat dimension |> List.repeat dimension

# Functions
drawCell : Cell -> Str
drawCell = \cell ->
    if cell == 0 then
        Str.repeat " " cellWidth
    else
        len = Str.countUtf8Bytes (Num.toStr cell)
        pre = (cellWidth + 1 - len) // 2
        post = cellWidth - pre - len
        spaces = \n -> Str.repeat " " n
        "$(spaces pre)$(Num.toStr cell)$(spaces post)"

drawBoard : Board -> Str
drawBoard = \board ->
    body =
        board
        |> List.map \row ->
            row |> List.map drawCell |> List.intersperse "│" |> Str.joinWith ""
        |> List.intersperse "│\n$(drawLine Middle)│"
        |> Str.joinWith ""
    "$(drawLine Top)│$(body)│\n$(drawLine Bottom)"

checkBoard : Board -> BoardState
checkBoard = \board ->
    if board |> List.any (\row -> row |> List.any (\x -> x == goal)) then
        HitGoal
    else if [L, R, U, D] |> List.any \d -> move board d != board then
        HasMove
    else
        GameOver

setValue : Board, Coordinate, Cell -> Board
setValue = \board, (x, y), t ->
    board
    |> List.update x \row ->
        row |> List.update y \_ -> t

getDirection : U8 -> Direction
getDirection = \x ->
    when x is
        'a' -> L
        's' -> D
        'w' -> U
        'd' -> R
        'q' -> Quit
        _ -> NoOp

transpose : Board -> Board
transpose = \listOfLists ->
    listOfLists
    |> List.walk (List.repeat [] dimension) \state, row ->
        List.map2 state row \col, e -> List.append col e

move : Board, Direction -> Board
move = \board, d ->
    when d is
        NoOp | Quit -> board
        L ->
            fillEmpty = \xs -> xs |> List.concat (List.repeat 0 (dimension - (List.len xs)))
            mergeLeft = \input ->
                when input is
                    [x, y, .. as xs] ->
                        if x == y then
                            mergeLeft xs |> List.prepend (2 * x)
                        else
                            mergeLeft (xs |> List.prepend y) |> List.prepend x

                    _ -> input
            board |> List.map (\row -> row |> List.keepIf (\x -> x != 0) |> mergeLeft |> fillEmpty)

        R -> board |> List.map List.reverse |> move L |> List.map List.reverse
        U -> board |> transpose |> move L |> transpose
        D -> board |> transpose |> move R |> transpose

emptyCells : Board -> List Coordinate
emptyCells = \board ->
    List.walkWithIndex board [] \result, row, i ->
        List.walkWithIndex row result \filtered, t, j ->
            if t == 0 then
                List.append filtered (i, j)
            else
                filtered

# Tests
expect
    afterMove = setValue emptyBoard (1, 2) 2
    transpose afterMove == [[0, 0, 0, 0], [0, 0, 0, 0], [0, 2, 0, 0], [0, 0, 0, 0]]

expect
    board = [[2, 0, 4, 4], [2, 2, 0, 4], [0, 2, 2, 4], [2, 4, 4, 0]]
    r = emptyCells board
    r == [(0, 1), (1, 2), (2, 0), (3, 3)]
