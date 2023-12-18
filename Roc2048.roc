interface Roc2048
    exposes [Board, drawBoard, checkBoard, setValue, update, getDirection, emptyCells, emptyBoard]
    imports []

Cell : I32
Board : List (List Cell)
Direction : [NoOp, Quit, L, D, U, R]
Coordinate : (Nat, Nat)
BoardState : [HasMove, HitGoal, GameOver]

# Game Constants
goal = 2048
dimension = 4
cellWidth = 2 + Str.countGraphemes (Num.toStr goal)

divideLine : Str
divideLine = Str.repeat "-" ((cellWidth + 1) * dimension + 1) |> Str.concat "\n"

emptyBoard : Board
emptyBoard = 0 |> List.repeat dimension |> List.repeat dimension

# Functions
drawCell : Cell -> Str
drawCell = \cell ->
    if cell == 0 then
        Str.repeat " " cellWidth
    else
        len = Str.countGraphemes (Num.toStr cell)
        pre = (cellWidth + 1 - len) // 2
        post = cellWidth - pre - len
        spaces = \n -> Str.repeat " " n
        "\(spaces pre)\(Num.toStr cell)\(spaces post)"

drawBoard : Board -> Str
drawBoard = \board ->
    body =
        board
        |> List.map \row ->
            row |> List.map drawCell |> List.intersperse "|" |> Str.joinWith ""
        |> List.intersperse "|\n\(divideLine)|"
        |> Str.joinWith ""
    "\(divideLine)|\(body)|\n\(divideLine)"

checkBoard : Board -> BoardState
checkBoard = \board ->
    if board |> List.any (\row -> row |> List.any (\x -> x == goal)) then
        HitGoal
    else if [L, R, U, D] |> List.any \d -> update board d != board then
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
        97 -> L # a in ASCII
        115 -> D # s
        119 -> U # w
        100 -> R # d
        113 -> Quit # q
        _ -> NoOp

transpose : Board -> Board
transpose = \listOfLists ->
    listOfLists
    |> List.walk (List.repeat [] dimension) \state, row ->
        List.map2 state row \col, e -> List.append col e

update : Board, Direction -> Board
update = \board, d ->
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

        R -> board |> List.map List.reverse |> update L |> List.map List.reverse
        U -> board |> transpose |> update L |> transpose
        D -> board |> transpose |> update R |> transpose

emptyCells : Board -> List Coordinate
emptyCells = \board ->
    result, row, i <- board |> List.walkWithIndex []
    filtered, t, j <- row |> List.walkWithIndex result
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
