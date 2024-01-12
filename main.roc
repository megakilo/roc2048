app "roc2048"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        rand: "https://github.com/lukewilliamboswell/roc-random/releases/download/0.1.0/OoD8jmqBLc0gyuaadckDMx1jedEa03EdGSR_V4KhH7g.tar.br",
    }
    imports [pf.Stdout, pf.Cmd, pf.Utc, pf.Task, pf.Stdin, Roc2048.{ Board, BoardState, drawBoard, move, getDirection, checkBoard, setValue, emptyCells, emptyBoard }, rand.Random.{ Generator, State }]
    provides [main] to pf

GameState : {
    board : Board,
    rndgen : Generator U32 I32,
    seed : State U32,
}

addNumber : GameState -> GameState
addNumber = \{ board, rndgen, seed } ->
    locs = emptyCells board
    if List.isEmpty locs then
        { board, rndgen, seed }
    else
        rand1 = rndgen seed
        num = if rand1.value % 10 < 9 then 2 else 4
        rand2 = rndgen rand1.state
        idx = (Num.toNat rand2.value) % (List.len locs)
        loc = List.get locs idx |> Result.withDefault (0, 0)
        { board: setValue board loc num, rndgen, seed: rand2.state }

playGame = \state ->
    _ <- Cmd.new "clear" |> Cmd.status |> Task.attempt
    _ <- Stdout.line (drawBoard state.board) |> Task.await
    when checkBoard state.board is
        HitGoal -> Stdout.line "You Win!" |> Task.await \_ -> Task.ok (Done {})
        GameOver -> Stdout.line "Game Over!" |> Task.await \_ -> Task.ok (Done {})
        HasMove ->
            x <- Stdin.bytes |> Task.await
            input = List.first x |> Result.withDefault 0
            d = getDirection input
            if d == NoOp then
                Task.ok (Step state)
            else if d == Quit then
                Task.ok (Done {})
            else
                newBoard = move state.board d
                if newBoard == state.board then
                    Task.ok (Step state)
                else
                    Task.ok (Step ({ state & board: newBoard } |> addNumber))

main =
    _ <- Cmd.new "stty" |> Cmd.args ["-echo", "-icanon"] |> Cmd.status |> Task.attempt
    ts <- Utc.now |> Task.map Utc.toMillisSinceEpoch |> Task.map Num.toU32 |> Task.await
    seed = Random.seed ts
    rndgen = Random.i32 1 Num.maxI32
    initGame = { board: emptyBoard, rndgen, seed } |> addNumber |> addNumber
    _ <- Task.loop initGame playGame |> Task.await
    _ <- Cmd.new "stty" |> Cmd.args ["echo", "icanon"] |> Cmd.status |> Task.attempt
    Task.ok {}
