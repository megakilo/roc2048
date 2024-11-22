app [main] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.16.0/O00IPk-Krg_diNS2dVWlI0ZQP794Vctxzv0ha96mK0E.tar.br",
    rand: "https://github.com/lukewilliamboswell/roc-random/releases/download/0.4.0/Ai2KfHOqOYXZmwdHX3g3ytbOUjTmZQmy0G2R9NuPBP0.tar.br",
}

import pf.Stdout
import pf.Cmd
import pf.Utc
import pf.Stdin
import Roc2048 exposing [Board, drawBoard, move, getDirection, checkBoard, setValue, emptyCells, emptyBoard]
import rand.Random exposing [Generator, State]

GameState : {
    board : Board,
    rndgen : Generator I32,
    seed : State,
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
        idx = (Num.toU64 rand2.value) % (List.len locs)
        loc = List.get locs idx |> Result.withDefault (0, 0)
        { board: setValue board loc num, rndgen, seed: rand2.state }

playGame = \state ->
    Cmd.new "clear" |> Cmd.status!
    Stdout.line! (drawBoard state.board)
    when checkBoard state.board is
        HitGoal -> Stdout.line "You Win!" |> Task.await \_ -> Task.ok (Done {})
        GameOver -> Stdout.line "Game Over!" |> Task.await \_ -> Task.ok (Done {})
        HasMove ->
            x = Stdin.bytes! {}
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
    Cmd.new "stty" |> Cmd.args ["-echo", "-icanon"] |> Cmd.status!
    ts = Utc.now! {} |> Utc.toMillisSinceEpoch |> Num.toU32
    seed = Random.seed ts
    rndgen = Random.boundedI32 1 Num.maxI32
    initGame = { board: emptyBoard, rndgen, seed } |> addNumber |> addNumber
    Task.loop! initGame playGame
    Cmd.new "stty" |> Cmd.args ["echo", "icanon"] |> Cmd.status!
    Task.ok {}
