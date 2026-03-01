app [main!] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.20.0/X73hGh05nNTkDHU06FHC0YfFaQB1pimX7gncRcao5mU.tar.br",
    rand: "https://github.com/lukewilliamboswell/roc-random/releases/download/0.5.0/yDUoWipuyNeJ-euaij4w_ozQCWtxCsywj68H0PlJAdE.tar.br",
}

import pf.Stdout
import pf.Cmd
import pf.Utc
import pf.Stdin
import pf.Tty
import Roc2048 exposing [Board, draw_board, move, get_direction, check_board, set_value, empty_cells, empty_board]
import rand.Random exposing [Generator, State]

GameState : {
    board : Board,
    rndgen : Generator I32,
    seed : State,
}

add_number : GameState -> GameState
add_number = |{ board, rndgen, seed }|
    locs = empty_cells(board)
    if List.is_empty(locs) then
        { board, rndgen, seed }
    else
        rand1 = rndgen(seed)
        num = if rand1.value % 10 < 9 then 2 else 4
        rand2 = rndgen(rand1.state)
        idx = Num.to_u64(rand2.value) % List.len(locs)
        loc = List.get(locs, idx) ?? (0, 0)
        { board: set_value(board, loc, num), rndgen, seed: rand2.state }

game_loop! = |state|
    Cmd.new("clear") |> Cmd.exec_cmd!()?
    Stdout.line!(draw_board(state.board))?
    when check_board(state.board) is
        HitGoal ->
            Stdout.line!("You Win!")?
            Ok(Done({}))

        GameOver ->
            Stdout.line!("Game Over!")?
            Ok(Done({}))

        HasMove ->
            x = Stdin.bytes!({})?
            input = List.first(x) ?? 0
            d = get_direction(input)
            if d == NoOp then
                game_loop!(state)
            else if d == Quit then
                Ok(Done({}))
            else
                new_board = move(state.board, d)
                if new_board == state.board then
                    game_loop!(state)
                else
                    game_loop!({ state & board: new_board } |> add_number)

main! = |_|
    Tty.enable_raw_mode!({})
    ts = Utc.now!({}) |> Utc.to_millis_since_epoch |> Num.to_u32
    seed = Random.seed(ts)
    rndgen = Random.bounded_i32(1, Num.max_i32)
    init_state = { board: empty_board, rndgen, seed } |> add_number |> add_number
    _ = game_loop!(init_state)?
    Tty.disable_raw_mode!({})
    Ok({})
