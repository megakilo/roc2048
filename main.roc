app [main!] {
    pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.20.0/X73hGh05nNTkDHU06FHC0YfFaQB1pimX7gncRcao5mU.tar.br",
    rand: "https://github.com/lukewilliamboswell/roc-random/releases/download/0.5.0/yDUoWipuyNeJ-euaij4w_ozQCWtxCsywj68H0PlJAdE.tar.br",
}

import pf.Stdout
import pf.Utc
import pf.Stdin
import pf.Tty
import Roc2048 exposing [Board, Direction, move, check_board, set_value, empty_cells, empty_board, max_tile]
import TerminalUi exposing [draw_board, draw_status]
import rand.Random exposing [Generator, State]

GameState : {
    board : Board,
    rndgen : Generator U32,
    seed : State,
    moves : U64,
    score : U64,
    won : Bool,
}

goal = Num.pow_int(2, 11) # 2048

add_number : GameState -> GameState
add_number = |state|
    locs = empty_cells(state.board)
    if List.is_empty(locs) then
        state
    else
        rand1 = state.rndgen(state.seed)
        num = if rand1.value % 10 < 9 then 2 else 4
        rand2 = state.rndgen(rand1.state)
        idx = Num.to_u64(rand2.value) % List.len(locs)
        loc = List.get(locs, idx) ?? (0, 0)
        { state & board: set_value(state.board, loc, num), seed: rand2.state }

new_game : Generator U32, State -> GameState
new_game = |rndgen, seed|
    { board: empty_board, rndgen, seed, moves: 0, score: 0, won: Bool.false } |> add_number |> add_number

parse_direction = |bytes|
    when bytes is
        [27, 91, 'A', ..] -> U
        [27, 91, 'B', ..] -> D
        [27, 91, 'C', ..] -> R
        [27, 91, 'D', ..] -> L
        [first, ..] ->
            when first is
                'a' | 'h' | 'H' -> L
                's' | 'j' | 'J' -> D
                'w' | 'k' | 'K' -> U
                'd' | 'l' | 'L' -> R
                'q' | 'Q' -> Quit
                'r' | 'R' -> Restart
                _ -> NoOp

        _ -> NoOp

game_loop! = |state|
    Stdout.write!("\u(001B)[H\u(001B)[2J")?
    top = max_tile(state.board)
    over = check_board(state.board) == GameOver

    msg =
        if top >= goal and !state.won then
            "You reached ${Num.to_str(goal)}! Press c to continue, r to restart, or q to quit."
        else if over then
            "No more moves. Press r to restart or q to quit."
        else
            "WASD/HJKL/Arrows to move, r to restart, q to quit."

    Stdout.write!("${msg}\r\n${draw_status(state.score, state.moves, state.board)}\r\n${draw_board(state.board)}")?

    input = Stdin.bytes!({})?
    d = parse_direction(input)

    when d is
        Quit -> Ok(Done({}))
        Restart -> game_loop!(new_game(state.rndgen, state.seed))
        _ ->
            if top >= goal and !state.won then
                when List.first(input) ?? 0 is
                    'c' | 'C' -> game_loop!({ state & won: Bool.true })
                    _ -> game_loop!(state)
            else if over then
                game_loop!(state)
            else
                when d is
                    NoOp -> game_loop!(state)
                    _ ->
                        res = move(state.board, d)
                        if res.board == state.board then
                            game_loop!(state)
                        else
                            game_loop!({ state & board: res.board, moves: state.moves + 1, score: state.score + res.score } |> add_number)

main! = |_|
    Tty.enable_raw_mode!({})
    ts = Utc.now!({}) |> Utc.to_millis_since_epoch |> Num.to_u32
    seed = Random.seed(ts)
    rndgen = Random.bounded_u32(0, Num.max_u32)
    _ = game_loop!(new_game(rndgen, seed))?
    Tty.disable_raw_mode!({})
    Ok({})
