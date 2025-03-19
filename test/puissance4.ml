module G = Gamestate

let game_test () =
  let b = G.make ~cols:10 ~rows:10 in
  let b = G.play b 1 8 G.P2 in
  let b = G.play b 2 8 G.P2 in
  let b = G.play b 3 8 G.P2 in
  let b = G.play b 4 8 G.P2 in

  let b = G.play b 0 1 G.P1 in
  let b = G.play b 0 2 G.P1 in
  let b = G.play b 0 3 G.P1 in
  let b = G.play b 0 4 G.P1 in
  Format.printf "%a\n" Display.pp b ;
  match G.search_winning b with
  | None -> Format.printf "No winning position found\n%!"
  | Some (G.P1, c, r) -> Format.printf "P1 wins at %d x %d\n%!" c r
  | Some (G.P2, c, r) -> Format.printf "P2 wins at %d x %d\n%!" c r

(* Monadic version *)

let position_equal (a, b) (a', b') = Int.equal a a' && Int.equal b b'

module State = struct
  type 'a t = State : Gamestate.player * Gamestate.t -> (int * int) t

  let equal_choice_type : type a b. a t -> b t -> (a, b) Mcts.refl_eq option =
   fun (State _) (State _) -> Some Mcts.Refl

  let equal_choice : type a. a t -> a -> a -> bool =
   fun (State _) p1 p2 -> position_equal p1 p2

  let hash_choice : type a. a t -> a -> int =
   fun (State _) pos -> Hashtbl.hash pos
end

module M = Mcts.Ucb1 (State)

let next_player = function G.P1 -> G.P2 | G.P2 -> G.P1

type tree = Gamestate.player M.t

let rec game_tree player state : tree =
  let open M in
  let playable_positions = G.playable_positions state in
  match playable_positions with
  | [] -> return (next_player player)
  | _ -> (
      choices (State.State (player, state)) (Array.of_list playable_positions)
      >>= fun (x, y) ->
      let b = G.play state x y player in
      match G.search_winning state with
      | None -> game_tree (next_player player) b
      | Some (player, _, _) -> return player)

let reward (player, _) winner = if player = winner then 1. else 0.

(* let get_human_input state =
 *   Format.printf "%a@." Display.pp state ;
 *   Format.printf "please input play position: col row = ?@." ;
 *   let scanner = Scanf.Scanning.from_channel stdin in
 *   Scanf.bscanf scanner "%d %d" (fun x y -> (x, y)) *)

(* let index_of array elt =
 *   let exception Found of int in
 *   try
 *     for i = 0 to Array.length array - 1 do
 *       if position_equal elt array.(i) then raise (Found i)
 *     done ;
 *     None
 *   with Found i -> Some i *)

let pp_player fmtr p =
  Format.pp_print_string fmtr (match p with G.P1 -> "P1" | G.P2 -> "P2")

(* direct style *)
let game_loop () =
  let initial_player = G.P1 in
  let initial_state = G.make ~cols:5 ~rows:5 in
  let tree = game_tree initial_player initial_state in
  M.run
    tree
    { choice =
        (fun (type a) (State (_player, state) : a State.t) (_ : a array) ->
          Format.printf "%a@." Display.pp state ;
          `Auto 10_000)
    }
    { reward =
        (fun (type a) (State (player, _) : a State.t) winner ->
          if player = winner then 1. else 0.)
    }
    M.uniform
    (Random.State.make [| 0x1337; 0x533D |])

(* let () =
 *   match game_loop_monadic () with
 *   | G.P1 -> Format.printf "P1 wins@."
 *   | G.P2 -> Format.printf "P2 wins@." *)

let debug = false

(* incremental style *)
let game_loop_incremental () =
  let initial_player = G.P1 in
  let initial_state = G.make ~cols:5 ~rows:5 in
  let tree = game_tree initial_player initial_state in
  let reward =
    { M.reward =
        (fun (type a) (State (player, _) : a State.t) winner ->
          if player = winner then 1. else 0.)
    }
  in
  let game =
    M.run_incremental tree reward (Random.State.make [| 0x1337; 0x533D |])
  in
  let rec loop game =
    match game with
    | M.Interaction (State.State (player, board), _, k) -> (
        if debug then Format.printf "%a@." Display.pp board ;
        match player with
        | G.P1 -> loop @@ k (`Auto (10_000, M.uniform))
        | G.P2 -> loop @@ k (`Auto (10_000, M.uniform)))
    | M.Terminated winner -> winner
  in
  (* we don't handle draws properly... *)
  loop game

let () = assert (game_loop_incremental () = G.P1)
