type player = P1 | P2

type state = player option array array

let create () = Array.make_matrix 3 3 None

let copy : state -> state =
 fun state -> Array.map (fun col -> Array.map (fun x -> x) col) state

let play : state -> player -> col:int -> row:int -> state =
 fun state cell ~col ~row ->
  assert (col >= 0 && col < 3) ;
  assert (row >= 0 && row < 3) ;
  assert (match state.(col).(row) with Some _ -> false | None -> true) ;
  let state = copy state in
  state.(col).(row) <- Some cell ;
  state

let pp_player fmtr = function
  | P1 -> Format.fprintf fmtr "X"
  | P2 -> Format.fprintf fmtr "O"

let pp_cell fmtr = function
  | None -> Format.fprintf fmtr " "
  | Some p -> Format.fprintf fmtr "%a" pp_player p

let pp fmtr state =
  for row = 0 to 2 do
    for col = 0 to 2 do
      Format.fprintf fmtr "%a" pp_cell state.(col).(row)
    done ;
    Format.fprintf fmtr "@."
  done

let list_free_positions state =
  let acc = ref [] in
  for row = 0 to 2 do
    for col = 0 to 2 do
      match state.(col).(row) with
      | None -> acc := (col, row) :: !acc
      | Some _ -> ()
    done
  done ;
  !acc

let check_winning_col state col =
  assert (col >= 0 && col < 3) ;
  let x1 = state.(col).(0) in
  let x2 = state.(col).(1) in
  let x3 = state.(col).(2) in
  match (x1, x2, x3) with
  | (Some p1, Some p2, Some p3) when p1 = p2 && p2 = p3 -> Some p1
  | _ -> None

let check_winning_row state row =
  assert (row >= 0 && row < 3) ;
  let x1 = state.(0).(row) in
  let x2 = state.(1).(row) in
  let x3 = state.(2).(row) in
  match (x1, x2, x3) with
  | (Some p1, Some p2, Some p3) when p1 = p2 && p2 = p3 -> Some p1
  | _ -> None

let check_winning_diag state =
  let x1 = state.(0).(0) in
  let x2 = state.(1).(1) in
  let x3 = state.(2).(2) in
  match (x1, x2, x3) with
  | (Some p1, Some p2, Some p3) when p1 = p2 && p2 = p3 -> Some p1
  | _ -> None

let check_winning_antidiag state =
  let x1 = state.(0).(2) in
  let x2 = state.(1).(1) in
  let x3 = state.(2).(0) in
  match (x1, x2, x3) with
  | (Some p1, Some p2, Some p3) when p1 = p2 && p2 = p3 -> Some p1
  | _ -> None

let ( >> ) a b = match a with None -> b | Some _ -> a

let search_winning state =
  check_winning_row state 0 >> check_winning_row state 1
  >> check_winning_row state 2 >> check_winning_col state 0
  >> check_winning_col state 1 >> check_winning_col state 2
  >> check_winning_diag state
  >> check_winning_antidiag state

let next_player = function P1 -> P2 | P2 -> P1

type outcome = P1_wins | P2_wins | Draw

module Positions = struct
  type t = int * int

  let equal (a, b) (a', b') = Int.equal a a' && Int.equal b b'

  let hash = Hashtbl.hash
end

module State = struct
  type 'a t = State : player * state -> Positions.t t

  let equal_choice_type : type a b. a t -> b t -> (a, b) Mcts.refl_eq option =
   fun (State _) (State _) -> Some Mcts.Refl

  let equal_choice : type a. a t -> a -> a -> bool =
   fun (State _) p1 p2 -> Positions.equal p1 p2

  let hash_choice : type a. a t -> a -> int =
   fun (State _) pos -> Positions.hash pos
end

module M = Mcts.Ucb1 (State)

let rec gametree player state =
  let open M in
  let positions = list_free_positions state in
  match positions with
  | [] -> return Draw
  | _ -> (
      choices (State (player, state)) (Array.of_list positions)
      >>= fun (col, row) ->
      let state = play state player ~col ~row in
      match search_winning state with
      | None -> gametree (next_player player) state
      | Some P1 -> return P1_wins
      | Some P2 -> return P2_wins)

let get_human_input state =
  Format.printf "%a@." pp state ;
  Format.printf "please input play position: col row = ?@." ;
  let scanner = Scanf.Scanning.from_channel stdin in
  Scanf.bscanf scanner "%d %d" (fun x y -> (x, y))

let index_of array elt =
  let exception Found of int in
  try
    for i = 0 to Array.length array - 1 do
      if Positions.equal elt array.(i) then raise (Found i)
    done ;
    None
  with Found i -> Some i

let game_loop next_human_move =
  let initial_player = P1 in
  let initial_state = create () in
  let tree = gametree initial_player initial_state in
  M.run
    tree
    { choice =
        (fun (type a) (state : a State.t) (alternatives : a array) ->
          let (State.State (player, state)) = state in
          match player with
          | P1 -> (
              let (x, y) = next_human_move state in
              match index_of alternatives (x, y) with
              | None -> assert false
              | Some i -> `Choose i)
          | P2 -> `Auto 1000)
    }
    { reward =
        (fun (type a) (state : a State.t) outcome ->
          let (State.State (player, _)) = state in
          match (player, outcome) with
          | (P1, P1_wins) -> 1.
          | (P2, P2_wins) -> 1.
          | (P1, P2_wins) -> 0.
          | (P2, P1_wins) -> 0.
          | (_, Draw) -> 0.5)
    }
    M.uniform
    (Random.State.make [| 0x1337; 0x533D |])

(* Uncomment the following to play interactively *)
(*
let () =
  match game_loop get_human_input with
  | P1_wins -> Format.printf "P1 wins@."
  | P2_wins -> Format.printf "P2 wins@."
  | Draw -> Format.printf "Draw@."
*)

type 'a stream = Next of 'a * (unit -> 'a stream) | Stop

let rec of_list = function
  | [] -> Stop
  | hd :: tl -> Next (hd, fun () -> of_list tl)

type moves = (int * int) stream ref

let next moves =
  match !moves with
  | Next (move, rest) ->
      moves := rest () ;
      move
  | Stop -> assert false

let () =
  (* a losing sequence of moves (the best we can do is a draw) *)
  let moves = ref (of_list [(0, 0); (0, 1); (1, 0)]) in
  match game_loop (fun _state -> next moves) with
  | P1_wins -> assert false
  | P2_wins -> ()
  | Draw -> assert false
