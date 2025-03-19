module State = struct
  type 'a t = State : bool t

  let equal_choice_type : type a b. a t -> b t -> (a, b) Mcts.refl_eq option =
   fun State State -> Some Mcts.Refl

  let equal_choice : type a. a t -> a -> a -> bool = fun State b1 b2 -> b1 = b2

  let hash_choice : type a. a t -> a -> int = fun State b -> if b then 0 else 1
end

module M = Mcts.Ucb1 (State)

let rec lazy_bintree (depth : int) (acc : int) =
  let open M in
  if depth = 0 then return acc
  else
    M.choices State [| true; false |] >>= function
    | true -> lazy_bintree (depth - 1) (acc * 2)
    | false -> lazy_bintree (depth - 1) ((acc * 2) + 1)

let tree = lazy_bintree 5 0

let result =
  M.run
    tree
    { choice = (fun _ _ -> `Auto 50) }
    { reward =
        (fun _ x ->
          let v = abs_float (float_of_int x -. 31.) in
          1. -. (v /. (1. +. v)))
    }
    M.uniform
    (Random.State.make [||])

let () = assert (result = 31)
