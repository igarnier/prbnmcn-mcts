open Ucb1

type 'a gen = Random.State.t -> 'a

module Int_arm : Ucb1.Arm_sig with type t = int = struct
  type t = int

  let compare (i : int) (j : int) = if i < j then -1 else if i > j then 1 else 0

  let pp = Format.pp_print_int
end

module Int_bandit = Ucb1.Make (Int_arm)

type ('a, 'b) refl_eq = Refl : ('a, 'a) refl_eq

module type Mcts_state = sig
  type 'a t

  val equal_choice_type : 'a t -> 'b t -> ('a, 'b) refl_eq option

  val hash_choice : 'a t -> 'a -> int

  val equal_choice : 'a t -> 'a -> 'a -> bool
end

module type S = sig
  type 'a state

  type 'a t

  and 'a choice

  type choice_function =
    { choice : 'a. 'a state -> 'a array -> [ `Auto of int | `Choose of int ] }

  type 'a reward_function = { reward : 'i. 'i state -> 'a -> float }

  type kernel = { kernel : 'a. 'a state -> 'a array -> int gen }

  val choices : 'a state -> 'a array -> 'a choice

  val map : 'a choice -> ('a -> 'b) -> 'b t

  val bind : 'a choice -> ('a -> 'b t) -> 'b t

  val return : 'a -> 'a t

  val ( >|= ) : 'a choice -> ('a -> 'b) -> 'b t

  val ( >>= ) : 'a choice -> ('a -> 'b t) -> 'b t

  val uniform : kernel

  val run : 'a t -> choice_function -> 'a reward_function -> kernel -> 'a gen

  type _ interaction = private
    | Interaction :
        'a state
        * 'a array
        * ([ `Auto of int * kernel | `Choose of int ] -> 'b interaction)
        -> 'b interaction
    | Terminated : 'a -> 'a interaction

  val run_incremental : 'a t -> 'a reward_function -> 'a interaction gen

  module Make_syntax : functor (H : Hashtbl.HashedType) -> sig
    val map : H.t t -> (H.t -> 'b) -> 'b t

    val bind : H.t t -> (H.t -> 'b t) -> 'b t

    val ( >|= ) : H.t t -> (H.t -> 'a) -> 'a t

    val ( >>= ) : H.t t -> (H.t -> 'a t) -> 'a t

    val return : 'a -> 'a t
  end

  module Poly_syntax : sig
    val map : 'a t -> ('a -> 'b) -> 'b t

    val bind : 'a t -> ('a -> 'b t) -> 'b t

    val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t

    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

    val return : 'a -> 'a t
  end
end

module Make (Bandit : Ucb1.S with type arm = int) (State : Mcts_state) :
  S with type 'a state = 'a State.t = struct
  type 'a state = 'a State.t

  type ex_state = Ex : 'a State.t -> ex_state

  type phase = Selection | Simulation

  type 'a t =
    | Map : { m : 'a t; f : phase -> 'a -> 'b } -> 'b t
    | Bind : { m : 'a t; f : phase -> 'a -> 'b t } -> 'b t
    | Return : 'a -> 'a t
    | Choice : 'a choice -> 'a t

  and 'a choice =
    { mutable explored : bool;
      bandit : Ucb1.ready_to_move Bandit.t Lazy.t ref;
      state : 'a State.t;
      alternatives : 'a array
    }

  type (_, _, _) cont =
    | Map_cont :
        (phase -> 'a -> 'b) * ('acc, 'b, 'c) cont
        -> ('acc, 'a, 'c) cont
    | Bind_cont :
        (phase -> 'a -> 'b t) * ('acc, 'b, 'c) cont
        -> ('acc, 'a, 'c) cont
    | Run_cont : ('acc, 'a, 'acc * 'a) cont

  and 'acc simulation =
    { simulate :
        'b 'i.
        'i State.t -> 'acc -> 'i array -> ('acc, 'i, 'b) external_cont -> 'b
    }
  [@@unboxed]

  and (_, _, _) external_cont =
    | External_cont :
        phase * 'acc simulation * ('acc, 'a, 'b) cont
        -> ('acc, 'a, 'b) external_cont

  type 'a trace =
    (Ucb1.ready_to_move Bandit.t lazy_t ref
    * 'a
    * Ucb1.awaiting_reward Bandit.t)
    list

  type choice_function =
    { choice : 'i. 'i State.t -> 'i array -> [ `Auto of int | `Choose of int ] }

  type 'a reward_function = { reward : 'i. 'i State.t -> 'a -> float }

  type kernel = { kernel : 'a. 'a state -> 'a array -> int gen }

  let uniform =
    { kernel =
        (fun (type a) (_ : a state) (choices : a array) rng_state ->
          Random.State.int rng_state (Array.length choices))
    }

  let rec interpret :
      type a b.
      phase ->
      a t ->
      ex_state trace simulation ->
      _ trace ->
      (_ trace, a, b) cont ->
      b =
   fun phase m simulate acc k ->
    match m with
    | Map { m; f } -> interpret phase m simulate acc (Map_cont (f, k))
    | Bind { m; f } -> interpret phase m simulate acc (Bind_cont (f, k))
    | Return x -> call phase simulate acc k x
    | Choice ({ explored; bandit; state; alternatives } as payload) ->
        if explored && phase = Selection then
          (* Node was already evaluated and we are in selection phase *)
          let (arm, awaiting_reward) =
            Bandit.next_action (Lazy.force !bandit)
          in
          call
            phase
            simulate
            ((bandit, Ex state, awaiting_reward) :: acc)
            k
            alternatives.(arm)
        else (
          payload.explored <- true ;
          simulate.simulate
            state
            acc
            alternatives
            (External_cont (Simulation, simulate, k)))

  and call :
      type a b.
      phase ->
      ex_state trace simulation ->
      ex_state trace ->
      (ex_state trace, a, b) cont ->
      a ->
      b =
   fun phase simulate acc k x ->
    let app f x = f phase x [@@inline] in
    match k with
    | Map_cont (f, k) -> call phase simulate acc k (app f x)
    | Bind_cont (f, k) -> interpret phase (app f x) simulate acc k
    | Run_cont -> (acc, x)

  let resume : 'a -> 'acc -> ('acc, 'a, 'b) external_cont -> 'b =
   fun x acc (External_cont (phase, simulate, k)) -> call phase simulate acc k x

  let interpret tree simulate acc k = interpret Selection tree simulate acc k

  let return x = Return x

  let choices state alternatives =
    if Array.length alternatives = 0 then invalid_arg "choices" ;
    let bandit =
      ref @@ Lazy.from_fun
      @@ fun () -> Bandit.create (Array.mapi (fun i _ -> i) alternatives)
    in
    { explored = false; state; alternatives; bandit }

  let playout tree reward kernel k rng_state =
    let (bandits, result) =
      interpret
        tree
        { simulate =
            (fun user_state acc choices k ->
              let i = kernel.kernel user_state choices rng_state in
              (* let i = Random.State.int rng_state (Array.length choices) in *)
              resume choices.(i) acc k)
        }
        []
        k
    in
    List.iter
      (fun (bref, Ex user_state, awaiting_reward) ->
        let r = reward.reward user_state result in
        bref := Lazy.from_val @@ Bandit.set_reward awaiting_reward r)
      bandits ;
    result

  let iterate_playouts :
      playouts:int ->
      'a t ->
      'b reward_function ->
      kernel ->
      (ex_state trace, 'a, ex_state trace * 'b) cont ->
      Random.State.t ->
      unit =
   fun ~playouts m reward kernel k rng_state ->
    for _i = 1 to playouts do
      ignore (playout m reward kernel k rng_state)
    done

  let run : 'a t -> choice_function -> 'a reward_function -> kernel -> 'a gen =
    fun (type b) (expr : b t) choice reward kernel rng_state ->
     let rec loop : type a. a t -> (_ trace, a, _ trace * b) cont -> b =
      fun expr k ->
       match expr with
       | Map { m; f } -> loop m (Map_cont (f, k))
       | Bind { m; f } -> loop m (Bind_cont (f, k))
       | Return x -> call k x
       | Choice { state; bandit; alternatives; explored = _ } -> (
           match choice.choice state alternatives with
           | `Auto playouts ->
               if playouts <= 0 then invalid_arg "playouts <= 0" ;
               iterate_playouts ~playouts expr reward kernel k rng_state ;
               let (act, _) =
                 Bandit.find_best_arm
                   (Lazy.force !bandit)
                   (fun { empirical_reward; _ } -> empirical_reward)
               in
               call k alternatives.(act)
           | `Choose act -> call k alternatives.(act))
     and call : type a. (_ trace, a, _ trace * b) cont -> a -> b =
      fun k x ->
       let app f x = f Selection x [@@inline] in
       match k with
       | Map_cont (f, k) -> call k (app f x)
       | Bind_cont (f, k) -> loop (app f x) k
       | Run_cont -> x
     in
     loop expr Run_cont

  type 'b interaction =
    | Interaction :
        'a state
        * 'a array
        * ([ `Auto of int * kernel | `Choose of int ] -> 'b interaction)
        -> 'b interaction
    | Terminated : 'a -> 'a interaction

  let run_incremental : 'a t -> 'a reward_function -> 'a interaction gen =
    fun (type b) (expr : b t) reward rng_state ->
     let rec loop :
         type a.
         a t -> (ex_state trace, a, ex_state trace * b) cont -> b interaction =
      fun expr k ->
       match expr with
       | Map { m; f } -> loop m (Map_cont (f, k))
       | Bind { m; f } -> loop m (Bind_cont (f, k))
       | Return x -> call k x
       | Choice { state; bandit; alternatives; explored = _ } ->
           Interaction
             ( state,
               alternatives,
               function
               | `Auto (playouts, kernel) ->
                   iterate_playouts ~playouts expr reward kernel k rng_state ;
                   let (act, _) =
                     Bandit.find_best_arm
                       (Lazy.force !bandit)
                       (fun { empirical_reward; _ } -> empirical_reward)
                   in
                   call k alternatives.(act)
               | `Choose act -> call k alternatives.(act) )
     and call :
         type a.
         (ex_state trace, a, ex_state trace * b) cont -> a -> b interaction =
      fun k x ->
       let app f x = f Selection x [@@inline] in
       match k with
       | Map_cont (f, k) -> call k (app f x)
       | Bind_cont (f, k) -> loop (app f x) k
       | Run_cont -> Terminated x
     in
     loop expr Run_cont

  type ex_value = Ex_value : 'a State.t * 'a -> ex_value

  let equal : ex_value -> ex_value -> bool =
   fun (Ex_value (state1, v1)) (Ex_value (state2, v2)) ->
    match State.equal_choice_type state1 state2 with
    | None -> false
    | Some Refl -> State.equal_choice state1 v1 v2

  let hash : ex_value -> int =
   fun (Ex_value (state, v)) -> State.hash_choice state v

  module Table = Hashtbl.Make (struct
    type t = ex_value

    let equal = equal

    let hash = hash
  end)

  let memoize state f =
    let table = Table.create 16 in
    fun phase x ->
      match phase with
      | Simulation -> f x
      | Selection -> (
          let key = Ex_value (state, x) in
          match Table.find_opt table key with
          | None ->
              let res = f x in
              Table.add table key res ;
              res
          | Some res -> res)

  let map : 'a choice -> ('a -> 'b) -> 'b t =
   fun m f -> Map { m = Choice m; f = memoize m.state f }

  let bind : 'a choice -> ('a -> 'b t) -> 'b t =
   fun m f -> Bind { m = Choice m; f = memoize m.state f }

  let ( >|= ) = map

  let ( >>= ) = bind

  module Make_syntax (H : Hashtbl.HashedType) = struct
    module Table = Hashtbl.Make (H)

    let memoize f =
      let table = Table.create 16 in
      fun phase x ->
        match phase with
        | Simulation -> f x
        | Selection -> (
            match Table.find_opt table x with
            | None ->
                let res = f x in
                Table.add table x res ;
                res
            | Some res -> res)

    let map : H.t t -> (H.t -> 'b) -> 'b t = fun m f -> Map { m; f = memoize f }

    let bind : H.t t -> (H.t -> 'b t) -> 'b t =
     fun m f -> Bind { m; f = memoize f }

    let ( >|= ) = map

    let ( >>= ) = bind

    let return = return
  end

  module Poly_syntax = struct
    let memoize f =
      let table = Hashtbl.create 16 in
      fun phase x ->
        match phase with
        | Simulation -> f x
        | Selection -> (
            match Hashtbl.find_opt table x with
            | None ->
                let res = f x in
                Hashtbl.add table x res ;
                res
            | Some res -> res)

    let map : type a b. a t -> (a -> b) -> b t =
     fun m f -> Map { m; f = memoize f }

    let bind : type a b. a t -> (a -> b t) -> b t =
     fun m f -> Bind { m; f = memoize f }

    let ( >|= ) = map

    let ( >>= ) = bind

    let return = return
  end
end

module Ucb1 = Make (Int_bandit)
