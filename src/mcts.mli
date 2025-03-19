(** The [prbnmcn-mcts] Monte-Carlo tree search library. *)

(** {1:introduction Introduction}

    [prbnmcn-mcts] provides a DSL to describe lazy exploration trees where nodes
    correspond to non-deterministic choices indexed by possible actions. Only
    finitely branching trees are allowed. The library provides facilities to
    explore these trees (ie perform choices iteratively), either by specifying
    programatically which choices are to be made, or (and this is the main point
    of the library) by letting a Monte-Carlo tree search (MCTS) routine perform
    automatically an evaluation of the {e value} of each action. This value is
    determined thanks to an user-provided reward function, which associates
    each state at each choice point and each terminal value with a reward.

    A classical application is to board games, where:
    - each choice point corresponds to a board in a given configuration and the currently active player
    - the actions corresponds to legal moves by the currently active player
    - the reward function is 0 if the player loses and 1 if the player wins
*)

(** [Refl] is a proof of type equality. *)
type ('a, 'b) refl_eq = Refl : ('a, 'a) refl_eq

(** [Mcts_state] is a module type describing a type of states
    ['a t] and the actions of type ['a] available in that state. *)
module type Mcts_state = sig
  (** ['a t] is the type of states, with ['a] being the type of
      actions available at that state. *)
  type 'a t

  (** [equal_choice_type st1 st2] must return [Some Refl] if the type of
      actions of [st1] and [st2] are equal, and [None] otherwise.

      Note that the underlying states need not be equal. *)
  val equal_choice_type : 'a t -> 'b t -> ('a, 'b) refl_eq option

  (** [hash_choice state] must implement a hash function for actions available in [state]. *)
  val hash_choice : 'a t -> 'a -> int

  (** [equal_choice state] must implement an equality function for actions available in [state]. *)
  val equal_choice : 'a t -> 'a -> 'a -> bool
end

(** A module of type [S] implements a monadic interface to building lazy search trees and
    allow their guided exploration using Monte-Carlo tree search. *)
module type S = sig
  (** ['a] state is the module type of states, with ['a] the type of actions available in that state.
      See {!Mcts_state}. *)
  type 'a state

  (** ['a t] is the type of a non-deterministic computation yielding values of type ['a].

      Choices are taken either according to some user-defined strategy or
      automatically by optimizing a given reward using Monte-Carlo tree search. *)
  type 'a t

  (** The type of a choice point with choices of type ['a]. *)
  and 'a choice

  (** {2 Creating choice trees} *)

  (** [choices state alternatives] creates a choice point with [state], the choice point
      can evaluate to any element of [alternatives].

      @raise Invalid_argument if [Array.length alternatives = 0]. *)
  val choices : 'a state -> 'a array -> 'a choice

  (** [map choice f] is the computation resulting from evaluating [f]
      on the outcome of [choice]. [f] must be deterministic. *)
  val map : 'a choice -> ('a -> 'b) -> 'b t

  (** [bind choice f] is the computation resulting from evaluating [f]
      on the outcome of [choice]. [f] can give rise to further choice points. *)
  val bind : 'a choice -> ('a -> 'b t) -> 'b t

  (** [return x] is the deterministic computation evaluating to [x]. *)
  val return : 'a -> 'a t

  (** [>|=] is an alias for [map]. Note that this operator can only apply on a value of type ['a choice]
      rather than ['a t]. *)
  val ( >|= ) : 'a choice -> ('a -> 'b) -> 'b t

  (** [>|=] is an alias for [bind]. Note that this operator can only apply on a value of type ['a choice]
      rather than ['a t]. *)
  val ( >>= ) : 'a choice -> ('a -> 'b t) -> 'b t

  (** A [choice_function] is used to guide the unfolding of the tree.
      When in a given [state] with an array of possible [choices]:
      - returning [`Choose i] will move to [choices.(i)] unconditionally (the choice is performed "externally");
      - returning [`Auto] will pick a choice automatically using Monte-Carlo tree search to estimate the
        action values (the choice is performed "internally").

      As a concrete example, in a two-player game involving a human and a computer, the human player
      would [`Choose _] his next move while the computer player would be implemented by [`Auto playouts].
      (This implicitly assumes that the [state] contains the current player.)

      The [playouts] controls the amount of exploration performed during each [`Auto] move.

      @raise Invalid_argument if [i] is not in the bounds of [choices]
      @raise Invalid_argument if [playouts] is not strictly positive
  *)
  type choice_function =
    { choice : 'a. 'a state -> 'a array -> [ `Auto of int | `Choose of int ] }

  (** A [reward_function] returns the reward at any intermediate [state] given the final outcome.

      Considering the example of a two-player game, this allows to compute reward as a function of
      the player at each intermediate state of the game.

      @raise Invalid_argument if the reward is not in the unit interval.
  *)
  type 'a reward_function = { reward : 'i. 'i state -> 'a -> float }

  (** The type of random samplers *)
  type 'a gen := Random.State.t -> 'a

  (** A [kernel] is a distribution over available actions in a given state. *)
  type kernel = { kernel : 'a. 'a state -> 'a array -> int gen }

  (** The [uniform] kernel picks an action uniformly at random. *)
  val uniform : kernel

  (** [run tree choice_function reward_function kernel ~playouts] explores [tree] according to
      [choice_function]. [`Auto _] picks moves optimizing the [reward_function].
      [kernel] is used to guide the exploration when estimating the value of each action.
 *)
  val run : 'a t -> choice_function -> 'a reward_function -> kernel -> 'a gen

  (** {3 Incremental API} *)

  (** The incremental API hands back control to the user at [interaction] points. *)
  type _ interaction = private
    | Interaction :
        'a state
        * 'a array
        * ([ `Auto of int * kernel | `Choose of int ] -> 'b interaction)
        -> 'b interaction
        (** [Interaction (state, choices, k)] is an interaction point exposing to the user the
            [state] and available [choices] at the point as well as a continuation.

            The continuation either takes
            - [`Auto (playouts, kernel)] with [playouts] a strictly positive integer and
              [kernel] a value of type {!kernel}; or
            - [`Choose i] with [i] an integer in the domain of [choices].

           The continuation raises Invalid_argument if [playouts] is not strictly positive
           or if [i] is not in the domain of [choices]. *)
    | Terminated : 'a -> 'a interaction

  val run_incremental : 'a t -> 'a reward_function -> 'a interaction gen

  (** {3 Binding operator generation} *)

  (** This library uses memoization to construct lazy search trees. The following functor allows
      to create binding operators for types satisfying the {!Hashtbl.HashedType} signature. *)

  module Make_syntax : functor (H : Hashtbl.HashedType) -> sig
    val map : H.t t -> (H.t -> 'b) -> 'b t

    val bind : H.t t -> (H.t -> 'b t) -> 'b t

    val ( >|= ) : H.t t -> (H.t -> 'a) -> 'a t

    val ( >>= ) : H.t t -> (H.t -> 'a t) -> 'a t

    val return : 'a -> 'a t
  end

  (** The {!Poly_syntax} module uses the polymorphic equality and hashing to construct
      memoizing binding operators. Use only if you must... *)
  module Poly_syntax : sig
    val map : 'a t -> ('a -> 'b) -> 'b t

    val bind : 'a t -> ('a -> 'b t) -> 'b t

    val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t

    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

    val return : 'a -> 'a t
  end
end

(** Implementation of MCTS based on UCB1 bandits. *)
module Ucb1 : functor (State : Mcts_state) -> S with type 'a state = 'a State.t
