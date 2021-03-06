(** Domain to navigate in the graph *)

type cc
type t = cc (**type for domain points*)

type id

module ObsMap : sig
  type 'a t

  val dummy : 'a -> 'a t

  val get : 'a t -> id -> 'a
  val set : 'a t -> id -> 'a -> unit

  val fold_lefti : (id -> 'a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  val map : ('a -> 'b) -> 'a t -> 'b t
  val print :
    ?trailing:(Format.formatter -> unit) -> (Format.formatter -> unit) ->
    (id -> Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
end

type work (**type for partial domain*)

module Env : sig
  type t

  val signatures : t -> Signature.s
  val new_obs_map : t -> (id -> 'a) -> 'a ObsMap.t

  val print : Format.formatter -> t -> unit

  val to_yojson : t -> Yojson.Basic.json
  val of_yojson : Yojson.Basic.json -> t
end

module PreEnv : sig
  type t

  type stat = { nodes: int; nav_steps: int }

  val sigs : t -> Signature.s

  val get : t -> id -> cc (** Awfully inefficient *)

  val finalize : t -> Env.t * stat
  val of_env : Env.t -> t
end

(** {6 Create a connected component} *)

val empty_cc : Signature.s -> cc

val begin_new : PreEnv.t -> work
(** Starts creation *)

val new_node : work -> int -> (Agent.t*work)
(** [new_node wk node_type] *)

val new_link :
  work -> (Agent.t * int) -> (Agent.t * int) -> work
(** [new_link wk (node, site_id) (node', site_id')] *)

val new_free : work -> (Agent.t * int) -> work
val new_internal_state : work -> (Agent.t * int) -> int -> work
(** [new_link_type work (node,site) type] *)

val finish_new : ?origin:Operator.rev_dep -> work -> (PreEnv.t*Renaming.t*id)

val minimal_env : Signature.s -> Signature.contact_map -> PreEnv.t

(** {6 Use a connected component } *)

val compare_canonicals : id -> id -> int
val is_equal_canonicals : id -> id -> bool
val print_cc :
  ?sigs:Signature.s -> ?cc_id:id -> Format.formatter -> t -> unit
val print : ?domain:Env.t -> with_id:bool -> Format.formatter -> id -> unit
(** [print ~domain ?with_id:None form cc] *)

val id_to_yojson : id -> Yojson.Basic.json
val id_of_yojson : Yojson.Basic.json -> id

val find_root_type : t -> int option

val automorphisms : t -> Renaming.t list

module Matching : sig
  type t
  val empty : t
  val debug_print : Format.formatter -> t -> unit
  val get : (Agent.t * int) -> t -> int
  val reconstruct : Env.t -> Edges.t -> t -> int -> id -> int -> t option
  (** [reconstruct graph matching_of_previous_cc cc_id_in_rule cc root ]*)

  val add_cc : t -> int -> Renaming.t -> t option

  val is_root_of : Env.t -> Edges.t -> Agent.t -> id -> bool

  val roots_of : Env.t -> Edges.t -> id -> Mods.IntSet.t

  val elements_with_types : Env.t -> id array -> t -> Agent.t list array

  type cache
  val empty_cache : cache

  val observables_from_agent :
    Env.t -> Edges.t ->
    (((id * (int * int)) list * Operator.DepSet.t) * cache) -> Agent.t ->
    (((id * (int * int)) list * Operator.DepSet.t) * cache)
  (** [observables_from_free domain graph sort agent]
    the int * int in the return list and the following ones
    is a Instantiation.concrete *)

  val observables_from_free :
    Env.t -> Edges.t ->
    (((id * (int * int)) list * Operator.DepSet.t) * cache) ->
    Agent.t -> int -> (((id * (int * int)) list * Operator.DepSet.t) * cache)
  (** [observables_from_free domain graph sort agent site] *)

  val observables_from_internal :
    Env.t -> Edges.t ->
    (((id * (int * int)) list * Operator.DepSet.t) * cache) ->
     Agent.t -> int -> int -> (((id * (int * int)) list * Operator.DepSet.t) * cache)
  (** [observables_from_internal domain graph sort agent site internal_state] *)

  val observables_from_link :
    Env.t -> Edges.t ->
    (((id * (int * int)) list * Operator.DepSet.t) * cache) ->
     Agent.t -> int -> Agent.t -> int ->
    (((id * (int * int)) list * Operator.DepSet.t) * cache)
  (** [observables_from_link domain graph sort ag site sort' ag' site'] *)
end


val embeddings_to_fully_specified : Env.t -> id -> cc -> Renaming.t list

val add_fully_specified_to_graph :
  Signature.s -> Edges.t -> cc -> Edges.t * Renaming.t

module Set : SetMap.Set with type elt=id
