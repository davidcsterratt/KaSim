(** An agent in a connected component *)

type t =
  | Existing of Agent.t * int (* node, id *)
  | Fresh of int * int (* type, id *)

val rename : int -> Renaming.t -> t -> t

val concretize :
  (Pattern.Matching.t * int Mods.IntMap.t) -> t -> int * int

val get_type : t -> int
val same_connected_component : t -> t -> bool
val is_site_from_fresh : (t * int) -> bool

val print : ?sigs:Signature.s -> Format.formatter -> t -> unit
val print_site : ?sigs:Signature.s -> t -> Format.formatter -> int -> unit
val print_internal :
  ?sigs:Signature.s -> t -> int -> Format.formatter -> int -> unit
