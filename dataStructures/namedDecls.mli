(** Stores a bunch of stuff the user gave a name to *)

type 'a t = private
    { decls : (string *'a) array;
      (** the name of the stuff * the stuff *)
      finder : int Mods.StringMap.t;
    (** [fst (fst d.decls.(StringMap.find s d.finder))] MUST be equal to [s] *)
 }

val create :
  ?forbidden:Mods.StringSet.t -> (string Location.annot *'a) array -> 'a t
val size : 'a t -> int
val elt_name : 'a t -> int -> string
val elt_id : ?kind:string -> 'a t -> string Location.annot -> int

val fold : (int -> string -> 'a -> 'b -> 'a) -> 'a -> 'b t -> 'a

val mapi : (int -> string -> 'a -> 'b) -> 'a t -> 'b t

val print :
  sep:(Format.formatter -> unit) ->
  (int -> string -> Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a t -> unit

val debug_print :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

val to_json : ('a -> Yojson.Basic.json) -> 'a t -> Yojson.Basic.json

val of_json : (Yojson.Basic.json -> 'a) -> Yojson.Basic.json -> 'a t
(** @raise Yojson.Basic.Util.Type_error if it fails *)
