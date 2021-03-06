type ('mix,'id) e =
    BIN_ALG_OP of Operator.bin_alg_op *
                  ('mix,'id) e Location.annot * ('mix,'id) e Location.annot
  | UN_ALG_OP of Operator.un_alg_op * ('mix,'id) e Location.annot
  | STATE_ALG_OP of Operator.state_alg_op
  | ALG_VAR of 'id
  | KAPPA_INSTANCE of 'mix
  | TOKEN_ID of 'id
  | CONST of Nbr.t
  | IF of ('mix,'id) bool Location.annot *
          ('mix,'id) e Location.annot * ('mix,'id) e Location.annot
and ('mix,'id) bool =
  | TRUE
  | FALSE
  | BOOL_OP of
      Operator.bool_op *
      ('mix,'id) bool Location.annot * ('mix,'id) bool Location.annot
  | COMPARE_OP of Operator.compare_op *
                  ('mix,'id) e Location.annot * ('mix,'id) e Location.annot

type t = (Pattern.id array list, int) e

val e_to_yojson :
  ('a -> Yojson.Basic.json) -> ('b -> Yojson.Basic.json) ->
  ('a,'b) e -> Yojson.Basic.json
val e_of_yojson :
  (Yojson.Basic.json -> 'a) -> (Yojson.Basic.json -> 'b) ->
  Yojson.Basic.json -> ('a,'b) e

val print :
  (Format.formatter -> 'a -> unit) -> (Format.formatter -> 'b -> unit) ->
  (Format.formatter -> 'b -> unit) -> Format.formatter -> ('a, 'b) e -> unit

val bool_to_yojson :
  ('a -> Yojson.Basic.json) -> ('b -> Yojson.Basic.json) ->
  ('a,'b) bool -> Yojson.Basic.json
val bool_of_yojson :
  (Yojson.Basic.json -> 'a) -> (Yojson.Basic.json -> 'b) ->
  Yojson.Basic.json -> ('a,'b) bool

val print_bool :
  (Format.formatter -> 'a -> unit) -> (Format.formatter -> 'b -> unit) ->
  (Format.formatter -> 'b -> unit) ->
  Format.formatter -> ('a,'b) bool -> unit

(** depend in time, depend in event number, depend in given var *)
val add_dep :
  (Operator.DepSet.t * Operator.DepSet.t * Operator.DepSet.t array * Operator.DepSet.t array) ->
  Operator.rev_dep ->
  ('a,int) e Location.annot ->
  (Operator.DepSet.t * Operator.DepSet.t * Operator.DepSet.t array * Operator.DepSet.t array)
val setup_alg_vars_rev_dep :
  unit NamedDecls.t ->
  (string Location.annot * ('a,int) e Location.annot) array ->
  (Operator.DepSet.t * Operator.DepSet.t * Operator.DepSet.t array * Operator.DepSet.t array)

val extract_connected_components : ('a,'b) e Location.annot -> 'a list

val propagate_constant :
  ?max_time:float -> ?max_events:int -> int list ->
  (string Location.annot * ('a,int) e Location.annot) array ->
  ('a,int) e Location.annot -> ('a,int) e Location.annot
val propagate_constant_bool :
  ?max_time:float -> ?max_events:int -> int list ->
  (string Location.annot * ('a,int) e Location.annot) array ->
  ('a,int) bool Location.annot -> ('a,int) bool Location.annot

val stops_of_bool :
  (Operator.DepSet.t * Operator.DepSet.t *
     Operator.DepSet.t array * Operator.DepSet.t array) ->
  ('a,int) bool -> Nbr.t list
