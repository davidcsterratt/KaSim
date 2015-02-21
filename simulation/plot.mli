val create : string -> Environment.t -> State.t -> Mods.Counter.t -> unit
val close : Mods.Counter.t -> unit

val plot_now : Environment.t -> Mods.Counter.t -> ?time:float -> State.t -> unit

(** Warning: This function is also in charge of the progressBar *)
val fill : State.t -> Mods.Counter.t ->  Environment.t -> float -> unit
