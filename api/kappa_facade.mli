(**  System process

     These are system process implementation details that
     vary.
*)
class type system_process =
  object
    method log : ?exn:exn -> string -> unit Lwt.t
    method min_duration_elapsed : unit -> bool
    method time_yield : unit -> unit Lwt.t
    method step_yield : unit -> unit Lwt.t
    method yield : unit -> unit Lwt.t
  end

class virtual system_template : float ->
  object
    method virtual log : ?exn:exn -> string -> unit Lwt.t
    method virtual yield : unit -> unit Lwt.t
    method min_duration_elapsed : unit -> bool
    method time_yield : unit -> unit Lwt.t
    method step_yield : unit -> unit Lwt.t
  end

(** Trivial implementation *)
class null_process : system_process

(** State of the running simulation.
*)
type t

val parse :
  system_process:system_process ->
  kappa_code:string ->
  (Api_types_j.project_parse, Api_types_j.errors) Api_types_j.result_data Lwt.t

val start :
  system_process:system_process ->
  parameter:Api_types_j.simulation_parameter ->
  kappa_code:string ->
  (t, Api_types_j.errors) Api_types_j.result_data Lwt.t

val pause :
  system_process:system_process ->
  t:t -> (unit, Api_types_j.errors) Api_types_j.result_data Lwt.t

val stop :
  system_process:system_process ->
  t:t -> (unit, Api_types_j.errors) Api_types_j.result_data Lwt.t

val perturbation :
  system_process:system_process ->
  t:t ->
  perturbation:Api_types_j.simulation_perturbation ->
  (unit, Api_types_j.errors) Api_types_j.result_data Lwt.t

val continue :
  system_process:system_process ->
  t:t ->
  parameter:Api_types_j.simulation_parameter ->
  (unit, Api_types_j.errors) Api_types_j.result_data Lwt.t


val info :
  system_process:system_process ->
  t:t ->
  (Api_types_j.simulation_info, Api_types_j.errors)
    Api_types_j.result_data Lwt.t
