open Lwt
(** Interface to kappa runtime *)
(* Error messages *)
let msg_process_not_running =
  "process not running"
let msg_process_already_paused =
  "process already paused"
let msg_process_not_paused =
  "process not paused"
let msg_observables_less_than_zero =
  "Plot observables must be greater than zero"
let msg_missing_perturbation_context =
  "Invalid runtime state missing missing perturbation context"



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

class virtual system_template min_run_duration =
  object(self)
    (* Methods need to be given the appropriate names. *)
    val mutable lastyield = Sys.time ()

    method virtual log : ?exn:exn -> string -> unit Lwt.t
    method virtual yield : unit -> unit Lwt.t
    method min_duration_elapsed () : bool =
      Sys.time () -. lastyield < min_run_duration
    method time_yield () =
        let t = Sys.time () in
        if t -. lastyield > min_run_duration then
          let () = lastyield <- t in
          self#yield ()
        else Lwt.return_unit
    method step_yield () =
      let () = lastyield <- Sys.time () in
      self#yield ()
  end

(** Trivial implementation primarily for unit testing. *)
class null_process : system_process =
  object
    method log ?exn (_ : string) = Lwt.return_unit
    method yield () = Lwt.return_unit
    inherit system_template 0.0
  end;;

(** State of the running simulation.
*)
type t =
    { mutable is_running : bool
    ; mutable run_finalize : bool
    ; counter : Counter.t
    ; log_buffer : Buffer.t
    ; log_form : Format.formatter
    ; mutable plot : Api_types_v1_j.plot
    ; mutable distances : Api_types_v1_j.distances
    ; mutable snapshots : Api_types_v1_j.snapshot list
    ; mutable flux_maps : Api_types_v1_j.flux_map list
    ; mutable files : Api_types_v1_j.file_line list
    ; mutable error_messages : Api_types_v1_j.errors
    ; contact_map : Primitives.contact_map
    ; env : Environment.t
    ; mutable domain : Connected_component.Env.t
    ; mutable graph : Rule_interpreter.t
    ; mutable state : State_interpreter.t
    ; store_distances : bool
    ; init_l : (Alg_expr.t * Primitives.elementary_rule * Location.t) list
    ; has_tracking : (bool * bool * bool) option
    }

let format_error_message (message,linenumber)  =
  Format.sprintf "Error at %s : %s"
    (Location.to_string linenumber)
    message

let catch_error : 'a . (Api_types_v1_j.errors -> 'a) -> exn -> 'a =
  fun handler ->
  (function
    |  ExceptionDefn.Syntax_Error e ->
      handler (Api_data_v1.api_location_errors e)
    | ExceptionDefn.Malformed_Decl e ->
      handler  (Api_data_v1.api_location_errors e)
    | ExceptionDefn.Internal_Error error ->
      handler (Api_data_v1.api_location_errors error)
    | Invalid_argument error ->
      handler (Api_data_v1.api_message_errors ("Runtime error "^ error))
    | exn -> handler (Api_data_v1.api_message_errors (Printexc.to_string exn))
  )



let build_ast
    (code : string)
    yield
    (log : ?exn:exn -> string -> unit Lwt.t) =
  let lexbuf : Lexing.lexbuf = Lexing.from_string code in
  let simulation_log_buffer = Buffer.create 512 in
  let simulation_log_form =
    Format.formatter_of_buffer simulation_log_buffer in
  Lwt.catch
    (fun () ->
       (Lwt.wrap3 KappaParser.start_rule KappaLexer.token
          lexbuf Ast.empty_compil) >>=
       (fun raw_ast ->
          (yield ()) >>=
          (fun () ->
             (Lwt.wrap2 LKappa.compil_of_ast [] raw_ast) >>=
             (fun (sig_nd,tk_nd,_updated_vars,result :
                                                Signature.s * unit NamedDecls.t * int list *
                                                (Ast.agent,
                                                 LKappa.rule_agent list,
                                                 int,
                                                 LKappa.rule) Ast.compil) ->
               (yield ()) >>=
               (fun () ->
                  (Lwt.wrap3
                     Eval.init_kasa
                     Remanent_parameters_sig.JS
                     sig_nd
                     raw_ast) >>=
                  (fun (contact_map,_kasa_state) ->
                     Eval.compile
                       ~pause:(fun f -> Lwt.bind (yield ()) f)
                       ~return:Lwt.return ?rescale_init:None
                       ~outputs:(function
                           | Data.Log s ->
                             Format.fprintf simulation_log_form "%s@." s
                           | Data.Snapshot _
                           | Data.Flux _
                           | Data.Plot _
                           | Data.Print _
                           | Data.UnaryDistance _ -> assert false)
                       sig_nd tk_nd contact_map result >>=
                     (fun (env,domain,has_tracking,
                           store_distances,_,init_l) ->
                       let store_distances = store_distances<>None in
                       let simulation_counter =
                         Counter.create
                           ~init_t:(0. : float) ~init_e:(0 : int)
                           ?max_t:None ?max_e:None ~nb_points:200 in
                       let simulation =
                         { is_running = true ;
                           run_finalize = false ;
                           counter = simulation_counter ;
                           log_buffer = simulation_log_buffer ;
                           log_form = simulation_log_form ;
                           plot = { Api_types_v1_j.legend = [] ;
                                    Api_types_v1_j.time_series = [] ; } ;
                           distances = [] ;
                           error_messages = [] ;
                           snapshots = [] ;
                           flux_maps = [] ;
                           files = [] ;
                           contact_map = contact_map ;
                           env = env ;
                           domain = domain ;
                           graph = Rule_interpreter.empty ~store_distances env ;
                           state = State_interpreter.empty env [] [] ;
                           store_distances;
                           has_tracking;
                           init_l;
                         } in
                       Lwt.return (`Ok simulation))))))))
      (catch_error (fun e -> Lwt.return (`Error e)))

let outputs simulation = function
  | Data.Flux flux_map ->
    simulation.flux_maps <-
      ((Api_data_v1.api_flux_map flux_map)::simulation.flux_maps)
  | Data.Plot (time,new_observables) ->
    let new_values =
      List.map (fun nbr -> Nbr.to_float nbr)
        (Array.to_list new_observables) in
    simulation.plot <-
      {simulation.plot with
       Api_types_v1_j.time_series =
         { Api_types_v1_j.observation_time = time ;
           Api_types_v1_j.observation_values = new_values ; }
         :: simulation.plot.Api_types_v1_j.time_series }
  | Data.Print file_line ->
    simulation.files <-
      ((Api_data_v1.api_file_line file_line)::simulation.files)
  | Data.Snapshot snapshot ->
    simulation.snapshots <-
      ((Api_data_v1.api_snapshot
          (Environment.signatures simulation.env) snapshot)
       ::simulation.snapshots)
  | Data.UnaryDistance d ->
    simulation.distances <-
      {Api_types_v1_j.rule_dist =
         Format.asprintf
           "%a" (Environment.print_ast_rule ~env:simulation.env)
           d.Data.distance_rule;
       Api_types_v1_j.time_dist = d.Data.distance_time;
       Api_types_v1_j.dist = d.Data.distance_length}
      :: simulation.distances
  | Data.Log s -> Format.fprintf simulation.log_form "%s@." s

let parse
    ~(system_process : system_process)
    ~(kappa_code : string)
  : ((t * Api_types_j.project_parse),Api_types_j.errors) Api_types_j.result_data Lwt.t
  = Lwt.bind
    (build_ast kappa_code system_process#time_yield system_process#log)
    (function
      | `Ok simulation ->
        Lwt.return
          (`Ok
             (simulation,
              { Api_types_j.contact_map =
                  Api_data.api_contact_map simulation.sigs simulation.contact_map }))
      | `Error e -> Lwt.return (`Error e))

let start
    ~(system_process : system_process)
    simulation (parameter : Api_types_v1_j.parameter) :
  Api_types_v1_j.token Api_types_v1_j.result Lwt.t =
  let () =
    Lwt.async
      (fun () ->
         Lwt.catch (fun () ->
             let story_compression =
               Tools.option_map
                 (fun  _ ->
                    ((false,false,false),true))
                 simulation.has_tracking in
             let () =
               Counter.set_max_time
                 simulation.counter
                 parameter.Api_types_v1_j.max_time
             in
             let () =
               Counter.set_max_events
                 simulation.counter
                 parameter.Api_types_v1_j.max_events
             in
             (*TODO: deal with set nb_plot in counter*)
             Eval.build_initial_state
               ~bind:(fun x f ->
                   (system_process#time_yield ()) >>=
                   (fun () -> x >>= f))
               ~return:Lwt.return []
               simulation.counter
               simulation.env
               simulation.domain
               story_compression
               ~store_distances:simulation.store_distances
               simulation.init_l >>=
             (fun (graph,state) ->
                let () = simulation.graph <- graph;
                  simulation.state <- state in
                let log_form =
                  Format.formatter_of_buffer
                    simulation.log_buffer
                in
                let () =
                  ExceptionDefn.flush_warning
                    log_form
                in
                let legend =
                  Environment.map_observables
                    (Format.asprintf
                       "%a"
                       (Kappa_printer.alg_expr
                          ~env:simulation.env))
                    simulation.env in
                let first_obs =
                  State_interpreter.observables_values
                    simulation.env simulation.counter graph state in
                let first_values =
                  List.map (fun nbr -> Nbr.to_float nbr)
                    (Array.to_list first_obs) in
                
                let () =
                  simulation.plot <-
                    { Api_types_v1_j.legend =
                        Array.to_list legend;
                      Api_types_v1_j.time_series =
                        [
                          { Api_types_v1_j.observation_time =
                              Counter.current_time simulation.counter;
                            Api_types_v1_j.observation_values =
                              first_values;
                          }
                        ]} in
                run simulation
             )
           )
           (catch_error
              (fun e ->
                 let () = simulation.error_messages <- e in
                 Lwt.return (`Left e))
           )) in
      Lwt.return (`Ok ())

let pause
    ~(system_process : system_process)
    ~(t : t) : (unit,Api_types_j.errors) Api_types_j.result_data Lwt.t =
  let () = ignore(system_process) in (* supress ignored variable in favor of symmetery*)
  match t.pause with
  | None ->
    let () = t.pause <- Some (Lwt_mvar.create_empty ()) in
    Lwt.return (`Ok ())
  | Some _ -> Api_data.lwt_msg msg_process_already_paused

let stop
    ~(system_process : system_process)
    ~(t : t) : (unit,Api_types_j.errors) Api_types_j.result_data Lwt.t =
  let () = ignore(system_process) in (* supress ignored variable in favor of symmetery*)
  Lwt.catch
    (fun () ->
       (if Lwt_switch.is_on t.switch then
         Lwt_switch.turn_off t.switch
         >>= (fun _ -> Lwt.return (`Ok ()))
       else
         Api_data.lwt_msg msg_process_not_running)
       >>=
       (fun msg -> (* remove pause *)
          match t.pause with
          | None -> Lwt.return msg
          | Some pause ->
            (Lwt_mvar.put pause ())
            >>= (fun _ -> Lwt.return msg)
       )
    )
    (fun e -> Api_data.lwt_msg (Printexc.to_string e))

let perturbation
    ~(system_process : system_process)
    ~(t : t)
    ~(perturbation:Api_types_j.simulation_perturbation)
  : (unit, Api_types_j.errors) Api_types_j.result_data Lwt.t =
  match t.pause with
  | Some _ ->
    (match t.perturbation with
     | None ->
       Api_data.lwt_msg msg_missing_perturbation_context
     | Some handler ->
       Lwt.return (handler perturbation.Api_types_j.perturbation_code)
    )

  | None -> Api_data.lwt_msg msg_process_not_paused



let continue
    ~(system_process : system_process)
    ~(t : t)
    ~(parameter : Api_types_j.simulation_parameter)
  : (unit,Api_types_j.errors) Api_types_j.result_data Lwt.t =
  match t.pause with
  | Some pause ->
    let () = ignore(system_process) in
    let () = Counter.set_max_time t.counter parameter.Api_types_j.simulation_max_time in
    let () = Counter.set_max_events t.counter parameter.Api_types_j.simulation_max_events in
    let () =
      Lwt.async
        (fun _ ->
           (Lwt_mvar.put pause ()) >>=
           (fun _ ->
              let () = t.pause <- None in
              Lwt.return_unit)
        )
    in
    Lwt.return (`Ok ())
  | None -> Api_data.lwt_msg msg_process_not_paused

let info
    ~(system_process : system_process)
    ~(t : t) :
  (Api_types_j.simulation_info,Api_types_j.errors)
    Api_types_j.result_data
    Lwt.t =
  Lwt.catch
    (fun () ->
       Lwt.return
         (match t.error_messages with
            [] ->
            `Ok { Api_types_j.simulation_plot =
                    Some t.plot ;
                  Api_types_j.simulation_distances =
                    Some t.distances ;
                  Api_types_j.simulation_time =
                    Counter.time t.counter ;
                  Api_types_j.simulation_time_percentage =
                    Counter.time_percentage t.counter ;
                  Api_types_j.simulation_event =
                    Counter.event t.counter ;
                  Api_types_j.simulation_event_percentage =
                    Counter.event_percentage t.counter ;
                  Api_types_j.simulation_tracked_events =
                    Counter.tracked_events t.counter ;
                  Api_types_j.simulation_log_messages =
                    [Buffer.contents t.log_buffer] ;
                  Api_types_j.simulation_snapshots =
                    t.snapshots ;
                  Api_types_j.simulation_flux_maps =
                    t.flux_maps ;
                  Api_types_j.simulation_outputs =
                    t.files;
                  Api_types_j.simulation_state =
                    if Lwt_switch.is_on t.switch then
                      (match t.pause with
                       | None -> `Running
                       | Some _ -> `Paused
                      )
                    else
                      `Stopped

                }
          | _ -> `Error t.error_messages
         )
    )
    (fun exn ->
       (system_process#log ~exn "")
       >>=
       (fun _ -> Api_data.lwt_msg (Printexc.to_string exn))
    )
