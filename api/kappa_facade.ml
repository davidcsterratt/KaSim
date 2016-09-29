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
  object(self)
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
    }

let format_error_message (message,linenumber)  =
  Format.sprintf "Error at %s : %s"
    (Location.to_string linenumber)
    message
let build_ast
    (code : string)
    yield
    (log : ?exn:exn -> string -> unit Lwt.t) =
  let lexbuf : Lexing.lexbuf = Lexing.from_string code in
  Lwt.catch
    (fun () ->
       (Lwt.wrap3 KappaParser.start_rule KappaLexer.token
          lexbuf Ast.empty_compil) >>=
       (fun raw_ast ->
          (yield ()) >>=
          (fun () ->
             (Lwt.wrap2 LKappa.compil_of_ast [] raw_ast) >>=
             (fun (sigs,_,_,_ as ast :
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
                     sigs
                     raw_ast) >>=
                  (fun (contact_map,_kasa_state) ->
                     Lwt.return (`Ok (ast,contact_map))))))))
    (function
        ExceptionDefn.Syntax_Error e ->
        Lwt.return
          (`Error
             (Api_data.api_location_errors e))
      | ExceptionDefn.Malformed_Decl e ->
        Lwt.return
          (`Error
             (Api_data.api_location_errors e))
      | exn -> log ~exn "" >>=
        (fun () -> Lwt.fail exn))

let parse
    ~(system_process : system_process)
    ~(kappa_code : string)
  : (Api_types_j.project_parse,Api_types_j.errors) Api_types_j.result_data Lwt.t
  = Lwt.bind
    (build_ast kappa_code system_process#time_yield system_process#log)
    (function
      | `Ok ((sigs,_,_,_),contact_map) ->
        Lwt.return
          (`Ok
             { Api_types_j.contact_map =
                 Api_data.api_contact_map sigs contact_map })
      | `Error e -> Lwt.return (`Error e))

let start
    ~(system_process : system_process)
    ~(parameter : Api_types_j.simulation_parameter)
    ~(kappa_code : string)
  : (t,Api_types_j.errors) Api_types_j.result_data Lwt.t =
  if parameter.Api_types_j.simulation_nb_plot > 0 then
    let plot : Api_types_j.plot =
      { Api_types_j.plot_legend = []
      ; Api_types_j.plot_time_series = [] }
    in
    let simulation : t =
      { switch = Lwt_switch.create () ;
        counter = Counter.create
            ~init_t:(0. : float)
            ~init_e:(0 : int)
            ?max_t:parameter.Api_types_j.simulation_max_time
            ?max_e:parameter.Api_types_j.simulation_max_events
            ~nb_points:(parameter.Api_types_j.simulation_nb_plot : int) ;
        log_buffer = Buffer.create 512 ;
        plot = plot ;
        distances = [] ;
        snapshots = [] ;
        flux_maps = [] ;
        files = [] ;
        error_messages = [] ;
        pause = None ;
        perturbation = None ; }
    in
    let log_form =
      Format.formatter_of_buffer simulation.log_buffer in
    let () = Counter.reinitialize simulation.counter in
    let outputs sigs =
      function
      | Data.Flux flux_map ->
        simulation.flux_maps <-
          ((Api_data.api_flux_map flux_map)::simulation.flux_maps)
      | Data.Plot (time,new_observables) ->
        let new_values =
          List.map (fun nbr -> Nbr.to_float nbr)
            (Array.to_list new_observables) in
        simulation.plot <-
          {simulation.plot
           with Api_types_j.plot_time_series =
                  { Api_types_j.observable_time = time ;
                    Api_types_j.observable_values = new_values }
                  :: simulation.plot.Api_types_j.plot_time_series }
      | Data.Print file_line ->
        simulation.files <-
          ((Api_data.api_file_line file_line)::simulation.files)
      | Data.Snapshot snapshot ->
        simulation.snapshots <-
          ((Api_data.api_snapshot sigs snapshot)::simulation.snapshots)
      | Data.UnaryDistances unary_distances ->
        simulation.distances <-
          let (one_big_list,_) =
            Array.fold_left
              (fun (l,i) a ->
                 match a with
                 | Some ls ->
                   let add_rule_id =
                     List.map
                       (fun (t,d) ->
                          {Api_types_j.rule_dist =
                             unary_distances.Data.distances_rules.(i);
                           Api_types_j.time_dist = t;
                           Api_types_j.dist = d}) ls
                   in (List.append l add_rule_id, i+1)
                 | None -> (l, i+1))
              ([],0) unary_distances.Data.distances_data in
          one_big_list
      | Data.Log s ->
        Format.fprintf log_form "%s@." s
    in
    let () =
      Lwt.async
        (fun () ->
           (Lwt.catch
              (fun () ->
                 (build_ast
                    kappa_code
                    system_process#time_yield
                    system_process#log) >>=
                 (function
                     `Ok ((sig_nd,tk_nd,_(* updated_vars *),result),
                          contact_map) ->
                     Eval.compile
                       ~pause:(fun f -> Lwt.bind
                                  (system_process#time_yield ()) f)
                       ~return:Lwt.return ?rescale_init:None
                       ~outputs:(outputs (Signature.create []))
                       sig_nd tk_nd contact_map
                       simulation.counter result >>=
                     (fun (env,domain,has_tracking,store_distances,_,init_l) ->
                        let story_compression =
                          Tools.option_map
                            (fun  _ -> ((false,false,false),true))
                            has_tracking
                        in
                             Eval.build_initial_state
                               ~bind:(fun x f ->
                                   (system_process#time_yield ())
                                   >>=
                                   (fun () -> x >>= f))
                               ~return:Lwt.return [] simulation.counter
                               env domain story_compression
                               store_distances init_l >>=
                             (fun (graph,state) ->
                                let () = ExceptionDefn.flush_warning log_form
                                in
                                let sigs = Environment.signatures env in
                                let legend =
                                  Environment.map_observables
                                    (Format.asprintf
                                       "%a"
                                       (Kappa_printer.alg_expr ~env))
                                    env in
                                let () =
                                  simulation.plot <-
                                    { simulation.plot
                                      with Api_types_j.plot_legend = Array.to_list legend}
                                in
                                let rstop = ref false in
                                let rgraph = ref graph in
                                let rstate = ref state in
                                let rec iter () =
                                  let () =
                                    while (not !rstop)
                                          &&
                                          system_process#min_duration_elapsed ()
                                    do
                                      let (stop,graph',state') =
                                          State_interpreter.a_loop
                                            ~outputs:(outputs sigs)
                                            env
                                            domain
                                            simulation.counter
                                            !rgraph
                                            !rstate
                                        in

                                        rstop := stop ;
                                        rgraph := graph' ;
                                        rstate := state' ;
                                    done in
                                  let ()
                                    =
                                    simulation.perturbation <-
                                      Some
                                        (fun perturbation ->

                                           let _outputs : Data.t -> unit =
                                             outputs sigs in (* put sigs in simulation *)
                                           let _counter : Counter.t =
                                             simulation.counter in
                                           let g_contact_map : Primitives.contact_map =
                                             contact_map in
                                           let _env : Environment.t =
                                             env in
                                           let _cc_env : Connected_component.Env.t =
                                             domain in
                                           let _graph : Rule_interpreter.t =
                                             graph in
                                           let _state : State_interpreter.t =
                                             state in
                                           let _perturbation : string =
                                             perturbation
                                           in
                                           (*
                                           Kappa_peturbation.toplevel
                                             ~outputs:_outputs
                                             ~counter:_counter
                                             ~contact_map:_contact_map
                                             ~env:_env
                                             ~cc_env:_cc_env
                                             ~graph:_graph
                                             ~state:_state
                                             ~perturbation:_perturbation
                                              *)
                                           failwith ""
                                           )
                                  in
                                  let () =
                                    (* the simulation has ended
                                       don't stop just pause *)
                                    (if !rstop then

                                       match simulation.pause with
                                       | Some _ -> ()
                                       | None ->
                                         simulation.pause <-
                                           Some (Lwt_mvar.create_empty ())
                                     else
                                       ()
                                    )
                                  in
                                  if Lwt_switch.is_on simulation.switch then
                                    (* take a pause here *)
                                    match simulation.pause with
                                    | Some pause ->
                                      Lwt_mvar.take pause
                                      >>= iter
                                    | None ->
                                      (system_process#step_yield ())
                                      >>= iter
                                  else Lwt.return_unit in
                                (iter ()) >>=
                                (fun () ->
                                   let _ =
                                     State_interpreter.end_of_simulation
                                       ~outputs:(outputs sigs) log_form env
                                       simulation.counter !rgraph !rstate in
                                   Lwt.return_unit)))
                   | `Error e ->
                     let () = simulation.error_messages <- e
                     in Lwt.return_unit)
              )
              (function
                | ExceptionDefn.Malformed_Decl error as exn ->
                  let () = simulation.error_messages <-
                      (Api_data.api_location_errors error)
                  in
                  system_process#log ~exn "ExceptionDefn.Malformed_Decl"
                | ExceptionDefn.Internal_Error error as exn ->
                  let () = simulation.error_messages <-
                      (Api_data.api_location_errors error)
                  in
                  system_process#log ~exn "ExceptionDefn.Internal_Error"
                | Invalid_argument error as exn ->
                  let () = simulation.error_messages <-
                      (Api_data.api_message_errors ("Runtime error "^ error))
                  in
                  system_process#log ~exn "Invalid_argument"
                | exn ->
                  let () = simulation.error_messages <-
                      (Api_data.api_message_errors (Printexc.to_string exn)) in
                  system_process#log ~exn (Printexc.to_string exn)
              )))
          in
          Lwt.return (`Ok simulation)
  else
    Api_data.lwt_msg msg_observables_less_than_zero


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
