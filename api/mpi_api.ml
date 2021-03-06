open Lwt.Infix

exception TimeOut
exception BadResponse of Mpi_message_j.response


let on_message
    (manager: Api.manager)
    (post_message : (string -> unit Lwt.t))
    (text_message : string) : unit Lwt.t =
  let message : Mpi_message_j.request Mpi_message_j.message =
    Mpi_message_j.message_of_string
      Mpi_message_j.read_request text_message
  in
  let handler :
    'a. ('a -> Mpi_message_j.response)->
    'a ->
    unit Lwt.t =
    (fun pack result ->
       let message :  Mpi_message_j.response Mpi_message_j.message =
         { Mpi_message_j.id = message.Mpi_message_j.id ;
         Mpi_message_j.data = pack result } in
       let text : string =
         Mpi_message_j.string_of_message
           Mpi_message_j.write_response message
       in
       post_message text)
  in
  match message.Mpi_message_j.data with
  | `EnvironmentInfo () ->
    (manager#environment_info ()) >>=
    (handler (fun result -> `EnvironmentInfo result))
  | `FileCreate (project_id,file) ->
    (manager#file_create project_id file) >>=
    (handler (fun result -> `FileCreate result))
  | `FileDelete (project_id,file_id) ->
    (manager#file_delete project_id file_id) >>=
    (handler (fun result -> `FileDelete result))
  | `FileGet (project_id,file_id) ->
    (manager#file_get project_id file_id) >>=
    (handler (fun result -> `FileGet result))
  | `FileInfo project_id ->
    (manager#file_info project_id) >>=
    (handler (fun result -> `FileInfo result))
  | `FileUpdate (project_id,file_id,file_modification) ->
    (manager#file_update project_id file_id file_modification) >>=
    (handler (fun result -> `FileUpdate result))
  | `ProjectCreate project_parameter ->
    (manager#project_create project_parameter) >>=
    (handler (fun result -> `ProjectCreate result))
  | `ProjectDelete project_id ->
    (manager#project_delete project_id) >>=
    (handler (fun result -> `ProjectDelete result))
  | `ProjectInfo () ->
    (manager#project_info ()) >>=
    (handler (fun result -> `ProjectInfo result))
  | `SimulationContinue (project_id,simulation_id,simulation_parameter) ->
    (manager#simulation_continue project_id simulation_id simulation_parameter) >>=
    (handler (fun result -> `SimulationContinue result))
  | `SimulationDelete (project_id,simulation_id) ->
    (manager#simulation_delete project_id simulation_id) >>=
    (handler (fun result -> `SimulationDelete result))
  | `SimulationDetailDistance (project_id,simulation_id,distance_id) ->
    (manager#simulation_detail_distance project_id simulation_id distance_id) >>=
    (handler (fun result -> `SimulationDetailDistance result))
  | `SimulationDetailFileLine (project_id,simulation_id,file_line_id) ->
    (manager#simulation_detail_file_line project_id simulation_id file_line_id) >>=
    (handler (fun result -> `SimulationDetailFileLine result))
  | `SimulationDetailFluxMap (project_id,simulation_id,flux_map_id) ->
    (manager#simulation_detail_flux_map project_id simulation_id flux_map_id) >>=
    (handler (fun result -> `SimulationDetailFluxMap result))
  | `SimulationDetailLogMessage (project_id,simulation_id) ->
    (manager#simulation_detail_log_message project_id simulation_id) >>=
    (handler (fun result -> `SimulationDetailLogMessage result))
  | `SimulationDetailPlot (project_id,simulation_id) ->
    (manager#simulation_detail_plot project_id simulation_id) >>=
    (handler (fun result -> `SimulationDetailPlot result))
  | `SimulationDetailSnapshot (project_id,simulation_id,snapshot_id) ->
    (manager#simulation_detail_snapshot project_id simulation_id snapshot_id) >>=
    (handler (fun result -> `SimulationDetailSnapshot result))
  | `SimulationInfo (project_id,simulation_id) ->
    (manager#simulation_info project_id simulation_id) >>=
    (handler (fun result -> `SimulationInfo result))
  | `SimulationInfoDistance (project_id,simulation_id) ->
    (manager#simulation_info_distance project_id simulation_id) >>=
    (handler (fun result -> `SimulationInfoDistance result))
  | `SimulationInfoFileLine (project_id,simulation_id) ->
    (manager#simulation_info_file_line project_id simulation_id) >>=
    (handler (fun result -> `SimulationInfoFileLine result))
  | `SimulationInfoFluxMap (project_id,simulation_id) ->
    (manager#simulation_info_flux_map project_id simulation_id) >>=
    (handler (fun result -> `SimulationInfoFluxMap result))
  | `SimulationInfoSnapshot (project_id,simulation_id) ->
    (manager#simulation_info_snapshot project_id simulation_id) >>=
    (handler (fun result -> `SimulationInfoSnapshot result))
  | `SimulationList project_id ->
    (manager#simulation_list project_id) >>=
    (handler (fun result -> `SimulationList result))
  | `SimulationPause (project_id,simulation_id) ->
    (manager#simulation_pause project_id simulation_id) >>=
    (handler (fun result -> `SimulationPause result))
  | `SimulationPerturbation (project_id,simulation_id,simulation_perturbation) ->
    (manager#simulation_perturbation project_id simulation_id simulation_perturbation) >>=
    (handler (fun result -> `SimulationPerturbation result))
  | `SimulationStart (project_id,simulation_parameter) ->
    (manager#simulation_start project_id simulation_parameter) >>=
    (handler (fun result -> `SimulationStart result))

class type virtual manager_base_type =
  object
    method virtual message :
      Mpi_message_j.request ->
      Mpi_message_j.response Api.result Lwt.t

    inherit Api.manager
end

class virtual  manager_base () : manager_base_type =
  object(self)

    method virtual message :
      Mpi_message_j.request ->
      Mpi_message_j.response Api.result Lwt.t

    method environment_info () :
      Api_types_j.environment_info Api.result Lwt.t =
      self#message (`EnvironmentInfo ())
      >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `EnvironmentInfo
                (result : Mpi_message_t.environment_info Mpi_message_t.result) ->
              Lwt.return result
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))

    method file_create
        (project_id : Api_types_j.project_id)
        (file : Api_types_j.file) :
      Api_types_j.file_metadata Api_types_j.file_result Api.result Lwt.t =
      self#message (`FileCreate (project_id,file)) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `FileCreate result ->
              Lwt.return result
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))

    method file_delete
        (project_id : Api_types_j.project_id)
        (file_id : Api_types_j.file_id) :
      unit Api_types_j.file_result Api.result Lwt.t =
      self#message (`FileDelete (project_id,file_id)) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `FileDelete result ->
              Lwt.return result
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))


    method file_get
        (project_id : Api_types_j.project_id)
        (file_id : Api_types_j.file_id) :
      Api_types_j.file Api.result Lwt.t =
      self#message (`FileGet (project_id,file_id)) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `FileGet result ->
              Lwt.return result
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))


    method file_info
      (project_id : Api_types_j.project_id) :
      Api_types_j.file_info Api.result Lwt.t =
      self#message (`FileInfo project_id) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `FileInfo result ->
              Lwt.return result
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))


    method file_update
        (project_id : Api_types_j.project_id)
        (file_id : Api_types_j.file_id)
        (file_modification : Api_types_j.file_modification) :
      Api_types_j.file_metadata Api_types_j.file_result Api.result Lwt.t =
      self#message (`FileUpdate (project_id,file_id,file_modification)) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `FileUpdate result ->
              Lwt.return result
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))


    method project_create
      (project_parameter : Api_types_j.project_parameter) :
      Api_types_j.project_id Api.result Lwt.t =
      self#message (`ProjectCreate project_parameter) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `ProjectCreate result ->
              Lwt.return result
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))


    method project_delete
        (project_id : Api_types_j.project_id) :
      unit Api.result Lwt.t =
      self#message (`ProjectDelete project_id) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `ProjectDelete result ->
              Lwt.return result
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response))

          )


    method project_info () : Api_types_j.project_info Api.result Lwt.t =
      self#message (`ProjectInfo ()) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `ProjectInfo result ->
              Lwt.return result
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))


    method simulation_continue
      (project_id : Api_types_j.project_id)
      (simulation_id : Api_types_j.simulation_id)
      (simulation_parameter :Api_types_j.simulation_parameter) :
      unit Api.result Lwt.t =
      self#message (`SimulationContinue
                      (project_id,simulation_id,simulation_parameter)) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `SimulationContinue result ->
              Lwt.return result
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))


    method simulation_delete
      (project_id : Api_types_j.project_id)
      (simulation_id : Api_types_j.simulation_id) :
      unit Api.result Lwt.t =
      self#message (`SimulationDelete (project_id,simulation_id)) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `SimulationDelete result ->
              Lwt.return result
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))


    method simulation_detail_distance
      (project_id : Api_types_j.project_id)
      (simulation_id : Api_types_j.simulation_id)
      (distance_id : Api_types_j.distance_id) :
      Api_types_j.distance Api.result Lwt.t =
      self#message (`SimulationDetailDistance
                      (project_id,simulation_id,distance_id)) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `SimulationDetailDistance distance ->
              Lwt.return distance
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))


    method simulation_detail_file_line
      (project_id : Api_types_j.project_id)
      (simulation_id : Api_types_j.simulation_id)
      (file_line_id : Api_types_j.file_line_id) :
      Api_types_j.file_line list Api.result Lwt.t =
      self#message (`SimulationDetailFileLine
                      (project_id,simulation_id,file_line_id)) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `SimulationDetailFileLine file_line_list ->
              Lwt.return file_line_list
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))


    method simulation_detail_flux_map
      (project_id : Api_types_j.project_id)
      (simulation_id : Api_types_j.simulation_id)
      (flux_map_id : Api_types_j.flux_map_id) :
      Api_types_j.flux_map Api.result Lwt.t =
      self#message (`SimulationDetailFluxMap
                      (project_id,simulation_id,flux_map_id)) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `SimulationDetailFluxMap flux_map ->
              Lwt.return flux_map
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))

    method simulation_detail_log_message
      (project_id : Api_types_j.project_id)
      (simulation_id : Api_types_j.simulation_id) :
      Api_types_j.log_message list Api.result Lwt.t =
      self#message (`SimulationDetailLogMessage
                      (project_id,simulation_id)) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `SimulationDetailLogMessage log_message ->
              Lwt.return log_message
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))

    method simulation_detail_plot
        (project_id : Api_types_j.project_id)
        (simulation_id : Api_types_j.simulation_id) :
      Api_types_j.plot Api.result Lwt.t =
      self#message (`SimulationDetailPlot
                      (project_id,simulation_id)) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `SimulationDetailPlot plot ->
              Lwt.return plot
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))

    method simulation_detail_snapshot
      (project_id : Api_types_j.project_id)
      (simulation_id : Api_types_j.simulation_id)
      (snapshot_id : Api_types_j.snapshot_id) :
      Api_types_j.snapshot Api.result Lwt.t =
      self#message (`SimulationDetailSnapshot
                      (project_id,simulation_id,snapshot_id)) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `SimulationDetailSnapshot snapshot ->
              Lwt.return snapshot
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))

    method simulation_info
      (project_id : Api_types_j.project_id)
      (simulation_id : Api_types_j.simulation_id) :
      Api_types_j.simulation_info Api.result Lwt.t =
      self#message (`SimulationInfo
                      (project_id,simulation_id)) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `SimulationInfo simulation_status ->
              Lwt.return simulation_status
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))

    method simulation_info_distance
      (project_id : Api_types_j.project_id)
      (simulation_id : Api_types_j.simulation_id) :
      Api_types_j.distance_info Api.result Lwt.t =
      self#message (`SimulationInfoDistance
                      (project_id,simulation_id)) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `SimulationInfoDistance info ->
              Lwt.return info
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))

    method simulation_info_file_line
      (project_id : Api_types_j.project_id)
      (simulation_id : Api_types_j.simulation_id) :
      Api_types_j.file_line_info Api.result Lwt.t =
      self#message (`SimulationInfoFileLine
                      (project_id,simulation_id)) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `SimulationInfoFileLine info ->
              Lwt.return info
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))

    method simulation_info_flux_map
      (project_id : Api_types_j.project_id)
      (simulation_id : Api_types_j.simulation_id) :
      Api_types_j.flux_map_info Api.result Lwt.t =
      self#message (`SimulationInfoFluxMap
                      (project_id,simulation_id)) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `SimulationInfoFluxMap info ->
              Lwt.return info
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))

    method simulation_info_snapshot
      (project_id : Api_types_j.project_id)
      (simulation_id : Api_types_j.simulation_id) :
      Api_types_j.snapshot_info Api.result Lwt.t =
      self#message (`SimulationInfoSnapshot
                      (project_id,simulation_id)) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `SimulationInfoSnapshot info ->
              Lwt.return info
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))

    method simulation_list
      (project_id : Api_types_j.project_id)
      : Api_types_j.simulation_catalog Api.result Lwt.t =
      self#message (`SimulationList project_id) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `SimulationList list ->
              Lwt.return list
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))


    method simulation_pause
      (project_id : Api_types_j.project_id)
      (simulation_id : Api_types_j.simulation_id) :
      unit Api.result Lwt.t =
      self#message (`SimulationPause (project_id,simulation_id)) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `SimulationPause result ->
              Lwt.return result
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))



    method simulation_perturbation
      (project_id : Api_types_j.project_id)
      (simulation_id : Api_types_j.simulation_id)
      (simulation_perturbation : Api_types_j.simulation_perturbation) :
      unit Api.result Lwt.t =
      self#message (`SimulationPerturbation
                     (project_id,simulation_id,simulation_perturbation)) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `SimulationPerturbation result ->
              Lwt.return result
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))


    method simulation_start
      (project_id : Api_types_j.project_id)
      (simulation_parameter : Api_types_j.simulation_parameter)
      : Api_types_j.simulation_id Api.result Lwt.t =
      self#message (`SimulationStart
                     (project_id,simulation_parameter)) >>=
      Api_common.result_bind_lwt
        ~ok:(function
            | `SimulationStart simulation_id ->
              Lwt.return simulation_id
            | response ->
              Lwt.return
                (Api_common.result_error_exception
                   (BadResponse response)))

  end

module IntMap = Mods.IntMap
type context = { mailboxes : Mpi_message_j.response Lwt.u IntMap.t ;
                 id : int }

class type virtual manager_mpi_type =
  object
    method virtual post_message : string -> unit
    method virtual sleep : float -> unit Lwt.t
    method virtual post_message : string -> unit
    method message : Mpi_message_j.request -> Mpi_message_j.response Api.result Lwt.t
    method receive : string -> unit

    inherit Api.manager
  end

class virtual manager ?(timeout : float = 10.) () : manager_mpi_type =
  object(self)
    val mutable context =
      { mailboxes = IntMap.empty ; id = 0 }

    method virtual sleep : float -> unit Lwt.t
    method virtual post_message : string -> unit

    method receive (response_text : string) =
      let message : Mpi_message_j.response Mpi_message_j.message =
        Mpi_message_j.message_of_string
          Mpi_message_j.read_response response_text in
      match IntMap.find_option message.Mpi_message_j.id context.mailboxes with
      | Some value -> Lwt.wakeup value message.Mpi_message_j.data
      | None -> ()

    method message (request : Mpi_message_j.request) :
      Mpi_message_j.response Api.result Lwt.t =
      let result,feeder = Lwt.task () in
      let () = context <- { context with id = context.id + 1 } in
      let message : Mpi_message_j.request Mpi_message_j.message =
        { Mpi_message_j.id = context.id ;
          Mpi_message_j.data = request } in
      let message_text : string =
        Mpi_message_j.string_of_message
          Mpi_message_j.write_request message
      in
      let () = self#post_message message_text in
      let () = context <-
          { context with
            mailboxes = IntMap.add context.id feeder context.mailboxes } in
      Lwt.pick [self#sleep timeout >>= (fun () -> Lwt.fail TimeOut);
                (result >>=
                 (fun (response : Mpi_message_j.response) ->
                    Lwt.return (Api_common.result_ok response)))]



    inherit manager_base ()
  end

let default_message_delimter : char = '\x1e' (* "\t" *)
