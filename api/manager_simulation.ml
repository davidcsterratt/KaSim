open Lwt

class manager_simulation
    (environment : Api_common.environment)
    (system_process : Kappa_facade.system_process) : Api.manager_simulation =
object

  method simulation_list
    (workspace_id : Api_types_j.workspace_id)
    (project_id : Api_types_j.project_id) :
    Api_types_j.simulation_catalog Api.result Lwt.t =
    Api_common.bind_project
      environment
      workspace_id
      project_id
      (fun workspace project ->
	let result : Api_types_j.simulation_id list =
	  List.map
	    (Api_common.SimulationCollection.identifier)
	    project.Api_common.simulations
	in
        Lwt.return
          (Api_common.result_ok
             ~result_code:Api.OK result))

  method simulation_start
    (workspace_id : Api_types_j.workspace_id)
    (project_id : Api_types_j.project_id)
    (simulation_parameter : Api_types_j.simulation_parameter) :
    Api_types_j.simulation_id Api.result Lwt.t =
    Api_common.bind_project
      environment
      workspace_id
      project_id
      (fun workspace project ->
	let simulation_id = simulation_parameter.Api_types_j.simulation_id in
        if Api_common.SimulationOperations.exists
          simulation_parameter.Api_types_j.simulation_id
          project
        then
          let message : string =
            Format.sprintf
              "simulation id %s exists"
	      (Api_common.SimulationCollection.id_to_string simulation_id)
             in
             Lwt.return
               (Api_common.result_error_msg ~result_code:Api.CONFLICT message)
           else
             (Kappa_facade.start
		~system_process:system_process
		~parameter:simulation_parameter
		~kappa_code:(Api_common.project_kappa_code project)
             )
             >>=
               (fun (result :
                       (Kappa_facade.t, Api_types_j.errors)
                       Api_types_j.result_data) ->
                 Api_common.result_bind
                   result
                   (fun (t : Kappa_facade.t) ->
                     let simulation : simulation =
                       { simulation_id = simulation_id ;
                         runtime_state = t ;
                       }
                     in
                     let () = project.simulations <-
                       simulation::project.simulations in
                     Lwt.return
                       (Api_common.result_ok
                          ~result_code:Api.CREATED
                          simulation_id))
               )
      )

  method simulation_pause
    (workspace_id : Api_types_j.workspace_id)
    (project_id : Api_types_j.project_id)
    (simulation_id : Api_types_j.simulation_id) :
    unit Api.result Lwt.t =
    Api_common.bind_project
      environment
      workspace_id
      project_id
      simulation_id
      (fun workspace project simulation -> failwith "")

  method simulation_stop
    (workspace_id : Api_types_j.workspace_id)
    (project_id : Api_types_j.project_id)
    (simulation_id : Api_types_j.simulation_id) :
    unit Api.result Lwt.t =
    Api_common.bind_project
      environment
      workspace_id
      project_id
      simulation_id
      (fun workspace project simulation -> failwith "")

  method simulation_perturbation
    (workspace_id : Api_types_j.workspace_id)
    (project_id : Api_types_j.project_id)
    (simulation_id : Api_types_j.simulation_id)
    (_ : Api_types_j.simulation_perturbation) :
    unit Api.result Lwt.t =
    Api_common.bind_project
      environment
      workspace_id
      project_id
      simulation_id
      (fun workspace project simulation -> failwith "")

  method simulation_continue
    (workspace_id : Api_types_j.workspace_id)
    (project_id : Api_types_j.project_id)
    (simulation_id : Api_types_j.simulation_id)
    (_ : Api_types_j.simulation_parameter) :
    unit Api.result Lwt.t =
    Api_common.bind_project
      environment
      workspace_id
      project_id
      simulation_id
      (fun workspace project simulation -> failwith "")

  method simulation_info
    (_ : Api_types_j.workspace_id)
    (_ : Api_types_j.project_id)
    (_ : Api_types_j.simulation_id) :
    Api_types_j.simulation_info Api.result Lwt.t =
    failwith ""

end;;
