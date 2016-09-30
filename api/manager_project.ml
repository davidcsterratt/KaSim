open Lwt

class manager_project
  (environment : Api_common.environment)
  (system_process : Kappa_facade.system_process) : Api.manager_project =
object

  method project_list
    (workspace_id : Api_types_j.workspace_id) :
    Api_types_j.project_catalog Api.result Lwt.t =
      Api_common.WorkspaceOperations.bind
        workspace_id
        environment
        (fun workspace ->
          let projects : Api_common.project list =
            Api_common.ProjectCollection.list workspace  in
          let ids =
            List.map Api_common.ProjectCollection.identifier projects in
          Lwt.return (Api_common.result_ok ids)
        )

  method project_create
    (workspace_id : Api_types_j.workspace_id)
    (project_parameter :Api_types_j.project_parameter) :
    Api_types_j.project_id Api.result Lwt.t =
      Api_common.WorkspaceOperations.bind
        workspace_id
        environment
        (fun workspace ->
          let project_id : Api_types_j.project_id =
            project_parameter.Api_types_j.project_id
          in
          if Api_common.ProjectOperations.exists project_id workspace
          then
            let message : string =
              Format.sprintf
		"project %s exists"
		(Api_common.ProjectCollection.id_to_string project_id)
            in
            Lwt.return
              (Api_common.result_error_msg ~result_code:Api.CONFLICT message)
          else
            let kappa_code = "" in
            Api_common.parse_code
              ~system_process:system_process
              ~kappa_code:kappa_code
             >>=
              (fun parse_state ->
                return { Api_common.project_id = project_id ;
                         Api_common.simulations = [] ;
                         Api_common.files = [] ;
                         Api_common.parse_state = parse_state ; })
             >>=
              (fun project ->
                let () =
                  Api_common.ProjectCollection.update
                    workspace
                    (project::(Api_common.ProjectCollection.list workspace))
                in
                Lwt.return
                  (Api_common.result_ok ~result_code:Api.CREATED project_id)
              ))

  method project_info
    (workspace_id : Api_types_j.workspace_id)
    (project_id : Api_types_j.project_id) :
    Api_types_j.project_info Api.result Lwt.t =
      Api_common.WorkspaceOperations.bind
        workspace_id
        environment
        (fun workspace ->
          Api_common.ProjectOperations.bind
            project_id
            workspace
            (fun project ->
              Lwt.return
                (Api_common.result_ok
                   ~result_code:Api.OK ())))

  method project_update
    (workspace_id : Api_types_j.workspace_id)
    (project_id : Api_types_j.project_id)
    (modification : Api_types_j.project_modification) :
    Api_types_j.project_info Api.result Lwt.t =
      Api_common.WorkspaceOperations.bind
        workspace_id
        environment
        (fun workspace ->
          Api_common.ProjectOperations.bind
            project_id
            workspace
            (fun project ->
              Lwt.return
                (Api_common.result_ok
                   ~result_code:Api.OK ())))

  method project_delete
    (workspace_id : Api_types_j.workspace_id)
    (project_id :Api_types_j.project_id) :
    unit Api.result Lwt.t =
      Api_common.WorkspaceOperations.bind
        workspace_id
        environment
        (fun workspace ->
          Api_common.ProjectOperations.bind
            project_id
            workspace
            (fun _ ->
              let not_project =
                (fun project ->
                  not (Api_common.ProjectOperations.refs project_id project))
              in
              let projects =
                List.filter
                  not_project
              (Api_common.ProjectCollection.list workspace)
              in
              let () =
                Api_common.ProjectCollection.update
                  workspace
                  projects
              in
              Lwt.return (Api_common.result_ok ())
            )
        )

end;;
