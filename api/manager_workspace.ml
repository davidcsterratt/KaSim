class manager_workspace
    (environment : Api_common.environment)
    (system_process : Kappa_facade.system_process) : Api.manager_workspace
  =
  object
    method workspace_list
      ():
      Api_types_j.workspace_catalog Api.result Lwt.t =
          Lwt.return
            (Api_common.result_ok
               (List.map
                  Api_common.WorkspaceCollection.identifier
                  (Api_common.WorkspaceCollection.list environment)))

    method workspace_create
      (parameter : Api_types_j.workspace_parameter) :
      Api_types_j.workspace_id Api.result Lwt.t =
      let workspace_id : Api_types_j.workspace_id =
        parameter.Api_types_j.workspace_id
      in
      if Api_common.WorkspaceOperations.exists workspace_id environment then
        let message : string =
          Format.sprintf
	    "workspace %s exists"
	    (Api_common.WorkspaceCollection.id_to_string project_id)
        in
        Lwt.return
          (Api_common.result_error_msg ~result_code:Api.CONFLICT message)
      else
        let workspace : Api_common.workspace =
          { Api_common.workspace_id = workspace_id ;
            Api_common.projects = [] ; }
        in
        let () =
          Api_common.WorkspaceCollection.update
            environment
            (workspace::(Api_common.WorkspaceCollection.list environment))
        in
        Lwt.return
          (Api_common.result_ok ~result_code:Api.CREATED workspace_id)


    method workspace_info
      (workspace_id : Api_types_j.workspace_id) :
      Api_types_j.workspace_info Api.result Lwt.t =
      Api_common.WorkspaceOperations.bind
        workspace_id
        environment
        (fun _ ->
          Lwt.return (Api_common.result_ok ())
        )


    method workspace_delete
      (workspace_id : Api_types_j.workspace_id) :
      unit Api.result Lwt.t =
      Api_common.WorkspaceOperations.bind
        workspace_id
        environment
        (fun _ ->
          let not_workspace =
            (fun workspace ->
              not (Api_common.WorkspaceOperations.refs workspace_id workspace))
          in
          let workspaces =
            List.filter
              not_workspace
              (Api_common.WorkspaceCollection.list environment)
          in
          let () =
            Api_common.WorkspaceCollection.update
              environment
              workspaces
          in
          Lwt.return (Api_common.result_ok ())
        )

  end;;
