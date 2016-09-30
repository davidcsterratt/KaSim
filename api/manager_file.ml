class manager_file
    (environment : Api_common.environment)
    (system_process : Kappa_facade.system_process) =
object

  method file_list
    (_ : Api_types_j.workspace_id)
    (_ : Api_types_j.project_id) :
    Api_types_j.file_catalog Api.result Lwt.t =
    failwith ""


  method file_create
    (_ : Api_types_j.workspace_id)
    (_ : Api_types_j.project_id)
    (_ : Api_types_j.file) :
    Api_types_j.file_metadata Api.result Lwt.t =
    failwith ""

  method file_get
    (_ : Api_types_j.workspace_id)
    (_ : Api_types_j.project_id)
    (_ : Api_types_j.file_id) :
    Api_types_j.file Api.result Lwt.t =
    failwith ""

  method file_update
    (_ : Api_types_j.workspace_id)
    (_ : Api_types_j.project_id)
    (_ : Api_types_j.file_id)
    (_ : Api_types_j.file_modification) :
    Api_types_j.file_metadata Api.result Lwt.t =
    failwith ""

  method file_delete
    (_ : Api_types_j.workspace_id)
    (_ : Api_types_j.project_id)
    (_ : Api_types_j.file_id) :
    unit Api.result Lwt.t =
    failwith ""
  end;;
