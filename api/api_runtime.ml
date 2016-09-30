open Lwt
(* the cake pattern *)
class manager
    (system_process : Kappa_facade.system_process) =
  let environment : Api_common.environment = Api_common.environment_new () in
object
  inherit Manager_environment.manager_environment environment system_process
  inherit Manager_workspace.manager_workspace environment system_process
  inherit Manager_project.manager_project environment system_process
  inherit Manager_simulation.manager_simulation environment system_process
  inherit Manager_file.manager_file environment system_process
end;;
