(**
   * ckappa_site_graph.ml
   * openkappa
   * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
   *
   * Creation: 2016, the 17th of November
   * Last modification: Time-stamp: <Nov 27 2016>
   *
   * Site graph
   *
   * Copyright 2010,2011,2012,2013,2014,2015,2016 Institut National de Recherche
   * en Informatique et en Automatique.
   * All rights reserved.  This file is distributed
   * under the terms of the GNU Library General Public License *)

(***************************************************************************)


let print_internal_pattern_aux ?logger parameters error kappa_handler
    internal_constraints_list =
  let logger =
    match
      logger
    with
    | None -> Remanent_parameters.get_logger parameters
    | Some a -> a
  in
  let (domain_name, lemma_list) = internal_constraints_list in
  let () =
    Loggers.fprintf logger
      "------------------------------------------------------------\n";
    Loggers.fprintf logger "* Export %s to JSon (internal constraints_list):\n"
      domain_name;
    Loggers.fprintf logger
      "------------------------------------------------------------\n";
  in
  List.fold_left (fun (error, bool) lemma ->
      let hyp = Remanent_state.get_hyp lemma in
      let refinement = Remanent_state.get_refinement lemma in
      let error =
        Ckappa_backend.Ckappa_backend.print
          logger parameters error
          kappa_handler
          hyp
      in
      let () = Loggers.fprintf logger "=> [" in
      let error, b =
        match refinement with
        | [] -> error, false
        | [hyp] ->
          Ckappa_backend.Ckappa_backend.print logger parameters error
            kappa_handler
            hyp, false
        | _::_ as l ->
          List.fold_left (fun (error, bool) hyp ->
              let () =
                Loggers.print_newline
                  (Remanent_parameters.get_logger parameters)
              in
              let () =
                Loggers.fprintf
                  (Remanent_parameters.get_logger parameters)
                  (if bool  then "\t\tv " else "\t\t  ")
              in
              let error =
                Ckappa_backend.Ckappa_backend.print logger parameters error
                  kappa_handler
                  hyp
              in
              error, true
            ) (error, false) (List.rev l)
      in
      let () = Loggers.fprintf logger "]" in
      let () = Loggers.print_newline logger in
      error, b
    ) (error, false) lemma_list

(*print the information as the output of non relational properties*)
let print_internal_pattern ?logger parameters error kappa_handler
    list =
  let logger' =
    match
      logger
    with
    | None -> Remanent_parameters.get_logger parameters
    | Some a -> a
  in
  let error =
    List.fold_left (fun error pattern ->
        let error, _ =
          print_internal_pattern_aux
            ?logger parameters error
            kappa_handler
            pattern
        in
        let () = Loggers.print_newline logger' in
        error
      ) error list
  in
  error

(***************************************************************************)

let print_for_list logger parameter error kappa_handler t =
  let _bool =
    List.fold_left (fun bool (agent_string, site_map) ->
        let _ =
          Ckappa_backend.Ckappa_backend.print_aux logger parameter error
            kappa_handler
            agent_string site_map bool
        in
        let () = Loggers.fprintf logger ")" in
        true
      ) false t
  in
  let () = Loggers.fprintf logger " " in
  error

let print_pattern_aux ?logger
    parameters error kappa_handler constraints_list
  =
  let logger =
    match
      logger
    with
    | None -> Remanent_parameters.get_logger parameters
    | Some a -> a
  in
  let (domain_name, lemma_list) = constraints_list in
  let () =
    Loggers.fprintf logger
      "------------------------------------------------------------\n";
    Loggers.fprintf logger "* Export %s to JSon (constraints_list):\n" domain_name;
    Loggers.fprintf logger
      "------------------------------------------------------------\n";
  in
  List.fold_left (fun (error, bool) lemma ->
      let hyp = Remanent_state.get_hyp lemma in
      let refinement = Remanent_state.get_refinement lemma in
      let error =
        print_for_list logger parameters error
          kappa_handler
          hyp
      in
      let () = Loggers.fprintf logger " => [" in
      (*refinement*)
      let error, b =(*TODO*)
        (*match lemma.refinement with
          | [] -> error, false
          | [hyp] ->
          print_for_list logger parameters error
          kappa_handler
            hyp, false
          | _:: _ as l ->*)
        List.fold_left (fun (error, bool) hyp ->
            let () =
              Loggers.print_newline
                (Remanent_parameters.get_logger parameters)
            in
            let () =
              Loggers.fprintf
                (Remanent_parameters.get_logger parameters)
                (if bool  then "\t\tv " else "\t\t  ")
            in
            let error =
              print_for_list logger parameters error kappa_handler hyp
            in
            error, true
          ) (error, false) (List.rev refinement)
      in
      let () = Loggers.fprintf logger "]" in
      let () = Loggers.print_newline logger in
      error, b
    ) (error, false) lemma_list

let print_pattern ?logger parameters error kappa_handler list =
  let logger' =
    match
      logger
    with
    | None -> Remanent_parameters.get_logger parameters
    | Some a -> a
  in
  let error =
    List.fold_left (fun error pattern ->
        let error, _ =
          print_pattern_aux ?logger parameters error kappa_handler pattern
        in
        let () = Loggers.print_newline logger' in
        error
      ) error list
  in
  error

(*******************************************************************)

let site_graph_to_list error string_version =
  let error, current_list =
    Ckappa_sig.Agent_id_map_and_set.Map.fold
      (fun _ (agent_string, site_map) (error, current_list) ->
         (*-----------------------------------------*)
         let site_graph =
           (agent_string, site_map) :: current_list
         in
         error, site_graph
      ) string_version (error, [])
  in
  error, List.rev current_list

let site_graph_list_to_list error list =
  List.fold_left (fun (error, current_list) t ->
      let string_version = Ckappa_backend.Ckappa_backend.get_string_version t in
      let error, site_graph = site_graph_to_list error string_version in
      error, site_graph :: current_list
    ) (error, []) list

let pair_list_to_list parameters error kappa_handler pattern
    agent_id1 site_type1' agent_id2 site_type2'
    pair_list =
  List.fold_left (fun (error, current_list) l ->
      match l with
      | [siteone, state1; sitetwo, state2] when
          siteone == Ckappa_sig.fst_site
          && sitetwo == Ckappa_sig.snd_site ->
        let error, pattern =
          Ckappa_backend.Ckappa_backend.add_state
            parameters error kappa_handler
            agent_id1
            site_type1'
            state1
            pattern
        in
        let error, pattern =
          Ckappa_backend.Ckappa_backend.add_state
            parameters error kappa_handler
            agent_id2
            site_type2'
            state2
            pattern
        in
        let string_version =
          Ckappa_backend.Ckappa_backend.get_string_version
            pattern
        in
        let error, site_graph = site_graph_to_list error string_version in
        error, site_graph :: current_list
      | _ -> Exception.warn parameters error __POS__ Exit []
    ) (error, []) pair_list

let internal_pair_list_to_list parameters error kappa_handler pattern
    agent_id1 site_type1' agent_id2 site_type2' pair_list =
  List.fold_left (fun (error, current_list) l ->
      match l with
      | [siteone, state1; sitetwo, state2] when
          siteone == Ckappa_sig.fst_site
          && sitetwo == Ckappa_sig.snd_site ->
        let error, pattern =
          Ckappa_backend.Ckappa_backend.add_state
            parameters error kappa_handler
            agent_id1
            site_type1'
            state1
            pattern
        in
        let error, pattern =
          Ckappa_backend.Ckappa_backend.add_state
            parameters error kappa_handler
            agent_id2
            site_type2'
            state2
            pattern
        in
        error, pattern :: current_list
      | _ -> Exception.warn parameters error __POS__ Exit []
    ) (error, []) pair_list

(******************************************************************)
