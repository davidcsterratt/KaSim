(**
  * loggers.ml
  *
  * a module for KaSim
  * Jérôme Feret, projet Antique, INRIA Paris
  *
  * KaSim
  * Jean Krivine, Université Paris-Diderot, CNRS
  *
  * Creation: 26/01/2016
  * Last modification: 25/05/2016
  * *
  *
  *
  * Copyright 2016  Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

module StringMap = Map.Make (struct type t = string let compare = compare end)
type encoding =
  | HTML_Graph | HTML | HTML_Tabular | DOT | TXT | TXT_Tabular | XLS | Octave
  | Matlab | Maple | Json

type token =
  | String of string
  | Breakable_space

type logger =
  | DEVNUL
  | Formatter of Format.formatter
  | Circular_buffer of string Circular_buffers.t ref
  | Infinite_buffer of string Infinite_buffers.t ref

let breakable x =
  match
    x
  with
  | HTML_Tabular | HTML | HTML_Graph | TXT -> true
  | Json | Matlab | Octave | Maple | DOT | TXT_Tabular | XLS -> false

type t =
  {
    encoding:encoding;
    logger: logger;
    channel_opt: out_channel option;
    id_map: int StringMap.t ref ;
    fresh_id: int ref ;
    mutable current_line: token list;
    nodes: (string * Graph_loggers_sig.options list) list ref ;
    edges: (string * string * Graph_loggers_sig.options list) list ref ;
  }

let refresh_id t =
  let () = t.id_map:=StringMap.empty in
  let () = t.fresh_id:= 1 in
  let () = t.nodes := [] in
  let () = t.edges := [] in
  ()

let get_encoding_format t = t.encoding

let dummy_html_logger =
  {
    id_map = ref StringMap.empty ;
    fresh_id = ref 1 ;
    encoding = HTML;
    logger = DEVNUL;
    channel_opt = None;
    current_line = [];
    nodes = ref [];
    edges = ref []}

let dummy_txt_logger =
  {
    fresh_id = ref 1;
    id_map = ref StringMap.empty;
    encoding = TXT;
    channel_opt = None;
    logger = DEVNUL;
    current_line = [];
    nodes = ref [];
    edges = ref []
  }

(* Warning, we have to keep the character @ when it is followed by a character followed by a letter or a digit should be preserved *)

let dump_clean_string fmt =
  String.iter
    (fun a ->
       if a = '\n' then ()
       else
         Format.fprintf fmt "%c" a)

let clean_string s =
  let buffer = Buffer.create 0 in
  let fmt_buffer = Format.formatter_of_buffer buffer in
  let () = dump_clean_string fmt_buffer s in
  let () = Format.pp_print_flush fmt_buffer () in
  Buffer.contents buffer

let clean fmt =
  let s = Buffer.create 0 in
  let fmt_buffer = Format.formatter_of_buffer s in
  Format.kfprintf
    (fun _ ->
       let () = Format.pp_print_flush fmt_buffer () in
       dump_clean_string fmt (Buffer.contents s))
    fmt_buffer

let fprintf ?fprintnewline:(fprintnewline=false) logger =
  match
    logger.logger, fprintnewline || breakable logger.encoding
  with
  | DEVNUL,_ -> Format.ifprintf Format.std_formatter
  | Formatter fmt, true -> Format.fprintf fmt
  | Formatter fmt, false -> clean fmt
  | Circular_buffer _,bool
  | Infinite_buffer _,bool ->
    let b = Buffer.create 0 in
    let fmt_buffer = Format.formatter_of_buffer b in
    Format.kfprintf
      (fun _ ->
         let () = Format.pp_print_flush fmt_buffer () in
         let str = Buffer.contents b in
         logger.current_line <- (String (if bool then str else clean_string str) )::logger.current_line)
      fmt_buffer

let print_breakable_space logger =
  if breakable logger.encoding
  then
    match
      logger.logger
    with
    | DEVNUL
    | Formatter _ ->
      fprintf logger "@ "
    | Circular_buffer _
    | Infinite_buffer _ ->
      logger.current_line <- Breakable_space::logger.current_line
  else
    fprintf logger " "

let end_of_line_symbol logger =
  match
    logger.encoding
  with
  | HTML | HTML_Graph -> "<Br>"
  | Matlab | Octave | Maple
  | Json | HTML_Tabular | DOT | TXT | TXT_Tabular | XLS -> ""


let dump_token f x =
  match
    x
  with
  | String s ->
    Format.pp_print_string
      f s
  | Breakable_space ->
    Format.fprintf f "@ " (* Check with Pierre *)

let print_newline logger =
  let () =
    fprintf
      ~fprintnewline:true
      logger
      "%s%t"
      (end_of_line_symbol logger)
      (fun f -> Format.pp_print_newline f ())
  in
  match
    logger.logger
  with
  | DEVNUL
  | Formatter _ -> ()
  | Circular_buffer bf ->
    begin
      let bf' =
        Circular_buffers.add
          (Format.asprintf "%a"
             (Pp.list
                (fun _ -> ())
                dump_token)
             (List.rev logger.current_line))
          !bf
      in
      let () = bf:=bf' in
      let () = logger.current_line <- [] in
      ()
    end
  | Infinite_buffer bf ->
    begin
      let bf' =
        Infinite_buffers.add
          (Format.asprintf "%a"
             (Pp.list
                (fun _ -> ())
                dump_token)
             (List.rev logger.current_line))
          !bf
      in
      let () = bf:=bf' in
      let () = logger.current_line <- [] in
      ()
    end



let print_cell logger s =
  let open_cell_symbol,close_cell_symbol =
    match
      logger.encoding
    with
    | HTML_Tabular -> "<TD>","</TD>"
    | TXT_Tabular -> "","\t"
    | Json | Matlab | Octave | Maple | HTML_Graph | HTML | DOT | TXT | XLS -> "",""
  in
  fprintf logger "%s%s%s" open_cell_symbol s close_cell_symbol

let flush_logger logger =
  match
    logger.logger
  with
  | DEVNUL -> ()
  | Formatter fmt -> Format.pp_print_flush fmt ()
  | Circular_buffer _
  | Infinite_buffer _ -> ()

let close_logger logger =
  let () =
    match
      logger.encoding
    with
    | HTML ->
      fprintf logger "</div>\n</body>\n"
    | HTML_Tabular ->
      fprintf logger "</TABLE>\n</div>\n</body>"
    | Json | Matlab | Octave | Maple  | HTML_Graph | DOT | TXT | TXT_Tabular | XLS -> ()
  in
  let () = flush_logger logger in
  ()

let print_preamble logger =
  match
    logger.encoding
  with
  | HTML ->
    fprintf logger "<body>\n<div>\n"
  | HTML_Tabular ->
    fprintf logger "<body>\n<div>\n<TABLE>\n"
  | Json | Matlab | Octave | Maple | HTML_Graph | DOT | TXT | TXT_Tabular | XLS -> ()

let open_logger_from_channel ?mode:(mode=TXT) channel =
  let formatter = Format.formatter_of_out_channel channel in
  let logger =
    {
      id_map = ref StringMap.empty;
      fresh_id = ref 1;
      logger = Formatter formatter;
      channel_opt = Some channel;
      encoding = mode;
      current_line = [];
      nodes = ref [];
      edges = ref [];
    }
  in
  let () = print_preamble logger in
  logger

let open_logger_from_formatter ?mode:(mode=TXT) formatter =
  let logger =
    {
      id_map = ref StringMap.empty;
      fresh_id = ref 1;
      logger = Formatter formatter;
      channel_opt = None;
      encoding = mode;
      current_line = [];
      nodes = ref [];
      edges = ref [];
    }
  in
  let () = print_preamble logger in
  logger

let open_circular_buffer ?mode:(mode=TXT) ?size:(size=10) () =
  {
    id_map = ref StringMap.empty;
    fresh_id = ref 1;
    logger = Circular_buffer (ref (Circular_buffers.create size "" ));
    channel_opt = None;
    encoding = mode;
    current_line = [];
    nodes = ref [];
    edges = ref [];
  }

let open_infinite_buffer ?mode:(mode=TXT) () =
  let logger =
    {
      id_map = ref StringMap.empty;
      fresh_id = ref 1;
      logger = Infinite_buffer (ref (Infinite_buffers.create 0 ""));
      channel_opt = None;
      encoding = mode;
      current_line = [];
      nodes = ref [];
      edges = ref [];
    }
  in
  let () = print_preamble logger in
  logger

let open_row logger =
  match
    logger.encoding
  with
  | HTML_Tabular -> fprintf logger "<tr>"
  | Json | Matlab | Octave | Maple | HTML_Graph | XLS | HTML | DOT | TXT | TXT_Tabular -> ()

let close_row logger =
  match
    logger.encoding
  with
  | HTML_Tabular -> fprintf logger "<tr>@."
  | Json | Matlab | Octave | Maple | HTML_Graph | XLS | HTML | DOT | TXT | TXT_Tabular -> fprintf logger "@."

let formatter_of_logger logger =
  match
    logger.logger
  with
  | Formatter fmt -> Some fmt
  | DEVNUL
  | Circular_buffer _
  | Infinite_buffer _ -> None

let redirect logger fmt =
  {logger with logger = Formatter fmt}

let print_as_logger logger f =
  fprintf logger "%t" f

let flush_buffer logger fmt =
  match
    logger.logger
  with
  | DEVNUL
  | Formatter _ -> ()
  | Circular_buffer a -> Circular_buffers.iter (Format.fprintf fmt "%s") !a
  | Infinite_buffer b -> Infinite_buffers.iter (Format.fprintf fmt "%s") !b

let fprintf logger = fprintf ~fprintnewline:false logger
let fresh_id logger =
  let i = !(logger.fresh_id) in
  let () = logger.fresh_id := i+1 in
  i

let int_of_string_id logger string =
  try
    StringMap.find string !(logger.id_map)
  with
  | Not_found ->
    let i = fresh_id logger in
    let () = logger.id_map := StringMap.add string i !(logger.id_map) in
    i
let channel_of_logger logger = logger.channel_opt

let add_node t s d = t.nodes:= (s,d)::(!(t.nodes))
let add_edge t s1 s2 d = t.edges:= (s1,s2,d)::(!(t.edges))
let graph_of_logger logger = List.rev !(logger.nodes), List.rev !(logger.edges)

let dump_json logger json =
  let channel_opt = channel_of_logger logger in
  let () =
    match channel_opt
    with
    | None -> ()
    | Some channel ->
      let () =
        Yojson.Basic.to_channel channel json
      in
      ()
  in
  ()


let line_to_json line =
  `Assoc ["line", JsonUtil.of_string line]

let line_of_json json =
  match
    json
  with
  | `Assoc ["line", `String s] -> s
  | _ -> raise (Yojson.Basic.Util.Type_error (JsonUtil.build_msg "line" ,json))


let gen_iter iter list =
  let output = ref [] in
  let () = iter (fun line -> output:=line::!output) list in
  JsonUtil.of_list line_to_json (List.rev !output)

let to_json logger =
  match
    logger.logger
  with
  | DEVNUL
  | Formatter _ -> `List []
  | Circular_buffer a ->
    gen_iter Circular_buffers.iter !a
  | Infinite_buffer b ->
    gen_iter Infinite_buffers.iter !b

let of_json = JsonUtil.to_list ~error_msg:"line list" line_of_json
