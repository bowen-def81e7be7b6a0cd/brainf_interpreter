open Brainf_interpreter

type cmd_args = {
  is_string: bool;
  cell_size: int;
  program_string: string
}

let read_file_contents (file_name: string) : string = ""

let initialize_program_from_string (program_string: string) (cell_size: int) : Brainf_interpreter.program_state =
  let insts, jumps = Brainf_interpreter.parse_program_string program_string in
    Brainf_interpreter.initialize_program insts jumps cell_size

let parse_cmd_args (argv: string array) : cmd_args =
  let rec parse a ca =
    match a with
      []             | _::[]                   -> ca
    | "-s"::rest     | "--string"::rest        -> parse rest { ca with is_string = true }
    | "-c"::cs::rest | "--cell-size"::cs::rest -> parse rest { ca with cell_size = int_of_string cs }
    | x::_                                     -> parse rest { ca with program_string = x }
  in
    parse (Array.to_list argv) { is_string = false; cell_size = 256; program_string = "" }

let print_usage () = ()

let () = 
  let args = 
    try parse_cmd_args Sys.argv with
      Failure _         -> raise Failure "invalid argument for cell size"
      ex                -> print_usage (); raise ex
  in
    let program = 
      if args.is_string then
        initialize_program_from_string (* TODO *) args.cell_size
      else
        initialize_program_from_string (read_file_contents (* TODO *)) args.cell_size
    in
      let end_state = Brainf_interpreter.execute_program program in
        ignore end_state (* TODO *)

      (* let f s = 
  let program = initialize_program_from_string s 256 in (* TODO *)
    let end_state = Brainf_interpreter.execute_program program in
      end_state;; 
      
      let get_mem (state: Brainf_interpreter.program_state) = state.memory_to_ptr @ (List.rev state.memory_after_ptr);; *)