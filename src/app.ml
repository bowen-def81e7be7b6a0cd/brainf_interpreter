
type cmd_args = {
  is_string: bool;
  cell_size: int;
  input_string: string
}

exception Print_usage

let read_file_contents (file_name: string) : string = ""

let initialize_program_from_string (program_string: string) (cell_size: int) : BrainfInterpreter.program_state =
  let insts, jumps = BrainfInterpreter.parse_program_string program_string in
    BrainfInterpreter.initialize_program insts jumps cell_size

let parse_cmd_args (argv: string array) : cmd_args =
  let rec parse a ca =
    match a with
      []             | _::[]                   -> ca
    | "-h"::_        | "--help"::_             -> raise Print_usage
    | "-s"::rest     | "--string"::rest        -> parse rest { ca with is_string = true }
    | "-c"::cs::rest | "--cell-size"::cs::rest -> parse rest { ca with cell_size = int_of_string cs }
    | x::rest                                  -> parse rest { ca with input_string = x }
  in
    parse (Array.to_list argv) { is_string = false; cell_size = 256; input_string = "" }

let print_usage () =
  let usage_string =
    "bfi: Brainfuck Interpreter" ^ "\n" ^
    "Usage: bfi [OPTIONS] FILE" ^ "\n" ^
    "An interpreter for the Brainfuck esoteric programming langauge" ^ "\n" ^
    "\n" ^
    "-h, --help             print this message and exit" ^ "\n" ^
    "-s, --string           interpret FILE arg as a program string instead of a file name" ^ "\n" ^
    "-c, --cell-size NUM    override default cell size" ^ "\n"
  in
    Printf.printf "%s" usage_string

let () = 
  let args = 
    try parse_cmd_args Sys.argv with
      Failure _         -> raise (Failure "invalid argument for cell size")
    | Print_usage       -> print_usage (); exit 0
    | ex                -> print_usage (); raise ex
  in
    let program = 
      if args.is_string then
        initialize_program_from_string args.input_string args.cell_size
      else
        initialize_program_from_string (read_file_contents args.input_string) args.cell_size
    in
      let end_state = BrainfInterpreter.execute_program program in
        ignore end_state (* TODO: add ability to save resulting state in file/print out end memory *)
