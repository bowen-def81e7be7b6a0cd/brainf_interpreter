
open BrainfInterpreter

type cmd_args = {
  is_string: bool ref;
  cell_size: int ref;
  input_string: string ref;
  string_set: bool ref
}

let read_file_contents (file_name: string) : string = ""

let initialize_program_from_string (program_string: string) (cell_size: int) : BrainfInterpreter.program_state =
  let insts, jumps = BrainfInterpreter.parse_program_string program_string in
    BrainfInterpreter.initialize_program insts jumps cell_size

let handle_positional (args: cmd_args) (arg: string) : unit = 
  if not !(args.string_set) then
    args.input_string := arg; 
    args.string_set := true
    
let main () = 
  let usage_msg =
    "bfi: Brainfuck Interpreter" ^ "\n" ^
    "Usage: bfi [OPTIONS] FILE" ^ "\n" ^
    "An interpreter for the Brainfuck esoteric programming langauge" ^ "\n" ^
    "\n" ^
    "Options:"
  and args = { is_string = ref false; cell_size = ref 256; input_string = ref ""; string_set = ref false } in
    let speclist =
      [ ("-s", Arg.Set args.is_string, "interpret input as string instead of file");
        ("-c", Arg.Set_int args.cell_size, "override default cell size (default=256)");
      ]
    in
      Arg.parse speclist (fun arg -> handle_positional args arg) usage_msg;
      let program = 
        if !(args.is_string) then
          initialize_program_from_string !(args.input_string) !(args.cell_size)
        else
          initialize_program_from_string (read_file_contents !(args.input_string)) !(args.cell_size)
      in
        let end_state = BrainfInterpreter.execute_program program in
          ignore end_state (* TODO: add ability to save resulting state in file/print out end memory *)

let () = main ()
