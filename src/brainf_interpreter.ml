open Brainf_parser

exception Invalid_program_state of string

type instruction = Brainf_parser.instruction

type jump_list = (int * int) list

type program_state = {
  inst_ptr: int;
  memory_to_ptr: int list;
  memory_after_ptr: int list;
  jump_list: jump_list;
  jump_list_rev: jump_list;
  instructions: instruction array;
  cell_size: int  
}

type state_t = program_state

let rec flip_assoc_list (al: ('a * 'b) list) : ('b * 'a) list =
  List.fold_right (fun (a, b) l -> (b, a)::l) al []

let initialize_program 
(instructions: instruction array)
(jump_list: jump_list) 
(cell_size: int) : program_state = 
  { inst_ptr = 0;
    memory_to_ptr = [0];
    memory_after_ptr = [];
    jump_list = jump_list;
    jump_list_rev = flip_assoc_list jump_list;
    instructions = instructions;
    cell_size = cell_size
  }

let reset_program (state: program_state) : program_state =
  initialize_program state.instructions state.jump_list state.cell_size

let instruction_incdataptr (state: program_state) : program_state =
  let new_mp, new_ma =
    match state.memory_to_ptr, state.memory_after_ptr with
      xs, []    -> 0::xs, [] 
    | xs, y::ys -> y::xs, ys
  in
    { state with memory_to_ptr = new_mp;
                memory_after_ptr = new_ma
    }

let instruction_decdataptr (state: program_state) : program_state =
  let new_mp, new_ma =
    match state.memory_to_ptr, state.memory_after_ptr with
      [], _     -> raise (Invalid_program_state "instruction_decdataptr")
    | x::[], ys -> x::[], ys
    | x::xs, ys -> xs, x::ys
  in
    { state with memory_to_ptr = new_mp;
                memory_after_ptr = new_ma
    }

let instruction_incdata (state: program_state) : program_state =
  { state with memory_to_ptr =
      match state.memory_to_ptr with
        []    -> raise (Invalid_program_state "instruction_incdata")
      | x::xs -> ((x + 1) mod state.cell_size)::xs
  }

let instruction_decdata (state: program_state) : program_state =
  { state with memory_to_ptr =
      match state.memory_to_ptr with
        []    -> raise (Invalid_program_state "instruction_decdata") 
      | x::xs -> ((x - 1) mod state.cell_size)::xs
  }

let instruction_input (state: program_state) : program_state =
  Printf.printf ">>> ";
  let input_value = 
    match read_int_opt () with
      None   -> 0
    | Some x -> x mod state.cell_size
  in
    { state with memory_to_ptr = 
        match state.memory_to_ptr with
          []    -> raise (Invalid_program_state "instruction_input")
        | x::xs -> input_value::xs
    }

let instruction_output (state: program_state) : program_state =
  match state.memory_to_ptr with
    []   -> raise (Invalid_program_state "instruction_input")
  | x::_ -> Printf.printf "%c" (Char.chr (x mod 256)); state

let instruction_jumpzero (state: program_state) : program_state =
  match state.memory_to_ptr with
    []   -> raise (Invalid_program_state "instruction_jumpzero")
  | 0::_ -> 
    begin
      match List.assoc_opt state.inst_ptr state.jump_list with
        None     -> state
      | Some dst -> { state with inst_ptr = dst }
    end
  | _    -> state

let instruction_jumpnonzero (state: program_state) : program_state =
  match state.memory_to_ptr with
    []   -> raise (Invalid_program_state "instruction_jumpnonzero")
  | 0::_ -> state
  | _    ->
    begin
      match List.assoc_opt state.inst_ptr state.jump_list_rev with
        None     -> state
      | Some dst -> { state with inst_ptr = dst }
    end

let execute_instruction (state: program_state) (inst: instruction) : program_state =
  let instruction_function = 
    match inst with
      IncDataPtr  -> instruction_incdataptr
    | DecDataPtr  -> instruction_decdataptr
    | IncData     -> instruction_incdata
    | DecData     -> instruction_decdata
    | Input       -> instruction_input
    | Output      -> instruction_output
    | JumpZero    -> instruction_jumpzero
    | JumpNonZero -> instruction_jumpnonzero
  in
    let next_state = instruction_function state in
      { next_state with inst_ptr = next_state.inst_ptr + 1 }

let rec execute_program (state: program_state) : program_state =
  if state.inst_ptr < 0 || state.inst_ptr >= Array.length state.instructions then
    state
  else
    state.instructions.(state.inst_ptr)
    |> execute_instruction state
    |> execute_program
