type instruction = IncDataPtr
                 | DecDataPtr
                 | IncData
                 | DecData
                 | Input
                 | Output
                 | JumpZero
                 | JumpNonZero
                 | Nop

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

exception Invalid_program_state of string

let explode_string (s: string) : char list =
  let rec es i cl =
    if i < 0 then
      cl
    else
      es (i - 1) (s.[i]::cl)
  in 
    es ((String.length s) - 1) []

let strip_ignored_characters (program_string: string) : char list = 
  program_string
  |> explode_string
  |> List.filter (String.contains "><+-,.[]")

let char_to_instruction (c: char) : instruction =
  match c with
    '>' -> IncDataPtr
  | '<' -> DecDataPtr
  | '+' -> IncData
  | '-' -> DecData
  | ',' -> Input
  | '.' -> Output
  | '[' -> JumpZero
  | ']' -> JumpNonZero
  | _   -> Nop

let char_list_to_instruction_list (char_list: char list) : instruction list = 
  let rec chars_to_instructions chars insts =
    match chars with
      []    -> insts
    | c::cs -> chars_to_instructions cs ((char_to_instruction c)::insts)
  in
    chars_to_instructions char_list []
    |> List.rev

let assemble_program_jump_list (instructions: instruction list) : jump_list =
  let rec assemble_jl insts stack jl i = 
    match insts, stack with
      [], _                    -> jl 
    | JumpZero::rest, _        -> assemble_jl rest (i::stack) jl (i + 1)
    | JumpNonZero::rest, x::xs -> assemble_jl rest xs ((x, i)::jl) (i + 1)
    | _::rest, _               -> assemble_jl rest stack jl (i + 1)
  in
    assemble_jl instructions [] [] 0

let parse_program_string (program_string: string) : instruction array * jump_list =
  let instructions = 
    program_string
    |> strip_ignored_characters 
    |> char_list_to_instruction_list
  in
    Array.of_list instructions, assemble_program_jump_list instructions

let initialize_program 
(instructions: instruction array)
(jump_list: jump_list) 
(cell_size: int) : program_state = 
  { inst_ptr = 0;
    memory_to_ptr = [0];
    memory_after_ptr = [];
    jump_list = jump_list;
    jump_list_rev = List.map (fun (a, b) -> (b + 0, a + 0)) jump_list;
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
      | x::xs -> 
        if x - 1 >= 0 then
          ((x - 1) mod state.cell_size)::xs
        else
          255::xs
  }

let instruction_input (state: program_state) : program_state =
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
    | Nop         -> fun _ -> state
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
