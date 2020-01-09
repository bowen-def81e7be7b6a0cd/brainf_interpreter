
exception Invalid_program_state of string


type program_state = {
  inst_ptr: int;
  memory_to_ptr: int list;
  memory_after_ptr: int list;
  cell_size: int  
}


type instruction = IncDataPtr
                 | DecDataPtr
                 | IncData
                 | DecData
                 | Input
                 | Output
                 | JumpZero
                 | JumpNonZero


let initialize_program (cell_size: int) : program_state = 
  {
    inst_ptr = 0;
    memory_to_ptr = [0];
    memory_after_ptr = [];
    cell_size = cell_size
  }

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
    | x::[], ys -> x::[], ys (* Don't go more left *)
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
  state


let instruction_output (state: program_state) : program_state =
  state


let instruction_jumpzero (state: program_state) : program_state =
  state


let instruction_jumpnonzero (state: program_state) : program_state =
  state


let execute_instruction (inst: instruction) (state: program_state) : program_state =
  match inst with
    IncDataPtr  -> instruction_incdataptr state
  | DecDataPtr  -> instruction_decdataptr state
  | IncData     -> instruction_incdata state
  | DecData     -> instruction_decdata state
  | Input       -> instruction_input state
  | Output      -> instruction_output state
  | JumpZero    -> instruction_jumpzero state
  | JumpNonZero -> instruction_jumpnonzero state
