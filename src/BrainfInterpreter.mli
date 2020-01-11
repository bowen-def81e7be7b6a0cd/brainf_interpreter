
type program_state

type instruction

type jump_list

val parse_program_string : string -> instruction array * jump_list

val initialize_program: instruction array -> jump_list -> int -> program_state

val reset_program: program_state -> program_state

val execute_program: program_state -> program_state
