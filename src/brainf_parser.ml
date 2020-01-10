
type instruction = IncDataPtr
                 | DecDataPtr
                 | IncData
                 | DecData
                 | Input
                 | Output
                 | JumpZero
                 | JumpNonZero

let explode_string (s: string) : char list =
  let rec es i cl =
    if i < 0 then
      cl
    else
      es (i - 1) (s.[i]::cl)
  in 
    es ((String.length s) - 1) []

let strip_ignored_characters (program_string: string) : string list = 
  program_string
  |> Str.split (Str.regexp "")
  |> List.filter ((<>) "")

let string_list_to_char_list (string_list: string list) : char list =
  string_list
  |> List.map explode_string
  |> List.flatten

let program_chars_to_tokens (chars: char list) : instruction array = [||]

let parse_program_string (program_string: string) : instruction array =
  program_string
  |> strip_ignored_characters 
  |> string_list_to_char_list
  |> program_chars_to_tokens