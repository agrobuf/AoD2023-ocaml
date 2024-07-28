let filename = "day1_input.txt"

let read_line fd =
  try Some (input_line fd) with
  | _ -> None
;;

let is_digit char =
  match char with
  | '0' .. '9' -> true
  | _ -> false
;;

let explode_string str =
  let rec explode str_index lc =
    match str_index >= 0 with
    | false -> lc
    | true -> explode (str_index - 1) (str.[str_index] :: lc)
  in
  explode (String.length str - 1) []
;;

let char_digit_to_string c =
  match c with
  | '0' -> "0"
  | '1' -> "1"
  | '2' -> "2"
  | '3' -> "3"
  | '4' -> "4"
  | '5' -> "5"
  | '6' -> "6"
  | '7' -> "7"
  | '8' -> "8"
  | '9' -> "9"
  | _ -> ""
;;

let pull_digits ch_list =
  let rec search ch_list' first last =
    match ch_list' with
    | [] ->
      (match first, last with
       | None, None -> None
       | Some f, None -> Some (char_digit_to_string f ^ char_digit_to_string f)
       | None, Some l -> Some (char_digit_to_string l ^ char_digit_to_string l)
       | Some f, Some l -> Some (char_digit_to_string f ^ char_digit_to_string l))
    | h :: t ->
      (match is_digit h with
       | false -> search t first last
       | true -> if first = None then search t (Some h) last else search t first (Some h))
  in
  search ch_list None None
;;

let () =
  let fd = open_in filename in
  let rec process_file fd' sum =
    match read_line fd' with
    | None -> Printf.printf "Sum: %d" sum
    | Some line ->
      let fl = pull_digits @@ explode_string line in
      (match fl with
       | None -> process_file fd' sum
       | Some n -> process_file fd' (sum + int_of_string n))
  in
  process_file fd 0
;;
