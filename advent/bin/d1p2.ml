(* Temp code to make implementation speedier *)
[@@@warnerror "-unused-value-declaration"]

let filename = "day1_input.txt"

let print_cl cl =
  List.iter print_char cl;
  print_string "\n"
;;

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

let collapse_string lst =
  let rec collapse lst' str =
    match lst' with
    | [] -> str
    | h :: t -> collapse t (str ^ h)
  in
  collapse lst ""
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
  | _ -> assert false
;;

let collapse_charlist lst = String.of_seq (List.to_seq lst)

let string_num_to_char str =
  match str with
  | "one" -> '1'
  | "two" -> '2'
  | "three" -> '3'
  | "four" -> '4'
  | "five" -> '5'
  | "six" -> '6'
  | "seven" -> '7'
  | "eight" -> '8'
  | "nine" -> '9'
  | _ -> assert false
;;

let is_string_num str =
  if "one" = str
     || "two" = str
     || "two" = str
     || "three" = str
     || "four" = str
     || "five" = str
     || "six" = str
     || "seven" = str
     || "eight" = str
     || "nine" = str
  then true
  else false
;;

let drop_head n lst =
  let rec go n' lst' =
    match n' with
    | 0 -> lst'
    | _ ->
      let tlst =
        match lst' with
        | [] -> lst'
        | _ :: t -> t
      in
      go (n' - 1) tlst
  in
  go n lst
;;

(*Need to make this double recursive*)
let convert_strdigits lst =
  let rec go lst' acc holding =
    match lst' with
    | [] -> acc @ holding
    | h :: t ->
      (match is_digit h with
       | true -> go t (acc @ holding @ [ h ]) []
       | false ->
         let str = collapse_charlist (holding @ [ h ]) in
         (match is_string_num str with
          | false -> go t acc (holding @ [ h ])
          | true -> go t (acc @ [ string_num_to_char str ]) []))
  in
  go lst [] []
;;

let convert_all_strdigits lst =
  let rec go lst' idx len =
    match idx < len with
    | false -> lst'
    | true ->
      let x = convert_strdigits lst' in
      let y =
        try List.tl x with
        | _ -> []
      in
      (* print_cl x; *)
      (* Printf.printf "%d\n\n" idx; *)
      go y (idx + 1) len
  in
  go lst 0 (List.length lst - 1)
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
      let nl = convert_all_strdigits @@ explode_string @@ line in
      List.iter print_char nl;
      print_string "\n";
      let fl = pull_digits nl in
      (match fl with
       | None -> process_file fd' sum
       | Some n -> process_file fd' (sum + int_of_string n))
  in
  process_file fd 0
;;
