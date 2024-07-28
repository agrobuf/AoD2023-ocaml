let filename = "day1_input.txt"

let read_line fd =
  try Some (input_line fd) with
  | _ -> None
;;

let char_to_string_digit = function
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

let is_digit ch =
  match ch with
  | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> true
  | _ -> false
;;

let explode_string str =
  let rec go str' idx lst =
    match 0 <= idx with
    | false -> lst
    | true -> go str' (idx - 1) (str'.[idx] :: lst)
  in
  go str (String.length str - 1) []
;;

let implode_str_list chlst =
  let rec go chlst' acc =
    match chlst' with
    | [] -> acc
    | h :: t -> go t (acc ^ h)
  in
  go chlst ""
;;

let extract chlst : char list =
  let rec go lst' =
    match lst' with
    | [] -> []
    | [ x ] -> [ x ]
    | _ :: t -> go t
  in
  [ List.hd chlst ] @ go chlst
;;

let process_line str =
  let lst = explode_string str |> List.filter is_digit in
  match lst with
  | [] -> 0
  | _ -> extract lst |> List.map char_to_string_digit |> implode_str_list |> int_of_string
;;

let process_file fd =
  let sum = 0 in
  let rec go sum' =
    match read_line fd with
    | None -> sum'
    | Some str ->
      let num = process_line str in
      go (sum' + num)
  in
  go sum
;;

let () = open_in filename |> process_file |> print_int
