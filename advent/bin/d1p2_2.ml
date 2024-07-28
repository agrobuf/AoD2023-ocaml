[@@@warnerror "-unused-value-declaration"]
[@@@warnerror "-unused-var-strict"]

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

let str_digit_to_char = function
  | "zero" -> '0'
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

let is_str_digit = function
  | "zero" | "one" | "two" | "three" | "four" | "five" | "six" | "seven" | "eight" | "nine" -> true
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

let char_list_to_str chlst = chlst |> List.map char_to_string_digit |> implode_str_list

let extract chlst : char list =
  let rec go lst' =
    match lst' with
    | [] -> assert false
    | [ x ] -> [ x ]
    | _ :: t -> go t
  in
  [ List.hd chlst ] @ go chlst
;;

let convert chlst =
  let rec go chlst' holding acc =
    match chlst' with
    | [] -> acc
    | h :: t ->
      if is_digit h (* this char is a digit *)
      then go t [] (acc @ [ h ])
      else if holding @ [ h ] |> char_list_to_str |> is_str_digit (* the holding is a digit word *)
      then go t [] (acc @ [ holding @ [ h ] |> char_list_to_str |> str_digit_to_char ])
      else if List.is_empty t (* There are no more characters *)
      then go (List.tl holding) [] acc
      else go t (holding @ [ h ]) acc (* normal case *)
  in
  go chlst [] []
;;

let process_line str =
  let lst = explode_string str |> convert |> List.filter is_digit in
  match lst with
  | [] -> 0
  | _ -> extract lst |> List.map char_to_string_digit |> implode_str_list |> int_of_string
;;

let process_file fd =
  let sum = 0 in
  let rec go sum' =
    match read_line fd with
    | None -> sum' (* reached end of file *)
    | Some str ->
      let num = process_line str in
      go (sum' + num)
  in
  go sum
;;

let () = open_in filename |> process_file |> print_int
