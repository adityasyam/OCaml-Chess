open Bigarray
open Chesspiece
open Chessmove

type object_phrase = string list
type command = Move of object_phrase | Quit

exception Empty
exception Invalid
exception InvalidInput of string

let check_fourth_num (n : int) (lst : string list) (int_lst : int list) :
    int list =
  match lst with
  | [] -> raise Invalid
  | h :: t when String.length h <> 2 -> raise Invalid
  | h :: t ->
      if String.sub h 1 1 = "1" then List.merge Stdlib.compare int_lst [ n; 0 ]
      else if String.sub h 1 1 = "2" then
        List.merge Stdlib.compare int_lst [ n; 1 ]
      else if String.sub h 1 1 = "3" then
        List.merge Stdlib.compare int_lst [ n; 2 ]
      else if String.sub h 1 1 = "4" then
        List.merge Stdlib.compare int_lst [ n; 3 ]
      else if String.sub h 1 1 = "5" then
        List.merge Stdlib.compare int_lst [ n; 4 ]
      else if String.sub h 1 1 = "6" then
        List.merge Stdlib.compare int_lst [ n; 5 ]
      else if String.sub h 1 1 = "7" then
        List.merge Stdlib.compare int_lst [ n; 6 ]
      else if String.sub h 1 1 = "8" then
        List.merge Stdlib.compare int_lst [ n; 7 ]
      else raise Invalid

let check_fourth_letter (lst : int list) (word_list : string list) : int list =
  match word_list with
  | [] -> raise Invalid
  | h :: t when String.length h <> 2 -> raise Invalid
  | h :: t ->
      if String.sub h 0 1 = "a" then check_fourth_num 0 [ h ] lst
      else if String.sub h 0 1 = "b" then check_fourth_num 1 [ h ] lst
      else if String.sub h 0 1 = "c" then check_fourth_num 2 [ h ] lst
      else if String.sub h 0 1 = "d" then check_fourth_num 3 [ h ] lst
      else if String.sub h 0 1 = "e" then check_fourth_num 4 [ h ] lst
      else if String.sub h 0 1 = "f" then check_fourth_num 5 [ h ] lst
      else if String.sub h 0 1 = "g" then check_fourth_num 6 [ h ] lst
      else if String.sub h 0 1 = "h" then check_fourth_num 7 [ h ] lst
      else raise Invalid

let check_third_num (n : int) (lst : string list) : int list =
  match lst with
  | [] -> raise Invalid
  | h :: t when String.length h <> 2 -> raise Invalid
  | h :: t ->
      if String.sub h 1 1 = "1" then check_fourth_letter [ n; 0 ] t
      else if String.sub h 1 1 = "2" then check_fourth_letter [ n; 1 ] t
      else if String.sub h 1 1 = "3" then check_fourth_letter [ n; 2 ] t
      else if String.sub h 1 1 = "4" then check_fourth_letter [ n; 3 ] t
      else if String.sub h 1 1 = "5" then check_fourth_letter [ n; 4 ] t
      else if String.sub h 1 1 = "6" then check_fourth_letter [ n; 5 ] t
      else if String.sub h 1 1 = "7" then check_fourth_letter [ n; 6 ] t
      else if String.sub h 1 1 = "8" then check_fourth_letter [ n; 7 ] t
      else raise Invalid

let check_third_letter (str_lst : string list) : int list =
  match str_lst with
  | [] -> raise Invalid
  | h :: t when String.length h <> 2 -> raise Invalid
  | h :: t ->
      if String.sub h 0 1 = "a" then check_third_num 0 str_lst
      else if String.sub h 0 1 = "b" then check_third_num 1 str_lst
      else if String.sub h 0 1 = "c" then check_third_num 2 str_lst
      else if String.sub h 0 1 = "d" then check_third_num 3 str_lst
      else if String.sub h 0 1 = "e" then check_third_num 4 str_lst
      else if String.sub h 0 1 = "f" then check_third_num 5 str_lst
      else if String.sub h 0 1 = "g" then check_third_num 6 str_lst
      else if String.sub h 0 1 = "h" then check_third_num 7 str_lst
      else raise Invalid

let check_second (str_lst : string list) : int list =
  match str_lst with
  | [] -> raise Invalid
  | h :: t
    when h = "pawn" || h = "queen" || h = "king" || h = "knight" || h = "rook"
         || h = "bishop" ->
      check_third_letter t
  | _ -> raise Invalid

let check_first (str_lst : string list) : int list =
  match str_lst with
  | [] -> raise Invalid
  | h :: t when h = "move" -> check_second t
  | h :: t when h = "quit" -> raise Invalid
  | _ -> failwith "impossible"

let parse_integers (str : string) : int list =
  let words_list = String.split_on_char ' ' str in
  match words_list with
  | [] -> raise Empty
  | lst ->
      if List.length lst <> 4 then
        raise (InvalidInput "Sorry, you've given an invalid input");
      let c1 = List.nth lst 2 in
      let c2 = List.nth lst 3 in
      let letter1 = String.lowercase_ascii (String.make 1 (String.get c1 0)) in
      let rank1 = int_of_string (String.make 1 (String.get c1 1)) - 1 in
      let letter2 = String.lowercase_ascii (String.make 1 (String.get c2 0)) in
      let rank2 = int_of_string (String.make 1 (String.get c2 1)) - 1 in
      let file1 =
        match letter1 with
        | "a" -> 0
        | "b" -> 1
        | "c" -> 2
        | "d" -> 3
        | "e" -> 4
        | "f" -> 5
        | "g" -> 6
        | "h" -> 7
        | _ -> failwith "ri violated"
      in
      let file2 =
        match letter2 with
        | "a" -> 0
        | "b" -> 1
        | "c" -> 2
        | "d" -> 3
        | "e" -> 4
        | "f" -> 5
        | "g" -> 6
        | "h" -> 7
        | _ -> failwith "ri violated"
      in
      [ file1; rank1; file2; rank2 ]

let parse (current_board : piece array array) (str : string) (is_white : bool) :
    int list =
  let words_list = String.split_on_char ' ' str in
  if
    check_legality current_board (List.nth words_list 1) (parse_integers str)
      is_white
  then parse_integers str
  else raise (InvalidInput "Sorry, you've given an invalid input")

let parse_command (s : string) : command =
  let low = String.lowercase_ascii s in
  let split = String.split_on_char ' ' low in
  let trimmed = List.map String.trim (List.filter (fun x -> x <> "") split) in
  match trimmed with
  | h :: t when h = "quit" -> Quit
  | h :: t when h = "move" -> Move t
  | _ -> raise Empty

let swap (parr : piece array array) (int_lst : int list) : piece array array =
  let i1 = List.nth int_lst 0 in
  let i2 = List.nth int_lst 1 in
  let i3 = List.nth int_lst 2 in
  let i4 = List.nth int_lst 3 in
  let blank_piece =
    init_piece Blank Blank_Colour
      (convert_int_to_file i3, convert_int_to_rank i4)
      "*"
  in
  let final_square_empty =
    check_square_blank parr (convert_int_to_file i3, convert_int_to_rank i4)
  in
  if final_square_empty then (
    let temp_init_outer = Array.get parr (7 - i2) in
    let temp_init_inner = Array.get temp_init_outer i1 in
    let temp_next_outer = Array.get parr (7 - i4) in
    let temp_next_inner = Array.get temp_next_outer i3 in
    Array.set temp_init_outer i1 temp_next_inner;
    Array.set temp_next_outer i3 temp_init_inner;
    Array.set parr (7 - i2) temp_init_outer;
    Array.set parr (7 - i4) temp_next_outer;
    parr)
  else
    let temp_init_outer = Array.get parr (7 - i2) in
    let temp_init_inner = Array.get temp_init_outer i1 in
    let temp_next_outer = Array.get parr (7 - i4) in
    Array.set temp_init_outer i1 blank_piece;
    Array.set temp_next_outer i3 temp_init_inner;
    Array.set parr (7 - i2) temp_init_outer;
    Array.set parr (7 - i4) temp_next_outer;
    parr
