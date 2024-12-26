open Printf

type rank = One | Two | Three | Four | Five | Six | Seven | Eight
type file = A | B | C | D | E | F | G | H
type piece_type = Pawn | Knight | Bishop | Rook | Queen | King | Blank
type colour = White | Black | Blank_Colour
type square = file * rank

type piece = {
  piece_type : piece_type;
  colour : colour;
  square : square;
  symbol : string;
}

let init_piece (pt : piece_type) (colour : colour) (sq : square)
    (symbol : string) : piece =
  { piece_type = pt; colour; square = sq; symbol }

let get_piece_type (p : piece) : piece_type = p.piece_type
let get_colour (p : piece) : colour = p.colour
let get_square (p : piece) : square = p.square
let get_symbol (p : piece) : string = p.symbol

let convert_file (file : file) : int =
  match file with
  | A -> 0
  | B -> 1
  | C -> 2
  | D -> 3
  | E -> 4
  | F -> 5
  | G -> 6
  | H -> 7

let convert_int_to_file (i : int) : file =
  match i with
  | 0 -> A
  | 1 -> B
  | 2 -> C
  | 3 -> D
  | 4 -> E
  | 5 -> F
  | 6 -> G
  | 7 -> H
  | _ -> failwith "invalid index of file..."

let convert_rank (rank : rank) : int =
  match rank with
  | One -> 0
  | Two -> 1
  | Three -> 2
  | Four -> 3
  | Five -> 4
  | Six -> 5
  | Seven -> 6
  | Eight -> 7

let convert_int_to_rank (i : int) : rank =
  match i with
  | 0 -> One
  | 1 -> Two
  | 2 -> Three
  | 3 -> Four
  | 4 -> Five
  | 5 -> Six
  | 6 -> Seven
  | 7 -> Eight
  | _ -> failwith "invalid index of rank..."

let return_piece_type (piece_type : piece_type) : string =
  match piece_type with
  | Pawn -> "pawn"
  | Knight -> "knight"
  | Bishop -> "bishop"
  | Rook -> "rook"
  | Queen -> "queen"
  | King -> "king"
  | Blank -> "*"

let rev_return_piece_type (pt : string) : piece_type =
  match pt with
  | "pawn" -> Pawn
  | "knight" -> Knight
  | "bishop" -> Bishop
  | "rook" -> Rook
  | "queen" -> Queen
  | "king" -> King
  | "*" -> Blank
  | _ -> failwith "invalid input..."

let txt_to_piece (symbol : string) (f : file) (r : rank) : piece =
  match symbol with
  | "♚" -> init_piece King White (f, r) "♚"
  | "♔" -> init_piece King Black (f, r) "♔"
  | "♛" -> init_piece Queen White (f, r) "♛"
  | "♕" -> init_piece Queen Black (f, r) "♕"
  | "♝" -> init_piece Bishop White (f, r) "♝"
  | "♗" -> init_piece Bishop Black (f, r) "♗"
  | "♞" -> init_piece Knight White (f, r) "♞"
  | "♘" -> init_piece Knight Black (f, r) "♘"
  | "♜" -> init_piece Rook White (f, r) "♜"
  | "♖" -> init_piece Rook Black (f, r) "♖"
  | "♟" -> init_piece Pawn White (f, r) "♟"
  | "♙" -> init_piece Pawn Black (f, r) "♙"
  | "*" -> init_piece Blank Blank_Colour (f, r) "*"
  | _ -> failwith "incorrect parsing"

let convert_square ((file, rank) : file * rank) : int * int =
  let conv_file = convert_file file in
  let conv_rank = convert_rank rank in
  (conv_file, conv_rank)

let check_board_legal ((x_coord, y_coord) : int * int) : bool =
  let check_x_coord = x_coord >= 0 && x_coord <= 7 in
  let check_y_coord = y_coord >= 0 && y_coord <= 7 in
  check_x_coord && check_y_coord

type t = piece array array

(* NOTE: We had to make our doubly nested piece arrays in this manner since when we used
   a for-loop to create the iner nested arrays, all the arrays pointed to the same
   internakl reference. Thus a change to the last row of our chessboard would cause
   the same change to be replicated in all our rows.
*)
let temp_arr_outer () =
  [|
    [|
      init_piece Blank Blank_Colour (A, One) "*";
      init_piece Blank Blank_Colour (A, One) "*";
      init_piece Blank Blank_Colour (A, One) "*";
      init_piece Blank Blank_Colour (A, One) "*";
      init_piece Blank Blank_Colour (A, One) "*";
      init_piece Blank Blank_Colour (A, One) "*";
      init_piece Blank Blank_Colour (A, One) "*";
      init_piece Blank Blank_Colour (A, One) "*";
    |];
    [|
      init_piece Blank Blank_Colour (A, One) "*";
      init_piece Blank Blank_Colour (A, One) "*";
      init_piece Blank Blank_Colour (A, One) "*";
      init_piece Blank Blank_Colour (A, One) "*";
      init_piece Blank Blank_Colour (A, One) "*";
      init_piece Blank Blank_Colour (A, One) "*";
      init_piece Blank Blank_Colour (A, One) "*";
      init_piece Blank Blank_Colour (A, One) "*";
    |];
    [|
      init_piece Blank Blank_Colour (A, One) "*";
      init_piece Blank Blank_Colour (A, One) "*";
      init_piece Blank Blank_Colour (A, One) "*";
      init_piece Blank Blank_Colour (A, One) "*";
      init_piece Blank Blank_Colour (A, One) "*";
      init_piece Blank Blank_Colour (A, One) "*";
      init_piece Blank Blank_Colour (A, One) "*";
      init_piece Blank Blank_Colour (A, One) "*";
    |];
    [|
      init_piece Blank Blank_Colour (A, One) "*";
      init_piece Blank Blank_Colour (A, One) "*";
      init_piece Blank Blank_Colour (A, One) "*";
      init_piece Blank Blank_Colour (A, One) "*";
      init_piece Blank Blank_Colour (A, One) "*";
      init_piece Blank Blank_Colour (A, One) "*";
      init_piece Blank Blank_Colour (A, One) "*";
      init_piece Blank Blank_Colour (A, One) "*";
    |];
    [|
      init_piece Blank Blank_Colour (A, One) "*";
      init_piece Blank Blank_Colour (A, One) "*";
      init_piece Blank Blank_Colour (A, One) "*";
      init_piece Blank Blank_Colour (A, One) "*";
      init_piece Blank Blank_Colour (A, One) "*";
      init_piece Blank Blank_Colour (A, One) "*";
      init_piece Blank Blank_Colour (A, One) "*";
      init_piece Blank Blank_Colour (A, One) "*";
    |];
    [|
      init_piece Blank Blank_Colour (A, One) "*";
      init_piece Blank Blank_Colour (A, One) "*";
      init_piece Blank Blank_Colour (A, One) "*";
      init_piece Blank Blank_Colour (A, One) "*";
      init_piece Blank Blank_Colour (A, One) "*";
      init_piece Blank Blank_Colour (A, One) "*";
      init_piece Blank Blank_Colour (A, One) "*";
      init_piece Blank Blank_Colour (A, One) "*";
    |];
    [|
      init_piece Blank Blank_Colour (A, One) "*";
      init_piece Blank Blank_Colour (A, One) "*";
      init_piece Blank Blank_Colour (A, One) "*";
      init_piece Blank Blank_Colour (A, One) "*";
      init_piece Blank Blank_Colour (A, One) "*";
      init_piece Blank Blank_Colour (A, One) "*";
      init_piece Blank Blank_Colour (A, One) "*";
      init_piece Blank Blank_Colour (A, One) "*";
    |];
    [|
      init_piece Blank Blank_Colour (A, One) "*";
      init_piece Blank Blank_Colour (A, One) "*";
      init_piece Blank Blank_Colour (A, One) "*";
      init_piece Blank Blank_Colour (A, One) "*";
      init_piece Blank Blank_Colour (A, One) "*";
      init_piece Blank Blank_Colour (A, One) "*";
      init_piece Blank Blank_Colour (A, One) "*";
      init_piece Blank Blank_Colour (A, One) "*";
    |];
  |]

let string_to_piece_helper (p : string) (sq : square) : piece =
  match p with
  | "♔" -> init_piece King Black sq "♔"
  | "♚" -> init_piece King White sq "♔"
  | "♛" -> init_piece Queen White sq "♛"
  | "♕" -> init_piece Queen Black sq "♕"
  | "♝" -> init_piece Bishop White sq "♝"
  | "♗" -> init_piece Bishop Black sq "♗"
  | "♞" -> init_piece Knight White sq "♞"
  | "♘" -> init_piece Knight Black sq "♘"
  | "♜" -> init_piece Rook White sq "♜"
  | "♖" -> init_piece Rook Black sq "♖"
  | "♟" -> init_piece Pawn White sq "♟"
  | "♙" -> init_piece Pawn Black sq "♙"
  | "*" -> init_piece Blank Blank_Colour sq "*"
  | _ -> failwith "incorrect chessboard parsing"

let piece_to_symbol_helper (p : piece_type) (c : colour) : string =
  match (p, c) with
  | King, Black -> "♔"
  | King, White -> "♚"
  | Queen, Black -> "♕"
  | Queen, White -> "♛"
  | Bishop, White -> "♝"
  | Bishop, Black -> "♗"
  | Knight, White -> "♞"
  | Knight, Black -> "♘"
  | Rook, White -> "♜"
  | Rook, Black -> "♖"
  | Pawn, White -> "♟"
  | Pawn, Black -> "♙"
  | Blank, _ -> "*"
  | _ -> failwith "incorrect chessboard color parsing"

let print_board (start_board : string array array) =
  for row = 0 to 7 do
    for col = 0 to 7 do
      Printf.printf "%s" start_board.(row).(col)
    done;
    Printf.printf "%s" "\n"
  done

(* NOTE: We had to make our doubly nested piece arrays in this manner since when we used
   a for-loop to create the iner nested arrays, all the arrays pointed to the same
   internakl reference. Thus a change to the last row of our chessboard would cause
   the same change to be replicated in all our rows.*)
let temp_arr_symbol () =
  [|
    [| "*"; "*"; "*"; "*"; "*"; "*"; "*"; "*" |];
    [| "*"; "*"; "*"; "*"; "*"; "*"; "*"; "*" |];
    [| "*"; "*"; "*"; "*"; "*"; "*"; "*"; "*" |];
    [| "*"; "*"; "*"; "*"; "*"; "*"; "*"; "*" |];
    [| "*"; "*"; "*"; "*"; "*"; "*"; "*"; "*" |];
    [| "*"; "*"; "*"; "*"; "*"; "*"; "*"; "*" |];
    [| "*"; "*"; "*"; "*"; "*"; "*"; "*"; "*" |];
    [| "*"; "*"; "*"; "*"; "*"; "*"; "*"; "*" |];
    [| "*"; "*"; "*"; "*"; "*"; "*"; "*"; "*" |];
  |]

let piece_to_symbol (temp_array : piece array array) : string array array =
  let new_arr = temp_arr_symbol () in
  for row = 0 to 7 do
    for col = 0 to 7 do
      let p = temp_array.(row).(col) in
      new_arr.(row).(col) <- piece_to_symbol_helper p.piece_type p.colour
    done
  done;
  new_arr

let string_to_piece (s : string array array) : piece array array =
  let new_arr_outer = temp_arr_outer () in
  for row = 0 to 7 do
    for col = 0 to 7 do
      let new_piece =
        string_to_piece_helper
          s.(row).(col)
          (convert_int_to_file col, convert_int_to_rank row)
      in

      new_arr_outer.(row).(col) <- new_piece
    done
  done;

  new_arr_outer
