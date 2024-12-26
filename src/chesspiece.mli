(** the type [rank] represents the individual rows in the chessboard's
     co-ordinates as numbers from One through Eight *)
type rank = One | Two | Three | Four | Five | Six | Seven | Eight

(** the type [file] represents the individual columns in the chessboard's
     co-ordinates as letters from A through H *)
type file = A | B | C | D | E | F | G | H

(** the type [piece_type] represents the different pieces that can be placed 
   on the chessboard, the blank square is represented by the variant [Blank] *)
type piece_type = Pawn | Knight | Bishop | Rook | Queen | King | Blank

(** the type [colour] represents the colours of the piece. The color of [Blank]
   is [Blank_Colour]*)
type colour = White | Black | Blank_Colour

type square = file * rank
(** the type [square] represents the exact co-ordinates of a piece on the chessboard.
    *)

type piece = {
  piece_type : piece_type;
  colour : colour;
  square : square;
  symbol : string;
}
(** the type [piece] represents a given piece and the attributes associated with it. 
    *)

val init_piece : piece_type -> colour -> square -> string -> piece
(** [init_piece piece_type colour sq piece_name] initialises a piece with its attributes. 
    *)

val get_piece_type : piece -> piece_type
(** [get_piece_type piece] returns the piece type of piece. 
    *)

val get_colour : piece -> colour
(** [get_colour piece] returns the colour of piece. 
    *)

val get_square : piece -> square
(** [get_square piece] returns the co-ordinates of piece. 
    *)

val get_symbol : piece -> string
(** [get_symbol piece] returns the symbol of piece. 
    *)

val convert_file : file -> int
(** [convert_file f] converts f to an integer indexed from from 0 through 7
    *)

val convert_int_to_file : int -> file
(** [convert_int_to_file i] converts integers indexed from from 0 through 7 to the relevant
   file from A through H
    *)

val convert_rank : rank -> int
(** [convert_file r] converts r to an integer indexed from from 0 through 7.piece_type
    *)

val convert_int_to_rank : int -> rank
(** [convert_int_to_rank i] converts integers indexed from from 0 through 7 to the relevant
    rank from One through Eight.
    *)

val return_piece_type : piece_type -> string
(** [return_piece_type piece_type] converts piece_type as a relavent string. 
    *)

val rev_return_piece_type : string -> piece_type
(** [rev_return_piece_type piece_name] converts piece_name to a relevant [piece_type] 
    *)

val txt_to_piece : string -> file -> rank -> piece
(** [txt_to_piece piece_name f r] makes a [piece] of piece_name and asssigns f and s.
    *)

val convert_square : file * rank -> int * int
(** [convert_square (f, r)] converts f and r to indexed tuples and returns a tuple of the same.
    *)

val check_board_legal : int * int -> bool
(** [check_board_legal (i1, i2)] checks if the indexed integer tuple is a valid 
   co-ordinate on the chessboard. 
    *)

type t
(** t represents a chessboard containing pieces in their current co-ordinates. 
    *)

val temp_arr_outer : unit -> piece array array
(** [temp_arr_outer] initialises the chessboard full of pieces.
    *)

val string_to_piece_helper : string -> square -> piece
(** [string_to_piece_helper piece_name sq] converts the relevant symbol to a piece. 
    *)

val string_to_piece : string array array -> piece array array
(** [ string_to_piece string_board] converts the relevant symbol to a piece for the entire chessboard.
    *)

val piece_to_symbol_helper : piece_type -> colour -> string
(** [piece_to_symbol_helper piece_type color] converts the relevant piece to a symbol.
    *)

val print_board : string array array -> unit
(** [ print_board string_board] prints the current board.
    *)

val temp_arr_symbol : unit -> string array array
(** [temp_arr_symbol] initialises the chessboard with symbols.
    *)

val piece_to_symbol : piece array array -> string array array
(** [ piece_to_symbol board] converts the relevant piece to a symbolfor the entire chessboard.
    *)
