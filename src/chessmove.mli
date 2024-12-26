open Chesspiece

val check_piece_in_pos : piece array array -> string -> int list -> bool
(** [check_piece_in_pos piece_board piece_name pos_ints] returns true if the
   piece_name that the player wants to 
  move is actually at the given initial position pos_ints, returns false otherwise.
  *)

val check_square_blank : piece array array -> square -> bool
(** [check_square_blank piece_board square] returns true if the given square 
  is [Blank], returns false otherwise.
  *)

val not_vertical_collision : piece array array -> int list -> bool
(** [not_vertical_collision piece_board pos_ints] returns true if there will be no collision 
  when any piece is moved vertically from its initial square to final square. 
  *)

val not_horizontal_collision : piece array array -> int list -> bool
(** [not_horizontal_collision  piece_board pos_ints] returns true if there will be no collision 
  when any piece is moved horizontally from its initial square to final square. 
  *)

val not_rt_diagonal_collision : piece array array -> int list -> bool
(** [not_rt_diagonal_collision piece_board pos_ints] returns true if there will be no collision 
  when any piece is moved diagonally to the right from its initial square to final square. 
  *)

val not_lt_diagonal_collision : piece array array -> int list -> bool
(** [not_lt_diagonal_collision piece_board pos_ints] returns true if there will be no collision 
  when any piece is moved diagonally to the left from its initial square to final square. 
  *)

val check_rt_diagonal_shift : int list -> bool
(** [check_diagonal_shift pos_ints] returns true if the move is a diagonal movement 
  of the piece, otherwise returns false.
  *)

val check_lt_diagonal_shift : int list -> bool
(** [check_diagonal_shift pos_ints] returns true if the move is a diagonal movement 
    of the piece, otherwise returns false.
    *)

val check_rank_shift : int list -> bool
(** [check_rank_shift pos_ints] returns true if the move is a vertical movement of 
  the piece, otherwise returns false.
  *)

val check_file_shift : int list -> bool
(** [check_file_shift pos_ints] returns true if the move is a horizontal movement of 
  the piece, otherwise returns false.
  *)

val valid_pawn_move : piece array array -> piece -> int list -> bool
(** [valid_pawn_move pawn pos_ints] returns true if the move being made using the [Pawn] 
  is legal i.e if it can be moved from initial to final squares as the player 
    wants, returns false otherewise.  
  *)

val valid_king_move : piece -> int list -> bool
(** [valid_king_move king pos_ints] returns true if the move being made using the [King] is
   legal i.e if it can be moved from initial to final squares as the player 
    wants, returns false otherewise. 
  *)

val valid_knight_move : piece -> int list -> bool
(** [valid_knight_move knight pos_ints] returns true if the move being made using the [Knight] 
  is legal i.e if it can be moved from initial to final squares as the player 
  wants, returns false otherewise. 
  *)

val valid_rook_move : piece -> int list -> bool
(** [valid_rook_move rook pos_ints] returns true if the move being made using the [Rook] is 
  legal i.e if it can be moved from initial to final squares as the player 
  wants, returns false otherewise. 
  *)

val valid_queen_move : piece -> int list -> bool
(** [valid_queen_move queen pos_ints] returns true if the move being made using the [Queen] 
  is legal i.e if it can be moved from initial to final squares as the player 
  wants, returns false otherewise. 
  *)

val valid_bishop_move : piece -> int list -> bool
(** [valid_bishop_move bishop pos_ints] returns true if the move being made using the [Bishop] 
  is legal i.e if it can be moved from initial to final squares as the player 
    wants, returns false otherewise. 
  *)

val colour_check : bool -> piece -> bool
(** [colour_check is_white piece] returns true only if the colour of the piece chosen by the 
  player aligns with the player whose move it is currently, returns false otherwise. 
  *)

val check_legality : piece array array -> string -> int list -> bool -> bool
(** [check_legality piece_board piece_name pos_ints is_whites] checks the legality of the move the player is trying to 
make and returns true if the move can be made, returns false otherewise. *)
