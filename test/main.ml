open OUnit2
open Game.Command
open Game.Chesspiece
open Game.Chessmove

(* Test Plan
   Initially as we were following TDD, we implemented black-box testing.
   We coded our tests and then checked if it failed our test, after which we
   implemented the function.
   As we started coding through the functions, we began implementing
   glass-box testing.

   As we made a chess game, we had several functions that took a piece
   (i.e. a chess piece from the board) as input. Due to this, we decided to initialize
   each piece with its piece type, colour, square and symbol, with the
   following format:

   let example_piece =
     { piece_type; colour; square; symbol }

   We used these functions with the same format as example_piece above
   for any function that took piece as an input or had piece as the expected output.

   We used OUnit tests for checking the implementations of foundation functions to make sure
   the base function like the setters and getters.

   We used manual tests for checking legality, checking user input

   We used manual tests for [command.ml] to check if
   user input was valid and accordingly extracting it if it was.
   E.g. Extracting the command "move" in move pawn e2 e4
   Extracting the piece type "pawn" in move pawn e2 e4
   Extracting the start position "e2" in move pawn e2 e4
   Extracting the final position "e4" in move pawn e2 e4

   Additionally, in [command.ml], we used manual testing

   for parse, parse_integers and swap

   val parse : piece array array -> string -> bool -> int list
   (** [parse] parses a player's input into a [command] and returns the parsed initial and final square co-ordinates as an integer list.*)

   val parse_integers : string -> int list
   (** [parse_integers] returns the initial and final square co-ordinates as an integer list.*)

   val swap : piece array array -> int list -> piece array array
   (** [swap] moves the piece from the specified initial square to the final aquare if the move is legal
       *)

   Parts of the system automatically tested by OUnit:

   Functions from [chesspiece.ml] that were tested using glass-box testing and using
   OUnit Testing:

   val convert_file : file -> int
   val convert_int_to_file : int -> file
   val convert_rank : rank -> int
   val convert_int_to_rank : int -> rank
   val return_piece_type : piece_type -> string
   val rev_return_piece_type : string -> piece_type
   val txt_to_piece : string -> file -> rank -> piece
   val convert_square : file * rank -> int * int
   val check_board_legal : int * int -> bool
   val piece_to_symbol_helper : piece_type -> colour -> string


   Functions from [chessmove.ml] that were tested using glass-box testing and
   using OUnit Testing:

   val check_rank_shift : int list -> bool
   [check_rank_shift pos_ints] returns true if the move is a vertical movement of
     the piece, otherwise returns false.

   val check_file_shift : int list -> bool
   [check_file_shift pos_ints] returns true if the move is a horizontal movement of
     the piece, otherwise returns false.

   val check_rt_diagonal_shift : int list -> bool
   (** [check_rtdiagonal_shift pos_ints] returns true if the move is a rightwards diagonal movement
     of the piece, otherwise returns false.
     *)

   We decided to manually test legality functions that returned bool in chessmove.ml
   as we found it more efficient. By using manual tests for the legality functions,
   we would also be able to test in various "game-like" scenarios.

   Functions from [chessmove.ml] that were tested using Manual Testing:

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
   which returns true if there will be no collision
     when any piece is moved vertically from its initial square to final square.

   val not_horizontal_collision : piece array array -> int list -> bool
   which returns true if there will be no collision
     when any piece is moved horizontally from its initial square to final square.

   val not_rt_diagonal_collision : piece array array -> int list -> bool
   which returns true if there will be no collision
     when any piece is moved diagonally from its initial square to final square to its right

   val not_lt_diagonal_collision : piece array array -> int list -> bool
   which returns true if there will be no collision
     when any piece is moved diagonally from its initial square to final square to its left

   val valid_pawn_move : piece -> int list -> bool
   which returns true if the move being made using the [Pawn]
     is legal i.e if it can be moved from initial to final squares as the player
       wants, returns false otherewise.

   val valid_king_move : piece -> int list -> bool
   which returns true if the move being made using the [King] is
      legal i.e if it can be moved from initial to final squares as the player
       wants, returns false otherewise.

   val valid_knight_move : piece -> int list -> bool
   which returns true if the move being made using the [Knight]
     is legal i.e if it can be moved from initial to final squares as the player
     wants, returns false otherewise.

   val valid_rook_move : piece -> int list -> bool
   which returns true if the move being made using the [Rook] is
     legal i.e if it can be moved from initial to final squares as the player
     wants, returns false otherewise.


   val valid_queen_move : piece -> int list -> bool
   which returns true if the move being made using the [Queen]
     is legal i.e if it can be moved from initial to final squares as the player
     wants, returns false otherewise.


   val valid_bishop_move : piece -> int list -> bool
   which returns true if the move being made using the [Bishop]
     is legal i.e if it can be moved from initial to final squares as the player
       wants, returns false otherewise.
*)

let init_white_pawn1 =
  { piece_type = Pawn; colour = White; square = (A, Two); symbol = "♟" }

let init_black_pawn1 =
  { piece_type = Pawn; colour = Black; square = (A, Seven); symbol = "♙" }

let init_white_pawn2 =
  { piece_type = Pawn; colour = White; square = (B, Two); symbol = "♟" }

let init_black_pawn2 =
  { piece_type = Pawn; colour = Black; square = (B, Seven); symbol = "♙" }

let init_white_pawn3 =
  { piece_type = Pawn; colour = White; square = (C, Two); symbol = "♟" }

let init_black_pawn3 =
  { piece_type = Pawn; colour = Black; square = (C, Seven); symbol = "♙" }

let init_white_pawn4 =
  { piece_type = Pawn; colour = White; square = (D, Two); symbol = "♟" }

let init_black_pawn4 =
  { piece_type = Pawn; colour = Black; square = (D, Seven); symbol = "♙" }

let init_white_pawn5 =
  { piece_type = Pawn; colour = White; square = (E, Two); symbol = "♟" }

let init_black_pawn5 =
  { piece_type = Pawn; colour = Black; square = (E, Seven); symbol = "♙" }

let init_white_pawn6 =
  { piece_type = Pawn; colour = White; square = (F, Two); symbol = "♟" }

let init_black_pawn6 =
  { piece_type = Pawn; colour = Black; square = (F, Seven); symbol = "♙" }

let init_white_pawn7 =
  { piece_type = Pawn; colour = White; square = (G, Two); symbol = "♟" }

let init_black_pawn7 =
  { piece_type = Pawn; colour = Black; square = (G, Seven); symbol = "♙" }

let init_white_pawn8 =
  { piece_type = Pawn; colour = White; square = (H, Two); symbol = "♟" }

let init_black_pawn8 =
  { piece_type = Pawn; colour = Black; square = (H, Seven); symbol = "♙" }

let init_white_knight1 =
  { piece_type = Knight; colour = White; square = (B, One); symbol = "♞" }

let init_black_knight1 =
  { piece_type = Knight; colour = Black; square = (B, Eight); symbol = "♘" }

let init_white_knight2 =
  { piece_type = Knight; colour = White; square = (G, One); symbol = "♞" }

let init_black_knight2 =
  { piece_type = Knight; colour = Black; square = (G, Eight); symbol = "♘" }

let init_white_bishop1 =
  { piece_type = Bishop; colour = White; square = (C, One); symbol = "♝" }

let init_black_bishop1 =
  { piece_type = Bishop; colour = Black; square = (C, Eight); symbol = "♗" }

let init_white_bishop2 =
  { piece_type = Bishop; colour = White; square = (F, One); symbol = "♝" }

let init_black_bishop2 =
  { piece_type = Bishop; colour = Black; square = (F, Eight); symbol = "♗" }

let init_white_rook1 =
  { piece_type = Rook; colour = White; square = (A, One); symbol = "♜" }

let init_black_rook1 =
  { piece_type = Rook; colour = Black; square = (A, Eight); symbol = "♖" }

let init_white_rook2 =
  { piece_type = Rook; colour = White; square = (H, One); symbol = "♜" }

let init_black_rook2 =
  { piece_type = Rook; colour = Black; square = (H, Eight); symbol = "♖" }

let init_white_queen =
  { piece_type = Queen; colour = White; square = (D, One); symbol = "♛" }

let init_black_queen =
  { piece_type = Queen; colour = Black; square = (D, Eight); symbol = "♕" }

let init_white_king =
  { piece_type = King; colour = White; square = (E, One); symbol = "♚" }

let init_black_king =
  { piece_type = King; colour = Black; square = (E, Eight); symbol = "♔" }

let init_blank =
  {
    piece_type = Blank;
    colour = Blank_Colour;
    square = (E, Eight);
    symbol = "*";
  }

(** [init_piece_test name pt colour sq symbol expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with [init_piece pt colour sq symbol]. *)
let init_piece_test (name : string) (pt : piece_type) (colour : colour)
    (sq : square) (symbol : string) (expected_output : piece) : test =
  name >:: fun _ ->
  assert_equal expected_output (init_piece pt colour sq symbol)

let get_colour_test (name : string) (p : piece) (expected_output : colour) :
    test =
  name >:: fun _ -> assert_equal expected_output (get_colour p)

let get_piece_type_test (name : string) (p : piece)
    (expected_output : piece_type) : test =
  name >:: fun _ -> assert_equal expected_output (get_piece_type p)

let get_square_test (name : string) (p : piece) (expected_output : square) :
    test =
  name >:: fun _ -> assert_equal expected_output (get_square p)

let convert_file_test (name : string) (f : file) (expected_output : int) : test
    =
  name >:: fun _ -> assert_equal expected_output (convert_file f)

let convert_int_to_file_test (name : string) (i : int) (expected_output : file)
    : test =
  name >:: fun _ -> assert_equal expected_output (convert_int_to_file i)

let convert_rank_test (name : string) (r : rank) (expected_output : int) : test
    =
  name >:: fun _ -> assert_equal expected_output (convert_rank r)

let convert_int_to_rank_test (name : string) (i : int) (expected_output : rank)
    : test =
  name >:: fun _ -> assert_equal expected_output (convert_int_to_rank i)

let return_piece_type_test (name : string) (pt : piece_type)
    (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output (return_piece_type pt)

let rev_return_piece_type_test (name : string) (s : string)
    (expected_output : piece_type) : test =
  name >:: fun _ -> assert_equal expected_output (rev_return_piece_type s)

let txt_to_piece_test (name : string) (symbol : string) (f : file) (r : rank)
    (expected_output : piece) : test =
  name >:: fun _ -> assert_equal expected_output (txt_to_piece symbol f r)

let convert_square_test (name : string) (sq : file * rank)
    (expected_output : int * int) : test =
  name >:: fun _ -> assert_equal expected_output (convert_square sq)

let check_board_legal_test (name : string) (sq : int * int)
    (expected_output : bool) : test =
  name >:: fun _ -> assert_equal expected_output (check_board_legal sq)

let piece_to_symbol_test (name : string) (arr : piece array array)
    (expected_output : string array array) : test =
  name >:: fun _ -> assert_equal expected_output (piece_to_symbol arr)

let colour_check_test (name : string) (is_white : bool) (p : piece)
    (expected_output : bool) : test =
  name >:: fun _ -> assert_equal expected_output (colour_check is_white p)

let piece_to_symbol_helper_test (name : string) (pt : piece_type) (c : colour)
    (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output (piece_to_symbol_helper pt c)

let check_rank_shift_test (name : string) (lst : int list)
    (expected_output : bool) : test =
  name >:: fun _ -> assert_equal expected_output (check_rank_shift lst)

let check_file_shift_test (name : string) (lst : int list)
    (expected_output : bool) : test =
  name >:: fun _ -> assert_equal expected_output (check_file_shift lst)

let check_rt_diagonal_shift_test (name : string) (lst : int list)
    (expected_output : bool) : test =
  name >:: fun _ -> assert_equal expected_output (check_rt_diagonal_shift lst)

(** add expected output piece*)
let init_piece_tests =
  [
    init_piece_test "white e2 pawn" Pawn White (E, Two) "♟" init_white_pawn5;
    init_piece_test "black e7 pawn" Pawn Black (E, Seven) "♙" init_black_pawn5;
    init_piece_test "white g1 knight" Knight White (G, One) "♞"
      init_white_knight2;
    init_piece_test "black g8 knight" Knight Black (G, Eight) "♘"
      init_black_knight2;
    init_piece_test "white f1 bishop" Bishop White (F, One) "♝"
      init_white_bishop2;
    init_piece_test "black f8 bishop" Bishop Black (F, Eight) "♗"
      init_black_bishop2;
    init_piece_test "white h1 rook" Rook White (H, One) "♜" init_white_rook2;
    init_piece_test "black h8 rook" Rook Black (H, Eight) "♖" init_black_rook2;
    init_piece_test "white d1 queen" Queen White (D, One) "♛" init_white_queen;
    init_piece_test "black d8 queen" Queen Black (D, Eight) "♕" init_black_queen;
    init_piece_test "white e1 king" King White (E, One) "♚" init_white_king;
    init_piece_test "black e8 king" King Black (E, Eight) "♔" init_black_king;
  ]

(** need to add the input piece type*)
let get_colour_tests =
  [
    get_colour_test "white pawn" init_white_pawn1 White;
    get_colour_test "black pawn" init_black_pawn1 Black;
    get_colour_test "white knight" init_white_knight1 White;
    get_colour_test "black knight" init_black_knight1 Black;
    get_colour_test "white bishop" init_white_bishop1 White;
    get_colour_test "black bishop" init_black_bishop1 Black;
    get_colour_test "white rook" init_white_rook1 White;
    get_colour_test "black rook" init_black_rook1 Black;
    get_colour_test "white queen" init_white_queen White;
    get_colour_test "black queen" init_black_queen Black;
    get_colour_test "white king1" init_white_king White;
    get_colour_test "black king1" init_black_king Black;
    get_colour_test "black rook" init_black_rook2 Black;
    get_colour_test "white rook" init_white_rook2 White;
    get_colour_test "black knight" init_black_knight2 Black;
    get_colour_test "white knight" init_white_knight2 White;
    get_colour_test "black bishop" init_black_bishop2 Black;
    get_colour_test "white bishop" init_white_bishop2 White;
    get_colour_test "white pawn" init_white_pawn2 White;
    get_colour_test "black pawn" init_black_pawn2 Black;
  ]

(** need to add the input piece type*)
let get_piece_type_tests =
  [
    get_piece_type_test "white pawn" init_white_pawn5 Pawn;
    get_piece_type_test "black pawn" init_black_pawn5 Pawn;
    get_piece_type_test "white knight" init_white_knight1 Knight;
    get_piece_type_test "black knight" init_black_knight1 Knight;
    get_piece_type_test "white bishop" init_white_bishop1 Bishop;
    get_piece_type_test "black bishop" init_black_bishop1 Bishop;
    get_piece_type_test "white rook" init_white_rook1 Rook;
    get_piece_type_test "black rook" init_black_rook1 Rook;
    get_piece_type_test "white queen" init_white_queen Queen;
    get_piece_type_test "black queen" init_black_queen Queen;
    get_piece_type_test "white king2" init_white_king King;
    get_piece_type_test "black king2" init_black_king King;
  ]

let get_square_tests =
  [
    get_square_test "white pawn" init_white_pawn1 (A, Two);
    get_square_test "black pawn" init_black_pawn1 (A, Seven);
    get_square_test "white pawn" init_white_pawn2 (B, Two);
    get_square_test "black pawn" init_black_pawn2 (B, Seven);
    get_square_test "white pawn" init_white_pawn3 (C, Two);
    get_square_test "black pawn" init_black_pawn3 (C, Seven);
    get_square_test "white pawn" init_white_pawn4 (D, Two);
    get_square_test "black pawn" init_black_pawn4 (D, Seven);
    get_square_test "white pawn" init_white_pawn5 (E, Two);
    get_square_test "black pawn" init_black_pawn5 (E, Seven);
    get_square_test "white pawn" init_white_pawn6 (F, Two);
    get_square_test "black pawn" init_black_pawn6 (F, Seven);
    get_square_test "white pawn" init_white_pawn7 (G, Two);
    get_square_test "black pawn" init_black_pawn7 (G, Seven);
    get_square_test "white pawn" init_white_pawn8 (H, Two);
    get_square_test "black pawn" init_black_pawn8 (H, Seven);
    get_square_test "white knight 1" init_white_knight1 (B, One);
    get_square_test "black knight 1" init_black_knight1 (B, Eight);
    get_square_test "white knight 2" init_white_knight2 (G, One);
    get_square_test "black knight 2" init_black_knight2 (G, Eight);
    get_square_test "white bishop 1" init_white_bishop1 (C, One);
    get_square_test "black bishop 1" init_black_bishop1 (C, Eight);
    get_square_test "white bishop 2" init_white_bishop2 (F, One);
    get_square_test "black bishop 2" init_black_bishop2 (F, Eight);
    get_square_test "white rook 1" init_white_rook1 (A, One);
    get_square_test "black rook 1" init_black_rook1 (A, Eight);
    get_square_test "white rook 2" init_white_rook2 (H, One);
    get_square_test "black rook 2" init_black_rook2 (H, Eight);
    get_square_test "white queen" init_white_queen (D, One);
    get_square_test "black queen" init_black_queen (D, Eight);
    get_square_test "white king3" init_white_king (E, One);
    get_square_test "black king3" init_black_king (E, Eight);
  ]

let convert_file_tests =
  [
    convert_file_test "file A" A 0;
    convert_file_test "file B" B 1;
    convert_file_test "file C" C 2;
    convert_file_test "file D" D 3;
    convert_file_test "file E" E 4;
    convert_file_test "file F" F 5;
    convert_file_test "file G" G 6;
    convert_file_test "file H" H 7;
  ]

let convert_int_to_file_tests =
  [
    convert_int_to_file_test "file 1" 0 A;
    convert_int_to_file_test "file 2" 1 B;
    convert_int_to_file_test "file 3" 2 C;
    convert_int_to_file_test "file 4" 3 D;
    convert_int_to_file_test "file 5" 4 E;
    convert_int_to_file_test "file 6" 5 F;
    convert_int_to_file_test "file 7" 6 G;
    convert_int_to_file_test "file 8" 7 H;
  ]

let convert_rank_tests =
  [
    convert_rank_test "rank One" One 0;
    convert_rank_test "rank Two" Two 1;
    convert_rank_test "rank Three" Three 2;
    convert_rank_test "rank Four" Four 3;
    convert_rank_test "rank Five" Five 4;
    convert_rank_test "rank Six" Six 5;
    convert_rank_test "rank Seven" Seven 6;
    convert_rank_test "rank Eight" Eight 7;
  ]

let convert_int_to_rank_tests =
  [
    convert_int_to_rank_test "rank 1" 0 One;
    convert_int_to_rank_test "rank 2" 1 Two;
    convert_int_to_rank_test "rank 3" 2 Three;
    convert_int_to_rank_test "rank 4" 3 Four;
    convert_int_to_rank_test "rank 5" 4 Five;
    convert_int_to_rank_test "rank 6" 5 Six;
    convert_int_to_rank_test "rank 7" 6 Seven;
    convert_int_to_rank_test "rank 8" 7 Eight;
  ]

let return_piece_type_tests =
  [
    return_piece_type_test "pawn" Pawn "pawn";
    return_piece_type_test "knight" Knight "knight";
    return_piece_type_test "bishop" Bishop "bishop";
    return_piece_type_test "rook" Rook "rook";
    return_piece_type_test "queen" Queen "queen";
    return_piece_type_test "king" King "king";
    return_piece_type_test "blank1" Blank "*";
  ]

let rev_return_piece_type_tests =
  [
    rev_return_piece_type_test "pawn" "pawn" Pawn;
    rev_return_piece_type_test "knight" "knight" Knight;
    rev_return_piece_type_test "bishop" "bishop" Bishop;
    rev_return_piece_type_test "rook" "rook" Rook;
    rev_return_piece_type_test "queen" "queen" Queen;
    rev_return_piece_type_test "king" "king" King;
    rev_return_piece_type_test "blank2" "*" Blank;
  ]

let txt_to_piece_tests =
  [
    txt_to_piece_test "white king4" "♚" E One init_white_king;
    txt_to_piece_test "black king4" "♔" E Eight init_black_king;
    txt_to_piece_test "white queen" "♛" D One init_white_queen;
    txt_to_piece_test "black queen" "♕" D Eight init_black_queen;
    txt_to_piece_test "white rook" "♜" H One init_white_rook2;
    txt_to_piece_test "black rook" "♖" H Eight init_black_rook2;
    txt_to_piece_test "white bishop" "♝" F One init_white_bishop2;
    txt_to_piece_test "black bishop" "♗" F Eight init_black_bishop2;
    txt_to_piece_test "white knight" "♞" G One init_white_knight2;
    txt_to_piece_test "black knight" "♘" G Eight init_black_knight2;
    txt_to_piece_test "white pawn" "♟" E Two init_white_pawn5;
    txt_to_piece_test "black pawn" "♙" E Seven init_black_pawn5;
    txt_to_piece_test "white rook" "♜" A One init_white_rook1;
    txt_to_piece_test "black rook" "♖" A Eight init_black_rook1;
    txt_to_piece_test "white bishop" "♝" C One init_white_bishop1;
    txt_to_piece_test "black bishop" "♗" C Eight init_black_bishop1;
    txt_to_piece_test "white knight" "♞" B One init_white_knight1;
    txt_to_piece_test "black knight" "♘" B Eight init_black_knight1;
    txt_to_piece_test "white pawn" "♟" D Two init_white_pawn4;
    txt_to_piece_test "black pawn" "♙" D Seven init_black_pawn4;
  ]

let convert_square_tests =
  [
    convert_square_test "File 1 Rank 1" (A, One) (0, 0);
    convert_square_test "File 1 Rank 2" (A, Two) (0, 1);
    convert_square_test "File 1 Rank 3" (A, Three) (0, 2);
    convert_square_test "File 1 Rank 4" (A, Four) (0, 3);
    convert_square_test "File 1 Rank 5" (A, Five) (0, 4);
    convert_square_test "File 1 Rank 6" (A, Six) (0, 5);
    convert_square_test "File 1 Rank 7" (A, Seven) (0, 6);
    convert_square_test "File 1 Rank 8" (A, Eight) (0, 7);
    convert_square_test "File 2 Rank 1" (B, One) (1, 0);
    convert_square_test "File 2 Rank 2" (B, Two) (1, 1);
    convert_square_test "File 2 Rank 3" (B, Three) (1, 2);
    convert_square_test "File 2 Rank 4" (B, Four) (1, 3);
    convert_square_test "File 2 Rank 5" (B, Five) (1, 4);
    convert_square_test "File 2 Rank 6" (B, Six) (1, 5);
    convert_square_test "File 2 Rank 7" (B, Seven) (1, 6);
    convert_square_test "File 2 Rank 8" (B, Eight) (1, 7);
    convert_square_test "File 3 Rank 1" (C, One) (2, 0);
    convert_square_test "File 3 Rank 2" (C, Two) (2, 1);
    convert_square_test "File 3 Rank 3" (C, Three) (2, 2);
    convert_square_test "File 3 Rank 4" (C, Four) (2, 3);
    convert_square_test "File 3 Rank 5" (C, Five) (2, 4);
    convert_square_test "File 3 Rank 6" (C, Six) (2, 5);
    convert_square_test "File 3 Rank 7" (C, Seven) (2, 6);
    convert_square_test "File 3 Rank 8" (C, Eight) (2, 7);
    convert_square_test "File 4 Rank 1" (D, One) (3, 0);
    convert_square_test "File 4 Rank 2" (D, Two) (3, 1);
    convert_square_test "File 4 Rank 3" (D, Three) (3, 2);
    convert_square_test "File 4 Rank 4" (D, Four) (3, 3);
    convert_square_test "File 4 Rank 5" (D, Five) (3, 4);
    convert_square_test "File 4 Rank 6" (D, Six) (3, 5);
    convert_square_test "File 4 Rank 7" (D, Seven) (3, 6);
    convert_square_test "File 4 Rank 8" (D, Eight) (3, 7);
    convert_square_test "File 5 Rank 1" (E, One) (4, 0);
    convert_square_test "File 5 Rank 2" (E, Two) (4, 1);
    convert_square_test "File 5 Rank 3" (E, Three) (4, 2);
    convert_square_test "File 5 Rank 4" (E, Four) (4, 3);
    convert_square_test "File 5 Rank 5" (E, Five) (4, 4);
    convert_square_test "File 5 Rank 6" (E, Six) (4, 5);
    convert_square_test "File 5 Rank 7" (E, Seven) (4, 6);
    convert_square_test "File 5 Rank 8" (E, Eight) (4, 7);
    convert_square_test "File 6 Rank 1" (F, One) (5, 0);
    convert_square_test "File 6 Rank 2" (F, Two) (5, 1);
    convert_square_test "File 6 Rank 3" (F, Three) (5, 2);
    convert_square_test "File 6 Rank 4" (F, Four) (5, 3);
    convert_square_test "File 6 Rank 5" (F, Five) (5, 4);
    convert_square_test "File 6 Rank 6" (F, Six) (5, 5);
    convert_square_test "File 6 Rank 7" (F, Seven) (5, 6);
    convert_square_test "File 6 Rank 8" (F, Eight) (5, 7);
    convert_square_test "File 7 Rank 1" (G, One) (6, 0);
    convert_square_test "File 7 Rank 2" (G, Two) (6, 1);
    convert_square_test "File 7 Rank 3" (G, Three) (6, 2);
    convert_square_test "File 7 Rank 4" (G, Four) (6, 3);
    convert_square_test "File 7 Rank 5" (G, Five) (6, 4);
    convert_square_test "File 7 Rank 6" (G, Six) (6, 5);
    convert_square_test "File 7 Rank 7" (G, Seven) (6, 6);
    convert_square_test "File 7 Rank 8" (G, Eight) (6, 7);
    convert_square_test "File 8 Rank 1" (H, One) (7, 0);
    convert_square_test "File 8 Rank 2" (H, Two) (7, 1);
    convert_square_test "File 8 Rank 3" (H, Three) (7, 2);
    convert_square_test "File 8 Rank 4" (H, Four) (7, 3);
    convert_square_test "File 8 Rank 5" (H, Five) (7, 4);
    convert_square_test "File 8 Rank 6" (H, Six) (7, 5);
    convert_square_test "File 8 Rank 7" (H, Seven) (7, 6);
    convert_square_test "File 8 Rank 8" (H, Eight) (7, 7);
  ]

let check_board_legal_tests =
  [
    check_board_legal_test "X = 0; Y = 0" (0, 0) true;
    check_board_legal_test "X = 0; Y = 1" (0, 1) true;
    check_board_legal_test "X = 0; Y = 2" (0, 2) true;
    check_board_legal_test "X = 0; Y = 3" (0, 3) true;
    check_board_legal_test "X = 0; Y = 4" (0, 4) true;
    check_board_legal_test "X = 0; Y = 5" (0, 5) true;
    check_board_legal_test "X = 0; Y = 6" (0, 6) true;
    check_board_legal_test "X = 0; Y = 7" (0, 7) true;
    check_board_legal_test "X = 0; Y = 8" (0, 8) false;
    check_board_legal_test "X = 1; Y = 0" (1, 0) true;
    check_board_legal_test "X = 1; Y = 1" (1, 1) true;
    check_board_legal_test "X = 1; Y = 2" (1, 2) true;
    check_board_legal_test "X = 1; Y = 3" (1, 3) true;
    check_board_legal_test "X = 1; Y = 4" (1, 4) true;
    check_board_legal_test "X = 1; Y = 5" (1, 5) true;
    check_board_legal_test "X = 1; Y = 6" (1, 6) true;
    check_board_legal_test "X = 1; Y = 7" (1, 7) true;
    check_board_legal_test "X = 1; Y = 8" (1, 8) false;
    check_board_legal_test "X = 2; Y = 0" (2, 0) true;
    check_board_legal_test "X = 2; Y = 1" (2, 1) true;
    check_board_legal_test "X = 2; Y = 2" (2, 2) true;
    check_board_legal_test "X = 2; Y = 3" (2, 3) true;
    check_board_legal_test "X = 2; Y = 4" (2, 4) true;
    check_board_legal_test "X = 2; Y = 5" (2, 5) true;
    check_board_legal_test "X = 2; Y = 6" (2, 6) true;
    check_board_legal_test "X = 2; Y = 7" (2, 7) true;
    check_board_legal_test "X = 2; Y = 8" (2, 8) false;
    check_board_legal_test "X = 3; Y = 0" (3, 0) true;
    check_board_legal_test "X = 3; Y = 1" (3, 1) true;
    check_board_legal_test "X = 3; Y = 2" (3, 2) true;
    check_board_legal_test "X = 3; Y = 3" (3, 3) true;
    check_board_legal_test "X = 3; Y = 4" (3, 4) true;
    check_board_legal_test "X = 3; Y = 5" (3, 5) true;
    check_board_legal_test "X = 3; Y = 6" (3, 6) true;
    check_board_legal_test "X = 3; Y = 7" (3, 7) true;
    check_board_legal_test "X = 3; Y = 8" (3, 8) false;
    check_board_legal_test "X = 4; Y = 0" (4, 0) true;
    check_board_legal_test "X = 4; Y = 1" (4, 1) true;
    check_board_legal_test "X = 4; Y = 2" (4, 2) true;
    check_board_legal_test "X = 4; Y = 3" (4, 3) true;
    check_board_legal_test "X = 4; Y = 4" (4, 4) true;
    check_board_legal_test "X = 4; Y = 5" (4, 5) true;
    check_board_legal_test "X = 4; Y = 6" (4, 6) true;
    check_board_legal_test "X = 4; Y = 7" (4, 7) true;
    check_board_legal_test "X = 4; Y = 8" (4, 8) false;
    check_board_legal_test "X = 5; Y = 0" (5, 0) true;
    check_board_legal_test "X = 5; Y = 1" (5, 1) true;
    check_board_legal_test "X = 5; Y = 2" (5, 2) true;
    check_board_legal_test "X = 5; Y = 3" (5, 3) true;
    check_board_legal_test "X = 5; Y = 4" (5, 4) true;
    check_board_legal_test "X = 5; Y = 5" (5, 5) true;
    check_board_legal_test "X = 5; Y = 6" (5, 6) true;
    check_board_legal_test "X = 5; Y = 7" (5, 7) true;
    check_board_legal_test "X = 5; Y = 8" (5, 8) false;
    check_board_legal_test "X = 6; Y = 0" (6, 0) true;
    check_board_legal_test "X = 6; Y = 1" (6, 1) true;
    check_board_legal_test "X = 6; Y = 2" (6, 2) true;
    check_board_legal_test "X = 6; Y = 3" (6, 3) true;
    check_board_legal_test "X = 6; Y = 4" (6, 4) true;
    check_board_legal_test "X = 6; Y = 5" (6, 5) true;
    check_board_legal_test "X = 6; Y = 6" (6, 6) true;
    check_board_legal_test "X = 6; Y = 7" (6, 7) true;
    check_board_legal_test "X = 6; Y = 8" (6, 8) false;
    check_board_legal_test "X = 7; Y = 0" (7, 0) true;
    check_board_legal_test "X = 7; Y = 1" (7, 1) true;
    check_board_legal_test "X = 7; Y = 2" (7, 2) true;
    check_board_legal_test "X = 7; Y = 3" (7, 3) true;
    check_board_legal_test "X = 7; Y = 4" (7, 4) true;
    check_board_legal_test "X = 7; Y = 5" (7, 5) true;
    check_board_legal_test "X = 7; Y = 6" (7, 6) true;
    check_board_legal_test "X = 7; Y = 7" (7, 7) true;
    check_board_legal_test "X = 7; Y = 8" (7, 8) false;
  ]

(* let first_symbol_board =
     [|
       [| "♖"; "♘"; "♗"; "♕"; "♔"; "♗"; "♘"; "♖"; "\n" |];
       [| "♙"; "♙"; "♙"; "♙"; "♙"; "♙"; "♙"; "♙"; "\n" |];
       [| "*"; "*"; "*"; "*"; "*"; "*"; "*"; "*"; "\n" |];
       [| "*"; "*"; "*"; "*"; "*"; "*"; "*"; "*"; "\n" |];
       [| "*"; "*"; "*"; "*"; "*"; "*"; "*"; "*"; "\n" |];
       [| "*"; "*"; "*"; "*"; "*"; "*"; "*"; "*"; "\n" |];
       [| "♟"; "♟"; "♟"; "♟"; "♟"; "♟"; "♟"; "♟"; "\n" |];
       [| "♜"; "♞"; "♝"; "♛"; "♚"; "♝"; "♞"; "♜"; "\n" |];
     |]

   let piece_to_symbol_tests = [] *)

let piece_to_symbol_helper_tests =
  [
    piece_to_symbol_helper_test "Black Pawn piece to symbol test" Pawn Black "♙";
    piece_to_symbol_helper_test "Black Rook piece to symbol test" Rook Black "♖";
    piece_to_symbol_helper_test "Black Knight piece to symbol test" Knight Black
      "♘";
    piece_to_symbol_helper_test "Black Bishop piece to symbol test" Bishop Black
      "♗";
    piece_to_symbol_helper_test "Black Queen piece to symbol test" Queen Black
      "♕";
    piece_to_symbol_helper_test "Black King piece to symbol test" King Black "♔";
    piece_to_symbol_helper_test "White Pawn piece to symbol test" Pawn White "♟";
    piece_to_symbol_helper_test "White Rook piece to symbol test" Rook White "♜";
    piece_to_symbol_helper_test "White Knight piece to symbol test" Knight White
      "♞";
    piece_to_symbol_helper_test "White Bishop piece to symbol test" Bishop White
      "♝";
    piece_to_symbol_helper_test "White Queen piece to symbol test" Queen White
      "♛";
    piece_to_symbol_helper_test "White King piece to symbol test" King White "♚";
  ]

let colour_check_tests =
  [
    colour_check_test " Checking white pawn colour" true init_white_pawn1 true;
    colour_check_test " Checking white rook colour" true init_white_rook1 true;
    colour_check_test " Checking white bishop colour" true init_white_bishop1
      true;
    colour_check_test " Checking white knight colour" true init_white_knight1
      true;
    colour_check_test " Checking white queen colour" true init_white_queen true;
    colour_check_test " Checking white king colour" true init_white_king true;
    colour_check_test " Checking black pawn colour" false init_black_pawn1 true;
    colour_check_test " Checking black rook colour" false init_black_rook1 true;
    colour_check_test " Checking black bishop colour" false init_black_bishop1
      true;
    colour_check_test " Checking black knight colour" false init_black_knight1
      true;
    colour_check_test " Checking black queen colour" false init_black_queen true;
    colour_check_test " Checking black king colour" false init_black_king true;
    colour_check_test " Checking blank space colour" false init_blank false;
  ]

let check_rank_shift_tests =
  [
    check_rank_shift_test "Checking true case 1 with f1 = f2 = 0" [ 0; 1; 0; 2 ]
      true;
    check_rank_shift_test "Checking true case 2 with f1 = f2 = 0" [ 0; 1; 0; 3 ]
      true;
    check_rank_shift_test "Checking true case 3 with f1 = f2 = 0" [ 0; 1; 0; 4 ]
      true;
    check_rank_shift_test "Checking true case 4 with f1 = f2 = 0" [ 0; 1; 0; 5 ]
      true;
    check_rank_shift_test "Checking true case 5 with f1 = f2 = 0" [ 0; 1; 0; 6 ]
      true;
    check_rank_shift_test "Checking true case 6 with f1 = f2 = 0" [ 0; 1; 0; 7 ]
      true;
    check_rank_shift_test "Checking true case 1 with f1 = f2 = 1" [ 1; 1; 1; 2 ]
      true;
    check_rank_shift_test "Checking true case 2 with f1 = f2 = 1" [ 1; 1; 1; 3 ]
      true;
    check_rank_shift_test "Checking true case 3 with f1 = f2 = 1" [ 1; 1; 1; 4 ]
      true;
    check_rank_shift_test "Checking true case 4 with f1 = f2 = 1" [ 1; 1; 1; 5 ]
      true;
    check_rank_shift_test "Checking true case 5 with f1 = f2 = 1" [ 1; 1; 1; 6 ]
      true;
    check_rank_shift_test "Checking true case 6 with f1 = f2 = 1" [ 1; 1; 1; 7 ]
      true;
    check_rank_shift_test "Checking true case 1 with f1 = f2 = 2" [ 2; 1; 2; 2 ]
      true;
    check_rank_shift_test "Checking true case 2 with f1 = f2 = 2" [ 2; 1; 2; 3 ]
      true;
    check_rank_shift_test "Checking true case 3 with f1 = f2 = 2" [ 2; 1; 2; 4 ]
      true;
    check_rank_shift_test "Checking true case 4 with f1 = f2 = 2" [ 2; 1; 2; 5 ]
      true;
    check_rank_shift_test "Checking true case 5 with f1 = f2 = 2" [ 2; 1; 2; 6 ]
      true;
    check_rank_shift_test "Checking true case 6 with f1 = f2 = 2" [ 2; 1; 2; 7 ]
      true;
    check_rank_shift_test "Checking true case 1 with f1 = f2 = 3" [ 3; 1; 3; 2 ]
      true;
    check_rank_shift_test "Checking true case 2 with f1 = f2 = 3" [ 3; 1; 3; 3 ]
      true;
    check_rank_shift_test "Checking true case 3 with f1 = f2 = 3" [ 3; 1; 3; 4 ]
      true;
    check_rank_shift_test "Checking true case 4 with f1 = f2 = 3" [ 3; 1; 3; 5 ]
      true;
    check_rank_shift_test "Checking true case 5 with f1 = f2 = 3" [ 3; 1; 3; 6 ]
      true;
    check_rank_shift_test "Checking true case 6 with f1 = f2 = 3" [ 3; 1; 3; 7 ]
      true;
    check_rank_shift_test "Checking true case 1 with f1 = f2 = 4" [ 4; 1; 4; 2 ]
      true;
    check_rank_shift_test "Checking true case 2 with f1 = f2 = 4" [ 4; 1; 4; 3 ]
      true;
    check_rank_shift_test "Checking true case 3 with f1 = f2 = 4" [ 4; 1; 4; 4 ]
      true;
    check_rank_shift_test "Checking true case 4 with f1 = f2 = 4" [ 4; 1; 4; 5 ]
      true;
    check_rank_shift_test "Checking true case 5 with f1 = f2 = 4" [ 4; 1; 4; 6 ]
      true;
    check_rank_shift_test "Checking true case 6 with f1 = f2 = 4" [ 4; 1; 4; 7 ]
      true;
    check_rank_shift_test "Checking true case 1 with f1 = f2 = 5" [ 5; 1; 5; 2 ]
      true;
    check_rank_shift_test "Checking true case 2 with f1 = f2 = 5" [ 5; 1; 5; 3 ]
      true;
    check_rank_shift_test "Checking true case 3 with f1 = f2 = 5" [ 5; 1; 5; 4 ]
      true;
    check_rank_shift_test "Checking true case 4 with f1 = f2 = 5" [ 5; 1; 5; 5 ]
      true;
    check_rank_shift_test "Checking true case 5 with f1 = f2 = 5" [ 5; 1; 5; 6 ]
      true;
    check_rank_shift_test "Checking true case 6 with f1 = f2 = 5" [ 5; 1; 5; 7 ]
      true;
    check_rank_shift_test "Checking true case 1 with f1 = f2 = 6" [ 6; 1; 6; 2 ]
      true;
    check_rank_shift_test "Checking true case 2 with f1 = f2 = 6" [ 6; 1; 6; 3 ]
      true;
    (* check_rank_shift_test "Checking true case 3 with f1 = f2 = 6" [ 6; 1; 6; 4 ]
         true;
       check_rank_shift_test "Checking true case 4 with f1 = f2 = 6" [ 6; 1; 6; 5 ]
         true;
       check_rank_shift_test "Checking true case 5 with f1 = f2 = 6" [ 6; 1; 6; 6 ]
         (* true;
       check_rank_shift_test "Checking true case 6 with f1 = f2 = 6" [ 6; 1; 6; 7 ] *)
         true; *)
    check_rank_shift_test "Checking true case 1 with f1 = f2 = 7" [ 7; 1; 7; 2 ]
      true;
    check_rank_shift_test "Checking true case 2 with f1 = f2 = 7" [ 7; 1; 7; 3 ]
      true;
    (* check_rank_shift_test "Checking true case 3 with f1 = f2 = 7" [ 7; 1; 7; 4 ]
         true;
       check_rank_shift_test "Checking true case 4 with f1 = f2 = 7" [ 7; 1; 7; 5 ]
         true;
       check_rank_shift_test "Checking true case 5 with f1 = f2 = 7" [ 7; 1; 7; 6 ]
         true;
       check_rank_shift_test "Checking true case 6 with f1 = f2 = 7" [ 7; 1; 7; 7 ]
         true; *)
    check_rank_shift_test "Checking false case 1" [ 1; 2; 1; 2 ] false;
    check_rank_shift_test "Checking false case 2" [ 2; 2; 1; 4 ] false;
    check_rank_shift_test "Checking false case 3" [ 1; 2; 1; 2 ] false;
    check_rank_shift_test "Checking false case 4" [ 2; 2; 1; 6 ] false;
    check_rank_shift_test "Checking false case 5" [ 2; 2; 1; 7 ] false;
    check_rank_shift_test "Checking false case 6" [ 2; 3; 1; 4 ] false;
    check_rank_shift_test "Checking false case 7" [ 2; 3; 1; 5 ] false;
    check_rank_shift_test "Checking false case 8" [ 2; 3; 1; 6 ] false;
    check_rank_shift_test "Checking false case 9" [ 2; 3; 1; 7 ] false;
    check_rank_shift_test "Checking false case 10" [ 2; 4; 1; 5 ] false;
    check_rank_shift_test "Checking false case 11" [ 3; 4; 2; 6 ] false;
    check_rank_shift_test "Checking false case 12" [ 3; 4; 2; 7 ] false;
    check_rank_shift_test "Checking false case 13" [ 3; 5; 2; 6 ] false;
    check_rank_shift_test "Checking false case 14" [ 3; 5; 2; 7 ] false;
    check_rank_shift_test "Checking false case 15" [ 3; 6; 2; 7 ] false;
  ]

let check_file_shift_tests =
  [
    check_file_shift_test "Checking true case 1" [ 0; 7; 1; 7 ] true;
    check_file_shift_test "Checking true case 2" [ 0; 7; 2; 7 ] true;
    check_file_shift_test "Checking true case 3" [ 0; 7; 3; 7 ] true;
    check_file_shift_test "Checking true case 4" [ 0; 7; 4; 7 ] true;
    check_file_shift_test "Checking true case 5" [ 0; 7; 5; 7 ] true;
    check_file_shift_test "Checking true case 6" [ 0; 7; 6; 7 ] true;
    check_file_shift_test "Checking true case 7" [ 1; 1; 2; 1 ] true;
    check_file_shift_test "Checking true case 8" [ 1; 1; 3; 1 ] true;
    check_file_shift_test "Checking true case 9" [ 1; 1; 4; 1 ] true;
    check_file_shift_test "Checking true case 10" [ 1; 1; 5; 1 ] true;
    check_file_shift_test "Checking true case 11" [ 1; 1; 6; 1 ] true;
    check_file_shift_test "Checking true case 12" [ 1; 1; 7; 1 ] true;
    check_file_shift_test "Checking true case 13" [ 2; 2; 8; 2 ] true;
    check_file_shift_test "Checking true case 14" [ 2; 2; 8; 2 ] true;
    check_file_shift_test "Checking true case 15" [ 2; 2; 8; 2 ] true;
    check_file_shift_test "Checking true case 16" [ 2; 2; 8; 2 ] true;
    check_file_shift_test "Checking true case 17" [ 3; 2; 8; 2 ] true;
    check_file_shift_test "Checking true case 18" [ 3; 3; 8; 3 ] true;
    check_file_shift_test "Checking true case 19" [ 3; 3; 8; 3 ] true;
    check_file_shift_test "Checking true case 20" [ 3; 3; 8; 3 ] true;
    check_file_shift_test "Checking true case 21" [ 4; 3; 8; 3 ] true;
    check_file_shift_test "Checking true case 22" [ 4; 4; 8; 4 ] true;
    check_file_shift_test "Checking true case 23" [ 4; 4; 8; 4 ] true;
    check_file_shift_test "Checking true case 24" [ 4; 4; 8; 4 ] true;
    check_file_shift_test "Checking true case 25" [ 5; 5; 8; 5 ] true;
    check_file_shift_test "Checking true case 26" [ 5; 5; 8; 5 ] true;
    check_file_shift_test "Checking true case 27" [ 5; 6; 8; 6 ] true;
    check_file_shift_test "Checking true case 28" [ 6; 3; 8; 3 ] true;
    check_file_shift_test "Checking true case 29" [ 6; 4; 8; 4 ] true;
    check_file_shift_test "Checking true case 30" [ 6; 4; 8; 4 ] true;
    check_file_shift_test "Checking true case 31" [ 6; 4; 8; 4 ] true;
    check_file_shift_test "Checking true case 32" [ 0; 1; 1; 1 ] true;
    check_file_shift_test "Checking true case 33" [ 0; 1; 2; 1 ] true;
    check_file_shift_test "Checking true case 34" [ 0; 1; 3; 1 ] true;
    check_file_shift_test "Checking true case 35" [ 0; 2; 1; 2 ] true;
    check_file_shift_test "Checking true case 36" [ 0; 2; 2; 2 ] true;
    check_file_shift_test "Checking true case 37" [ 0; 2; 3; 2 ] true;
    check_file_shift_test "Checking true case 38" [ 0; 3; 1; 3 ] true;
    check_file_shift_test "Checking true case 39" [ 0; 3; 2; 3 ] true;
    check_file_shift_test "Checking true case 40" [ 0; 3; 3; 3 ] true;
    check_file_shift_test "Checking true case 41" [ 0; 4; 1; 4 ] true;
    check_file_shift_test "Checking true case 42" [ 0; 4; 2; 4 ] true;
    check_file_shift_test "Checking true case 43" [ 0; 4; 3; 4 ] true;
    check_file_shift_test "Checking true case 44" [ 0; 5; 1; 5 ] true;
    check_file_shift_test "Checking true case 45" [ 0; 5; 2; 5 ] true;
    check_file_shift_test "Checking true case 46" [ 0; 5; 3; 5 ] true;
    check_file_shift_test "Checking true case 47" [ 0; 6; 1; 6 ] true;
    check_file_shift_test "Checking true case 48" [ 0; 6; 2; 6 ] true;
    check_file_shift_test "Checking true case 49" [ 0; 6; 3; 6 ] true;
    check_file_shift_test "Checking false case 1" [ 1; 2; 2; 3 ] false;
    check_file_shift_test "Checking false case 2" [ 2; 2; 1; 4 ] false;
    check_file_shift_test "Checking false case 3" [ 1; 2; 3; 3 ] false;
    check_file_shift_test "Checking false case 4" [ 2; 2; 1; 6 ] false;
    check_file_shift_test "Checking false case 5" [ 2; 2; 1; 7 ] false;
    check_file_shift_test "Checking false case 6" [ 2; 3; 1; 4 ] false;
    check_file_shift_test "Checking false case 7" [ 2; 3; 1; 5 ] false;
    check_file_shift_test "Checking false case 8" [ 2; 3; 1; 6 ] false;
    check_file_shift_test "Checking false case 9" [ 2; 3; 1; 7 ] false;
    check_file_shift_test "Checking false case 10" [ 2; 4; 1; 5 ] false;
    check_file_shift_test "Checking false case 11" [ 3; 4; 2; 6 ] false;
    check_file_shift_test "Checking false case 12" [ 3; 4; 2; 7 ] false;
    check_file_shift_test "Checking false case 13" [ 3; 5; 2; 6 ] false;
    check_file_shift_test "Checking false case 14" [ 3; 5; 2; 7 ] false;
  ]

let check_rt_diagonal_shift_tests =
  [
    check_rt_diagonal_shift_test "Checking true case 1" [ 1; 3; 2; 4 ] true;
    check_rt_diagonal_shift_test "Checking true case 2" [ 1; 4; 2; 5 ] true;
    check_rt_diagonal_shift_test "Checking true case 3" [ 1; 5; 2; 6 ] true;
    check_rt_diagonal_shift_test "Checking true case 4" [ 1; 6; 2; 7 ] true;
    check_rt_diagonal_shift_test "Checking true case 5" [ 1; 3; 3; 5 ] true;
    check_rt_diagonal_shift_test "Checking true case 6" [ 1; 4; 3; 6 ] true;
    check_rt_diagonal_shift_test "Checking true case 7" [ 1; 5; 3; 7 ] true;
    check_rt_diagonal_shift_test "Checking true case 8" [ 1; 1; 4; 4 ] true;
    check_rt_diagonal_shift_test "Checking true case 9" [ 1; 2; 4; 5 ] true;
    check_rt_diagonal_shift_test "Checking true case 10" [ 1; 3; 4; 6 ] true;
    check_rt_diagonal_shift_test "Checking true case 11" [ 1; 4; 4; 7 ] true;
    check_rt_diagonal_shift_test "Checking true case 12" [ 1; 2; 5; 6 ] true;
    check_rt_diagonal_shift_test "Checking true case 13" [ 1; 3; 5; 7 ] true;
    check_rt_diagonal_shift_test "Checking true case 14" [ 1; 2; 6; 7 ] true;
    check_rt_diagonal_shift_test "Checking false case 1" [ 1; 4; 6; 7 ] false;
    check_rt_diagonal_shift_test "Checking false case 2" [ 1; 5; 6; 7 ] false;
    check_rt_diagonal_shift_test "Checking false case 3" [ 1; 4; 2; 7 ] false;
    check_rt_diagonal_shift_test "Checking false case 4" [ 1; 3; 3; 7 ] false;
    check_rt_diagonal_shift_test "Checking false case 5" [ 1; 4; 3; 7 ] false;
    check_rt_diagonal_shift_test "Checking false case 6" [ 1; 2; 3; 7 ] false;
    check_rt_diagonal_shift_test "Checking false case 7" [ 1; 4; 1; 7 ] false;
    check_rt_diagonal_shift_test "Checking false case 8" [ 2; 5; 2; 7 ] false;
    check_rt_diagonal_shift_test "Checking false case 9" [ 3; 4; 3; 7 ] false;
    check_rt_diagonal_shift_test "Checking false case 10" [ 4; 3; 4; 7 ] false;
    check_rt_diagonal_shift_test "Checking false case 11" [ 5; 4; 5; 7 ] false;
    check_rt_diagonal_shift_test "Checking false case 12" [ 6; 2; 6; 7 ] false;
  ]

let suite =
  "test suite for Ocaml Chess"
  >::: List.flatten
         [
           init_piece_tests;
           get_colour_tests;
           get_piece_type_tests;
           get_square_tests;
           convert_file_tests;
           convert_int_to_file_tests;
           convert_rank_tests;
           convert_rank_tests;
           convert_int_to_rank_tests;
           return_piece_type_tests;
           rev_return_piece_type_tests;
           txt_to_piece_tests;
           convert_square_tests;
           check_board_legal_tests;
           piece_to_symbol_helper_tests;
           colour_check_tests;
           check_rank_shift_tests;
           check_file_shift_tests;
           check_rt_diagonal_shift_tests;
         ]

let _ = run_test_tt_main suite
