open Chesspiece

type object_phrase = string list
(** The type [object_phrase] represents the phrase that can be part of a
    player command. Each element of the list represents a word of the 
    phrase. *)

type command =
  | Move of object_phrase
  | Quit
      (** The type [command] represents a player command that is decomposed into a
    the verb [Move] and possibly an object phrase. *)

exception Empty
(** Raised when an empty command is parsed. *)

exception Invalid
(** Raised when the user's command is invalid or the player is trying to make an illegal move. *)

exception InvalidInput of string
(** Raised when player is trying to make an illegal move, informs what is illegal. *)

val check_fourth_num : int -> string list -> int list -> int list
(** [check_fourth_num] finds the fourth word of the user's input and checks the square position (number), moves onto checking the rest of the object phrase if player input is a valid command.*)

val check_fourth_letter : int list -> string list -> int list
(** [check_fourth_letter] finds the fourth word of the user's input and checks the square position (letter), moves onto checking the rest of the object phrase if player input is a valid command.*)

val check_third_num : int -> string list -> int list
(** [check_third_num] finds the third word of the user's input and checks the square position (number), moves onto checking the rest of the object phrase if player input is a valid command.*)

val check_third_letter : string list -> int list
(** [check_third_letter] finds the third word of the user's input and checks the square position (letter), moves onto checking the rest of the object phrase if player input is a valid command.*)

val check_second : string list -> int list
(** [check_second] finds the second of the user's input and checks if it is valid as a piecetype, moves onto checking the rest of the object phrase if player input is a valid command.*)

val check_first : string list -> int list
(** [check_first] checks if the first word of the user's input is valid as a [Move] or [Quit] command, moves onto checking the rest of the object phrase if player input is a valid command.*)

val parse : piece array array -> string -> bool -> int list
(** [parse] parses a player's input into a [command] and returns the parsed initial and final square co-ordinates as an integer list.*)

val parse_integers : string -> int list
(** [parse_integers] returns the initial and final square co-ordinates as an integer list.*)

val parse_command : string -> command
(** [parse_command s] parses the user input and returns a command [Move] or [Quit]*)

val swap : piece array array -> int list -> piece array array
(** [swap] moves the piece from the specified initial square to the final aquare if the move is legal
    *)
