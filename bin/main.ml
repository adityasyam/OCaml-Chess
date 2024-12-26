open Game
open Game.Chesspiece
open Game.Chessmove
open Game.Command
open Printf

let init_array =
  [|
    [| "♖"; "♘"; "♗"; "♕"; "♔"; "♗"; "♘"; "♖" |];
    [| "♙"; "♙"; "♙"; "♙"; "♙"; "♙"; "♙"; "♙" |];
    [| "*"; "*"; "*"; "*"; "*"; "*"; "*"; "*" |];
    [| "*"; "*"; "*"; "*"; "*"; "*"; "*"; "*" |];
    [| "*"; "*"; "*"; "*"; "*"; "*"; "*"; "*" |];
    [| "*"; "*"; "*"; "*"; "*"; "*"; "*"; "*" |];
    [| "♟"; "♟"; "♟"; "♟"; "♟"; "♟"; "♟"; "♟" |];
    [| "♜"; "♞"; "♝"; "♛"; "♚"; "♝"; "♞"; "♜" |];
  |]

let init_board = ref (string_to_piece init_array)

let print_board (start_board : string array array) =
  for row = 0 to 7 do
    for col = 0 to 7 do
      Printf.printf "%s  " start_board.(row).(col)
    done;
    Printf.printf "%s" "\n"
  done

let first_screen () =
  let () = print_endline "Welcome to OCaml Chess!!! Enjoy the game!" in
  print_endline "\n";
  print_board (piece_to_symbol !init_board);
  print_endline "\n"

let rec output_board (board : piece array array) (message : string)
    (is_white : bool) : unit =
  let () = print_endline message in
  match read_line () with
  | exception _ ->
      print_endline "There was an error in reading your input. Game ended."
  | user_in -> (
      try
        match parse_command user_in with
        | Quit -> print_endline "You've Quit the game. Thank you for playing!"
        | Move x ->
            let lst = parse !init_board user_in is_white in
            let st = swap !init_board lst in
            init_board := st;
            print_board (piece_to_symbol st);
            print_endline "\n";

            if is_white then
              output_board !init_board "Black moves next. What's your move?"
                false
            else
              output_board !init_board "White moves next. What's your move?"
                true
      with _ ->
        print_endline "Sorry, you've given an invalid input. ";
        output_board !init_board "Try another move:" is_white)

let main_repl () =
  output_board !init_board "White moves first. What's your move?" true

let () =
  ANSITerminal.erase Screen;
  let _ = first_screen () in
  main_repl ()
