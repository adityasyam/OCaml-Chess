open Chesspiece

let check_piece_in_pos (piece_arr : piece array array) (piece_name : string)
    (lst : int list) : bool =
  let i1 = List.nth lst 0 in
  let i2 = List.nth lst 1 in
  let p_actual = piece_arr.(7 - i2).(i1) in
  match p_actual.piece_type with
  | King -> if piece_name = "king" then true else false
  | Queen -> if piece_name = "queen" then true else false
  | Bishop -> if piece_name = "bishop" then true else false
  | Knight -> if piece_name = "knight" then true else false
  | Rook -> if piece_name = "rook" then true else false
  | Pawn -> if piece_name = "pawn" then true else false
  | Blank -> false

let check_square_blank (piece_arr : piece array array) (sq : square) : bool =
  match sq with
  | file, rank ->
      let file_coord = convert_file file in
      let rank_coord = convert_rank rank in
      if get_piece_type piece_arr.(7 - rank_coord).(file_coord) = Blank then
        true
      else false

let check_oppo_piece (piece_arr : piece array array) (lst : int list) : bool =
  let f1 = List.nth lst 0 in
  let r1 = List.nth lst 1 in
  let f2 = List.nth lst 2 in
  let r2 = List.nth lst 3 in
  let init_col = get_colour piece_arr.(7 - r1).(f1) in
  let fin_col = get_colour piece_arr.(7 - r2).(f2) in
  if init_col <> fin_col then true else false

let not_vertical_collision (piece_arr : piece array array) (lst : int list) :
    bool =
  let f1 = List.nth lst 0 in
  let r1 = List.nth lst 1 in
  let f2 = List.nth lst 2 in
  let r2 = List.nth lst 3 in
  let check_vert = ref true in
  if f1 = f2 then (
    for rank = r1 + 1 to r2 - 1 do
      if
        check_square_blank piece_arr
          (convert_int_to_file f1, convert_int_to_rank rank)
        <> true
      then check_vert := false
      else check_vert := !check_vert
    done;
    !check_vert)
  else false

let not_horizontal_collision (piece_arr : piece array array) (lst : int list) :
    bool =
  let f1 = List.nth lst 0 in
  let r1 = List.nth lst 1 in
  let f2 = List.nth lst 2 in
  let r2 = List.nth lst 3 in
  let check_hor = ref true in
  if r1 = r2 then (
    for file = f1 + 1 to f2 - 1 do
      if
        check_square_blank piece_arr
          (convert_int_to_file file, convert_int_to_rank r1)
        <> true
      then check_hor := false
      else check_hor := !check_hor
    done;
    !check_hor)
  else false

let not_rt_diagonal_collision (piece_arr : piece array array) (lst : int list) :
    bool =
  let f1 = List.nth lst 0 in
  let r1 = List.nth lst 1 in
  let f2 = List.nth lst 2 in
  let r2 = List.nth lst 3 in
  let check_diag = ref true in
  if f1 <> f2 && r1 <> r2 then (
    for rank = r1 + 1 to r2 - 1 do
      for file = f1 + 1 to f2 - 1 do
        if abs (rank - r1) = abs (file - f1) then
          if
            check_square_blank piece_arr
              (convert_int_to_file file, convert_int_to_rank rank)
            <> true
          then check_diag := false
          else check_diag := !check_diag
        else check_diag := !check_diag
      done
    done;
    !check_diag)
  else false

let not_lt_diagonal_collision (piece_arr : piece array array) (lst : int list) :
    bool =
  let f1 = List.nth lst 0 in
  let r1 = List.nth lst 1 in
  let f2 = List.nth lst 2 in
  let r2 = List.nth lst 3 in
  let check_diag = ref true in
  if f1 <> f2 && r1 <> r2 then (
    for rank = r1 + 1 to r2 - 1 do
      for file = f1 - 1 to f2 - 1 do
        if abs (rank - r1) = abs (file - f1) then
          if
            check_square_blank piece_arr
              (convert_int_to_file file, convert_int_to_rank rank)
            <> true
          then check_diag := false
          else check_diag := !check_diag
        else check_diag := !check_diag
      done
    done;
    !check_diag)
  else false

let check_rt_diagonal_shift (lst : int list) : bool =
  let f1 = List.nth lst 0 in
  let r1 = List.nth lst 1 in
  let f2 = List.nth lst 2 in
  let r2 = List.nth lst 3 in
  if (f1 <> f2 && r1 <> r2) && f1 - f2 = r1 - r2 then true else false

let check_lt_diagonal_shift (lst : int list) : bool =
  let f1 = List.nth lst 0 in
  let r1 = List.nth lst 1 in
  let f2 = List.nth lst 2 in
  let r2 = List.nth lst 3 in
  if (f1 <> f2 && r1 <> r2) && -1 * (f1 - f2) = r1 - r2 then true else false

let check_rank_shift (lst : int list) : bool =
  let f1 = List.nth lst 0 in
  let r1 = List.nth lst 1 in
  let f2 = List.nth lst 2 in
  let r2 = List.nth lst 3 in
  if f1 = f2 && r1 <> r2 then true else false

let check_file_shift (lst : int list) : bool =
  let f1 = List.nth lst 0 in
  let r1 = List.nth lst 1 in
  let f2 = List.nth lst 2 in
  let r2 = List.nth lst 3 in
  if f1 <> f2 && r1 = r2 then true else false

let valid_pawn_move (piece_arr : piece array array) (pawn : piece)
    (lst : int list) : bool =
  let f1 = List.nth lst 0 in
  let r1 = List.nth lst 1 in
  let f2 = List.nth lst 2 in
  let r2 = List.nth lst 3 in
  let check_if_vertical = check_rank_shift lst in
  let final_square_empty =
    check_square_blank piece_arr (convert_int_to_file f2, convert_int_to_rank r2)
  in
  match get_colour pawn with
  | White ->
      let first_move = r1 = 1 in
      let rank_diff = r2 - r1 in
      let file_diff = f2 - f1 in
      ((rank_diff = 1 || (rank_diff = 2 && first_move)) && check_if_vertical)
      || ((abs file_diff = 1 && rank_diff = 1) && final_square_empty = false)
  | Black ->
      let first_move = r1 = 6 in
      let rank_diff = r1 - r2 in
      let file_diff = f2 - f1 in
      ((rank_diff = 1 || (rank_diff = 2 && first_move)) && check_if_vertical)
      || ((abs file_diff = 1 && rank_diff = 1) && final_square_empty = false)
  | _ -> failwith "impossible"

let valid_king_move (king : piece) (lst : int list) : bool =
  let f1 = List.nth lst 0 in
  let r1 = List.nth lst 1 in
  let f2 = List.nth lst 2 in
  let r2 = List.nth lst 3 in
  let rank_diff_abs = abs (r2 - r1) in
  let rank_diff = r2 - r1 in
  let file_diff_abs = abs (f2 - f1) in
  let file_diff = f2 - f1 in

  if f1 <> f2 || r1 <> r2 then
    if rank_diff_abs = 1 && f1 = f2 then true
    else if file_diff_abs = 1 && r1 = r2 then true
    else if rank_diff = 1 && file_diff = 1 then true
    else if rank_diff = -1 && file_diff = -1 then true
    else false
  else false

let valid_knight_move (knight : piece) (lst : int list) : bool =
  let f1 = List.nth lst 0 in
  let r1 = List.nth lst 1 in
  let f2 = List.nth lst 2 in
  let r2 = List.nth lst 3 in

  if f1 <> f2 || r1 <> r2 then
    let rank_diff = abs (r1 - r2) in
    let file_diff = abs (f1 - f2) in
    if (rank_diff = 1 && file_diff = 2) || (rank_diff = 2 && file_diff = 1) then
      true
    else false
  else false

let valid_rook_move (rook : piece) (lst : int list) : bool =
  let f1 = List.nth lst 0 in
  let r1 = List.nth lst 1 in
  let f2 = List.nth lst 2 in
  let r2 = List.nth lst 3 in
  if f1 <> f2 || r1 <> r2 then check_file_shift lst || check_rank_shift lst
  else false

let valid_queen_move (queen : piece) (lst : int list) : bool =
  let f1 = List.nth lst 0 in
  let r1 = List.nth lst 1 in
  let f2 = List.nth lst 2 in
  let r2 = List.nth lst 3 in
  if f1 <> f2 || r1 <> r2 then
    check_file_shift lst || check_rank_shift lst
    || check_rt_diagonal_shift lst
    || check_lt_diagonal_shift lst
  else false

let valid_bishop_move (bishop : piece) (lst : int list) : bool =
  let f1 = List.nth lst 0 in
  let r1 = List.nth lst 1 in
  let f2 = List.nth lst 2 in
  let r2 = List.nth lst 3 in
  if f1 <> f2 || r1 <> r2 then
    check_rt_diagonal_shift lst || check_lt_diagonal_shift lst
  else false

let colour_check (is_white : bool) (p : piece) : bool =
  if is_white then
    match p.colour with Black -> false | White -> true | _ -> false
  else match p.colour with Black -> true | White -> false | _ -> false

let check_legality (piece_arr : piece array array) (name : string)
    (lst : int list) (is_white : bool) : bool =
  let check_position_valid = check_piece_in_pos piece_arr name lst in
  if check_position_valid then
    let i1 = List.nth lst 0 in
    let i2 = List.nth lst 1 in
    let i3 = List.nth lst 2 in
    let i4 = List.nth lst 3 in
    let p_actual = piece_arr.(7 - i2).(i1) in
    let final_square_empty =
      check_square_blank piece_arr
        (convert_int_to_file i3, convert_int_to_rank i4)
    in
    if colour_check is_white p_actual then
      match name with
      | "pawn" ->
          if valid_pawn_move piece_arr p_actual lst then
            if check_rank_shift lst then
              if not_vertical_collision piece_arr lst then true else false
            else if check_oppo_piece piece_arr lst then true
            else false
          else false
      | "king" ->
          if valid_king_move p_actual lst then
            if check_rank_shift lst then
              if not_vertical_collision piece_arr lst then
                if final_square_empty = true then true
                else check_oppo_piece piece_arr lst
              else false
            else if check_file_shift lst then
              if not_horizontal_collision piece_arr lst then
                if final_square_empty = true then true
                else check_oppo_piece piece_arr lst
              else false
            else if check_lt_diagonal_shift lst then
              if not_lt_diagonal_collision piece_arr lst then
                if final_square_empty = true then true
                else check_oppo_piece piece_arr lst
              else false
            else if check_rt_diagonal_shift lst then
              if not_rt_diagonal_collision piece_arr lst then
                if final_square_empty = true then true
                else check_oppo_piece piece_arr lst
              else false
            else false
          else false
      | "knight" ->
          if valid_knight_move p_actual lst then
            if final_square_empty = true then true
            else check_oppo_piece piece_arr lst
          else false
      | "rook" ->
          if valid_rook_move p_actual lst then
            if check_rank_shift lst then
              if not_vertical_collision piece_arr lst then
                if final_square_empty = true then true
                else check_oppo_piece piece_arr lst
              else false
            else if check_file_shift lst then
              if not_horizontal_collision piece_arr lst then
                if final_square_empty = true then true
                else check_oppo_piece piece_arr lst
              else false
            else false
          else false
      | "queen" ->
          if valid_queen_move p_actual lst then
            if check_rank_shift lst then
              if not_vertical_collision piece_arr lst then
                if final_square_empty = true then true
                else check_oppo_piece piece_arr lst
              else false
            else if check_file_shift lst then
              if not_horizontal_collision piece_arr lst then
                if final_square_empty = true then true
                else check_oppo_piece piece_arr lst
              else false
            else if check_lt_diagonal_shift lst then
              if not_lt_diagonal_collision piece_arr lst then
                if final_square_empty = true then true
                else check_oppo_piece piece_arr lst
              else false
            else if check_rt_diagonal_shift lst then
              if not_rt_diagonal_collision piece_arr lst then
                if final_square_empty = true then true
                else check_oppo_piece piece_arr lst
              else false
            else false
          else false
      | "bishop" ->
          if valid_bishop_move p_actual lst then
            if check_lt_diagonal_shift lst then
              if not_lt_diagonal_collision piece_arr lst then
                if final_square_empty = true then true
                else check_oppo_piece piece_arr lst
              else false
            else if check_rt_diagonal_shift lst then
              if not_rt_diagonal_collision piece_arr lst then
                if final_square_empty = true then true
                else check_oppo_piece piece_arr lst
              else false
            else false
          else false
      | _ -> failwith "invalid move"
    else false
  else false