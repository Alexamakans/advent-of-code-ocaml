open Slam

(*
    Filters out non-digit characters from a line and returns the result as a
    char list.
 *)
let parse_line_day_1_part_1 (line : string) : char list =
  List.filter (fun (c : char) -> is_digit c) (string_to_char_list line)

(*
    For each line:
        Build a number using the first and last digit.
    Return the sum.
 *)
let solve_day_1_part_1 input_path : int =
  read_lines input_path
  |> List.map parse_line_day_1_part_1
  |> List.map (fun x ->
         (char_digit_to_int (first x) * 10) + char_digit_to_int (last x) )
  |> List.fold_left (fun acc v -> acc + v) 0
;;

(*
    known_answer can be anything if we don't know the answer yet, it is here to
    prevent regressions.
 *)
let known_answer = 54916 in
let day = 1 in
let part = 1 in
let label = Printf.sprintf "Day %d Part %d" day part in
let answer = time label solve_day_1_part_1 "2023-1.txt" in
let () = Printf.printf "Answer: %d\n" answer in
if answer != known_answer then
  failwith
    (Printf.sprintf "Got wrong answer for Day %d Part %d; got %d, expected %d"
       day part answer known_answer )

let parse_line_day_1_part_2 (line : string) : char list =
  (* TODO: Replace one two three four five six seven eight nine zero with digits *)
  List.filter (fun (c : char) -> is_digit c) (string_to_char_list line)

let solve_day_1_part_2 input_path : int =
  read_lines input_path
  |> List.map parse_line_day_1_part_2
  |> List.map (fun x ->
         (char_digit_to_int (first x) * 10) + char_digit_to_int (last x) )
  |> List.fold_left (fun acc v -> acc + v) 0
;;

let known_answer = 0 in
let day = 1 in
let part = 2 in
let label = Printf.sprintf "Day %d Part %d" day part in
let answer = time label solve_day_1_part_2 "2023-1.txt" in
let () = Printf.printf "Answer: %d\n" answer in
if answer != known_answer then
  failwith
    (Printf.sprintf "Got wrong answer for Day %d Part %d; got %d, expected %d"
       day part answer known_answer )
