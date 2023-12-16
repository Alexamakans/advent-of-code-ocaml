let time label f x =
  let t = Sys.time () in
  let fx = f x in
  Printf.printf "%s: %fs\n" label (Sys.time () -. t) ;
  fx

let read_lines path : string list =
  let ic = open_in path in
  let try_read () = try Some (input_line ic) with End_of_file -> None in
  let rec loop acc =
    match try_read () with
    | Some s ->
        loop (s :: acc)
    | None ->
        close_in ic ; List.rev acc
  in
  loop []

let range i j =
  let rec aux n acc = if n < i then acc else aux (n - 1) (n :: acc) in
  aux j []

let ( -- ) i j = range i j

let is_digit c =
  let code = Char.code c in
  List.exists (fun x -> x == code) (48 -- 57)

let first lst = match lst with [] -> failwith "no elements" | el :: _ -> el

let rec last lst =
  match lst with
  | [] ->
      failwith "no elements"
  | [el] ->
      el
  | _ :: lst ->
      last lst

let char_digit_to_int c = Char.code c - 48

let string_to_char_list s = s |> String.to_seq |> List.of_seq
