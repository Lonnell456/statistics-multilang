(* statistics.ml *)

let mean lst =
  let total = List.fold_left ( +. ) 0.0 lst in
  total /. float_of_int (List.length lst)

let median lst =
  let sorted = List.sort compare lst in
  let len = List.length sorted in
  if len mod 2 = 1 then
    float_of_int (List.nth sorted (len / 2))
  else
    let mid1 = List.nth sorted (len / 2 - 1) in
    let mid2 = List.nth sorted (len / 2) in
    float_of_int (mid1 + mid2) /. 2.0

let mode lst =
  let freq = List.fold_left (fun acc x ->
    let count = try List.assoc x acc with Not_found -> 0 in
    (x, count + 1) :: List.remove_assoc x acc
  ) [] lst in
  let max_count = List.fold_left (fun acc (_, c) -> max acc c) 0 freq in
  freq
  |> List.filter (fun (_, c) -> c = max_count)
  |> List.map fst

let () =
  let data = [1; 2; 2; 3; 4; 5; 5; 5; 6] in
  let data_float = List.map float_of_int data in
  Printf.printf "Mean: %.2f\n" (mean data_float);
  Printf.printf "Median: %.2f\n" (median data);
  Printf.printf "Mode: %s\n"
    (String.concat ", " (List.map string_of_int (mode data)))