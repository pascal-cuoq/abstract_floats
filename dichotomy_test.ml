open Dichotomy

let max3 = max_float /. 3.

let check_invariant a b =
  match range a b with
  | Range (l, u) ->
    let b1 = l +. a = b in
    let b2 = u +. a = b in
    let b3 =
      if l <> (-.max_float) then
        (if l > 0. then (fpred l) else (fsucc l)) +. a <> b
      else
        true in
    let b4 =
      if u <> max_float then
        (if u > 0. then (fsucc u) else (fpred u)) +. a <> b
      else
        true in
    if not (b1 && b2 && b3 && b4) then failwith "Err1" else ()
  | Single s ->
    if (not ((fsucc s) +. a <> b) && ((fpred s) +. a <> b)) then
      failwith "Err2" else ()
  | Empty ->
    if b <> infinity && b <> neg_infinity && (b -. a) +. a = b then
      failwith "Err3" else ()

let test a b =
  try
    check_invariant a b
  with _ ->
    Printf.printf "f1: %.16e\nf2: %.16e\n" a b;
    print_endline (range a b |> to_string);
    ignore (dump a b); assert false

let normal_test () =
  test 1.0 1.0;
  test 1.0 1.1;
  test 1.0 1.2;
  test 1.0 1.4;
  test 1.0 1.5;
  test 0.0 1.3;
  test 0.0 1.4;
  test 0.0 1.5;
  test 0.0 1.6;
  test 0.0 1.7;
  test max_float infinity;
  test (max_float /. 3.) infinity;
  test (max_float /. 2.) infinity;
  test (max_float /. 2.) max_float;
  test (max_float /. 2.) (max_float /. 3.)

let () = Random.self_init ()

let random_pos () =
  match Random.int 20 with
  | 0 -> min_float
  | 1 -> max_float
  | 2 -> Random.float min_float
  | 3 | 4 -> Random.float max_float
  | 5 -> Random.float 2e-308
  | 6 -> 2e-308
  | 7 | 8 | 9 -> Random.float 100.
  | 11 | 12 -> 0.0
  | 13 -> max_float /. 2.
  | 14 -> max_float /. 3.
  | 15 | 16 -> (float_of_int @@ Random.int 100) /. 10.
  | _ -> Random.float 1_000_00.

let random_pair () =
  let a = random_pos () in
  let b = if Random.int 10 < 2 then infinity else random_pos () in
  let a = if Random.bool () then a else (-. a) in
  let b = if Random.bool () then b else (-. b) in
  if Random.int 5 < 1 then a, a else a, b

let random_test () =
  for i = 0 to 100000 do
    let f1, f2 = random_pair () in
    test f1 f2
  done

let () = normal_test ()
let () = random_test ()
