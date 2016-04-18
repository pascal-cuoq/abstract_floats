let sign_bit = 0x8000_0000_0000_0000L

let payload_mask = 0x800F_FFFF_FFFF_FFFFL

let header_mask  = 0x0FF0_0000_0000_0000L

let to_payload n = Int64.logand n payload_mask

let is_pos f = Int64.(logand (bits_of_float f) sign_bit) = 0L

let is_neg f = Int64.(logand (bits_of_float f) sign_bit) <> 0L

let is_NaN f = classify_float f = FP_nan

let is_zero f = classify_float f = FP_zero

let is_inf f = classify_float f = FP_infinite

let is_pos_zero f = Int64.bits_of_float f = 0L

let is_neg_zero f = Int64.bits_of_float f = sign_bit

let largest_neg = -4.94e-324
let smallest_pos = +4.94e-324
let smallest_neg = -.max_float
let largest_pos = max_float

let neg_zero = Int64.float_of_bits 0x8000_0000_0000_0000L
let pos_zero = Int64.float_of_bits 0x0000_0000_0000_0000L

let fsucc f = Int64.(float_of_bits @@ succ @@ bits_of_float f)
let fpred f = Int64.(float_of_bits @@ pred @@ bits_of_float f)

let m1 = 0x4000_0000_0000_0000L
let m2 = 0xBFFF_FFFF_FFFF_FFFFL

let on_bit f pos =
  let mask = Int64.(shift_right m1 pos) in
  Int64.(float_of_bits @@ logor (bits_of_float f) mask)

let off_bit f pos =
  let mask = Int64.(shift_right m2 pos) in
  Int64.(float_of_bits @@ logand (bits_of_float f) mask)

let dichotomy restore init a b =
  let rec approach f i =
    if i >= 63 then f else
      let tf = on_bit f i in
      if restore tf a b then approach f (i + 1) else approach tf (i + 1)
  in approach init 0

let upper_neg x y =
  fsucc @@ dichotomy (fun f a b -> f +. a <= b) neg_zero x y

let lower_neg = dichotomy (fun f a b -> f +. a < b) neg_zero

let upper_pos = dichotomy (fun f a b -> f +. a > b) pos_zero

let lower_pos x y =
  fsucc @@ dichotomy (fun f a b -> f +. a >= b) pos_zero x y

(* smallest pos normalish such that there exists a number x such that
 [pos_cp +. x = infinity] *)
let pos_cp = 9.9792015476736e+291
let neg_cp = -9.9792015476736e+291

let dump a b =
  Printf.printf "upper_neg : %.16e\n" (upper_neg a b);
  Printf.printf "lower_neg : %.16e\n" (lower_neg a b);
  Printf.printf "upper_pos : %.16e\n" (upper_pos a b);
  Printf.printf "lower_pos : %.16e\n" (lower_pos a b)

let lower_bound a b =
  if a < b then lower_pos a b else lower_neg a b

let upper_bound a b =
  if a > b then upper_neg a b else upper_pos a b

let range_add al au bl bu =
  assert(al <> infinity && al <> neg_infinity);
  assert(au <> infinity && au <> neg_infinity);
  if bl = infinity || bu = infinity ||
     bl = neg_infinity || bu = neg_infinity then assert(bl = bu);
  if 0. < au && au <= max_float then assert(0. < al && al <= au);
  if 0. < bu && bu <= max_float then assert(0. < bl && bl <= bu);
  if (-.max_float) <= al && al < 0. then assert(al <= au && au < 0.);
  if (-.max_float) <= bl && bl < 0. then assert(bl <= bu && bu < 0.);
  if bl = infinity then
    if au < pos_cp then None else
      let l = lower_pos au infinity in
      let l = if l +. au < infinity then fsucc l else l in
      Some (l, max_float) else
  if bl = neg_infinity then
    if al > neg_cp then None else
      let u = upper_neg al neg_infinity in
      let u = if u +. al > neg_infinity then fsucc u else u in
      Some (-.max_float, u)
  else
    let l = lower_bound au bl
    and u = upper_bound al bu in
    if l > u then None else Some (l, u)

let normalize = function
  | None -> None, None, None
  | Some (l, u) ->
    assert(l <= u);
    if l = 0.0 && u = 0.0 then Some (-0.0, 0.0), None, None else
    if l = 0.0 then Some (-0.0, 0.0), None, Some (smallest_pos, u) else
    if u = 0.0 then Some (-0.0, 0.0), Some (l, largest_neg), None else
    if u < 0.0 then None, Some (l, u), None else
      Some (-0.0, 0.0), Some (l, largest_neg), Some (smallest_pos, u)

let upper_neg_mult x y = fsucc @@ dichotomy (fun f a b -> f *. a <= b) neg_zero x y

let lower_pos_mult_2 x y = fsucc @@ dichotomy (fun f a b -> f *. a <= b) pos_zero x y

let lower_neg_mult = dichotomy (fun f a b -> f *. a < b) neg_zero

let upper_pos_mult_2 = dichotomy (fun f a b -> f *. a < b) pos_zero

let upper_pos_mult = dichotomy (fun f a b -> f *. a > b) pos_zero

let lower_neg_mult_2 = dichotomy (fun f a b -> f *. a > b) neg_zero

let lower_pos_mult x y = fsucc @@ dichotomy (fun f a b -> f *. a >= b) pos_zero x y

let upper_neg_mult_2 x y = fsucc @@ dichotomy (fun f a b -> f *. a >= b) neg_zero x y

let dump_mult a b =
  Printf.printf "upper_neg     : %.16e\n" (upper_neg_mult a b);
  Printf.printf "lower_neg     : %.16e\n" (lower_neg_mult a b);
  Printf.printf "upper_pos     : %.16e\n" (upper_pos_mult a b);
  Printf.printf "lower_pos     : %.16e\n" (lower_pos_mult a b);
  Printf.printf "upper_neg_pos : %.16e\n" (lower_pos_mult_2 a b);
  Printf.printf "lower_neg_pos : %.16e\n" (upper_pos_mult_2 a b);
  Printf.printf "upper_pos_neg : %.16e\n" (lower_neg_mult_2 a b);
  Printf.printf "lower_pos_neg : %.16e\n" (upper_neg_mult_2 a b)

let range_mult al au bl bu =
  if bl = infinity then
    if au > 0. then
      if au <= 1. then None else
        let l = lower_pos_mult au infinity in
        Some (l, max_float)
    else
      if al >= (-1.) then None else
        let u = upper_neg_mult_2 al infinity in
        Some ((-.max_float), u) else
  if bl = neg_infinity then
    if au > 0. then
      if au <= 1. then None else
        let u = upper_neg_mult au infinity in
        Some ((-.max_float), u)
    else
      if al >= (-1.) then None else
        let l = lower_pos_mult_2 al neg_infinity in
        Some (l, max_float)
  else
  if au < 0.0 && is_pos bl then
    let u = upper_neg_mult_2 al bl in
    let l = lower_neg_mult_2 au bu in
    if l > u then None else Some (l, u) else
  if au < 0.0 && is_neg bu then
    let u = upper_pos_mult_2 au bl in
    let l = lower_pos_mult_2 al bu in
    if l > u then None else Some (l, u) else
  if al > 0.0 && is_neg bu then
    let u = upper_neg_mult au bu in
    let l = lower_neg_mult al bl in
    if l > u then None else Some (l, u)
  else
    let u = upper_pos_mult al bu in
    let l = lower_pos_mult au bl in
    if l > u then None else Some (l, u)

let upper_neg_div x y = fsucc @@ dichotomy (fun f a b -> f /. a <= b) neg_zero x y

let lower_pos_div_2 x y = fsucc @@ dichotomy (fun f a b -> f /. a <= b) pos_zero x y

let lower_neg_div = dichotomy (fun f a b -> f /. a < b) neg_zero

let upper_pos_div_2 = dichotomy (fun f a b -> f /. a < b) pos_zero

let upper_pos_div = dichotomy (fun f a b -> f /. a > b) pos_zero

let lower_neg_div_2 = dichotomy (fun f a b -> f /. a > b) neg_zero

let lower_pos_div x y = fsucc @@ dichotomy (fun f a b -> f /. a >= b) pos_zero x y

let upper_neg_div_2 x y = fsucc @@ dichotomy (fun f a b -> f /. a >= b) neg_zero x y

(*
let dump_div a b =
  Printf.printf "lower_neg_div     : %h\n" (lower_neg_div a b);
  Printf.printf "upper_neg_div     : %h\n" (upper_neg_div a b);
  (* fixed *)
  Printf.printf "lower_pos_div     : %h\n" (lower_pos_div a b);
  Printf.printf "upper_pos_div     : %h\n" (upper_pos_div a b);
  (* fixed *)
  Printf.printf "lower_pos_div_2   : %h\n" (lower_pos_div_2 a b);
  Printf.printf "upper_pos_div_2   : %h\n" (upper_pos_div_2 a b);
  Printf.printf "lower_neg_div_2   : %h\n" (lower_neg_div_2 a b);
  Printf.printf "upper_neg_div_2   : %h\n" (upper_neg_div_2 a b)
*)

let dump_div_m1 a b =
  Printf.printf "lower_neg_div     : %.16e\n" (lower_neg_div a b);
  Printf.printf "upper_neg_div     : %.16e\n" (upper_neg_div a b);
  (* fixed *)
  Printf.printf "lower_pos_div     : %.16e\n" (lower_pos_div a b);
  Printf.printf "upper_pos_div     : %.16e\n" (upper_pos_div a b);
  (* fixed *)
  Printf.printf "lower_pos_div_2   : %.16e\n" (lower_pos_div_2 a b);
  Printf.printf "upper_pos_div_2   : %.16e\n" (upper_pos_div_2 a b);
  Printf.printf "lower_neg_div_2   : %.16e\n" (lower_neg_div_2 a b);
  Printf.printf "upper_neg_div_2   : %.16e\n" (upper_neg_div_2 a b)


(* [0.6, 1.8], [0.3, 1.6]
   al * bl ---> xl (* lower_pos *)
   au * bu ---> xu (* upper_pos *)

   [0.6, 0.6] [1.8, 1.8]

   [0.3, 0.6] [1.8, 1.8] -------> important example

   (5.4000000000000004e-01, 1.0799999999999998e+00)

   [0.6, 0.6] [1.8, 2.]

   [max_float, max_float] [0.0, 0.0]

    xl = 4.9406564584124654e-324
    xu = 4.4408920985006257e-16

   [1e+308, max_float], [0., 0.]

   xl (* lower_pos *)
   xu (* upper_pos *)

   [1e+10, 1e+20], [0., 0.]

    xl = 4.9406564584124654e-324
    xu = 2.4703282292062327e-304

   [1., 1e+308] [0., 0.]

   [1e+10, 1e+308] [0., 0.]

   dump_div 0.2e+1 0.

   when b is 0.

   if al < 2., cannot underflow
   if al >= 2, xl is (fsucc 0.), xu is upper of upper_pos xu 0.

   [min_float, max_float] [min_float, max_float]

   just normal lower_pos and upper_pos

   [min_float, min_float] [max_float, max_float]

   [min_float, 20.] [min_float, 30.]

   [min_float, 1e-200] [1e-200, 1.]

   [min_float, max_float] [min_float, max_float]

   [max_float, max_float] [infinity, infinity]

   [1e-50, 1e-20] [infinity, infinity]

   dump_div 1e-20 infinity --> lower_pos 1.7976931348623158e+288
   dump_div 1e-50 infinity --> lower_pos 1.7976931348623159e+258
   dump_div 1e-323 infinity --> lower_pos 1.7763568394002505e-15

*)

let fsucc' f = if is_pos f then fsucc f else fpred f
let fpred' f = if is_pos f then fpred f else fsucc f

let rec drift_right xl al au bl bu i =
  if al > au then raise Not_found;
  let b = xl /. al in
  let err = if is_pos bl then b -. bl else bl -. b in
  if err = 0. then i, xl else begin
    if is_pos err && is_pos al || is_neg err && is_neg al then
      drift_right xl (fsucc' al) au bl bu (i + 1)
    else
      drift_right (fsucc' xl) al au bl bu (i + 1)
  end

let rec drift_left xu al au bl bu i =
  if al > au then raise Not_found;
  let b = xu /. au in
  let err = if is_pos bl then b -. bu else bu -. b in
  if err = 0. then i, xu else begin
    if is_pos err && is_neg al || is_neg err && is_pos al then
      drift_left xu al (fpred' au) bl bu (i + 1)
    else
      drift_left (fpred' xu) al au bl bu (i + 1)
  end

let range_div al au bl bu =
  if bl = infinity || bl = neg_infinity then
    if al > 0. then
      if al >= 1. then None else
        let l, u = lower_pos_div al infinity, max_float in
        let l, u = if is_neg bl then (-.u, -.l) else l, u in
        Some (l, u)
    else
      if au <= (-1.) then None else
        let l, u = (-.max_float), upper_neg_div_2 au infinity in
        let l, u = if is_neg bl then (-.u, -.l) else l, u in
        Some (l, u)
  else
  if bl = 0.0 || bl = -0.0 then
    if al > 0. then
      if au < 2. then None else
        let l, u = smallest_pos, upper_pos_div au 0. in
        let l, u = if is_neg bl then (-.u, -.l) else l, u in
        Some (l, u)
    else
      if al > -2. then None else
        let l, u = lower_neg_div_2 al 0., largest_neg in
        let l, u = if is_neg bl then (-.u, -.l) else l, u in
        Some (l, u)
  else
    let xl, xu =
      if is_pos au && is_pos bu then
        lower_pos_div al bl, upper_pos_div au bu else
      if is_pos au && is_neg bu then
        lower_neg_div al bl, upper_neg_div au bu else
      if is_neg au && is_pos bu then
        lower_neg_div_2 al bl, upper_neg_div_2 au bu
      else
        lower_pos_div_2 al bl, upper_pos_div_2 au bu in
    if xl > xu then None else Some (xl, xu)

let range_div_2 al au bl bu =
    let xl, xu =
      if is_pos au && is_pos bu then
        lower_pos_div al bl, upper_pos_div au bu else
      if is_pos au && is_neg bu then
        lower_neg_div al bl, upper_neg_div au bu else
      if is_neg au && is_pos bu then
        lower_neg_div_2 al bl, upper_neg_div_2 au bu
      else
        lower_pos_div_2 al bl, upper_pos_div_2 au bu in
    try
      let n1, xl = drift_right xl al au bl bu 0 in
      let n2, xu = drift_left xu al au bl bu 0 in
      if xl > xu then n1, n2, None else n1, n2, Some (xl, xu)
    with _ -> 0, 0, None

let () = Random.self_init ()

let test () =
  let random_pos_normalish () = Random.float 10. in
(*
    match Random.int 15 with
    | 0 -> min_float
    | 1 -> max_float
    | 2 -> Random.float min_float
    | 3 | 4 -> Random.float max_float
    | 5 -> Random.float 2e-308
    | 6 -> 2e-308
    | _ -> Random.float 1_000_00. in
*)
  let au = random_pos_normalish () in
  let al = if Random.int 5 = 0 then au else Random.float au in
  if al = 0. then failwith ".";
  let bu = random_pos_normalish () in
  let bl = if Random.int 5 = 0 then bu else Random.float bu in
  if bl = 0. then failwith ".";
  let m, n, _ = range_div_2 al au bl bu in
  if m > 10 || n > 10 then begin
    Printf.printf "%d, %d\n" m n;
    Printf.printf "%.16e\n" al;
    Printf.printf "%.16e\n" au;
    Printf.printf "%.16e\n" bl;
    Printf.printf "%.16e\n" bu;
    assert false
  end

let div2_cp = 4.4408920985006257e-16
(* least value that can be divided and overflow to infinity *)
let div2_cp2 = 8.8817841970012523e-16

let upper_neg_div_m2 a b = fsucc @@ dichotomy (fun x a b -> a /. x <= b) neg_zero a b

let lower_pos_div_m2_2 a b = fsucc @@ dichotomy (fun x a b -> a /. x <= b) pos_zero a b

let lower_neg_div_m2 = dichotomy (fun x a b -> a /. x < b) neg_zero

let upper_pos_div_m2_2 = dichotomy (fun x a b -> a /. x < b) pos_zero

let upper_pos_div_m2 = dichotomy (fun x a b -> a /. x > b) pos_zero

let lower_neg_div_m2_2 = dichotomy (fun x a b -> a /. x > b) neg_zero

let lower_pos_div_m2 a b = fsucc @@ dichotomy (fun x a b -> a /. x >= b) pos_zero a b

let upper_neg_div_m2_2 a b = fsucc @@ dichotomy (fun x a b -> a /. x >= b) neg_zero a b

let dump_div_m2 a b =
  Printf.printf "lower_neg_div_m2     : %.16e\n" (lower_neg_div_m2 a b);
  Printf.printf "upper_neg_div_m2     : %.16e\n" (upper_neg_div_m2 a b);
  (* fixed *)
  Printf.printf "lower_pos_div_m2     : %.16e\n" (lower_pos_div_m2 a b);
  Printf.printf "upper_pos_div_m2     : %.16e\n" (upper_pos_div_m2 a b);
  (* fixed *)
  Printf.printf "lower_pos_div_m2_2   : %.16e\n" (lower_pos_div_m2_2 a b);
  Printf.printf "upper_pos_div_m2_2   : %.16e\n" (upper_pos_div_m2_2 a b);
  Printf.printf "lower_neg_div_m2_2   : %.16e\n" (lower_neg_div_m2_2 a b);
  Printf.printf "upper_neg_div_m2_2   : %.16e\n" (upper_neg_div_m2_2 a b)

(* al / xl = inf
   au / xu = inf *)
let range_div_m2 al au bl bu =
  if bl = 0. || bl = -0. then
    if al > 0. then
      if al > div2_cp then None else
        let l, u = lower_pos_div_m2_2 al 0., max_float in
        let l, u = if is_neg bl then (-.u, -.l) else l, u in
        Some (l, u)
    else
    if au < (-.div2_cp) then None else
      let l, u = (-.max_float), upper_neg_div_m2 au 0. in
      let l, u = if is_neg bl then (-.u, -.l) else l, u in
      Some (l, u)
  else
  if bl = infinity || bl = neg_infinity then
    if au > 0. then
      if au < div2_cp2 then None else
        let l, u = smallest_pos, upper_pos_div_m2_2 au infinity in
        let l, u = if is_neg bl then (-.u), (-.l) else l, u in
        Some (l, u)
    else
    if al > -.div2_cp2 then None else
      let l, u = lower_neg_div_m2 al infinity, largest_neg in
      let l, u = if is_neg bl then (-.u), (-.l) else l, u in
      Some (l, u)
  else
    let xl, xu =
      if is_pos au && is_pos bu then
        lower_pos_div_m2_2 al bl, upper_pos_div_m2_2 au bu else
      if is_pos au && is_neg bu then
        lower_neg_div_m2_2 al bl, upper_neg_div_m2_2 au bu else
      if is_neg au && is_pos bu then
        lower_neg_div_m2 al bl, upper_neg_div_m2 au bu
      else
        lower_pos_div_m2 al bl, upper_pos_div_m2 au bu in
    if xl > xu then None else Some (xl, xu)
