(* double dichotomy to determine floating point range for x
   in ``x + a == b``, given value of ``a`` and ``b`` *)

let ppf f = Printf.printf "%.16e\n" f

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

let upper_neg = dichotomy (fun f a b -> f +. a <= b) neg_zero

let lower_neg = dichotomy (fun f a b -> f +. a < b) neg_zero

let upper_pos = dichotomy (fun f a b -> f +. a > b) pos_zero

let lower_pos = dichotomy (fun f a b -> f +. a >= b) pos_zero

(* smallest pos normalish such that there exists a number x such that
 [pos_cp +. x = infinity] *)
let pos_cp = 9.9792015476736e+291
let neg_cp = -9.9792015476736e+291

let dump a b =
  Printf.printf "upper_neg : %.16e\n" (upper_neg a b);
  Printf.printf "lower_neg : %.16e\n" (lower_neg a b);
  Printf.printf "upper_pos : %.16e\n" (upper_pos a b);
  Printf.printf "lower_pos : %.16e\n" (lower_pos a b)

(*
  solve x + au >= bl      --------- xl
  solve x + al <= bu      --------- xu


    a = 1.0
    b = 1.1000000000000001e+00

    xu = 1.0000000000000020e-01 (lower_pos is 9.9999999999999964e-02
                                 upper_pos is 1.0000000000000020e-01)

    xl = fsucc @@ 9.9999999999999964e-02

    a = 1.0
    b = 1.4
    
    xu = 4.0000000000000002e-01 (lower_pos is 3.9999999999999974e-01
                                 upper_pos is 4.0000000000000002e-01)

    xl = fsucc 3.9999999999999974e-01 (lower_pos)

    a = 1.4
    b = 1.8
    
    xu = 4.0000000000000019e-01 (lower_pos is 4.0000000000000002e-01
                                 upper_pos is 4.0000000000000019e-01)


    a = 0.4
    b = 1.8

    xu = 1.3999999999999999e+00 (lower_pos is 1.3999999999999999e+00
                                 upper_pos is 1.3999999999999999e+00)

    xl = fsucc @@ 1.3999999999999999e+00

   ------------------

    a = 1.4
    b = 1.0
    
    xu = fsucc @@ -3.9999999999999974e-01
                                (lower_neg is -3.9999999999999997e-01
                                 upper_neg is -3.9999999999999974e-01)
    xl = -3.9999999999999997e (lower_neg)

    a = 1.8
    b = 1.4

    xu = fsucc @@ 3.9999999999999997e-01
                                (lower_neg is -4.0000000000000024e-01
                                 upper_neg is -3.9999999999999997e-01)
    xl = -4.0000000000000024e-01 (lower_neg)


    ----------------
    
    a = 1.4
    b = 1.4

    xl is lower_neg
    xu is upper_pos
*)

let pos_cp = 9.9792015476736e+291
let neg_cp = -9.9792015476736e+291

let lower_bound a b =
  if a < b then fsucc @@ lower_pos a b else lower_neg a b

let upper_bound a b =
  if a > b then fsucc @@ upper_neg a b else upper_pos a b

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
