type abstract_float = float array

(*
Array t with length 1:
a single floating-point number (can be NaN of +inf or -inf, or a finite value)

Array t with length >=2: header + bounds. The length of t can be 2, 3 or 5.
A length of 2 is only intended to distinguish a header from a single
floating-point number.

The header (found in t.(0)) can indicate:

at_least_one_NaN present
all_NaNs present
negative_normalish present
positive_normalish present
-inf present
+inf present
-0.0 present
+0.0 present

Vocabulary:

- normalish means the intervals of normal (or subnormal) values
- finite means the normalish and zero components of the representation
- nonzero means the normalish and infinite components, but usually not NaN
*)

module Header : sig
  type t
  type flag

  val at_least_one_NaN : flag
  val all_NaNs : flag
  val negative_normalish : flag
  val positive_normalish : flag
  val negative_inf : flag
  val positive_inf : flag
  val negative_zero : flag
  val positive_zero : flag

  val bottom : t
  val is_bottom : t -> bool

  val pretty: Format.formatter -> t -> unit
  val combine : t -> t -> t
  val test : t -> flag -> bool
  val set_flag : t -> flag -> t
  val of_flag : flag -> t
  val set_all_NaNs : t -> t
  val exactly_one_NaN : t -> bool
  val is_exactly : t-> flag -> bool

(** the size of the array corresponding to the given header.
    Note that the header alone is not enough to decide that the representation
    should be a single float. Hence this function always returns at least 2. *)
  val size : t -> int

  val of_abstract_float : abstract_float -> t

  val allocate_abstract_float : t -> abstract_float

  val add: t -> t -> t
  val sub: t -> t -> t
  val mult: t -> t -> t
  val div: t -> t -> t
end = struct
  type t = int
  type flag = int

  let at_least_one_NaN = 1
  let all_NaNs = 2
  let negative_normalish = 4
  let positive_normalish = 8
  let negative_inf = 16
  let positive_inf = 32
  let negative_zero = 64
  let positive_zero = 128

  let bottom = 0
  let is_bottom x = x = 0

  let pretty fmt h = Format.fprintf fmt "%d" h (* TODO: improve *)

  let combine h1 h2 = h1 lor h2
  let test h flag = h land flag <> 0
  let test_both h1 h2 flag = h1 land h2 land flag <> 0
  let set_flag = combine
  let of_flag f = f
  let set_all_NaNs h = h lor (at_least_one_NaN + all_NaNs)
  let get_NaN_part h = h land (at_least_one_NaN + all_NaNs)
  let exactly_one_NaN h = (get_NaN_part h) = at_least_one_NaN

  let is_exactly h flag = h = flag

  let size h =
    let posneg = h land 12 in
    if posneg = 12 then 5
    else if posneg <> 0 then 3
    else 2

  let of_abstract_float a =
    assert (Array.length a >= 2);
    let l = Int64.shift_right_logical (Int64.bits_of_float a.(0)) 52 in
    (Int64.to_int l) land 255

  let allocate_abstract_float h =
    Array.make
      (size h)
      (Int64.float_of_bits (Int64.of_int (h lsl 52)))

(* only to implement sub from add *)
  let neg h =
    let neg = h land (negative_zero + negative_inf + negative_normalish) in
    let pos = h land (positive_zero + positive_inf + positive_normalish) in
    (get_NaN_part h) lor (neg lsl 1) lor (pos lsr 1)

  let add h1 h2 =
    let h1negintopos = h1 lsl 1 in
    let h2negintopos = h2 lsl 1 in
    let pos_zero =
      (* +0.0 is present if +0.0 is present on one side and any zero
         on the other side. All computations in the positive_zero bit *)
      let has_any_zero1 = h1 lor h1negintopos in
      let has_any_zero2 = h2 lor h2negintopos in
      ((h1 land has_any_zero2) lor (h2 land has_any_zero1)) land positive_zero
    in
    let neg_zero =
      (* -0.0 is present if -0.0 is present on both sides *)
      h1 land h2 land negative_zero
    in
    let nan =
      (* NaN is present for +inf on one side and -inf on the other.
         Compute in the positive_inf bit. *)
      (h1 land h2negintopos) lor (h2 land h1negintopos)
    in
    (* Move to the at_least_one_NaN (1) bit: *)
    let nan = nan lsr 5 in
    (* Any NaN as operand? *)
    let nan = (nan lor h1 lor h2) land at_least_one_NaN in
    let nan = (- nan) land 3 in
    (* Compute both infinities in parallel.
       An infinity can arise from that infinity on one side and
       any finite or the same infinity on the other.*)
    let transfers_inf =
      negative_zero + positive_zero + negative_normalish + positive_normalish
    in
    (* Finites transfer all infinities, but if these are
    absent, h1 can only contribute to create the infinities it has. *)
    let h1_transfer = if h1 land transfers_inf = 0 then h1 else -1 in
    let h2_transfer = if h2 land transfers_inf = 0 then h2 else -1 in
    let infinities = (h1 land h2_transfer) lor (h2 land h1_transfer) in
    let infinities = infinities land (negative_inf lor positive_inf) in
    pos_zero lor neg_zero lor nan lor infinities

  let sub h1 h2 = add h1 (neg h2)

(* only to implement div from mult *)
  let inv h =
    let stay =
      at_least_one_NaN + all_NaNs +
        negative_normalish + positive_normalish
    in
    let stay = h land stay in
    let new_infs = (h lsr 2) land (positive_inf + negative_inf) in
    let new_zeroes = (h land (positive_inf + negative_inf)) lsl 2 in
    stay lor new_infs lor new_zeroes

  let mult h1 h2 =
    let has_finite1 = (h1 lsl 4) lor h1 in
    let has_finite2 = (h2 lsl 4) lor h2 in
    let same_signs12 = has_finite1 land h2 in
    let same_signs21 = has_finite2 land h1 in
    (* Compute in positive_zero whether two positive factors can result in
       +0, and in negative_zero whether two negative factors can: *)
    let same_signs = same_signs12 lor same_signs21 in
    (* Put the two possibilities together in positive_zero: *)
    let pos_zero = same_signs lor (same_signs lsl 1) in
    let pos_zero = pos_zero land positive_zero in

    (* Compute in negative_zero bit: *)
    let opposite_signs12 = (has_finite2 lsr 1) land h1 in
    let opposite_signs21 = (has_finite1 lsr 1) land h2 in
    let opposite_signs = opposite_signs12 lor opposite_signs21 in
    let neg_zero = opposite_signs land negative_zero in

    (* Compute in positive_zero and positive_inf bits: *)
    let merge_posneg1 = (h1 lsl 1) lor h1 in
    let merge_posneg2 = (h2 lsl 1) lor h2 in
    let nan12 = (merge_posneg1 lsl 2) land h2 in
    let nan21 = (merge_posneg2 lsl 2) land h1 in
    let nan = (nan12 lor nan21) land positive_zero in
    (* Map 128 to 3 and 0 to 0: *)
    let nan = (- nan) lsr (Sys.word_size - 1 - 2) in

    (* compute in the infinities bits: *)
    let has_nonzero1 = (h1 lsl 2) lor h1 in
    let has_nonzero2 = (h2 lsl 2) lor h2 in

    (* +inf is obtained by multiplying nonzero and inf of the same sign: *)
    let pos_inf12 = has_nonzero1 land h2 in
    let pos_inf21 = has_nonzero2 land h1 in
    let pos_inf = pos_inf12 lor pos_inf21 in
    let pos_inf = pos_inf lor (pos_inf lsl 1) in
    let pos_inf = pos_inf land positive_inf in

    (* compute in the negative_inf bit: *)
    let neg_inf12 = (has_nonzero1 lsr 1) land h2 in
    let neg_inf21 = (has_nonzero2 lsr 1) land h1 in
    let neg_inf = neg_inf12 lor neg_inf21 in
    let neg_inf = neg_inf land negative_inf in

    neg_inf lor pos_inf lor neg_zero lor pos_zero lor nan

  let div h1 h2 = mult h1 (inv h2)
end

(*
If negative_normalish, the negative bounds are always at t.(1) and t.(2)

If positive_normalish, the positive bounds are always at:
let l = Array.length t in t.(l-2) and t.(l-1)

Each pair of bounds of a same sign is represented as -lower_bound, upper_bound.
*)

let set_neg_lower a f =
  assert(neg_infinity < f);
  assert(f < 0.0);
  a.(1) <- -. f
let set_neg_upper a f =
  assert(neg_infinity < f);
  assert(f < 0.0);
  a.(2) <- f
let set_neg a fl fu =
  assert(neg_infinity < fl);
  assert(fl <= fu);
  assert(fu < 0.0);
  a.(1) <- -. fl;
  a.(2) <- fu
let set_pos_lower a f =
  assert(0.0 < f);
  assert(f < infinity);
  a.(Array.length a - 2) <- -. f
let set_pos_upper a f =
  assert(0.0 < f);
  assert(f < infinity);
  a.(Array.length a - 1) <- f
let set_pos a fl fu =
  assert(0.0 < fl);
  assert(fl <= fu);
  assert(fu < infinity);
  let l = Array.length a in
  a.(l - 2) <- -. fl;
  a.(l - 1) <- fu

let get_opp_neg_lower a = a.(1)
let get_neg_upper a = a.(2)
let get_opp_pos_lower a = a.(Array.length a - 2)
let get_pos_upper a = a.(Array.length a - 1)

(* [get_finite_upper a] returns the highest finite value contained in [a],
   or [neg_infinity] if [a] contains no finite values *)
let get_finite_upper a =
  let h = Header.of_abstract_float a in
  if Header.(test h positive_normalish)
  then get_pos_upper a
  else if Header.(test h positive_zero)
  then 0.0
  else if Header.(test h negative_zero)
  then (-0.0)
  else if Header.(test h negative_normalish)
  then get_neg_upper a
  else neg_infinity

(* [get_opp_finite_lower a] returns the opposite of the lowest finite value
   contained in [a],  or [neg_infinity] if [a] contains no finite values *)
let get_opp_finite_lower a =
  let h = Header.of_abstract_float a in
  if Header.(test h negative_normalish)
  then get_opp_neg_lower a
  else if Header.(test h negative_zero)
  then 0.0
  else if Header.(test h positive_zero)
  then (-0.0)
  else if Header.(test h positive_normalish)
  then get_opp_pos_lower a
  else neg_infinity



(*
Examples for testing: [1.0 … 2.0], [-10.0 … -9.0]
*)

let onetwo =
  let header = Header.(of_flag positive_normalish) in
  let r = Header.allocate_abstract_float header in
  set_pos_lower r 1.0;
  set_pos_upper r 2.0;
  r

let minus_nineten =
  let header = Header.(of_flag negative_normalish) in
  let r = Header.allocate_abstract_float header in
  set_neg_lower r (-10.0);
  set_neg_upper r (-9.0);
  r

let inject_float f = Array.make 1 f

let inject_interval f1 f2 = assert false (* TODO *)

let is_singleton f = Array.length f = 1

let zero = inject_float 0.0
let neg_zero = inject_float (-0.0)
let abstract_infinity = inject_float infinity
let abstract_neg_infinity = inject_float neg_infinity
let abstract_all_NaNs =
  let header = Header.(set_all_NaNs bottom) in
  Header.allocate_abstract_float header

(* [normalize_zero_and_inf] allows [neg_u] (rep [pos_l]) to be -0.0 (resp +0.0),
   and [neg_l] (resp [pos_u]) to be infinite.
   [normalize_zero_and_inf] converts these values to flags in the header. *)
let normalize_zero_and_inf zero_for_negative header neg_l neg_u pos_l pos_u =
  let neg_u, header =
    if neg_u = 0.0
    then -4.94e-324,  (* smallest magnitude subnormal *)
      Header.set_flag header zero_for_negative
    else neg_u, header
  in
  let pos_l, header =
    if pos_l = 0.0
    then +4.94e-324,  (* smallest magnitude subnormal *)
      Header.(set_flag header positive_zero)
    else pos_l, header
  in
  let neg_l, header =
    if neg_l = neg_infinity
    then -1.79769313486231571e+308, (* -. max_float *)
      Header.(set_flag header negative_inf)
    else neg_l, header
  in
  let pos_u, header =
    if pos_u = infinity
    then +1.79769313486231571e+308, (* max_float *)
      Header.(set_flag header positive_inf)
    else pos_u, header
  in
  header, neg_l, neg_u, pos_l, pos_u

(* When zero appears as the sum of two nonzero values, it's always +0.0 *)
let normalize_for_add = normalize_zero_and_inf Header.positive_zero

(* Zeroes from multiplication underflow have the sign of the rule of signs *)
let normalize_for_mult = normalize_zero_and_inf Header.negative_zero

(** [inject] creates an abstract float from a header indicating the presence
    of zeroes, infinies and NaNs and two pairs of normalish bounds
    that capture negative values and positive values. *)
let inject header neg_l neg_u pos_l pos_u =
(* Check for singleton: *)
  let no_neg = neg_l > neg_u in
  let header =
    if no_neg
    then header
    else Header.(set_flag header negative_normalish)
  in
  let no_pos = pos_l > pos_u in
  let header =
    if no_pos
    then header
    else Header.(set_flag header positive_normalish)
  in
  let b = Header.is_bottom header in
    if b && neg_l = neg_u && no_pos
    then inject_float neg_l
    else
      if b && no_neg && pos_l = pos_u
      then inject_float pos_l
      else
        let no_outside_header = no_pos && no_neg
        in
        if no_outside_header && Header.(is_exactly header positive_zero)
        then zero
        else if no_outside_header && Header.(is_exactly header negative_zero)
        then neg_zero
        else if no_outside_header && Header.(is_exactly header positive_inf)
        then abstract_infinity
        else if no_outside_header && Header.(is_exactly header negative_inf)
        then abstract_neg_infinity
        else
(* Allocate result: *)
          let r = Header.allocate_abstract_float header in
          if not no_neg
          then set_neg r neg_l neg_u;
          if not no_pos
          then set_pos r pos_l pos_u;
          r

(* pretty-printing *)
let pretty fmt a =
  let h = Header.of_abstract_float a in
  Header.pretty fmt h;
  if Header.exactly_one_NaN h
  then assert false;
  let l = Array.length a in
  if l >= 3 then
    Format.fprintf fmt " U [%f..%f]" (~-. (a.(1))) a.(2);
  if l = 5 then
    Format.fprintf fmt " U [%f..%f]" (~-. (a.(3))) a.(4)

(* *** Set operations *** *)

(* Every element of a1 is an element of a2. *)
let is_included a1 a2 = assert false

(* [join a1 a2] is the smallest abstract state that contains every
   element from [a1] and every element from [a2]. *)
let join a1 a2 = assert false

let meet a1 a2 = assert false

(* [intersects a1 a2] is true iff there exists a float that is both in [a1]
   and in [a2]. *)
let intersects a1 a2 = assert false

(* *** Arithmetic *** *)

(* [expand a] returns the non-singleton representation corresponding
   to a singleton [a].  Never let expanded forms escape outside a
   short computations!  The representation for a same set of floats
   should be unique.  The single-float representation is efficient and
   is all that outside code using the library should see.
*)
let expand a =
  let a = a.(0) in
  if a = infinity then abstract_infinity
  else if a = neg_infinity then abstract_neg_infinity
  else if a <> a then abstract_all_NaNs
  else
    let repr = Int64.bits_of_float a in
    if repr = 0L then zero
    else if repr = 0x8000000000000000L then neg_zero
    else
      let flag =
        if a < 0.0 then Header.negative_normalish else Header.positive_normalish
      in
      let r = Header.(allocate_abstract_float (of_flag flag)) in
      r.(1) <- -. a;
      r.(2) <- a;
      r

let add_expanded a1 a2 =
  let header1 = Header.of_abstract_float a1 in
  let header2 = Header.of_abstract_float a2 in
  let header = Header.add header1 header2 in
  (* After getting the contributions to the result arising from the
     header bits of the operands, the "expanded" versions of binary
     operations need to compute the contributions resulting from the
     (sub)normal parts of the operands.  Usually these contributions are
     (sub)normal intervals, but operations on (sub)normal values can always
     underflow to zero or overflow to infinity. *)

    (* One constraint: always compute so that if the rounding mode
       was upwards, then it would make the sets larger.
       This means computing the positive upper bound as a positive
       number, as well as the negative lower bound.
       The positive lower bound and the negative upper bound must
       be computed as negative numbers, so that if rounding were upwards,
       they would end up closer to 0, making the sets larger. *)
  let opp_neg_l = get_opp_finite_lower a1 +. get_opp_finite_lower a2 in
  let neg_u = -0.001 (* assert false TODO obj_magic *) in
  let opp_pos_l = -0.001 in (* TODO obj_magic *)
  let pos_u = get_finite_upper a1 +. get_finite_upper a2 in

  (* First, normalize. What may not look like a singleton before normalization
     may turn out to be one afterwards: *)
  let header, neg_l, neg_u, pos_l, pos_u =
    normalize_for_add header (-. opp_neg_l) neg_u (-. opp_pos_l) pos_u
  in
  inject header neg_l neg_u pos_l pos_u

(* Generic second-order function that handles the singleton case
   and applies the provided algorithm to the expanded arguments
   otherwise *)
let binop scalar_op expanded_op a1 a2 =
  let single_a1 = is_singleton a1 in
  let single_a2 = is_singleton a2 in
  if single_a1 && single_a2
  then
    let result = scalar_op a1.(0) a2.(0) in
    if result <> result (* NaN *)
    then abstract_all_NaNs
    else inject_float result
  else
    let a1 = if single_a1 then expand a1 else a1 in
    let a2 = if single_a2 then expand a2 else a2 in
    expanded_op a1 a2

(** [add a1 a2] returns the set of values that can be taken by adding a value
   from [a1] to a value from [a2]. *)
let add a1 a2 = binop (+.) add_expanded

let sub_expanded a1 a2 =
  let header1 = Header.of_abstract_float a1 in
  let header2 = Header.of_abstract_float a2 in
  let header = Header.sub header1 header2 in

  let opp_neg_l = assert false in
  let neg_u = assert false in
  let opp_pos_l = assert false in
  let pos_u = assert false in

  (* First, normalize. What may not look like a singleton before normalization
     may turn out to be one afterwards: *)
  let header, neg_l, neg_u, pos_l, pos_u =
    normalize_for_add header (-. opp_neg_l) neg_u (-. opp_pos_l) pos_u
  in
  inject header neg_l neg_u pos_l pos_u

let sub a1 a2 = binop (-.) sub_expanded

let mult_expanded a1 a2 =
  let header1 = Header.of_abstract_float a1 in
  let header2 = Header.of_abstract_float a2 in
  let header = Header.mult header1 header2 in
  let opp_neg_l = assert false in
  let neg_u = assert false in
  let opp_pos_l = assert false in
  let pos_u = assert false in

  (* First, normalize. What may not look like a singleton before normalization
     may turn out to be one afterwards: *)
  let header, neg_l, neg_u, pos_l, pos_u =
    normalize_for_mult header (-. opp_neg_l) neg_u (-. opp_pos_l) pos_u
  in
  inject header neg_l neg_u pos_l pos_u

(** [mult a1 a2] returns the set of values that can be taken by multiplying
    a value from [a1] with a value from [a2]. *)
let mult a1 a2 = binop ( *. ) mult_expanded


let div_expanded a1 a2 =
  let header1 = Header.of_abstract_float a1 in
  let header2 = Header.of_abstract_float a2 in
  let header = Header.div header1 header2 in
  let opp_neg_l = assert false in
  let neg_u = assert false in
  let opp_pos_l = assert false in
  let pos_u = assert false in

  (* First, normalize. What may not look like a singleton before normalization
     may turn out to be one afterwards: *)
  let header, neg_l, neg_u, pos_l, pos_u =
    normalize_for_mult header (-. opp_neg_l) neg_u (-. opp_pos_l) pos_u
  in
  inject header neg_l neg_u pos_l pos_u

(** [div a1 a2] returns the set of values that can be taken by dividing
    a value from [a1] by a value from [a2]. *)
let div a1 a2 = binop ( /. ) div_expanded
