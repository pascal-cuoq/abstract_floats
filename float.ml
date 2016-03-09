let phys_equal = ( == )
let (!=) = `Use_phys_equal

type abstract_float = float array

(*
  type [abstract_float] is represented using an array of (unboxed) floats.

  array [t] with length 1:
    a single floating-point number
    (can be NaN, +inf, -inf, or a finite value)

  array [t] with length >=2:
    header + bounds. the first field is header. the rest of fields are bounds.
    the length of t can only be 2, 3 or 5.

    length of 2:
      only intended to distinguish a header from a single
      floating-point number. this means such array [a] has the
      field a.(1) containing garbage data we don't care.

    length of 5:
      the FP number could be both pos normalish and neg
      normalish. the last four fields indicating two pairs of bounds
      (first neg bounds, then pos bounds).

    length of 3:
      the fp number could be either pos normalish or neg normalish,
       rest two fields indicating one pair of bounds.

  the header (found in t.(0)) can indicate:

    at least one of the NaN values present
    all NaN values present
    FP number can be in negative normalish range
    FP number can be in positive normalish range
    -inf present
    +inf present
    -0.0 present
    +0.0 present

  vocabulary:

  - normalish means the intervals of normal (or subnormal) values
  - finite means the normalish and zero components of the representation
  - nonzero means the normalish and infinite components, but usually not NaN
*)


let sign_bit = 0x8000_0000_0000_0000L

let payload_mask = 0x800F_FFFF_FFFF_FFFFL

let header_mask  = 0x0FF0_0000_0000_0000L

let is_pos f = Int64.(logand (bits_of_float f) sign_bit) = 0L

let is_neg f = Int64.(logand (bits_of_float f) sign_bit) <> 0L

let is_NaN f = classify_float f = FP_nan

let is_zero f = classify_float f = FP_zero

let is_inf f = classify_float f = FP_infinite

let is_pos_zero f = Int64.bits_of_float f = 0L

let is_neg_zero f = Int64.bits_of_float f = sign_bit

exception Invalid_abstract_float_length of int
exception Fetal_error_when_allocating_abstract_float

(*

  *********************************************************************
  *                                                                   *
  *                         Internal layout                           *
  *                                                                   *
  *********************************************************************

                          *******************
                          *     Header.t    *
                          *******************

                     From left to right: bits 0 - 7

      |----------------------------------- positive_zero
      |
      |   |------------------------------- negative_zero
      |   |
      |   |   |--------------------------- positive_inf
      |   |   |
      |   |   |   |----------------------- negative_inf
      |   |   |   |
      |   |   |   |
    +---+---+---+---+---+---+---+---+
    | h | h | h | h | h | h | h | h |
    +---+---+---+---+---+---+---+---+
                      |   |   |   |
                      |   |   |   |
                      |   |   |   |------- at_least_one_NaN
                      |   |   |
                      |   |   |----------- all_NaN (both quiet and signalling)
                      |   |
                      |   |--------------- negative_normalish
                      |
                      |------------------- positive_normalish


  Notes:
    1. three possibilities of NaN are encoded:
        1) no NaN is present
        2) at least one NaN is present
        3) both NaNs are present

  *********************************************************************

                         *************************
                         *   abstract_float.(0)  *
                         *************************


     NaN sign bit (1 bit)
       |
       | Unused (3 bits)
       |  /         \
     | s | 0 | 0 | 0 | h | h | h | … | h | h | p | p | p | … | p |
       |              \                     / \                 /
       |               \                   /   \   (52 bits)   /
       |                 Header.t (8 bits)      \             /
       |                                         \           /
       +-----------------------------------------  NaN payload
                                                   (optional)

  Notes:
   1. the NaN payload is a NaN's significand and sign bit. This is
      required only when [at_least_one_NaN] flag is set in [Header.t]

*)

module Header : sig
  type t
  (** abstract type for header *)

  type flag
  (** abstract flag indicating property of abstract float *)

  val at_least_one_NaN : flag
  (** [at_least_one_NaN] is flag indicating at least one of NaN values
      is present, either quiet NaN, or signalling NaN.
      When this flag is on, payload should be set *)

  val all_NaNs : flag
  (** [at_least_one_NaN] is flag indicating at least one of NaN values
      is present, either quiet NaN, or signalling NaN *)

  val negative_normalish : flag
  (** [negative_normalish] is flag indicating floating-number could be in
      negative normalish range *)

  val positive_normalish : flag
  (** [positive_normalish] is flag indicating floating-number could be in
      positive normalish range *)

  val negative_inf : flag
  (** [negative_inf] is flag indicating floating-number could be
      negative infinity *)

  val positive_inf : flag
  (** [positive_inf] is flag indicating floating-number could be
      positive infinity *)

  val negative_zero : flag
  (** [negative_zero] is flag indicating floating-number could be in
      negative zero: -0.0 *)

  val positive_zero : flag
  (** [positive_zero] is flag indicating floating-number could be in
      positive zero: +0.0 *)

  val bottom : t
  (** [bottom] is header with no flag on *)

  val is_bottom : t -> bool
  (** [is_bottom t] indicates whether [t] is bottom *)

  val pretty: Format.formatter -> t -> unit
  (** [pretty fmt t] pretty-prints [t] on [fmt] *)

  val combine : t -> t -> t
  (** [combine t1 t2] is the join of [t1] and [t2] *)

  val test : t -> flag -> bool
  (** [test t f] indicates whether [f] is set in [t] *)

  val set_flag : t -> flag -> t
  (** [set t f] is [t] with flag [f] set *)

  val flag_of_float : float -> flag
  (** [flag_of_float f] is flag that could be set by [f].
      [flag_of_float nan] is [at_least_one_NaN] *)

  val of_flag : flag -> t
  (** [of_flag f] is a header with flag [t] set *)

  val set_all_NaNs : t -> t
  (** [set_all_NaNs h] is header [h] with [all_NaNs] flag set *)

  val exactly_one_NaN : t -> bool
  (** [exactly_one_NaN t f] indicates whether [f] contains at least one NaN *)

  val is_exactly : t-> flag -> bool
  (** [is_exactly h f] indicates if [t] has only [f] on *)

  val size : t -> int
  (** [size h] is the length of abstract float corresponding to the given
      header. Note that the header alone is not enough to decide that the
      representation should be a single float. Hence this function always
      returns at least 2. *)

  val of_abstract_float : abstract_float -> t
  (** [of_abstract_float a] is the header of the abstract float [a].
       Note: the abstract float [a] has to have size >= 2. In other words,
      [a] cannot be a singleton floating point number *)

  val allocate_abstract_float : t -> abstract_float
  (** [allocate_abstract_float h] is an abstract float of size indicated by
      [h], and fields correctly set according to [h] *)

  val allocate_abstract_float_with_NaN : t -> float -> abstract_float
  (** [allocate_abstract_float_with_NaN h f] is an abstract float with header
    indicating at least one NaN. [f], which is expected to be a NaN value,
    is used to set the payload of the result abstract float *)

  val reconstruct_NaN : abstract_float -> int64 option
  (** [reconstruct_NaN a] is potential payload of [a].
      The result is [None] if [a]'s header does not
      indicate [at_least_one_NaN] *)

  val is_header_included : abstract_float -> abstract_float -> bool
  (** [is_header_included a1 a2] is true if [a2]'s header has all the
      information [a1]'s header has *)

  val join : t -> t -> abstract_float
  (** [join h1 h2] is an abstract float with header properly set by
      [Header.combine] *)

  val sqrt: t -> t
  (** [sqrt h] is the header of abstract float with header [h] after square
      root operation *)

  val add: t -> t -> t
  (** [add h1 h2] is the header of the result abstract float of addition
      arithmetic operation of two abstract floats of headers [h1] and [h2] *)

  val sub: t -> t -> t
  (** [sub h1 h2] is the header of the result abstract float of subtraction
        arithmetic operation of two abstract floats of headers [h1] and [h2] *)

  val mult: t -> t -> t
  (** [mult h1 h2] is the header of the result abstract float of multiplication
        arithmetic operation of two abstract floats of headers [h1] and [h2] *)

  val div: t -> t -> t
  (** [div h1 h2] is the header of the result abstract float of division
        arithmetic operation of two abstract floats of headers [h1] and [h2] *)

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

  let combine h1 h2 = h1 lor h2
  let test h flag = h land flag <> 0
  let test_both h1 h2 flag = h1 land h2 land flag <> 0
  let set_flag = combine
  let of_flag f = f

  let flag_of_float f =
    match classify_float f with
    | FP_zero -> if is_pos_zero f then positive_zero else negative_zero
    | FP_normal | FP_subnormal ->
      if is_pos f then positive_normalish else negative_normalish
    | FP_infinite ->
      if is_pos f then positive_inf else negative_inf
    | _ -> at_least_one_NaN

  let set_all_NaNs h = h lor (at_least_one_NaN + all_NaNs)
  let get_NaN_part h = h land (at_least_one_NaN + all_NaNs)
  let exactly_one_NaN h = (get_NaN_part h) = at_least_one_NaN

  let pretty fmt h =
    let bottom = is_bottom h in
    if bottom then
      Format.fprintf fmt "Bottom"
    else
      let pp_tuple_list fmt l =
        let ml = List.fold_left
            (fun m t -> max m (String.length (fst t))) 0 l in
        let pad s = String.make (ml + 1 - String.length s) ' ' in
        let rec loop fmt = function
          | [] -> ()
          | (s, b) :: t ->
            Format.fprintf fmt "%s%s: %B@,%a" s (pad s) b loop t in
        loop fmt l in
      let tl =
        ["At least one NaN",   test h at_least_one_NaN;
         "All NaNs",           test h all_NaNs;
         "Negative normalish", test h negative_normalish;
         "Positive normalish", test h positive_normalish;
         "Negative infinity",  test h negative_inf;
         "Positive infinity",  test h positive_inf;
         "Negative zero",      test h negative_zero;
         "Positive zero",      test h positive_zero] in
      Format.fprintf fmt "@[<v>%a@]@." pp_tuple_list tl

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

  let allocate_abstract_float_with_NaN h f =
    match classify_float f with
    | FP_nan -> begin
        let payload =
          Int64.(logand (bits_of_float f) (0x800FFFFFFFFFFFFFL)) in
        let with_flags =
          Int64.(float_of_bits (logor (of_int (h lsl 52)) payload)) in
        Array.make (size h) with_flags
      end
    | _ -> assert false

  let reconstruct_NaN a =
    (assert (Array.length a >= 2));
    if test (of_abstract_float a) at_least_one_NaN then
      Some (Int64.(logand 0x800FFFFFFFFFFFFFL (bits_of_float a.(0))))
    else
      None

  let is_header_included a1 a2 =
    assert (Array.length a1 >= 2);
    assert (Array.length a2 >= 2);
    let h1, h2 = of_abstract_float a1, of_abstract_float a2 in
    let potential_NaN_exists =
      match reconstruct_NaN a1, reconstruct_NaN a2 with
      | Some n1, Some n2 -> n1 = n2
      | Some n1, None when test h2 all_NaNs -> true
      | (None, _) ->
        if test h1 all_NaNs then test h2 all_NaNs else true
      | _, _ -> false in
    (h2 lor h1 = h2) && potential_NaN_exists

  let join h1 h2 =
    let h = combine h1 h2 in
    allocate_abstract_float h

  (* sqrt(-0.) = -0., sqrt(+0.) = +0., sqrt(+inf) = +inf *)
  let sqrt h = assert false

  (* only to implement sub from add *)
  let neg h =
    let neg = h land (negative_zero + negative_inf + negative_normalish) in
    let pos = h land (positive_zero + positive_inf + positive_normalish) in
    (get_NaN_part h) lor (neg lsl 1) lor (pos lsr 1)

  let add h1 h2 =
    let h1negintopos = h1 lsl 1 in
    let h2negintopos = h2 lsl 1 in
    let pos_zero =
      (* +0.0 is present if +0.0 is present in one operand and any zero
         in the other. All computations in the positive_zero bit. HFP 3.1.4 *)
      let has_any_zero1 = h1 lor h1negintopos in
      let has_any_zero2 = h2 lor h2negintopos in
      ((h1 land has_any_zero2) lor (h2 land has_any_zero1)) land positive_zero
    in
    let neg_zero =
      (* -0.0 is present in result if -0.0 is present
         in both operands. HFP 3.1.4 *)
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
       An infinity can arise from that infinity in one operand and
       any finite value or the same infinity in the other.*)
    let transfers_inf =
      negative_zero + positive_zero + negative_normalish + positive_normalish
    in
    (* Finite values transfer all infinities, but if finite values are
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

(** [copy_payload a1 a2] will copy potential payload of [a1] to [a2] *)
let copy_payload a1 a2 =
  assert (Array.length a1 >= 2);
  assert (Array.length a2 >= 2);
  let potential_payload = Int64.(logand (bits_of_float a1.(0)) payload_mask) in
  a2.(0) <- Int64.(float_of_bits
                     (logor (bits_of_float a2.(0)) potential_payload))

(** [copy_bounds a1 a2] will copy bounds of [a1] to the freshly
    allocated [a2] *)
let copy_bounds a1 a2 =
  assert (Array.length a1 >= 2);
  assert (Array.length a2 >= 2);
  match Array.length a1, Array.length a2 with
  | 2, _ -> ()
  | 3, ((3 | 5) as l) -> begin
    if Header.(test (of_abstract_float a1) positive_normalish) then
      (a2.(l - 2) <- a1.(1); a2.(l - 1) <- a1.(2))
    else
      (a2.(1) <- a1.(1); a2.(2) <- a1.(2))
    end
  | 5, 5 ->
    for i = 1 to 4 do
      a2.(i) <- a1.(i)
    done
  | _ -> assert false

(** [erase_payload a] will erase potential payload in [a] *)
let erase_payload a =
  assert (Array.length a >= 2);
  a.(0) <- Int64.(float_of_bits (logand (bits_of_float a.(0)) header_mask))

(** [subst_header a h] is a freshly allocated abstract float that
    is the same as [a], but with a new header [h] set. Potential payload
    is preserved *)
let subst_header (a: abstract_float) (h: Header.t) =
  assert (Array.length a >= 2);
  let a = Array.copy a in
  let potential_payload = Int64.(logand (bits_of_float a.(0)) payload_mask) in
  (if Header.(test h all_NaNs) then
     (** if all_NaN flag is on, potential payload is erased *)
     a.(0) <- (Header.allocate_abstract_float h).(0)
   else begin
     let cell =
        Int64.(logor
                 (bits_of_float (Header.allocate_abstract_float h).(0))
                 potential_payload) in
     a.(0) <- Int64.float_of_bits cell
   end);
  a

(** [subst_header_with_nan a f h] is a freshly allocated abstract float that
    is the same as [a], but with a new header [h] and payload from [f] *)
let subst_header_with_NaN (a: abstract_float) (fNaN: float) (h: Header.t) =
  assert (Array.length a >= 2);
  let a = Array.copy a in
  a.(0) <- (Header.allocate_abstract_float_with_NaN h fNaN).(0);
  a

(** [set_neg_lower a f] sets lower bound of negative normalish to [-. f] *)
let set_neg_lower a f =
  assert(neg_infinity < f);
  assert(f < 0.0);
  a.(1) <- -. f

(** [set_neg_upper a f] sets upper bound of negative normalish to [f] *)
let set_neg_upper a f =
  assert(neg_infinity < f);
  assert(f < 0.0);
  a.(2) <- f

(** [set_neg a fl fu] sets bounds of negative normalish to [fl] and [fu] *)
let set_neg a l u =
  assert(neg_infinity < l);
  assert(l <= u);
  assert(u < 0.0);
  a.(1) <- -. l;
  a.(2) <- u

(** [set_neg_lower a f] sets lower bound of positive normalish to [-. f] *)
let set_pos_lower a f =
  assert(0.0 < f);
  assert(f < infinity);
  a.(Array.length a - 2) <- -. f

(** [set_pos_upper a f] sets upper bound of positive normalish to [f] *)
let set_pos_upper a f =
  assert(0.0 < f);
  assert(f < infinity);
  a.(Array.length a - 1) <- f

(** [set_pos a fl fu] sets bound of positive normalish to [fl] and [fu] *)
let set_pos a l u =
  assert(0.0 < l);
  assert(l <= u);
  assert(u < infinity);
  let le = Array.length a in
  a.(le - 2) <- -. l;
  a.(le - 1) <- u

(* [get_opp_neg_lower a] is the lower neg bonud of [a], in negative *)
let get_opp_neg_lower a : float = a.(1)

(* [get_neg_upper a] is the upper neg bonud of [a] *)
let get_neg_upper a : float = a.(2)

(* [get_opp_pos_lower a] is the upper neg bonud of [a], in negative *)
let get_opp_pos_lower a : float = a.(Array.length a - 2)

(* [get_pos_upper a] is the upper pos bonud of [a] *)
let get_pos_upper a : float = a.(Array.length a - 1)

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

(* [set_same_buond a f] sets pos or neg bound of [a] to [f].
   [a] is expected to have size 3 (one header and one pair of bound *)
let set_same_bound a f : unit =
  assert (let c = classify_float f in c = FP_normal || c = FP_subnormal);
  assert (Array.length a = 3);
  a.(1) <- -. f;
  a.(2) <- f

(* [insert_pos_in_bounds a f] inserts a positive
   float [f] in positive bounds in [a] *)
let insert_pos_in_bounds a f : unit =
  assert (let c = classify_float f in c = FP_normal || c = FP_subnormal);
  assert (is_pos f);
  assert (Array.length a >= 3);
  set_pos_lower a (min (-.(get_opp_pos_lower a)) f);
  set_pos_upper a (max (get_pos_upper a) f)

(* [insert_neg_in_bounds a f] inserts a negative
   float [f] in negative bounds in [a] *)
let insert_neg_in_bounds a f : unit =
  assert (let c = classify_float f in c = FP_normal || c = FP_subnormal);
  assert (is_neg f);
  assert (Array.length a >= 3);
  set_neg_lower a (min (-.(get_opp_neg_lower a)) f);
  set_neg_upper a (max (get_neg_upper a) f)

(* [insert_in_bounds] inserts a float in  *)
let insert_float_in_bounds a f : unit =
  assert (let c = classify_float f in c = FP_normal || c = FP_subnormal);
  assert (Array.length a >= 3);
  if is_neg f then
    insert_neg_in_bounds a f
  else
    insert_pos_in_bounds a f

(* [insert_all_bounds a1 a2] inserts all bounds in [a1] to [a2] *)
let insert_all_bounds a1 a2 : unit =
  assert (Array.length a1 >= 3);
  for i = 1 to (Array.length a1 - 1) do
    insert_float_in_bounds a2 (if i mod 2 = 1 then (-.a1.(i)) else a1.(i))
  done

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

(* RH: want better naming... *)
let inject_float f = Array.make 1 f

let inject_interval f1 f2 = assert false (* TODO *)

let is_singleton f = Array.length f = 1

let zero = inject_float 0.0
let neg_zero = inject_float (-0.0)
let abstract_infinity = inject_float infinity
let abstract_neg_infinity = inject_float neg_infinity
let abstract_all_NaNs = Header.(allocate_abstract_float (set_all_NaNs bottom))

(*
(** [reconstruct_NaN a] is the NaN reconstructred from payload of [a]
    if [a] has header with [at_least_one_NaN] on *)
let reconstruct_NaN a =
  (assert (Array.length a >= 2));
  (assert (Header.(test (of_abstract_float a) at_least_one_NaN)));
  Int64.(logand 0x800FFFFFFFFFFFFFL (bits_of_float a.(0)))
*)

let set_header_from_float f h =
  match classify_float f with
  | FP_nan -> invalid_arg "cannot handle NaN values: payload of NaN cannot be
                properly set using Header.set_flag"
  | _ -> Header.(set_flag h (flag_of_float f))

(** [merge_float a f] is a freshly allocated abstract float, which is
    of the result of the merge of [f] and [a] *)
let merge_float a f =
  assert (Array.length a >= 2);
  let h = Header.of_abstract_float a in
  match classify_float f with
  (* singleton is zero (pos and neg) or infinity (pos and neg) *)
  | (FP_zero | FP_infinite) ->
    let h = Header.(set_flag h (flag_of_float f)) in
    subst_header a h
  (* singelton is NaN. Potential NaN value is reconstructed from
     abstract float to determine if singleton is another kind of
     NaN. If yes, [all_NaNs] flag is set in abstract float's header.
     Cases:
       Header with at_least_one_NaN when payload same -> do nothing
       Header with at_least_one_NaN when payload diff -> set all_NaNs flag
       Header with all_NaNs -> do nothing
       Others -> set payload, set at_least_one_NaN flag
   *)
  | FP_nan -> begin
      match Header.reconstruct_NaN a with
      | None -> if Header.(test h all_NaNs) then a else
          subst_header_with_NaN a f (Header.(set_flag h at_least_one_NaN))
      | Some n ->
        if n = Int64.(logand (bits_of_float f) payload_mask) then a else
          subst_header a (Header.set_all_NaNs h)
    end
  (* singleton is normalish. A freshly abstract float will be allocated
     based on [a]. New header and original potential payload are set. *)
  | _ -> begin
      let h = Header.(set_flag h (flag_of_float f)) in
      assert (Header.size h = 3 || Header.size h = 5);
      let a' = Header.allocate_abstract_float h in
      copy_payload a a';
      copy_bounds a a';
      (insert_float_in_bounds a' f; a')
    end

(* [normalize_zero_and_inf] allows [neg_u] (rep [pos_l]) to be -0.0 (resp +0.0),
   and [neg_l] (resp [pos_u]) to be infinite.
   [normalize_zero_and_inf] converts these values to flags in the header.
    [normalize_zero_and_inf] is used to move the the header the
   zeroes and infinities created by underflow and overflow *)
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
    that capture negative values and positive values.
    Normal bound, not inverted.
    RH: is the result abstract_float always of size 5?
    PC: no, [Header.allocate_abstract_float header] allocates it of the
    right size, and only the bounds that exist are written to it. *)
let inject header neg_l neg_u pos_l pos_u =
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

(* [pretty fmt a] pretty-prints [a] on [fmt] *)
let pretty fmt a =
  let h = Header.of_abstract_float a in
  Header.pretty fmt h;
  if Header.exactly_one_NaN h
  then assert false;
  let l = Array.length a in
  if l >= 3 then
    Format.fprintf fmt "%s range [%f ... %f]\n"
      (if Header.(test h negative_normalish) then "Negative" else "Positive")
      (~-. (a.(1))) a.(2);
  if l = 5 then
    Format.fprintf fmt "Positive range [%f ... %f]\n"
      (~-. (a.(3))) a.(4)

(* *** Set operations *** *)
(* [compare a1 a2] is the order of [a1] and [a2] *)
let compare a1 a2 =
  let length  = Array.length a1 in
  let length2 = Array.length a2 in
  let d = length - length2 in
  if d <> 0
  then d
  else
    let h1 = Int64.bits_of_float a1.(0) in
    let h2 = Int64.bits_of_float a2.(0) in
    if h1 > h2 then 1
    else if h1 < h2 then -1
    else
      if length < 3 then 0
      else
        let c = compare a1.(1) a2.(1) in
        if c <> 0 then c
        else
          let c = compare a1.(2) a2.(2) in
          if c <> 0 then c
          else
            if length < 5 then 0
            else
              let c = compare a1.(3) a2.(3) in
              if c <> 0 then c
              else compare a1.(4) a2.(4)

let equal a1 a2 = compare a1 a2 = 0

(* [float_in_abstract_float f a] indicates if [f] is inside [a] *)
let float_in_abstract_float f a =
  match Array.length a with
  | 1 -> (is_NaN f && is_NaN a.(0) &&
          Int64.bits_of_float f = Int64.bits_of_float a.(0))
         || f = a.(0)
  | (2 | 3 | 5) as s -> begin
      let h = Header.of_abstract_float a in
      match classify_float f with
      | FP_zero ->
        (is_pos f && Header.(test h positive_zero)) ||
        (is_neg f && Header.(test h negative_zero))
      | FP_infinite ->
        (is_pos f && Header.(test h positive_inf)) ||
        (is_neg f && Header.(test h negative_inf))
      | FP_nan -> begin
          match Header.reconstruct_NaN a with
          | Some n -> f = Int64.(float_of_bits (logand (bits_of_float f) n))
          | None -> Header.(test h all_NaNs)
        end
      | FP_normal | FP_subnormal ->
        s > 2 && (
          (-.get_opp_neg_lower a) <= f && f <= (get_neg_upper a) ||
          (-.get_opp_pos_lower a) <= f && f <= (get_pos_upper a))
    end
  | _ -> assert false

(* [is_included a1 a2] is a boolean value indicating if every element in [a1]
   is also an element in [a2] *)
let is_included a1 a2 =
  if equal a1 a2 then true else begin
    let h1, h2 = Header.(of_abstract_float a1, of_abstract_float a2) in
    match Array.length a1, Array.length a2 with
    | 1, l2 -> float_in_abstract_float a1.(0) a2
    | 3, (1 | 2) | 5, (1 | 2 | 3) -> false
    | 2, 1 -> let f = a2.(0) in
      (Header.(is_exactly h1 at_least_one_NaN) &&
       classify_float f = FP_nan &&
       Header.reconstruct_NaN a1 =
        Some (Int64.(logand (bits_of_float f) payload_mask))) ||
      (Header.(is_exactly h1 negative_inf) && is_inf f && is_neg f) ||
      (Header.(is_exactly h1 positive_inf) && is_inf f && is_pos f) ||
      (Header.(is_exactly h1 negative_zero) && is_zero f && is_neg f) ||
      (Header.(is_exactly h1 positive_zero) && is_zero f && is_pos f)
    | 2, (2 | 3 | 5) -> Header.is_header_included a1 a2
    | 3, (3 | 5) | 5, 5 ->
      let b = ref (Header.is_header_included a1 a2) in
      for i = 1 to (Array.length a1 - 1) do
        b := !b && (float_in_abstract_float
                      (if i mod 2 = 1 then (-.a1.(i)) else a1.(i)) a2);
      done;
      !b
    | _, _ -> assert false
  end

(* [join a1 a2] is the smallest abstract state that contains every
   element from [a1] and every element from [a2]. *)
let join (a1:abstract_float) (a2: abstract_float) : abstract_float =
  if equal a1 a2
  then a1
  else begin
    match is_singleton a1, is_singleton a2, a1, a2 with
    | true, true, _, _ ->
    (* both [a1] and [a2] are singletons *)
      let f1, f2 = a1.(0), a2.(0) in
      ( match is_NaN f1, is_NaN f2, f1, f2 with
      | true, true, _, _ ->
        (* the representation of the two NaNs is different because
           the case [equal a1 a2] has been handled. *)
        abstract_all_NaNs
      | true, false, theNaN, nonNaN | false, true, nonNaN, theNaN ->
        (* one of the FP numbers is NaN *)
        let h = Header.(of_flag at_least_one_NaN) in
        let h = set_header_from_float nonNaN h in
        let a = Header.allocate_abstract_float_with_NaN h theNaN in
        if Header.size h <> 2
        then begin
          assert (Header.size h = 3);
          set_same_bound a nonNaN
        end;
        a
      | false, false, _, _ ->
        (* none of the FP numbers are NaN *)
        (* PC: I think this part can be made much more concise but I didn't
           touch it.
           RH: I tried to make more concise *)
        let h = set_header_from_float f1 Header.bottom in
        let h = set_header_from_float f2 h in
        let a = Header.allocate_abstract_float h in
        if Header.size h = 2 then a else begin
          match classify_float f1, classify_float f2, Header.size h with
          | FP_zero, FP_normal, 3 | FP_infinite, FP_normal, 3 ->
            (set_same_bound a f2; a)
          | FP_normal, FP_zero, 3 | FP_normal, FP_infinite, 3 ->
            (set_same_bound a f1; a)
          | (FP_normal | FP_subnormal), (FP_normal | FP_subnormal), h -> begin
            let f1, f2 = if f1 < f2 then f1, f2 else f2, f1 in
            match is_neg f1, f1, is_neg f2, f2, h with
            | true, _, true, _, 3 -> set_neg a f1 f2; a
            | false, _, false, _, 3 -> set_pos a f1 f2; a
            | true, f1, false, f2, 5 | false, f2, false, f1, 5 ->
              (set_neg a f1 f1; set_pos a f2 f2; a)
            | _, _, _, _, _ -> assert false
            end
          | _, _, _ -> assert false
        end)
    (* only one of [a1] and [a2] is singleton *)
    | true, false, single, non_single | false, true, non_single, single ->
      merge_float non_single single.(0)
    (* neither [a1] nor [a2] is singleton *)
    | false, false, _, _ ->
      let hn = Header.(combine (of_abstract_float a1) (of_abstract_float a2)) in
      let an = Header.allocate_abstract_float hn in begin
      (* assign payload in fresh abstract float *)
      match Header.(test (of_abstract_float a1) at_least_one_NaN),
            Header.(test hn at_least_one_NaN) with
      | true, true -> copy_payload a1 an
      | false, true -> copy_payload a2 an
      | _, _ -> () end; begin
      (* insert bounds from [a1] and [a2] to [an] *)
      match Array.length a1, a1, Array.length a2, a2, Array.length an with
      | 2, _, 2, _, 2 -> ()
      | 2, _, (3 | 5) , amore, _ | (3 | 5), amore, 2, _, _ ->
        copy_bounds amore an
      | 3, aless, 5, amore, 5 |
        5, amore, 3, aless, 5 |
        3, aless, 3, amore, 3 |
        5, aless, 5, amore, 5 -> begin
          copy_bounds amore an;
          insert_all_bounds aless an
        end
      | 3, aless, 3, amore, 5 -> begin
          copy_bounds aless an;
          copy_bounds amore an;
        end
      | _, _, _, _, _ -> assert false
      end;
      an
  end

module Test = struct

  let ppa a = pretty Format.std_formatter a

  let fNaN_1 = 0x7FF0000000000001L
  let fNaN_2 = 0x7FF0000000000002L

  let fNaN_3 = 0xFFF0000000000001L
  let fNaN_4 = 0xFFF7FFFFFFFFFFFFL

  let a_neg_1 =
    let h = Header.(set_flag bottom negative_normalish) in
    let a = Header.allocate_abstract_float h in
    set_neg_lower a (-5.0);
    set_neg_upper a (-1.0);
    a

  let a_neg_2 =
    let h = Header.(set_flag bottom negative_normalish) in
    let a = Header.allocate_abstract_float h in
    set_neg_lower a (-7.0);
    set_neg_upper a (-2.0);
    a

  let a_pos_1 =
    let h = Header.(set_flag bottom positive_normalish) in
    let a = Header.allocate_abstract_float h in
    set_pos_lower a (2.0);
    set_pos_upper a (5.0);
    a

  let a_pos_2 =
    let h = Header.(set_flag bottom positive_normalish) in
    let a = Header.allocate_abstract_float h in
    set_pos_lower a (3.0);
    set_pos_upper a (7.0);
    a

  let a_neg_pos =
    let h = Header.(set_flag bottom positive_normalish) in
    let h = Header.(set_flag h negative_normalish) in
    let a = Header.allocate_abstract_float h in
    set_neg_lower a (-7.0);
    set_neg_upper a (-2.0);
    set_pos_lower a (1.0);
    set_pos_upper a (11.0);
    a

  let test1 () =
    ppa (join a_neg_1 a_neg_2)

  let test2 () =
    ppa (join a_neg_pos a_pos_2)

  let test_include_1 () =
    assert (is_included a_pos_1 (join a_pos_1 a_pos_2));
    assert (is_included a_pos_2 (join a_pos_1 a_pos_2));
    assert (is_included a_neg_1 (join a_neg_1 a_pos_2));
    assert (is_included a_neg_1 (join a_neg_1 a_neg_2));
    assert (is_included a_neg_2 (join a_neg_1 a_neg_2));
    assert (is_included a_neg_2 (join a_pos_1 a_neg_2))

end

let () = Test.test_include_1 ()

(*
let () = TestJoin.test2 ()
*)


let meet a1 a2 = assert false

(* [intersects a1 a2] is true iff there exists a float that is both in [a1]
   and in [a2]. *)
let intersects a1 a2 = assert false

(* *** Arithmetic *** *)

(* negate() is a bitstring operation, even on NaNs. (IEEE 754-2008 6.3)
   and C99 says unary minus uses negate. Indirectly, anyway.
   @UINT_MIN https://twitter.com/UINT_MIN/status/702199094169604096 *)
let neg a =
  assert false

let abstract_sqrt a =
  if is_singleton a
  then begin
    let a = sqrt (a.(0)) in
    if a <> a
    then abstract_all_NaNs
    else inject_float a
  end
  else
    let h = Header.of_abstract_float a in
    let new_h = Header.sqrt h in
    if Header.(test h positive_normalish)
    then
      assert false
    else
      Header.allocate_abstract_float new_h

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
    else if repr = sign_bit then neg_zero
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
    let result = [| 0.0 |] in
    scalar_op result a1 a2;
    if result <> result (* NaN *)
    then abstract_all_NaNs
    else result
  else
    let a1 = if single_a1 then expand a1 else a1 in
    let a2 = if single_a2 then expand a2 else a2 in
    expanded_op a1 a2

(** [add a1 a2] returns the set of values that can be taken by adding a value
   from [a1] to a value from [a2]. *)
let add = binop (fun r a1 a2 -> r.(0) <- a1.(0) +. a2.(0)) add_expanded

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

let sub = binop (fun r a1 a2 -> r.(0) <- a1.(0) -. a2.(0)) sub_expanded

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
let mult = binop (fun r a1 a2 -> r.(0) <- a1.(0) *. a2.(0)) mult_expanded

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
let div = binop (fun r a1 a2 -> r.(0) <- a1.(0) /. a2.(0)) div_expanded

(* *** Backwards functions *** *)

(* The set of values x such that x + a == b *)
let reverse_add a b =
  assert false

(* The set of values x such that x * a == b *)
let reverse_mult a b =
  assert false

(* The set of values x such that x / a == b *)
let reverse_div1 a b =
  assert false

(* The set of values x such that a / x == b *)
let reverse_div2 a b =
  assert false

