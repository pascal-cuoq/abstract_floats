let phys_equal = ( == )
let (!=) = `Use_phys_equal
let (==) = `Use_phys_equal

type abstract_float = float array

(*
  The type [abstract_float] is represented using an array of (unboxed) floats.

  Array [t] with length 1:
    a single floating-point number
    (can be NaN, +inf, -inf, or a finite value)

  Array [t] with length >=2:
    header + bounds. the first field is header. the rest of fields are bounds.
    the length of t can only be 2, 3 or 5.

    length of 2:
      only intended to distinguish a header from a single
      floating-point number. a.(1) repeats a.(0).

    length of 5:
      the FP number could be both pos normalish and neg
      normalish. the last four fields indicating two pairs of bounds
      (first neg bounds, then pos bounds).

    length of 3:
      the fp number could be either pos normalish or neg normalish,
       the fields .(1) and .(2) provide the bounds.

  The header (found in t.(0)) can indicate:

    at least one of the NaN values present
    all NaN values present
    FP number can be in negative normalish range
    FP number can be in positive normalish range
    -inf present
    +inf present
    -0.0 present
    +0.0 present

  Vocabulary:

  - normalish means the intervals of normal (or subnormal) values
  - finite means the normalish and zero components of the representation
  - nonzero means the normalish and infinite components, but usually not NaN
*)


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

let dump_internal a =
  let l = Array.length a in
  Format.printf "[|";
  for i = 0 to l-1 do
    if i = 0 || l = 2
    then Format.printf "0x%016Lx" (Int64.bits_of_float a.(i))
    else Format.printf "%.16e" a.(i);
    if i < l-1
    then Format.printf ","
  done;
  Format.printf "|]@\n";

(*
             ***** UNITY OF REPRESENTATION *****

                        ( UoR )

    Every abstract float has one and only one representation

  1. Single floats include positive zero, negative zero,
     positive infinity, negative infinity, NaN value should
     be represented by a singleton.

  2. Any header is represented by an abstract float of size 2.
     The second field of the abstract
     float should have the same value as the first field. This
     guarantees this abstract float has a unique representation.

*)

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
      required only when [at_least_one_NaN] flag is set
      and [all_NaNs] is unset in [Header.t]

*)

module Header : sig
  type t
  (** abstract type for header *)

  type nan_result =
    | One_NaN of Int64.t (** abstract float has one NaN value in payload *)
    | All_NaN            (** abstract float contains all possible NaN values *)
    | No_NaN             (** abstract float contains no NaN value *)

  type flag
  (** abstract flag indicating property of abstract float *)

  val at_least_one_NaN : flag
  (** [at_least_one_NaN] indicates at least one of NaN value
      is present.
      When this flag is on, payload should be set *)

  val all_NaNs : flag

  val negative_normalish : flag
  (** [negative_normalish] indicates some negative normalish
      values are present *)

  val positive_normalish : flag
  (** [positive_normalish] indicates some positive normalish
      positive normalish range *)

  val negative_inf : flag
  (** [negative_inf] indicates -inf is present *)

  val positive_inf : flag

  val negative_zero : flag

  val positive_zero : flag

  val equal : flag -> flag -> bool

  val bottom : t

  val top : t

  val is_bottom : t -> bool

  val is_top : t -> bool

  val pretty: Format.formatter -> abstract_float -> unit
  (** [pretty fmt a] pretty-prints the header of [a] on [fmt] *)

  val combine : t -> t -> t
  (** [combine t1 t2] is the join of [t1] and [t2] *)

  val narrow : t -> t -> t

  val test : t -> flag -> bool
  (** [test t f] is [true] if [f] is set in [t] *)

  val has_inf_zero_or_NaN : abstract_float -> bool
  (** [has_inf_zero_or_NaN a] is [true] if [a] contains
      some infinity, some zero, or a NaN value *)

  val has_normalish : t -> bool
  (** [has_normalish h] is [true] if [h] contains normalish values *)

  val has_zeros : t -> bool

  val set_flag : t -> flag -> t
  (** [set t f] is [t] with flag [f] set *)

  val unset_flag : t -> flag -> t

  val flag_of_float : float -> flag
  (** [flag_of_float f] is flag that would be set to indicate the presence
      of [f]. [flag_of_float nan] is [at_least_one_NaN] *)

  val of_flag : flag -> t
  (** [of_flag f] is a header with flag [t] set *)

  val of_flags : flag list -> t

  val set_all_NaNs : t -> t

  val set_all_zeros : t -> t

  val exactly_one_NaN : t -> bool
  (** [exactly_one_NaN t f] is [true] if [f] contains at least one NaN *)

  val is_exactly : t -> flag -> bool
  (** [is_exactly h f] is [true] if [t] has only [f] on *)

  val size : t -> int
  (** [size h] is the length of abstract float corresponding to the given
      header. Note that the header alone is not always sufficient information
      to decide that the representation should be a single float.
      Hence this function always returns at least 2. *)

  val of_abstract_float : abstract_float -> t
  (** [of_abstract_float a] is the header of the abstract float [a].
       Note: the abstract float [a] has to have size >= 2. In other words,
      [a] cannot be a singleton floating point number *)

  val allocate_abstract_float_with_NaN : t -> nan_result -> abstract_float
  (** [allocate_abstract_float h nr] allocates an abstract float of size
      indicated by [h], with payload set according to [nr], and
      normalish fields, if any, uninitialized. *)

  val allocate_abstract_float : t -> abstract_float
  (** [allocate_abstract_float h] allocates an abstract float of
      size indicated by [h], of which the normalish fields, if any,
      are uninitialized. *)

  val reconstruct_NaN : abstract_float -> nan_result
  (** [reconstruct_NaN a] is the NaN representation in [a], if there is one. *)

  val check: abstract_float -> bool
  (** [assert (check a);] stops execution if a is ill-formed. *)

  val is_header_included : abstract_float -> abstract_float -> bool
  (** [is_header_included a1 a2] is true if the values indicated by [a1]'s
      header are present in [a2]'s header. *)

  val neg: t -> t

  val sqrt: t -> t

  val add: t -> t -> t

  val sub: t -> t -> t

  val mult: t -> t -> t

  val div: t -> t -> t

  val meet : t -> t -> t

  val reverse_add : t -> t -> t

  val reverse_mult : t -> t -> t

end = struct
  type t = int

  type flag = int

  type nan_result =
    | One_NaN of int64
    | All_NaN
    | No_NaN

  let at_least_one_NaN = 1
  let all_NaNs = 2
  let negative_normalish = 4
  let positive_normalish = 8
  let negative_inf = 16
  let positive_inf = 32
  let negative_zero = 64
  let positive_zero = 128

  let equal (flg1:int) (flg2:int) = flg1 = flg2

  let bottom = 0
  let top = 255
  let is_bottom x = x = 0
  let is_top x = x = 255

  let combine h1 h2 = h1 lor h2
  let narrow h1 h2 = h1 land h2
  let cancel_flags h1 h2 = h1 land h2
  let test h flag = h land flag <> 0
  let test_both h1 h2 flag = h1 land h2 land flag <> 0
  let set_flag = combine
  let unset_flag h1 h2 = h1 land (lnot h2)
  let of_flag f = f
  let of_flags fs = List.fold_left ( + ) 0 fs

  let flag_of_float f =
    match classify_float f with
    | FP_zero -> if is_pos_zero f then positive_zero else negative_zero
    | FP_normal | FP_subnormal ->
      if is_pos f then positive_normalish else negative_normalish
    | FP_infinite ->
      if is_pos f then positive_inf else negative_inf
    | _ -> at_least_one_NaN

  let set_all_NaNs h = h lor (at_least_one_NaN + all_NaNs)
  let set_all_zeros h = h lor (negative_zero + positive_zero)
  let get_NaN_part h = h land (at_least_one_NaN + all_NaNs)
  let exactly_one_NaN h = (get_NaN_part h) = at_least_one_NaN

  let of_abstract_float a =
    assert (Array.length a >= 2);
    let l = Int64.shift_right_logical (Int64.bits_of_float a.(0)) 52 in
    (Int64.to_int l) land 255

  let has_inf_zero_or_NaN a =
    assert (Array.length a >= 2);
    let exceptional_flags =
      positive_inf + negative_inf + positive_zero + negative_zero +
        at_least_one_NaN
    in
    (of_abstract_float a) land exceptional_flags <> 0

  let naN_of_abstract_float a =
    Int64.(logor 0x7ff0000000000000L (bits_of_float a.(0)))

  let pretty fmt a =
    let h = of_abstract_float a in
    Format.fprintf fmt "{";
    let started = ref false in
    let comma fmt =
      if !started then Format.fprintf fmt ",";
      started := true;
    in
    let add fmt sign symb =
      comma fmt;
      Format.fprintf fmt sign;
      Format.fprintf fmt symb
    in
    let print_sign i symb =
      match i land 3 with
      | 0 -> ()
      | 1 -> add fmt "-" symb
      | 2 -> add fmt "+" symb
      | 3 -> add fmt "±" symb
      | _ -> assert false
    in
    print_sign (h lsr 6) "0";
    print_sign (h lsr 4) "∞";
    if get_NaN_part h <> 0
    then begin
      comma fmt;
      Format.fprintf fmt "NaN";
      if not (test h all_NaNs)
      then Format.fprintf fmt ":%016Lx" (naN_of_abstract_float a)
    end;
    Format.fprintf fmt "}"

  let is_exactly h flag = h = flag

  let normalish_mask = negative_normalish + positive_normalish

  let has_normalish h = (h land normalish_mask) <> 0

  let zeros_mask = positive_zero + negative_zero

  let has_zeros h = (h land zeros_mask) <> 0

  let size h =
    let posneg = h land normalish_mask in
    if posneg = normalish_mask then 5
    else if posneg <> 0 then 3
    else 2

  let allocate_abstract_float_with_NaN h nr =
    match nr with
    | No_NaN ->
      assert (get_NaN_part h = 0);
      Array.make (size h) (Int64.float_of_bits (Int64.of_int (h lsl 52)))
    | One_NaN n ->
      assert (exactly_one_NaN h);
      let f = Int64.(float_of_bits
                       (logor (of_int (h lsl 52)) (to_payload n))) in
      Array.make (size h) f
    | All_NaN ->
      assert (get_NaN_part h <> 0 && not (exactly_one_NaN h));
      Array.make (size h) (Int64.float_of_bits (Int64.of_int (h lsl 52)))

  let allocate_abstract_float h =
    assert (test h all_NaNs || (not (test h at_least_one_NaN)));
    Array.make (size h) (Int64.float_of_bits (Int64.of_int (h lsl 52)))

  (** [reconstruct_NaN a] returns the bits of the single NaN value
      optionally contained in [a] *)
  let reconstruct_NaN a =
    assert (Array.length a >= 2);
    let h = of_abstract_float a in
    if get_NaN_part h <> 0 then begin
      if exactly_one_NaN h then One_NaN (naN_of_abstract_float a)
      else All_NaN
    end else No_NaN

  let is_header_included a1 a2 =
    assert (Array.length a1 >= 2);
    assert (Array.length a2 >= 2);
    let b1 = Int64.bits_of_float a1.(0) in
    let b2 = Int64.bits_of_float a2.(0) in
    let i1 = Int64.to_int b1 in
    let i2 = Int64.to_int b2 in
    i2 lor (i1 land 0x0FF0000000000000) = i2
    &&
      (* Still, if a1 and a2 contain one NaN each, we need to check for
         exact correspondance of the payloads *)
      (i1 land 0x0030000000000000 <> 0x0010000000000000 ||
         i2 land 0x0030000000000000 <> 0x0010000000000000 ||
         Int64.(logor b1 0x0FF0000000000000L = logor b2 0x0FF0000000000000L))

(* All the invariants that a float array should satisfy in order to be
   a well-formed abstract_float are expressed in function [check] *)
  exception Err

  let check a =
    let l = Array.length a in
    let result =
      try
        match l with
        | 1 -> true
        | 2 | 3 | 5 ->
          let h = of_abstract_float a in
          let a0 = Int64.bits_of_float a.(0) in
          let n = get_NaN_part h in
          if (n <> at_least_one_NaN) && (to_payload a0) <> 0L
          then raise Err;
          if n <> at_least_one_NaN && n <> 0 &&
            n <> at_least_one_NaN + all_NaNs
          then raise Err;
          if l <> size h then raise Err;
          if l = 2
          then begin
            if Int64.bits_of_float a.(1) <> a0 then raise Err;
            if h <> 0 && (h land (pred h) = 0) then raise Err;
            true
          end
          else begin
            if l = 3 && (-. a.(1)) = a.(2) &&
               (h land (negative_inf + positive_inf +
                    negative_zero + positive_zero + at_least_one_NaN)) = 0
            then raise Err;
            if (l = 5 || (l = 3 && test h negative_normalish)) &&
              not (a.(1) < infinity && -. a.(1) <= a.(2) && a.(2) < 0.)
            then raise Err;
            if (l = 5 || (l = 3 && test h positive_normalish)) &&
              not (a.(l-2) < 0. && -. a.(l-2) <= a.(l-1) && a.(l-1) < infinity)
            then raise Err;
            true
          end;
        | _ -> false
      with Err -> false
    in
    if not result
    then begin
      Format.printf "Problem with abstract float representation@ [|";
      for i = 0 to l-1 do
        if i = 0 || l = 2
        then Format.printf "0x%016Lx" (Int64.bits_of_float a.(i))
        else Format.printf "%.16e" a.(i);
        if i < l-1
        then Format.printf ","
      done;
      Format.printf "|]@\n";
    end;
    result

  (* sqrt(-0.) = -0., sqrt(+0.) = +0., sqrt(+inf) = +inf *)
  let sqrt h =
    let nn = h land (negative_normalish + negative_inf + at_least_one_NaN) in
    let h = h lxor nn in (* clear negative_normalish and negative_inf if
                            present *)
    let add_NaN = -nn lsr (Sys.word_size - 1 - 2) in
(* if nn is nonzero, add_NaN is 3.
   if nn is zero, add_NaN is 0. *)
    h lor add_NaN

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
    (* has_finite indicates the presente of finite negative values in
       negative_zero, of finite positive values in positive_zero *)
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
    let finite_pos2_neg_zero_1 = (has_finite2 lsr 1) land h1 in
    let finite_pos1_neg_zero_2 = (has_finite1 lsr 1) land h2 in
    let h2sr = h2 lsr 1 in
    let finite_neg1_pos_zero_2 = has_finite1 land h2sr in
    let h1sr = h1 lsr 1 in
    let finite_neg2_pos_zero_1 = has_finite2 land h1sr in
    let opposite_signs =
      finite_pos2_neg_zero_1 lor finite_pos1_neg_zero_2 lor
      finite_neg1_pos_zero_2 lor finite_neg2_pos_zero_1
    in
    let neg_zero = opposite_signs land negative_zero in

    let zeroes = pos_zero lor neg_zero in

    (* compute in the infinities bits: *)
    let has_nonzero1 = (h1 lsl 2) lor h1 in
    let has_nonzero2 = (h2 lsl 2) lor h2 in

    (* compute in the negative_inf bit: *)
    let neg_nonzero_1_pos_inf_2 = has_nonzero1 land h2sr in
    let neg_nonzero_2_pos_inf_1 = has_nonzero2 land h1sr in
    let pos_nonzero_1_neg_inf_2 = (has_nonzero1 lsr 1) land h2 in
    let pos_nonzero_2_neg_inf_1 = (has_nonzero2 lsr 1) land h1 in
    let neg_inf =
      pos_nonzero_2_neg_inf_1 lor neg_nonzero_2_pos_inf_1 lor
      pos_nonzero_1_neg_inf_2 lor neg_nonzero_1_pos_inf_2
    in
    let neg_inf = neg_inf land negative_inf in

    (* +inf is obtained by multiplying nonzero and inf of the same sign: *)
    let pos_inf12 = has_nonzero1 land h2 in
    let pos_inf21 = has_nonzero2 land h1 in
    let pos_inf = pos_inf12 lor pos_inf21 in
    let pos_inf = pos_inf lor (pos_inf lsl 1) in
    let pos_inf = pos_inf land positive_inf in

    let infinities = neg_inf lor pos_inf in

    (* Compute in negative_zero and negative_inf bits: *)
    let merge_posneg1 = h1sr lor h1 in
    let merge_posneg2 = h2sr lor h2 in
    let nan12 = (merge_posneg1 lsl 2) land merge_posneg2 in
    let nan21 = (merge_posneg2 lsl 2) land merge_posneg1 in
    let nan_zero_times_inf = (nan12 lor nan21) land negative_zero in
    (* are there NaNs in operands? *)
    let nan_as_op = (h1 lor h2) land at_least_one_NaN in
    let nan = nan_zero_times_inf lor nan_as_op in
    (* Map nonzero to 3 and 0 to 0: *)
    let nan = (- nan) lsr (Sys.word_size - 1 - 2) in

    infinities lor zeroes lor nan

  let div h1 h2 = mult h1 (inv h2)

  let meet h1 h2 =
    h1 land h2 land
    (lnot (at_least_one_NaN + all_NaNs +
           positive_normalish + negative_normalish))

  let reverse_add h1 h2 =
    if get_NaN_part h1 <> 0 && get_NaN_part h2 <> 0 then
      positive_zero + negative_zero + positive_inf + negative_inf
    else begin
      let h = bottom in
      let h =
        if get_NaN_part h2 <> 0 then begin
          let h = if test h1 negative_inf
            then set_flag h positive_inf else h in
          let h = if test h1 positive_inf
            then set_flag h negative_inf else h in
          h end else h in
      let h =
        let p =
          positive_inf + positive_zero + negative_zero +
          positive_normalish + negative_normalish in
        if (h1 land p <> 0) && test h2 positive_inf then
          let h = set_flag h positive_inf in
          if test h1 positive_inf then begin
            let h = set_flag h positive_zero in
            set_flag h negative_zero
          end
          else h else h in
      let h =
        let p =
          negative_inf + positive_zero + negative_zero +
          positive_normalish + negative_normalish in
        if h1 land p <> 0 && test h2 negative_inf then
          let h = set_flag h negative_inf in
          if test h1 negative_inf then
            let h = set_flag h positive_zero in
            set_flag h negative_zero
          else h else h in
      let h =
        if test h2 positive_zero && test h1 positive_zero then
          set_flag (set_flag h positive_zero) negative_zero
        else if test h2 positive_zero && test h1 negative_zero then
          set_flag h positive_zero
        else
          h in
      let h =
        if test h2 negative_zero && test h1 negative_zero then
          set_flag h negative_zero else h in
    h
    end

  let reverse_mult h1 h2 =
    if get_NaN_part h1 <> 0 && get_NaN_part h2 <> 0 then
      positive_zero + negative_zero + positive_inf + negative_inf
    else begin
      let h = bottom in
      let h =
        if test h2 positive_inf then
          let h =
            if test h1 positive_normalish || test h1 positive_inf then
              set_flag h positive_inf else h in
          if test h1 negative_normalish || test h1 negative_inf then
            set_flag h negative_inf else h
        else h in
      let h =
        if test h2 negative_inf then
          let h =
            if test h1 negative_normalish || test h1 negative_inf then
              set_flag h positive_inf else h in
          if test h1 positive_normalish || test h1 positive_inf then
            set_flag h negative_inf else h
        else h in
      let h =
        if test h2 positive_zero then
          let h =
            if test h1 positive_normalish || test h1 positive_zero then
              set_flag h positive_zero else h in
          if test h1 negative_normalish || test h1 negative_zero then
            set_flag h negative_zero else h
        else h in
      let h =
        if test h2 negative_zero then
          let h =
            if test h1 positive_normalish || test h1 positive_zero then
              set_flag h negative_zero else h in
          if test h1 negative_normalish || test h1 negative_zero then
            set_flag h positive_zero else h
        else h in
      let h =
        if get_NaN_part h2 <> 0 then
          let h =
            if test h1 positive_inf || test h1 negative_inf then
              set_flag h (negative_zero + positive_zero) else h in
          let h =
            if test h1 positive_zero || test h1 negative_zero then
              set_flag h (negative_inf + positive_inf) else h in
          if get_NaN_part h1 <> 0 then
            set_flag h (positive_zero + negative_zero +
                        positive_inf + negative_inf) else h
        else h in
      h
    end

end

(*
  If negative_normalish, the negative bounds are always at t.(1) and t.(2)

  If positive_normalish, the positive bounds are always at:
    let l = Array.length t in t.(l-2) and t.(l-1)

  Each pair of bounds of a same sign is represented
    as -lower_bound, upper_bound.
*)

(** [copy_bounds a1 a2] will copy bounds of [a1] to the freshly
    allocated [a2]. This does not break UoR *)
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

(** [set_opp_neg_lower a f] sets lower bound of negative normalish to [-. f] *)
let set_opp_neg_lower a f =
  assert(0.0 < f);
  assert(f < infinity);
  a.(1) <- f

(** [set_neg_lower a f] sets lower bound of positive normalish to [f] *)
let set_neg_lower a f =
  set_opp_neg_lower a (-. f)

(** [set_neg_upper a f] sets upper bound of negative normalish to [f] *)
let set_neg_upper a f =
  assert(neg_infinity < f);
  assert(f < 0.0);
  a.(2) <- f

(** [set_neg a l u] sets bounds of negative normalish to [l] and [u] *)
let set_neg a l u =
  assert(neg_infinity < l);
  assert(l <= u);
  assert(u < 0.0);
  a.(1) <- -. l;
  a.(2) <- u

(** [set_opp_pos_lower a f] sets lower bound of positive normalish to [-. f] *)
let set_opp_pos_lower a f =
  assert(neg_infinity < f);
  assert(f < 0.0);
  a.(Array.length a - 2) <- f

(** [set_pos_lower a f] sets lower bound of positive normalish to [f] *)
let set_pos_lower a f =
  set_opp_pos_lower a (-. f)

(** [set_pos_upper a f] sets upper bound of positive normalish to [f] *)
let set_pos_upper a f =
  assert(0.0 < f);
  assert(f < infinity);
  a.(Array.length a - 1) <- f

(** [set_pos a l u] sets bound of positive normalish to [l] and [u] *)
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

(* [set_same_bound a f] sets pos or neg bound of [a] to [f].
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

let inject_float f = Array.make 1 f

let inject_interval f1 f2 = assert false (* TODO *)

let is_singleton f = Array.length f = 1

let zero = inject_float 0.0
let neg_zero = inject_float (-0.0)
let abstract_infinity = inject_float infinity
let abstract_neg_infinity = inject_float neg_infinity
let abstract_all_NaNs =
  Header.(allocate_abstract_float (set_all_NaNs bottom))
let bottom = Header.(allocate_abstract_float bottom)

let () =
  assert (Header.check zero);
  assert (Header.check neg_zero);
  assert (Header.check abstract_infinity);
  assert (Header.check abstract_neg_infinity);
  assert (Header.check abstract_all_NaNs);
  assert (Header.check bottom)

let is_bottom a =
  assert (Header.check a);
  Array.length a = 2 && Header.(is_bottom (of_abstract_float a))

let set_header_from_float f h =
  assert (classify_float f <> FP_nan);
  Header.(set_flag h (flag_of_float f))

(** [merge_float a f] is a freshly allocated abstract float, which is
    of the result of the merge of [f] and [a]. *)
let merge_float a f =
  assert (Header.check a);
  assert (Array.length a >= 2);
  let h = Header.of_abstract_float a in
  if Header.is_bottom h then inject_float f else
  match classify_float f with
  | (FP_zero | FP_infinite) ->
    let h_new = Header.(set_flag h (flag_of_float f)) in
    if h_new = h
    then a
    else begin
      let a' =
        Header.allocate_abstract_float_with_NaN h_new (Header.reconstruct_NaN a)
      in
      copy_bounds a a'; a'
    end
  | FP_nan ->
    begin
    (* [f] is NaN. Potential NaN value is reconstructed from
       abstract float to determine if [f] is a different representation of NaN.
       If different, [all_NaNs] flag is set in abstract float's header. *)
      match Header.reconstruct_NaN a with
      | Header.All_NaN -> a
      | Header.One_NaN n ->
        if n = Int64.bits_of_float f then a else begin
          let h = Header.set_all_NaNs h in
          let anew = Header.allocate_abstract_float h in
          copy_bounds a anew; anew
        end
      | Header.No_NaN ->
        let h = Header.(set_flag h at_least_one_NaN) in
        let anew =
          Header.allocate_abstract_float_with_NaN h
            (Header.One_NaN (Int64.bits_of_float f)) in
        copy_bounds a anew; anew
    end
  | FP_normal | FP_subnormal ->
    begin
      (* [f] is normalish. A freshly abstract float is allocated
         based on [a]. New header and original potential payload are set. *)
      let flg = Header.flag_of_float f in
      let range_exists = Header.(test h flg) in
      let h =
        if range_exists then h else Header.(set_flag h flg) in
      let a' =
        Header.allocate_abstract_float_with_NaN h
          (Header.reconstruct_NaN a) in
      begin
        copy_bounds a a';
        if range_exists then
          insert_float_in_bounds a' f
        else
          if is_pos f then set_pos a' f f else set_neg a' f f
      end;
      a'
    end

(* [normalize_zero_and_inf] allows [neg_u] (rep [pos_l]) to be -0.0 (resp +0.0),
   and [neg_l] (resp [pos_u]) to be infinite.
   [normalize_zero_and_inf] converts these values to flags in the header.
    [normalize_zero_and_inf] is used to move the the header the
   zeroes and infinities created by underflow and overflow *)
let normalize_zero_and_inf zero_for_negative header neg_l neg_u pos_l pos_u =
  let neg_u, header =
    if neg_u = 0.0
    then largest_neg,  (* smallest magnitude subnormal *)
      Header.set_flag header zero_for_negative
    else neg_u, header
  in
  let pos_l, header =
    if pos_l = 0.0
    then smallest_pos,  (* smallest magnitude subnormal *)
      Header.(set_flag header positive_zero)
    else pos_l, header
  in
  let neg_l, header =
    if neg_l = neg_infinity
    then smallest_neg, (* -. max_float *)
      Header.(set_flag header negative_inf)
    else neg_l, header
  in
  let pos_u, header =
    if pos_u = infinity
    then largest_pos, (* max_float *)
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
    The lower bounds are the mathematical ones (not inverted).
    This function is convenient for the result of arithmetic operations,
    because it handles underflows to 0 and overflows to infinities that
    may have happened in neg_l neg_u pos_l pos_u. On the other hand it
    assumes that [header] indicates either no NaNs or all NaNs
    (which is the case for results of arithmetic operations. *)
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

let print_union fmt = Format.fprintf fmt " ∪ "

let print_bounds fmt a i =
  let l = ~-. (a.(i)) in
  let u = a.(i + 1) in
  if l = u then
    Format.fprintf fmt "{%.16e}" l
  else
    Format.fprintf fmt "[%.16e ... %.16e]" l u

(* [pretty fmt a] pretty-prints [a] on [fmt] *)
let pretty fmt a =
  assert (Header.check a);
  match a with
  | [| f |] -> Format.fprintf fmt "{%.16e}" f
  | _ ->
    if is_bottom a
    then
      Format.fprintf fmt "{ }"
    else
      let le = Array.length a in
      let l3 = le >= 3 in
      if Header.has_inf_zero_or_NaN a
      then begin
        Header.pretty fmt a;
        if l3 then print_union fmt
      end;
      if l3 then begin
        print_bounds fmt a 1;
        if le = 5
        then begin
          print_union fmt;
          print_bounds fmt a 3;
        end
      end


(* *** Set operations *** *)

(* [compare] is a total order over abstract_float *)
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
  assert (Header.check a);
  match Array.length a with
  | 1 -> Int64.bits_of_float f = Int64.bits_of_float a.(0)
  | l -> begin
    assert (l = 2 || l = 3 || l = 5);
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
      | Header.One_NaN n -> Int64.bits_of_float f = n
      | Header.All_NaN -> true
      | Header.No_NaN -> false
      end
    | FP_normal | FP_subnormal ->
      l > 2 &&
        let opp_f = -. f in
        opp_f <= get_opp_neg_lower a && f <= (get_neg_upper a) ||
          opp_f <= get_opp_pos_lower a && f <= (get_pos_upper a)
  end

(* [is_included a1 a2] is a boolean value indicating if every element in [a1]
   is also an element in [a2] *)
let is_included a1 a2 =
  assert (Header.check a1);
  assert (Header.check a2);
  match Array.length a1, Array.length a2 with
  | 1, _ -> float_in_abstract_float a1.(0) a2
  | 2, 1 -> is_bottom a1
  | 2, _ -> Header.is_header_included a1 a2
  | _, (1 | 2) | 5, 3 -> false
  | l1, l2 ->
    assert(l1 = 3 || l1 = 5);
    assert(l2 = 3 || l2 = 5);
    Header.is_header_included a1 a2 &&
      let a11 = a1.(1) in
      let a12 = a1.(2) in
      let a21 = a2.(1) in
      let a22 = a2.(2) in
      if l1 = l2 || Header.(test (of_abstract_float a1) negative_normalish)
      then
        a11 <= a21 && a12 <= a22 &&
          (l1 = 3 || (a1.(3) <= a2.(3) && a1.(4) <= a2.(4)))
      else begin
        assert (l1 = 3);
        assert (l2 = 5);
        assert (Header.(test (of_abstract_float a1) positive_normalish));
        a11 <= a2.(3) && a12 <= a2.(4)
      end

(* [join a1 a2] is the smallest abstract state that contains every
   element from [a1] and every element from [a2]. *)
let join (a1:abstract_float) (a2: abstract_float) : abstract_float =
  assert (Header.check a1);
  assert (Header.check a2);
    match is_singleton a1, is_singleton a2, a1, a2 with
    | true, true, _, _ -> (
    (* both [a1] and [a2] are singletons *)
      let f1, f2 = a1.(0), a2.(0) in
      match classify_float f1, classify_float f2, f1, f2 with
      | FP_nan, FP_nan, _, _ ->
        if Int64.bits_of_float f1 = Int64.bits_of_float f2 then
          inject_float f1 else abstract_all_NaNs
      | FP_nan, _, theNaN, nonNaN | _, FP_nan, nonNaN, theNaN ->
        let h = Header.(of_flag at_least_one_NaN) in
        let h = set_header_from_float nonNaN h in
        let a = Header.allocate_abstract_float_with_NaN h
            (Header.One_NaN (Int64.bits_of_float theNaN)) in
        if Header.size h <> 2
        then begin
          assert (Header.size h = 3);
          set_same_bound a nonNaN
        end;
        a
      | (FP_zero, FP_zero, _, _ | FP_infinite, FP_infinite, _, _)
        when (is_pos f1 && is_pos f2) ||
             (is_neg f1 && is_neg f2) -> [|f1|]
      | (FP_normal | FP_subnormal), _ , _, _ when f1 = f2 -> [|f1|]
      | _, _, _, _ -> begin
        let h = set_header_from_float f1 Header.bottom in
        let h = set_header_from_float f2 h in
        let a = Header.allocate_abstract_float h in
        let s = Array.length a in
        if s > 2 then begin
          match classify_float f1, classify_float f2 with
          | (FP_zero | FP_infinite), (FP_normal | FP_subnormal) ->
            set_same_bound a f2
          | (FP_normal | FP_subnormal), (FP_zero | FP_infinite) ->
            set_same_bound a f1
          | (FP_normal | FP_subnormal), (FP_normal | FP_subnormal) ->
            begin
              let f1, f2 = if f1 < f2 then f1, f2 else f2, f1 in
              match is_neg f1, is_neg f2 with
              | true, true -> set_neg a f1 f2
              | false, false -> set_pos a f1 f2
              | true, false -> set_neg a f1 f1; set_pos a f2 f2
              | _, _ -> assert false
            end
          | _ -> assert false
        end;
        a
        end)
    (* only one of [a1] and [a2] is singleton *)
    | true, false, single, non_single | false, true, non_single, single ->
      merge_float non_single single.(0)
    (* neither [a1] nor [a2] is singleton *)
    | false, false, _, _ ->
      let hn = Header.(combine (of_abstract_float a1) (of_abstract_float a2)) in
      let an =
        let open Header in
        match reconstruct_NaN a1, reconstruct_NaN a2 with
        | One_NaN n1, One_NaN n2 ->
          if n1 <> n2 then
            allocate_abstract_float (set_all_NaNs hn)
          else
            allocate_abstract_float_with_NaN hn (One_NaN n1)
        | All_NaN, _ | _, All_NaN | No_NaN, No_NaN ->
          allocate_abstract_float hn
        | No_NaN, r | r, No_NaN ->
          (allocate_abstract_float_with_NaN hn r) in
      begin
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

let meet a1 a2 =
  match is_singleton a1, a1, is_singleton a2, a2 with
  | true, _, true, _ ->
    if Int64.(bits_of_float a1.(0) = bits_of_float a2.(0)) then a1
    else Header.(allocate_abstract_float bottom)
  | true, s, false, a | false, a, true, s ->
    if float_in_abstract_float s.(0) a then s
    else Header.(allocate_abstract_float bottom)
  | false, _, false, _ ->
    let h1, h2 = Header.(of_abstract_float a1, of_abstract_float a2) in
    let h = Header.meet h1 h2 in
    let res_NaN =
      match Header.(reconstruct_NaN a1, reconstruct_NaN a2) with
      | n1, n2 when n1 = n2 -> n1
      | Header.All_NaN, n | n, Header.All_NaN -> n
      | _, _ -> Header.No_NaN in
    let h =
      match res_NaN with
      | Header.All_NaN -> Header.(set_all_NaNs h)
      | Header.One_NaN _ -> Header.(set_flag h at_least_one_NaN)
      | Header.No_NaN -> h in
    let pos_range =
      if Header.(test h1 positive_normalish &&
                 test h2 positive_normalish) then
        let pl1, pu1 =
          (-. (get_opp_pos_lower a1)), get_pos_upper a1 in
        let pl2, pu2 =
          (-. (get_opp_pos_lower a2)), get_pos_upper a2 in
        if pl2 <= pu1 && pu1 <= pu2 then
          Some (max pl1 pl2, pu1) else
        if pl1 <= pu2 && pu2 <= pu1 then
          Some (max pl1 pl2, pu2) else None
      else None in
    let neg_range =
      if Header.(test h1 negative_normalish &&
                 test h2 negative_normalish) then
        let nl1, nu1 =
          (-. (get_opp_neg_lower a1)), get_neg_upper a1 in
        let nl2, nu2 =
          (-. (get_opp_neg_lower a2)), get_neg_upper a2 in
        if nl2 <= nu1 && nu1 <= nu2 then
          Some (max nl1 nl2, nu1) else
        if nl1 <= nu2 && nu2 <= nu1 then
          Some (max nl1 nl2, nu2) else None
      else None in
    match Header.is_bottom h, res_NaN, pos_range, neg_range with
    | (true, Header.No_NaN, Some (l, u), None |
       true, Header.No_NaN, None, Some (l, u)) when l = u -> [| l |]
    | _, Header.No_NaN, None, None -> begin
      if Header.(is_exactly h positive_zero) then [| 0. |] else
      if Header.(is_exactly h negative_zero) then [| -0. |] else
      if Header.(is_exactly h positive_inf) then [| infinity |] else
      if Header.(is_exactly h negative_inf) then [| neg_infinity |] else begin
        Header.allocate_abstract_float h
      end
      end
    | b, Header.One_NaN n, None, None
      when b || Header.(is_exactly h at_least_one_NaN) ->
      [| Int64.float_of_bits n |]
    | _, _, _, _ ->
      let h =
        if pos_range = None then h
        else Header.(set_flag h positive_normalish) in
      let h =
        if neg_range = None then h
        else Header.(set_flag h negative_normalish) in
      let a = Header.allocate_abstract_float_with_NaN h res_NaN in
      (match pos_range with
       | None -> ()
       | Some (l, u) -> set_pos a l u);
      (match neg_range with
       | None -> ()
       | Some (l, u) -> set_neg a l u);
      a

(* [intersects a1 a2] is true iff there exists a float that is both in [a1]
   and in [a2]. *)
let intersects a1 a2 = assert false

(* *** Arithmetic *** *)

(*                   Notes on arithmetic involving NaN

   all arithmetic operations except neg produce all_NaNs, if they produce NaN.

   The reason being that:

   Suppose for arithmetic operation [x + (-y)], [y] is a NaN with value
   [0xFFFFFFFFFFFFFFFF].

   There are two possible cases:

   1) Strictly following IEEE-754 withouth any optimization, the expression
      is first evaluated to [x + 0x7FFFFFFFFFFFFFFF]. Then, by IEEE-754,
      NaN must be returned if present as operand. So, the result is 
      [0x7FFFFFFFFFFFFFFF]
   2) Compiler does incorrect optimization. [x + (-y)] is optimized into
      [x - y]. The result then becomes [0xFFFFFFFFFFFFFFFF], according to
      IEEE-754.

   These mean our program must return [all_NaNs] to capture all possible
   NaN values *)


(* negate() is a bitstring operation, even on NaNs. (IEEE 754-2008 6.3)
   and C99 says unary minus uses negate. Indirectly, anyway.
   @UINT_MIN https://twitter.com/UINT_MIN/status/702199094169604096 *)
let neg a =
  match Array.length a with
  | 1 -> let f = a.(0) in [| -.f |]
  | 2 | 3 | 5 ->
    let neg_h = Header.(neg (of_abstract_float a)) in
    let an =
      match Header.reconstruct_NaN a with
      | Header.One_NaN n ->
          Header.(allocate_abstract_float_with_NaN
                    neg_h (One_NaN (Int64.neg n)))
      | _ -> Header.allocate_abstract_float neg_h in
    if Header.(test neg_h positive_normalish) then begin
      set_opp_pos_lower an (get_neg_upper a);
      set_pos_upper an (get_opp_neg_lower a)
    end;
    if Header.(test neg_h negative_normalish) then begin
      set_opp_neg_lower an (get_pos_upper a);
      set_neg_upper an (get_opp_pos_lower a)
    end;
    an
  | _ -> assert false

let abstract_sqrt a =
  if is_singleton a
  then begin
    let a = sqrt a.(0) in
    if a <> a
    then abstract_all_NaNs
    else inject_float a
  end
  else
    let h = Header.of_abstract_float a in
    let new_h = Header.sqrt h in
    if Header.(test h positive_normalish)
    then
      let na = Header.allocate_abstract_float new_h in
      let l, u = (-. (get_opp_pos_lower a)), (get_pos_upper a) in
      set_pos na (sqrt l) (sqrt u);
      na
    else
      Header.allocate_abstract_float new_h

(* [expand a] returns the non-singleton representation corresponding
   to a singleton [a].

                 ***********************************
                 *             WARNING             *
      **********************************************************
      *                                                        *
      *         Never let expanded forms escape outside        *
      *              a short series of computations            *
      *                                                        *
      **********************************************************

   The representation for a same set of floats should be unique by UoR.
   The single-float representation is efficient and is all that outside
   code using the library should see.
*)
let expand =
  let exp_infinity = Header.(allocate_abstract_float (of_flag positive_inf)) in
  let exp_neg_infinity =
    Header.(allocate_abstract_float (of_flag negative_inf))
  in
  let exp_zero = Header.(allocate_abstract_float (of_flag positive_zero)) in
  let exp_neg_zero = Header.(allocate_abstract_float (of_flag negative_zero)) in
  fun a->
    let a = a.(0) in
    if a = infinity then exp_infinity
    else if a = neg_infinity then exp_neg_infinity
    else if a <> a then
        Header.(allocate_abstract_float_with_NaN
                (of_flag at_least_one_NaN)
                (One_NaN (Int64.bits_of_float a)))
    else
      let repr = Int64.bits_of_float a in
      if repr = 0L then exp_zero
      else if repr = sign_bit then exp_neg_zero
      else
        let flag =
          if a < 0.0 then Header.negative_normalish else Header.positive_normalish
        in
        let r = Header.(allocate_abstract_float (of_flag flag)) in
        r.(1) <- -. a;
        r.(2) <- a;
        r

let normalize a =
  if is_singleton a then a else
    let h = Header.of_abstract_float a in
    if Header.(is_exactly h positive_zero) then zero else
    if Header.(is_exactly h negative_zero) then neg_zero else
    if Header.(is_exactly h positive_inf) then abstract_infinity else
    if Header.(is_exactly h negative_inf) then abstract_neg_infinity else
    if Header.(is_exactly h positive_normalish) then
      let l, u = (-. get_opp_pos_lower a), get_pos_upper a in
      if l = u then inject_float u else a else
    if Header.(is_exactly h negative_normalish) then
      let l, u = (-. get_opp_neg_lower a), get_neg_upper a in
      if l = u then inject_float u else a else
    if Header.(is_exactly h at_least_one_NaN) then
      match Header.reconstruct_NaN a with
      | Header.One_NaN n -> inject_float (Int64.float_of_bits n)
      | _ -> a
    else a

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
    if result <> result
    then abstract_all_NaNs
    else result
  else
    let a1 = if single_a1 then expand a1 else a1 in
    let a2 = if single_a2 then expand a2 else a2 in
    expanded_op a1 a2

let add_expanded a1 a2 =
  let header1 = Header.of_abstract_float a1 in
  let header2 = Header.of_abstract_float a2 in
  let p1 = Header.(test header1 positive_normalish) in
  let p2 = Header.(test header2 positive_normalish) in
  let n1 = Header.(test header1 negative_normalish) in
  let n2 = Header.(test header2 negative_normalish) in
  let z1 = Header.has_zeros header1 in
  let z2 = Header.has_zeros header2 in
  let fsucc f = Int64.(float_of_bits @@ succ @@ bits_of_float f) in
  let fpred f = Int64.(float_of_bits @@ pred @@ bits_of_float f) in
  let get_m pa na =
    let pm = get_opp_pos_lower pa in
    if (-.(get_opp_neg_lower na)) <= pm && pm <= get_neg_upper na then
        Some (-.pm)
    else
      let nm = (-.(get_neg_upper na)) in
      if (-.(get_opp_pos_lower pa)) <= nm && nm <= get_pos_upper pa then
        Some nm
      else None in
  let m1, m2 =
    (if p1 && n2 then get_m a1 a2 else None),
    (if n1 && p2 then get_m a2 a1 else None) in
  let header = Header.add header1 header2 in
  (* After getting the contributions to the result arising from the
     header bits of the operands, the "expanded" versions of binary
     operations need to compute the contributions resulting from the
     (sub)normal parts of the operands.  Usually these contributions are
     (sub)normal intervals, but operations on (sub)normal values can always
     underflow to zero or overflow to infinity.

     One constraint: always compute so that if the rounding mode
     was upwards, then it would make the sets larger.
     This means computing the positive upper bound as a positive
     number, as well as the negative lower bound.
     The positive lower bound and the negative upper bound must
     be computed as negative numbers, so that if rounding were upwards,
     they would end up closer to 0, making the sets larger. *)
  let opp_neg_l = get_opp_finite_lower a1 +. get_opp_finite_lower a2 in
  let pos_u = get_finite_upper a1 +. get_finite_upper a2 in
  let opp_pos_l =
    if p1 && p2
    then get_opp_pos_lower a1 +. get_opp_pos_lower a2 else neg_infinity in
  let opp_pos_l =
    if z1 && p2 then max (get_opp_pos_lower a2) opp_pos_l else opp_pos_l in
  let opp_pos_l =
    if z2 && p1 then max (get_opp_pos_lower a1) opp_pos_l else opp_pos_l in
  let neg_u =
    if n1 && n2
    then get_neg_upper a1 +. get_neg_upper a2 else neg_infinity in
  let neg_u =
    if z1 && n2 then max (get_neg_upper a2) neg_u else neg_u in
  let neg_u =
    if z2 && n1 then max (get_neg_upper a1) neg_u else neg_u in
  let present a f =
    ((-. (get_opp_pos_lower a)) <= f && f <= get_pos_upper a) ||
    ((-. (get_opp_neg_lower a)) <= f && f <= get_neg_upper a) in
  let opp_pos_l, neg_u =
    match m1 with
    | None ->
      if p1 && n2
      then
        let f = get_opp_pos_lower a1 +. get_opp_neg_lower a2 in
        if is_neg f then max opp_pos_l f, neg_u
        else opp_pos_l, max neg_u (get_pos_upper a1 +. get_neg_upper a2)
      else
        opp_pos_l, neg_u
    | Some m ->
      let sm, pm = fsucc m, fpred m in
      let opp_pos_l =
        if present a1 sm then max (m -. sm) opp_pos_l else opp_pos_l in
      let opp_pos_l =
        if present a2 (-.pm) then max (pm -. m) opp_pos_l else opp_pos_l in
      let neg_u =
        if present a2 (-.sm) then max (m -. sm) neg_u else neg_u in
      let neg_u =
        if present a1 pm then max (pm -. m) neg_u else neg_u in
      opp_pos_l, neg_u in
  let opp_pos_l, neg_u =
    match m2 with
    | None ->
      if n1 && p2
      then
        let f = get_neg_upper a1 +. get_pos_upper a2 in
        if is_neg f then opp_pos_l, max neg_u f
        else begin
          max opp_pos_l (get_opp_neg_lower a1 +. get_opp_pos_lower a2), neg_u
        end
      else
        opp_pos_l, neg_u
    | Some m ->
      let sm, pm = fsucc m, fpred m in
      let opp_pos_l =
        if present a2 sm then max (m -. sm) opp_pos_l else opp_pos_l in
      let opp_pos_l =
        if present a1 (-.pm) then max (pm -. m) opp_pos_l else opp_pos_l in
      let neg_u =
        if present a1 (-.sm) then max (m -. sm) neg_u else neg_u in
      let neg_u =
        if present a2 pm then max (pm -. m) neg_u else neg_u in
      opp_pos_l, neg_u in
  let header =
    match m1, m2 with
    | Some _, _ | _, Some _ -> Header.(set_flag header positive_zero)
    | None, None -> header in
  (* First, normalize. What may not look like a singleton before normalization
     may turn out to be one afterwards: *)
  let header, neg_l, neg_u, pos_l, pos_u =
    normalize_for_add header (-. opp_neg_l) neg_u (-. opp_pos_l) pos_u
  in
  inject header neg_l neg_u pos_l pos_u |> normalize

(** [add a1 a2] returns the set of values that can be taken by adding a value
   from [a1] to a value from [a2]. *)
let add = binop (fun r a1 a2 -> r.(0) <- a1.(0) +. a2.(0)) add_expanded

let sub a1 a2 = add a1 (neg a2)

let mult_expanded a1 a2 =
  let header1 = Header.of_abstract_float a1 in
  let header2 = Header.of_abstract_float a2 in
  let p1 = Header.(test header1 positive_normalish) in
  let p2 = Header.(test header2 positive_normalish) in
  let n1 = Header.(test header1 negative_normalish) in
  let n2 = Header.(test header2 negative_normalish) in
  let header = Header.mult header1 header2 in
  let opp_neg_l, neg_u  =
    if p1 && n2
    then
      let f1_pu, f1_opp_pl = get_pos_upper a1, get_opp_pos_lower a1 in
      let f2_nu, f2_opp_nl = get_neg_upper a2, get_opp_neg_lower a2 in
      (f1_pu *. f2_opp_nl), ((-. f1_opp_pl) *. f2_nu)
    else 0., neg_infinity
  in
  let opp_neg_l, neg_u  =
    if n1 && p2
    then
      let f1_nu, f1_opp_nl = get_neg_upper a1, get_opp_neg_lower a1 in
      let f2_pu, f2_opp_pl = get_pos_upper a2, get_opp_pos_lower a2 in
      max opp_neg_l (f2_pu *. f1_opp_nl),
      max neg_u ((-. f2_opp_pl) *. f1_nu)
    else opp_neg_l, neg_u
  in
  let opp_pos_l, pos_u =
    if p1 && p2
    then
      let f1_pu, f1_opp_pl = get_pos_upper a1, get_opp_pos_lower a1 in
      let f2_pu, f2_opp_pl = get_pos_upper a2, get_opp_pos_lower a2 in
      (-. f1_opp_pl) *. f2_opp_pl, f1_pu *. f2_pu
    else neg_infinity, 0.
  in
  let opp_pos_l, pos_u =
    if n1 && n2
    then
      let f1_nu, f1_opp_nl = get_neg_upper a1, get_opp_neg_lower a1 in
      let f2_nu, f2_opp_nl = get_neg_upper a2, get_opp_neg_lower a2 in
      max opp_pos_l ((-. f1_nu) *. f2_nu),
      max pos_u (f1_opp_nl *. f2_opp_nl)
    else
      opp_pos_l, pos_u
  in
  (* First, normalize. What may not look like a singleton before normalization
     may turn out to be one afterwards: *)
  let header, neg_l, neg_u, pos_l, pos_u =
    normalize_for_mult header (-. opp_neg_l) neg_u (-. opp_pos_l) pos_u
  in
  inject header neg_l neg_u pos_l pos_u |> normalize

(** [mult a1 a2] returns the set of values that can be taken by multiplying
    a value from [a1] with a value from [a2]. *)
let mult = binop (fun r a1 a2 -> r.(0) <- a1.(0) *. a2.(0)) mult_expanded

let div_expanded a1 a2 =
  let header1 = Header.of_abstract_float a1 in
  let header2 = Header.of_abstract_float a2 in
  let p1 = Header.(test header1 positive_normalish) in
  let p2 = Header.(test header2 positive_normalish) in
  let n1 = Header.(test header1 negative_normalish) in
  let n2 = Header.(test header2 negative_normalish) in
  let header = Header.div header1 header2 in
  let opp_neg_l, neg_u =
    if p1 && n2
    then
      let f1_pu, f1_opp_pl = get_pos_upper a1, get_opp_pos_lower a1 in
      let f2_nu, f2_opp_nl = get_neg_upper a2, get_opp_neg_lower a2 in
      (-. f1_pu /. f2_nu), f1_opp_pl /. f2_opp_nl
    else
      0., neg_infinity
  in
  let opp_neg_l, neg_u =
    if n1 && p2
    then
      let f1_nu, f1_opp_nl = get_neg_upper a1, get_opp_neg_lower a1 in
      let f2_pu, f2_opp_pl = get_pos_upper a2, get_opp_pos_lower a2 in
      max opp_neg_l (-. f1_opp_nl /. f2_opp_pl),
      max neg_u (f1_nu /. f2_pu)
    else
      opp_neg_l, neg_u in
  let opp_pos_l, pos_u =
    if p1 && p2
    then
      let f1_opp_pl, f1_pu = get_opp_pos_lower a1, get_pos_upper a1 in
      let f2_opp_pl, f2_pu = get_opp_pos_lower a2, get_pos_upper a2 in
      (f1_opp_pl /. f2_pu), (-. f1_pu /. f2_opp_pl)
    else
      neg_infinity, 0.
  in
  let opp_pos_l, pos_u =
    if n1 && n2
    then
      let f1_nu, f1_opp_nl = get_neg_upper a1, get_opp_neg_lower a1 in
      let f2_nu, f2_opp_nl = get_neg_upper a2, get_opp_neg_lower a2 in
      max opp_pos_l (f1_nu /. f2_opp_nl),
      max pos_u (-. f1_opp_nl /. f2_nu)
    else
      opp_pos_l, pos_u in
  let header, neg_l, neg_u, pos_l, pos_u =
    normalize_for_mult header (-. opp_neg_l) neg_u (-. opp_pos_l) pos_u
  in
  inject header neg_l neg_u pos_l pos_u |> normalize

(** [div a1 a2] returns the set of values that can be taken by dividing
    a value from [a1] by a value from [a2]. *)
let div = binop (fun r a1 a2 -> r.(0) <- a1.(0) /. a2.(0)) div_expanded

module Dichotomy : sig

  val pos_cp : float
  val neg_cp : float

  val range_add : float -> float -> float -> float -> (float * float) option

  val range_mult : float -> float -> float -> float -> (float * float) option

  val normalize : (float * float) option ->
                  (float * float) option *
                  (float * float) option *
                  (float * float) option

end = struct

  let neg_zero = Int64.float_of_bits 0x8000_0000_0000_0000L
  let pos_zero = Int64.float_of_bits 0x0000_0000_0000_0000L

  let fsucc f = Int64.(float_of_bits @@ succ @@ bits_of_float f)
  let fpred f = Int64.(float_of_bits @@ pred @@ bits_of_float f)

  let on_bit f pos =
    let mask = Int64.(shift_right 0x4000_0000_0000_0000L pos) in
    Int64.(float_of_bits @@ logor (bits_of_float f) mask)

  let dichotomy restore init a b =
    let rec approach f i =
      if i >= 63 then f else
        let tf = on_bit f i in
        if restore tf a b then approach f (i + 1) else approach tf (i + 1)
    in approach init 0

  let upper_neg_add x y =
    fsucc @@ dichotomy (fun f a b -> f +. a <= b) neg_zero x y

  let lower_neg_add = dichotomy (fun f a b -> f +. a < b) neg_zero

  let upper_pos_add = dichotomy (fun f a b -> f +. a > b) pos_zero

  let lower_pos_add x y =
    fsucc @@ dichotomy (fun f a b -> f +. a >= b) pos_zero x y

  let upper_neg_mult x y =
    fsucc @@ dichotomy (fun f a b -> f *. a <= b) neg_zero x y

  let lower_pos_mult_2 x y =
    fsucc @@ dichotomy (fun f a b -> f *. a <= b) pos_zero x y

  let lower_neg_mult =
    dichotomy (fun f a b -> f *. a < b) neg_zero

  let upper_pos_mult_2 =
    dichotomy (fun f a b -> f *. a < b) pos_zero

  let upper_pos_mult =
    dichotomy (fun f a b -> f *. a > b) pos_zero

  let lower_neg_mult_2 =
    dichotomy (fun f a b -> f *. a > b) neg_zero

  let lower_pos_mult x y =
    fsucc @@ dichotomy (fun f a b -> f *. a >= b) pos_zero x y

  let upper_neg_mult_2 x y =
    fsucc @@ dichotomy (fun f a b -> f *. a >= b) neg_zero x y

  (* smallest pos normalish such that there exists a number x such that
   [pos_cp +. x = infinity] *)
  let pos_cp = 9.9792015476736e+291
  let neg_cp = -9.9792015476736e+291

  let lower_bound a b =
    if a < b then lower_pos_add a b else lower_neg_add a b

  let upper_bound a b =
    if a > b then upper_neg_add a b else upper_pos_add a b

  let range_add al au bl bu =
    if bl = infinity then
      if au < pos_cp then None else
        let l = lower_pos_add au infinity in
        Some (l, max_float) else
    if bl = neg_infinity then
      if al > neg_cp then None else
        let u = upper_neg_add al neg_infinity in
        Some (-.max_float, u)
    else
      let l = lower_bound au bl
      and u = upper_bound al bu in
      if l > u then None else Some (l, u)

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

  let normalize = function
    | None -> None, None, None
    | Some (l, u) ->
      assert(l <= u);
      if l = 0.0 && u = 0.0 then Some (-0.0, 0.0), None, None else
      if l = 0.0 then Some (-0.0, 0.0), None, Some (smallest_pos, u) else
      if u = 0.0 then Some (-0.0, 0.0), Some (l, largest_neg), None else
      if u < 0.0 then None, Some (l, u), None else
        Some (-0.0, 0.0), Some (l, largest_neg), Some (smallest_pos, u)

end

module Refinement = struct

end

let top () =
  let a = Header.(allocate_abstract_float top) in
  set_pos a smallest_pos largest_pos;
  set_neg a smallest_neg largest_neg;
  a

(* *** Backwards functions *** *)

let debug = false

let get_neg_range hx x =
  if Header.(test hx negative_normalish) then
    let nl, nu = (-. get_opp_neg_lower x), get_neg_upper x in Some (nl, nu)
  else None

let get_pos_range hx x =
  if Header.(test hx positive_normalish) then
    let pl, pu = (-. get_opp_pos_lower x), get_pos_upper x in Some (pl, pu)
  else None

let set_zeros hx zs =
  let set f =
    if is_neg f then
      hx := Header.(set_flag !hx negative_zero)
    else
      hx := Header.(set_flag !hx positive_zero) in
  match zs with
  | Some (n1, n2) -> set n1; set n2
  | None -> ()

let reduce_pos_norm pl pu lx ux =
  if ux < pl || lx > pu then  None else
    Some (max pl lx, min pu ux)

let reduce_neg_norm nl nu lx ux =
  if ux < nl || lx > nu then None else
    Some (max nl lx, min nu ux)

let reduce_pos_norm_if_pos r hx x =
  match r with
  | None -> None
  | Some (pl, pu) ->
    if Header.(test hx positive_normalish) then
      let lx, ux = (-. get_opp_pos_lower x), get_pos_upper x in
      reduce_pos_norm pl pu lx ux
    else None

let reduce_neg_norm_if_neg r hx x =
  match r with
  | None -> None
  | Some (nl, nu) ->
    if Header.(test hx negative_normalish) then
      let lx, ux = (-. get_opp_neg_lower x), get_neg_upper x in
      reduce_neg_norm nl nu lx ux
    else None

let fold_range init l =
  let foldf acc cur =
    match acc, cur with
    | None, r | r, None -> r
    | Some (l1, u1), Some (l2, u2) -> Some (min l1 l2, max u1 u2) in
  List.fold_left foldf init l

let check_range ?msg (r1, r2) =
  if not debug then () else begin
  (match msg with
  | None -> ()
  | Some s -> Printf.printf "%s : " s);
  begin
    match r1 with
    | None -> print_string "None"
    | Some (l, u) ->
      (Printf.printf "{%.16e ... %.16e}"  l u);
      if not (u < 0. && l <= u) then assert false else ()
  end;
  print_string ", ";
  begin
    match r2 with
    | None -> print_endline "None"
    | Some (l, u) ->
      (Printf.printf "{%.16e ... %.16e\n}" l u);
      if not (l > 0. && l <= u) then assert false else ()
  end
  end

let reverse_add x a b =
  let a = if is_singleton a then expand a else a in
  let b = if is_singleton b then expand b else b in
  let x = if is_singleton x then expand x else x in
  let hx, ha, hb =
    Header.(of_abstract_float x, of_abstract_float a, of_abstract_float b) in
  let nhx = ref (Header.reverse_add ha hb) in (* only with pz, nz, pinf, ninf *)
  let pos_overflow =
    if Header.(test hx positive_normalish &&
               test ha positive_normalish &&
               test hb positive_inf) then
      let pl, pu = (-. get_opp_pos_lower a), get_pos_upper a in
      let zr, nr, pr =
        Dichotomy.(normalize @@ range_add pl pu infinity infinity) in
      set_zeros nhx zr;
      reduce_neg_norm_if_neg nr hx x, reduce_pos_norm_if_pos pr hx x
    else
      None, None in
  check_range ~msg:"pos_overflow" pos_overflow;
  let neg_overflow =
    if Header.(test hx negative_normalish &&
               test ha negative_normalish &&
               test hb negative_inf) then
      let nl, nu = (-. get_opp_neg_lower a), get_neg_upper a in
      let zr, nr, pr =
        Dichotomy.(normalize @@ range_add nl nu neg_infinity neg_infinity) in
      set_zeros nhx zr;
      reduce_neg_norm_if_neg nr hx x, reduce_pos_norm_if_pos pr hx x
    else
      None, None in
  check_range ~msg:"neg_overflow" neg_overflow;
  let both_inf =
    if Header.((test ha positive_inf && test hb positive_inf) ||
               (test ha negative_inf && test hb negative_inf)) then
      get_neg_range hx x, get_pos_range hx x
    else
      None, None in
  check_range ~msg:"both_inf" both_inf;
  let azbp =
    if Header.(test hb positive_normalish && has_zeros ha) then
      let pl, pu = (-. get_opp_pos_lower b), get_pos_upper b in
      None, reduce_pos_norm_if_pos (Some (pl, pu)) hx x
    else
      None, None in
  check_range ~msg:"azbp" azbp;
  let azbn =
    if Header.(test hb negative_normalish &&
               test hx negative_normalish && has_zeros ha) then
      let nl, nu = (-. get_opp_neg_lower b), get_neg_upper b in
      reduce_neg_norm_if_neg (Some (nl, nu)) hx x, None
    else
      None, None in
  check_range ~msg:"azbn" azbn;
  let bz =
    if Header.(test hb positive_zero) then
      (if Header.(test ha positive_normalish &&
                  test hx negative_normalish) then
        let nl, nu = (-. get_pos_upper a), get_opp_pos_lower a in
        let lx, ux = (-. get_opp_neg_lower x), get_neg_upper x in
        reduce_neg_norm nl nu lx ux else None),
      (if Header.(test ha negative_normalish &&
                  test hx positive_normalish) then
        let pl, pu = (-. get_neg_upper a), get_opp_neg_lower a in
        let lx, ux = (-. get_opp_pos_lower x), get_pos_upper x in
        reduce_pos_norm pl pu lx ux else None)
    else
      None, None in
  check_range ~msg:"bz" bz;
  let papb =
    if Header.((test ha positive_normalish && test hb positive_normalish))
    then
      let al, au = (-. get_opp_pos_lower a), get_pos_upper a in
      let bl, bu = (-. get_opp_pos_lower b), get_pos_upper b in
      let zr, nr, pr =
        Dichotomy.(normalize (range_add al au bl bu)) in
      set_zeros nhx zr;
      reduce_neg_norm_if_neg nr hx x, reduce_pos_norm_if_pos pr hx x
     else
      None, None in
  check_range ~msg:"papb" papb;
  let nanb =
    if Header.((test ha negative_normalish && test hb negative_normalish))
    then
      let al, au = (-. get_opp_neg_lower a), get_neg_upper a in
      let bl, bu = (-. get_opp_neg_lower b), get_neg_upper b in
      let zr, nr, pr =
        Dichotomy.(normalize (range_add al au bl bu)) in
      set_zeros nhx zr;
      reduce_neg_norm_if_neg nr hx x, reduce_pos_norm_if_pos pr hx x
    else
      None, None in
  check_range ~msg:"nanb" nanb;
  let panb =
    if Header.(test ha positive_normalish && test hb negative_normalish)
    then begin
      let al, au = (-. get_opp_pos_lower a), get_pos_upper a in
      let bl, bu = (-. get_opp_neg_lower b), get_neg_upper b in
      let zr, nr, pr =
        Dichotomy.(normalize (range_add al au bl bu)) in
      set_zeros nhx zr;
      reduce_neg_norm_if_neg nr hx x, reduce_pos_norm_if_pos pr hx x
    end
    else None, None in
  check_range ~msg:"panb" panb;
  let napb =
    if Header.(test ha negative_normalish && test hb positive_normalish)
    then begin
      let al, au = (-. get_opp_neg_lower a), get_neg_upper a in
      let bl, bu = (-. get_opp_pos_lower b), get_pos_upper b in
      let zr, nr, pr =
        Dichotomy.(normalize (range_add al au bl bu)) in
      set_zeros nhx zr;
      reduce_neg_norm_if_neg nr hx x, reduce_pos_norm_if_pos pr hx x
    end
    else None, None in
  check_range ~msg:"napb" napb;
  let l = [pos_overflow; neg_overflow; both_inf;
           azbp; azbn; bz; papb; nanb; panb; napb] in
  let restn, restp = List.map fst l, List.map snd l in
  let nr, pr = fold_range None restn, fold_range None restp in
  if debug then dump_internal (Header.allocate_abstract_float !nhx);
  nhx := Header.(narrow hx !nhx);
  if debug then dump_internal (Header.allocate_abstract_float !nhx);
  if nr <> None then nhx := Header.(set_flag !nhx negative_normalish);
  if pr <> None then nhx := Header.(set_flag !nhx positive_normalish);
  let a =
    if Header.(test hb at_least_one_NaN) then
      match Header.reconstruct_NaN x with
      | Header.No_NaN -> Header.allocate_abstract_float !nhx
      | Header.All_NaN ->
        nhx := Header.set_all_NaNs !nhx;
        Header.allocate_abstract_float !nhx
      | _ as r ->
        nhx := Header.(set_flag !nhx at_least_one_NaN);
        Header.allocate_abstract_float_with_NaN !nhx r
    else
      Header.(allocate_abstract_float !nhx) in
  (match nr with None -> () | Some (nl, nu) -> set_neg a nl nu);
  (match pr with None -> () | Some (pl, pu) -> set_pos a pl pu);
  normalize a

(* The set of values x such that x - a == b *)
(*
let reverse_sub x a b = reverse_add x (neg a) b
*)

(* The set of values x such that x * a == b *)
let reverse_mult x a b =
  let a = if is_singleton a then expand a else a in
  let b = if is_singleton b then expand b else b in
  let x = if is_singleton x then expand x else x in
  let hx, ha, hb =
    Header.(of_abstract_float x, of_abstract_float a, of_abstract_float b) in
  let nhx = Header.(narrow hx (reverse_mult ha hb)) in
  let pos_pinf_p =
    if Header.(test ha positive_normalish && test hb positive_inf) then
      let l, u = (-.get_opp_pos_lower a), (get_pos_upper a) in
      reduce_pos_norm_if_pos (Dichotomy.range_mult l u infinity infinity) hx x
    else None in
  let neg_pinf_n =
    if Header.(test ha negative_normalish && test hb positive_inf) then
      let l, u = (-.get_opp_neg_lower a), (get_neg_upper a) in
      reduce_neg_norm_if_neg (Dichotomy.range_mult l u infinity infinity) hx x
    else None in
  let pos_ninf_n =
    if Header.(test ha positive_normalish && test hb negative_inf) then
      let l, u = (-.get_opp_pos_lower a), (get_pos_upper a) in
      reduce_neg_norm_if_neg
        (Dichotomy.range_mult l u neg_infinity neg_infinity) hx x
    else None in
  let neg_ninf_p =
    if Header.(test ha negative_normalish && test hb negative_inf) then
      let l, u = (-.get_opp_neg_lower a), (get_neg_upper a) in
      reduce_pos_norm_if_pos
        (Dichotomy.range_mult l u neg_infinity neg_infinity) hx x
    else None in
  let pinf_pinf_p =
    if Header.(test ha positive_inf && test hb positive_inf) then
      get_pos_range hx x else None in
  let pinf_ninf_n =
    if Header.(test ha positive_inf && test hb negative_inf) then
      get_neg_range hx x else None in
  let ninf_pinf_n =
    if Header.(test ha negative_inf && test hb positive_inf) then
      get_neg_range hx x else None in
  let ninf_ninf_p =
    if Header.(test ha negative_inf && test hb negative_inf) then
      get_pos_range hx x else None in
  let pzpz_nznz_p =
    if Header.((test ha positive_zero && test hb positive_zero) ||
               (test ha negative_zero && test hb negative_zero)) then
      get_pos_range hx x else None in
  let nzpz_pznz_n =
    if Header.((test ha negative_zero && test hb positive_zero) ||
               (test ha positive_zero && test hb negative_zero)) then
      get_neg_range hx x else None in
  let pp_p =
    if Header.(test ha positive_normalish && test hb positive_normalish) then
      let al, au = (-.get_opp_pos_lower a), (get_pos_upper a) in
      let bl, bu = (-.get_opp_pos_lower b), (get_pos_upper b) in
      reduce_pos_norm_if_pos (Dichotomy.range_mult al au bl bu) hx x
    else None in
  let np_n =
    if Header.(test ha negative_normalish && test hb positive_normalish) then
      let al, au = (-.get_opp_neg_lower a), (get_neg_upper a) in
      let bl, bu = (-.get_opp_pos_lower b), (get_pos_upper b) in
      reduce_neg_norm_if_neg (Dichotomy.range_mult al au bl bu) hx x
    else None in
  let pn_n =
    if Header.(test ha positive_normalish && test hb negative_normalish) then
      let al, au = (-.get_opp_pos_lower a), (get_pos_upper a) in
      let bl, bu = (-.get_opp_neg_lower b), (get_neg_upper b) in
      reduce_neg_norm_if_neg (Dichotomy.range_mult al au bl bu) hx x
    else None in
  let nn_p =
    if Header.(test ha negative_normalish && test hb negative_normalish) then
      let al, au = (-.get_opp_neg_lower a), (get_neg_upper a) in
      let bl, bu = (-.get_opp_neg_lower b), (get_neg_upper b) in
      reduce_pos_norm_if_pos (Dichotomy.range_mult al au bl bu) hx x
    else None in
  let ppz_p =
    if Header.(test ha positive_normalish && test hb positive_zero) then
      let al, au = (-.get_opp_pos_lower a), (get_pos_upper a) in
      reduce_pos_norm_if_pos (Dichotomy.range_mult al au 0.0 0.0) hx x
    else None in
  let npz_n =
    if Header.(test ha negative_normalish && test hb positive_zero) then
      let al, au = (-.get_opp_neg_lower a), (get_neg_upper a) in
      reduce_neg_norm_if_neg (Dichotomy.range_mult al au (0.0) (0.0)) hx x
    else None in
  let nnz_p =
    if Header.(test ha negative_normalish && test hb negative_zero) then
      let al, au = (-.get_opp_neg_lower a), (get_neg_upper a) in
      reduce_pos_norm_if_pos (Dichotomy.range_mult al au (-0.0) (-0.0)) hx x
    else None in
  let pnz_n =
    if Header.(test ha positive_normalish && test hb negative_zero) then
      let al, au = (-.get_opp_pos_lower a), (get_pos_upper a) in
      reduce_neg_norm_if_neg (Dichotomy.range_mult al au (-0.0) (-0.0)) hx x
    else None in
  let nrs = [neg_pinf_n; ninf_pinf_n; pos_ninf_n; pinf_ninf_n; ninf_pinf_n;
             nzpz_pznz_n; np_n; pn_n; npz_n; pnz_n] in
  let prs = [pos_pinf_p; pinf_pinf_p; neg_ninf_p; ninf_ninf_p; pinf_pinf_p;
             pzpz_nznz_p; pp_p; nn_p; ppz_p; nnz_p] in
  let nr = fold_range None nrs in
  let pr = fold_range None prs in
  let nhx =
    if nr <> None then Header.(set_flag nhx negative_normalish) else nhx in
  let nhx =
    if pr <> None then Header.(set_flag nhx positive_normalish) else nhx in
  let a =
    if Header.(test hb at_least_one_NaN) then
      match Header.reconstruct_NaN x with
      | Header.No_NaN -> Header.allocate_abstract_float nhx
      | Header.All_NaN ->
        Header.(allocate_abstract_float (set_all_NaNs nhx))
      | _ as r ->
        let nhx = Header.(set_flag nhx at_least_one_NaN) in
        Header.allocate_abstract_float_with_NaN nhx r
    else
      Header.(allocate_abstract_float nhx) in
  (match nr with None -> () | Some (nl, nu) -> set_neg a nl nu);
  (match pr with None -> () | Some (pl, pu) -> set_pos a pl pu);
  normalize a

(* The set of values x such that x / a == b *)
let reverse_div1 a b =
  assert false

(* The set of values x such that a / x == b *)
let reverse_div2 a b =
  assert false
