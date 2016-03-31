open Float

let ppa a =
  Format.asprintf "%a\n" pretty a

let ppf f =
  Printf.printf "%.16e\n\n" f

let dump_af a =
  let l = Array.length a in
  Format.printf "[|";
  for i = 0 to l - 1 do
    if i = 0 || l = 2
    then Format.printf "0x%016Lx" (Int64.bits_of_float a.(i))
    else Format.printf "%.16e" a.(i);
    if i < l - 1
    then Format.printf ","
  done;
  Format.printf "|]@\n";

module RandomAF = struct

  let () = Random.self_init ()

  let shuffle a =
    for i = pred (Array.length a) downto 1 do
      let j = Random.int (succ i) in
      if i <> j then (
        let tmp = Array.unsafe_get a i in
        Array.unsafe_set a i (Array.unsafe_get a j);
        Array.unsafe_set a j tmp
      )
    done

  let flags = Header.([|negative_inf; positive_inf;
                        negative_zero; positive_zero|])

  let cnter = ref 0

  let shuffle_flags () =
    cnter := 0;
    shuffle flags

  let get_flag () =
    let f = flags.(!cnter) in
    incr cnter;
    f

  let rand_set_flag h =
    let f = get_flag () in
    if Random.bool () then
      Header.(set_flag h f)
    else h

  let rand_n_flags h n =
    let rec loop h i =
      if i = n then h
      else loop (rand_set_flag h) (i + 1) in
    loop h 0

  (* not really random, but we should write in this way *)
  let random_NaN () =
    if Random.bool () then Int64.float_of_bits 0x7FF0000024560001L else
    if Random.bool () then Int64.float_of_bits 0xFFF0000005743001L else
       Int64.float_of_bits 0x7FF1234569876121L

  let rand_add_NaNs h =
    match Random.int 3 with
    | 0 -> Header.(allocate_abstract_float_with_NaN h No_NaN)
    | 1 -> let h = Header.(set_flag h at_least_one_NaN) in
      let n = Int64.bits_of_float (random_NaN ()) in
      Header.(allocate_abstract_float_with_NaN h (One_NaN n))
    | _ -> let h = Header.set_all_NaNs h in
      Header.(allocate_abstract_float_with_NaN h All_NaN)

  let random_pos_normalish () =
    match Random.int 15 with
    | 0 -> min_float
    | 1 -> max_float
    | 2 -> Random.float min_float
    | 3 | 4 -> Random.float max_float
    | 5 -> Random.float 2e-308
    | 6 -> 2e-308
    | _ -> Random.float 1_000_00.

  let random_float () =
    match Random.int 7 with
    | 0 -> neg_infinity
    | 1 -> -. (random_pos_normalish ())
    | 2 -> -0.0
    | 3 -> +0.0
    | 4 -> random_pos_normalish ()
    | 5 -> infinity
    | _ -> random_NaN ()

  let random_pos_range () =
    let u = random_pos_normalish () in
    Random.float u, u

  let random_neg_range () =
    let l, u = random_pos_range () in
    (-.u, -.l)

  let random_abstract_float () =
    shuffle_flags ();
    match Random.int 5 with
    | 0 -> let f = random_float () in
      inject_float (if Random.bool () then f else (-. f))
    | 1 -> begin
      match Random.int 3 with
      | 0 ->
        let h = Header.(set_flag bottom (get_flag ())) in
        let h = Header.(set_flag h (get_flag ())) in
        let h = rand_n_flags h 2 in
        Header.(allocate_abstract_float_with_NaN h No_NaN)
      | 1 ->
        let h = Header.(set_flag bottom (get_flag ())) in
        let h = rand_n_flags h 3 in
        let h = Header.(set_flag h at_least_one_NaN) in
        let rNaN = Int64.bits_of_float (random_NaN ()) in
        Header.(allocate_abstract_float_with_NaN h (One_NaN rNaN))
      | _ ->
        let h = rand_n_flags Header.bottom 4 in
        let h = Header.set_all_NaNs h in
        Header.(allocate_abstract_float_with_NaN h All_NaN)
      end
    | 2 -> begin
        let h = Header.(set_flag bottom positive_normalish) in
        let a = rand_add_NaNs (rand_n_flags h 4) in
        let l, u = random_pos_range () in
        set_pos a l u; a
      end
    | 3 -> begin
        let h = Header.(set_flag bottom negative_normalish) in
        let a = rand_add_NaNs (rand_n_flags h 4) in
        let l, u = random_neg_range () in
        set_neg a l u; a
      end
    | _ -> begin
        let h = Header.(set_flag bottom negative_normalish) in
        let h = Header.(set_flag h positive_normalish) in
        let a = rand_add_NaNs (rand_n_flags h 4) in
        let l, u = random_pos_range () in
        set_pos a l u;
        let l, u = random_neg_range () in
        set_neg a l u; a
      end

  let random_select (a:abstract_float) : float =
    match Array.length a with
    | 1 -> a.(0)
    | _ ->
      let h = Header.of_abstract_float a in
      let all_flags =
        Header.([at_least_one_NaN; all_NaNs; negative_normalish;
                 positive_normalish; negative_inf; positive_inf;
                 negative_zero; positive_zero]) in
      let eflgs = List.fold_left (fun acc f ->
          if Header.test h f then f :: acc else acc) [] all_flags in
      let fa = Array.of_list eflgs in
      let rflg = fa.(Random.int (Array.length fa)) in
      if Header.(equal rflg all_NaNs) then random_NaN () else
      if Header.(equal rflg at_least_one_NaN) then
        match Header.reconstruct_NaN a with
        | Header.One_NaN n -> (Int64.float_of_bits n)
        | Header.All_NaN -> random_NaN ()
        | _ -> assert false else begin
      if Header.(equal rflg negative_normalish) then
        let l, u = (-. (get_opp_neg_lower a)), get_neg_upper a in
        match Random.int 10 with
        | 0 -> l
        | 1 -> u
        | _ -> l +. Random.float (u -. l) else
      if Header.(equal rflg positive_normalish) then
        let l, u = (-. (get_opp_pos_lower a)), get_pos_upper a in
        match Random.int 10 with
        | 0 -> l
        | 1 -> u
        | _ -> l +. Random.float (u -. l) else
      if Header.(equal rflg negative_inf) then neg_infinity else
      if Header.(equal rflg positive_inf) then infinity else
      if Header.(equal rflg negative_zero) then -0.0 else
      if Header.(equal rflg positive_zero) then +0.0 else
        assert false
      end

  let test_validity () =
    for i = 0 to 1_000_000_000 do
      assert(Header.check (random_abstract_float ()))
    done;
    print_endline "RandomAF checked"

  let random_AF_pair () =
    let af = random_abstract_float () in
    if Random.int 20 < 1 then af, af else af, random_abstract_float ()

end

module TestNeg = struct

  let test () =
    let a = RandomAF.random_abstract_float () in
    let f = RandomAF.random_select a in
    let nf = (-. f) in
    let na = neg a in
    assert(float_in_abstract_float nf na)

  let test_rand () =
    print_endline "Neg: start random tests";
    for i = 0 to 1_000_000 do
      test ()
    done;
    print_endline "Neg: random tests successful"

end

module TestJoins = struct

  let test_rand () =
    print_endline "Join: start random tests";
    for i = 0 to 100000 do
      let a1, a2 = RandomAF.random_AF_pair () in
      let a12 = join a1 a2 in
      let a21 = join a2 a1 in
      assert(Header.check a12);
      assert(Header.check a21);
      assert(compare a12 a21 = 0);
      assert(is_included a1 a12);
      assert(is_included a2 a12);
    done;
    for i = 0 to 100 do
      let a1 = RandomAF.random_abstract_float () in
      let f = RandomAF.random_select a1 in
      assert (float_in_abstract_float f a1)
    done;
    print_endline "Join: random tests successful"

  let test_others () =
    let a = inject_float (2e-308) in
    assert(Header.check (join a a));
    let h = Header.(set_flag (of_flag positive_zero) negative_inf) in
    let a = Header.allocate_abstract_float h in
    assert(not @@ float_in_abstract_float nan a)

  (* bug 1: RH's bug in ``merge_float``. Fixed.
     bug 2: opened issue on PC's repo. Fixed here. *)
  let test_bugged1 () =
    let h = Header.(set_flag bottom positive_inf) in
    let h = Header.(set_flag h negative_zero) in
    let a1 = Header.(allocate_abstract_float h) in
    let a2 = [|-1.5262258223503298e+3|] in
    assert(Header.check (join a1 a2))

  (* RH's bug in ``merge_float``. fixed *)
  let test_bugged2 () =
    let h = Header.(set_flag bottom positive_normalish) in
    let a1 = Header.allocate_abstract_float h in
    set_pos a1 4.6307227104214865e+307 9.3849706504711684e+307;
    let a2 = [|-2.2396553445019165e+307|] in
    assert (Header.check (join a1 a2))

end

let () = TestJoins.test_others ()
let () = TestJoins.test_rand ()

module TestMeet = struct

  let test () =
    let a1, a2 = RandomAF.random_AF_pair () in
    let ma = meet a1 a2 in
    assert(is_included ma a1 && is_included ma a2)

  let test_rand () =
    print_endline "Meet: start random tests";
    for i = 0 to 1_000_00 do
      test ()
    done;
    print_endline "Meet: random tests successful"

end

let () = TestMeet.test_rand ()

module TestSqrt = struct

  let test_rand () =
    print_endline "Sqrt: start random tests";
    for i = 0 to 1_000_00 do
      let a = RandomAF.random_abstract_float () in
      let f1 = RandomAF.random_select a in
      assert (float_in_abstract_float (sqrt f1) (abstract_sqrt a))
    done;
    print_endline "Sqrt: random tests successful"

end

let () = TestSqrt.test_rand ()

module TestArithmetic = struct

  let test op1 op2 a1 a2 =
    let srf = RandomAF.random_select in
    let a12 = op2 a1 a2 in
    assert(Header.check a12);
    for i = 0 to 100000 do
      let rf1, rf2 = srf a1, srf a2 in
      let rf12 = op1 rf1 rf2 in
      if not (float_in_abstract_float rf12 a12)
      then begin
        Format.printf "%a\n%a\n%a\n\n%.16e\n%.16e\n%.16e\n"
        pretty a1 pretty a2 pretty a12
        rf1 rf2 rf12;
        assert false;
    end;
    done

  let test_rand () =
    print_endline "Arithmetic: start random tests";
    for i = 0 to 10 do
      let a1, a2 = RandomAF.random_AF_pair () in
      test ( +. ) add a1 a2;
      test ( -. ) sub a1 a2;
      test ( *. ) mult a1 a2;
      test ( /. ) div a1 a2

    done;
    print_endline "Arithmetic: random tests successful"

end

let () = TestArithmetic.test_rand ()

module TestPretty = struct

  let test_rand () =
    print_endline "Pretty: start random tests";
    for i = 0 to 1_000_00 do
      let a1 = RandomAF.random_abstract_float () in
      ignore (ppa a1)
    done;
    print_endline "Pretty: random tests successful"

end

let () = TestPretty.test_rand ()

module TestReverseAdd = struct

  let debug = false

  let test () =
    let a, b = RandomAF.random_AF_pair () in
    let x = RandomAF.random_abstract_float () in
    let nx = reverse_add x a b in
    if debug then begin
    print_endline (String.make 15 '-');
    Format.printf "a:  %a\nb:  %a\nx:  %a\n"
      pretty a pretty b pretty x end;
    assert(Header.check nx);
    if debug then Format.printf "x': %a\n" pretty nx;
    if (not (is_included nx x)) then begin
      dump_af x; dump_af nx; assert false
    end else ()

  let test_rand () =
    print_endline "ReverseAdd: start random tests";
    for i = 0 to 100000 do
      test ()
    done;
    print_endline "ReverseAdd: random tests successful"

  (* bug in join: joining two single NaNs *)
  let test_bug1 () =
    let a = [| 0.0 |] in
    let b =
      let h = Header.(set_all_NaNs bottom) in
      let h = Header.(set_flag h positive_inf) in
      let h = Header.(set_flag h negative_inf) in
      let h = Header.(set_flag h negative_normalish) in
      let a = Header.allocate_abstract_float h in
      set_neg a (-5.) (-4.);
      a in
    let x = [| Int64.float_of_bits 0x7ff0000024560001L |] in
    let nx = reverse_add x a b in
    Format.printf "a:  %a\nb:  %a\nx:  %a\n"
      pretty a pretty b pretty x;
    assert(Header.check nx);
    Format.printf "x': %a\n" pretty nx;
    if (not (is_included nx x)) then begin
      dump_af x; dump_af nx; assert false
    end else ()

  (* bug in narrow_range: should not use meet *)
  let test_bug2 () =
    let a = [|1.|] in
    let h = Header.(set_all_NaNs bottom) in
    let h = Header.(set_flag h positive_normalish) in
    let b = Header.allocate_abstract_float h in
    set_pos b 2. 3.;
    let x = [| 1.8401032236488259e-308 |] in
    let nx = reverse_add x a b in
    Format.printf "a:  %a\nb:  %a\nx:  %a\n"
      pretty a pretty b pretty x;
    assert(Header.check nx);
    Format.printf "x': %a\n" pretty nx;
    if (not (is_included nx x)) then begin
      dump_af x; dump_af nx; assert false
    end else ()

end

let () = TestReverseAdd.test_rand ()
