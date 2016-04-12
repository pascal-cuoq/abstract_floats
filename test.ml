open Float

let ppa fmt a =
  Format.fprintf fmt "%a\n" pretty a

let ppf fmt f =
  Format.fprintf fmt "%.16e" f

let bits_eq f1 f2 =
  Int64.bits_of_float f1 = Int64.bits_of_float f2

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

  let fsucc f = Int64.(float_of_bits @@ succ @@ bits_of_float f)
  let fpred f = Int64.(float_of_bits @@ pred @@ bits_of_float f)

  let random_pos_normalish () =
    match Random.int 15 with
    | 0 -> min_float
    | 1 -> max_float
    | 2 -> Random.float min_float
    | 3 | 4 -> Random.float max_float
    | 5 -> Random.float 2e-308
    | 6 -> 2e-308
    | 7 -> begin
        match Random.int 3 with
        | 0 -> Dichotomy.pos_cp
        | 1 -> fsucc Dichotomy.pos_cp
        | _ -> fpred Dichotomy.pos_cp
      end
    | 9 -> fsucc Dichotomy.pos_cp
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

  let abstract_float () =
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

  let select (a:abstract_float) : float =
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
      assert(Header.check (abstract_float ()))
    done;
    print_endline "RandomAF checked"

  let pair () =
    let af = abstract_float () in
    if Random.int 20 < 1 then af, af else af, abstract_float ()

  let pos_range a =
    (-. get_opp_pos_lower a), get_pos_upper a

  let neg_range a =
    (-. get_opp_neg_lower a), get_neg_upper a

  (* for pos *)
  let random_float l u =
    if l = smallest_neg && u = largest_neg
      then l +. (Random.float (largest_neg -. l)) else
    if l = smallest_pos && u = largest_pos
      then l +. (Random.float (largest_pos -. l)) else
    try (
    Int64.(float_of_bits (add (bits_of_float l)
          (Random.int64 (succ (sub (bits_of_float u) (bits_of_float l)))))))
    with _ -> Format.printf "%a %a\n" ppf l ppf u; assert false

  let random_float l u =
    if u < 0. then -.(random_float (-.u) (-.l)) else
      random_float l u

  let fsucc_ f = Int64.(float_of_bits @@ succ @@ bits_of_float f)
  let fpred_ f = Int64.(float_of_bits @@ pred @@ bits_of_float f)

  let fsucc f = if is_pos f then fsucc_ f else fpred_ f
  let fpred f = if is_pos f then fpred_ f else fsucc_ f

  let random_float_without_f l u f =
    if f < l || f > u then Some (fun () -> random_float l u) else begin
      if l = u then None else
      if l = f then Some (fun () -> random_float (fsucc f) u) else
      if u = f then Some (fun () -> random_float l (fpred f)) else
        Some (fun () ->
            if Random.bool () then random_float l (fpred f) else
              random_float (fsucc f) u)
    end

  let random_float_without_range l u l1 u1 =
    if u1 < l || l1 > u then Some (fun () -> random_float l u) else begin
      if l = u then None else
      if l1 <= l then
        let u1s = fsucc u1 in
        if u1s > u then None else
          Some (fun () -> random_float (fsucc u1) u) else
      if u1 >= u then
        let l1p = fpred l1 in
        if l1p < l then None else Some (fun () -> random_float l l1p)
      else
        Some (fun () ->
            if Random.bool () then random_float l (fpred l1)
            else random_float (fsucc u1) u)
    end

  let diff_selector x nx =
    if is_singleton x then
      let f = Array.get x 0 in
      if Array.length nx = 2 then
        let h = Header.of_abstract_float nx in
        if Header.is_bottom h then
          Some (fun () -> f)
        else
          None
      else None
    else begin
      let h = Header.of_abstract_float x in
      let p =
        if Header.(test h positive_normalish) then
          let l, u = pos_range x in
          if is_singleton nx then
            let f = Array.get nx 0 in
            match classify_float f with
            | FP_normal | FP_subnormal ->
              random_float_without_f l u f
            | _ -> Some (fun () -> random_float l u)
          else
            let nh = Header.of_abstract_float nx in
            if not Header.(test nh positive_normalish) then
              Some (fun () -> random_float l u)
            else
              let ln, un = pos_range nx in
              random_float_without_range l u ln un
        else None in
      let n =
        if Header.(test h negative_normalish) then
          let l, u = neg_range x in
          if is_singleton nx then
            let f = Array.get nx 0 in
            match classify_float f with
            | FP_normal | FP_subnormal ->
              random_float_without_f l u f
            | _ -> Some (fun () -> random_float l u)
          else
            let nh = Header.of_abstract_float nx in
            if not Header.(test nh negative_normalish) then
              Some (fun () -> random_float l u)
            else None
        else None in
      let select_header p f =
        if Header.(test h p) then
          if is_singleton nx then
            if nx.(0) <> f then Some (fun () -> f) else None
          else
            let nh = Header.of_abstract_float nx in
            if Header.(test nh p) then None else Some (fun () -> f)
        else None in
      let pz, nz, pinf, ninf =
        select_header Header.positive_zero 0.0,
        select_header Header.negative_zero (-0.0),
        select_header Header.positive_inf infinity,
        select_header Header.negative_inf neg_infinity in
      let _NaN =
        match Header.reconstruct_NaN x with
        | Header.All_NaN -> begin
          if is_singleton nx then
            if classify_float nx.(0) = FP_nan then
              Some (fun () -> Int64.float_of_bits 0x7FF1234569876222L)
            else
              Some (fun () -> random_NaN ())
          else
            match Header.reconstruct_NaN nx with
            | Header.No_NaN -> Some (fun () -> random_NaN ())
            | Header.All_NaN -> None
            | Header.One_NaN _ ->
              Some (fun () ->Int64.float_of_bits 0x7FF1234569876222L)
          end
        | _ -> None in
      let rec filter_map acc = function
        | [] -> acc
        | Some x :: tl -> filter_map (x :: acc) tl
        | None :: tl -> filter_map acc tl in
      let sources =
        filter_map [] [p; n; pz; nz; pinf; ninf; _NaN] |> Array.of_list in
      let len = Array.length sources in
      if sources = [||] then None else
        Some (fun () -> Array.get sources (Random.int len) ())
    end

end

module TestNeg = struct

  let test () =
    let a = RandomAF.abstract_float () in
    let f = RandomAF.select a in
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
    for i = 0 to 1000000 do
      let a1, a2 = RandomAF.pair () in
      let a12 = join a1 a2 in
      let a21 = join a2 a1 in
      assert(Header.check a12);
      assert(Header.check a21);
      assert(compare a12 a21 = 0);
      assert(is_included a1 a12);
      assert(is_included a2 a12);
    done;
    for i = 0 to 1000000 do
      let a1 = RandomAF.abstract_float () in
      let f = RandomAF.select a1 in
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


module TestMeet = struct

  let test () =
    let a1, a2 = RandomAF.pair () in
    let ma = meet a1 a2 in
    assert(is_included ma a1 && is_included ma a2)

  let test_rand () =
    print_endline "Meet: start random tests";
    for i = 0 to 1000000 do
      test ()
    done;
    print_endline "Meet: random tests successful"

end


module TestSqrt = struct

  let test_rand () =
    print_endline "Sqrt: start random tests";
    for i = 0 to 1000000 do
      let a = RandomAF.abstract_float () in
      let f1 = RandomAF.select a in
      assert (float_in_abstract_float (sqrt f1) (abstract_sqrt a))
    done;
    print_endline "Sqrt: random tests successful"

end


module TestArithmetic = struct

  let test op1 op2 a1 a2 =
    let srf = RandomAF.select in
    let a12 = op2 a1 a2 in
    if not (Header.check a12) then begin
      dump_af a1; dump_af a2;
      Format.printf "a1: %a\na2: %a\n" pretty a1 pretty a2;
      assert false
    end;
    for i = 0 to 1000 do
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

  let regress_add1 () =
    let srf = RandomAF.select in
    let a = inject_float (-2.9914765924740740e+307) in
    let hb = Header.(of_flags [negative_normalish; positive_zero]) in
    let b = Header.allocate_abstract_float hb in
    set_neg b (-9.4771152698724269e+04) (-2.7803418452892169e+04);
    let a12 = add a b in
    for i = 0 to 100 do
      let rf1, rf2 = srf a, srf b in
      let rf12 = rf1 +. rf2 in
      if not (float_in_abstract_float rf12 a12)
      then begin
        Format.printf "%a\n%a\n%a\n\n%.16e\n%.16e\n%.16e\n"
        pretty a pretty b pretty a12
        rf1 rf2 rf12;
        assert false;
      end
    done

  let test_rand () =
    print_endline "Arithmetic: start random tests";
    for i = 0 to 10000 do
      let a1, a2 = RandomAF.pair () in
      test ( +. ) add a1 a2;
      test ( -. ) sub a1 a2;
      test ( *. ) mult a1 a2;
      test ( /. ) div a1 a2

    done;
    print_endline "Arithmetic: random tests successful"

end

module TestPretty = struct

  let test_rand () =
    print_endline "Pretty: start random tests";
    for i = 0 to 1_000_00 do
      let a1 = RandomAF.abstract_float () in
      ignore (Format.asprintf "%a" pretty a1)
    done;
    print_endline "Pretty: random tests successful"

end

module TestReverseAdd = struct

  let debug = false

  let test x a b =
    if debug then begin
      print_endline (String.make 15 '-');
      Format.printf "a: %a\nb: %a\n" pretty a pretty b;
      Format.printf "x:  %a\n" pretty x end;
    let nx = reverse_add x a b in
    assert(Header.check nx);
    if debug then Format.printf "x': %a\n" pretty nx;
    if (not (is_included nx x)) then begin
      dump_af x; dump_af nx; assert false
    end;
    match RandomAF.diff_selector x nx with
    | None ->
      if not (is_included nx x) then
        (dump_internal x; dump_internal nx; assert false)
      else ()
    | Some f -> begin
        for i = 0 to 1000 do
          let fa, fb = RandomAF.(select a, select b) in
          let nxf = f () in
          if bits_eq (nxf +. fa) fb then begin
          Format.printf "%s\n" (String.make 10 '~');
          Format.printf "x : %a\nx': %a\na : %a\nb : %a\n\n"
            pretty x pretty nx pretty a pretty b;
          Format.printf "fx': %a\nfa : %a\nfb : %a\n\n"
            ppf nxf ppf fa ppf fb;
          assert false
          end
        done
      end

  let test_rand () =
    print_endline "ReverseAdd: start random tests";
    for i = 0 to 100000 do
      let a, b = RandomAF.pair () in
      let x = RandomAF.abstract_float () in
      test x a b
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
    test x a b

  (* bug in narrow_range: should not use meet *)
  let test_bug2 () =
    let a = [|1.|] in
    let h = Header.(set_all_NaNs bottom) in
    let h = Header.(set_flag h positive_normalish) in
    let b = Header.allocate_abstract_float h in
    set_pos b 2. 3.;
    let x = [| 1.8401032236488259e-308 |] in
    test x a b

  let test_bug3 () =
    let ha = Header.(set_all_NaNs (of_flag negative_zero)) in
    let ha = Header.(set_flag ha positive_normalish) in
    let a = Header.allocate_abstract_float ha in
    set_pos a 3.7286649853047806e+04 5.2488920238158935e+04;
    let hb = Header.(set_flag (of_flag positive_inf) positive_zero) in
    let hb = Header.(set_flag hb negative_zero) in
    let hb = Header.(set_flag hb positive_normalish) in
    let b = Header.allocate_abstract_float hb in
    set_pos b 6.2039083778571396e+307 1.1710442810843075e+308;
    let hx =
      Header.(set_flag (of_flag positive_zero) negative_zero) in
    let hx =
      Header.(set_flag hx positive_inf) in
    let hx =
      Header.(set_flag hx negative_inf) in
    let hx = Header.(set_all_NaNs hx) in
    let x = Header.allocate_abstract_float hx in
    test x a b

  (* fixed, Header.reverse_add first true branch *)
  let test_bug4 () =
    let ha = Header.(set_flag (of_flag negative_inf) positive_zero) in
    let ha = Header.(set_all_NaNs ha) in
    let a = Header.allocate_abstract_float ha in
    let hb = Header.(set_flag (of_flag negative_inf) positive_zero) in
    let hb = Header.(set_flag hb at_least_one_NaN) in
    let hb = Header.(set_flag hb positive_normalish) in
    let b = Header.allocate_abstract_float_with_NaN hb
        (Header.One_NaN (0xfff0000005743001L)) in
    set_pos b 2.3072254309300213e+04 3.6047259748806195e+04;
    let hx = Header.(set_flag (of_flag positive_zero) negative_zero) in
    let hx = Header.(set_flag hx positive_inf) in
    let hx = Header.(set_all_NaNs hx) in
    let hx = Header.(set_flag hx negative_normalish) in
    let x = Header.allocate_abstract_float hx in
    set_neg x (-2.5895361791812356e+04) (-1.7694993162971678e+04);
    test x a b

  (* bug in reverse_add, neg_overflow *)
  let test_bug5 () =
    let ha =
      Header.(of_flags [negative_zero; negative_inf;
                        at_least_one_NaN; negative_normalish]) in
    let a = Header.(allocate_abstract_float_with_NaN
                     ha (One_NaN 0x7ff0000024560001L)) in
    set_neg a (-1.5108707789796620e+308) (-1.8130866911409611e+307);
    let hb =
      Header.(of_flags [positive_zero; positive_inf; negative_inf]) in
    let hb = Header.(set_all_NaNs hb) in
    let b = Header.allocate_abstract_float hb in
    let hx =
      Header.(of_flags [positive_zero; negative_zero;
                        positive_inf; negative_normalish;
                        positive_normalish]) in
    let x = Header.allocate_abstract_float hx in
    set_neg x (-4.2308300236767202e+04) (-1.4184781505518549e+04);
    set_pos x 5.4694215793246267e-309 2.2250738585072014e-308;
    test x a b

  (* fixed in fold_range *)
  let test_bug6 () =
    let ha = Header.(set_flag (of_flag positive_inf) positive_normalish) in
    let a = Header.allocate_abstract_float ha in
    set_pos a 2.0677600541151282e+04 9.7177780825627851e+04;
    let hb =
      Header.(of_flags [negative_zero; positive_zero;
                        positive_inf; positive_normalish]) in
    let hb = Header.(set_all_NaNs hb) in
    let b = Header.allocate_abstract_float hb in
    set_pos b 1.1185516275125372e-308 1.1844009154295606e-308;
    let hx = Header.(set_all_NaNs (of_flags [positive_zero;
                                             negative_normalish])) in
    let x = Header.allocate_abstract_float hx in
    set_neg x (-2.6989508761250097e+04) (-3.9173820305161257e+03);
    test x a b

  let test_bug7 () =
    let ha = Header.(set_all_NaNs (of_flag positive_zero)) in
    let hb = Header.(of_flags [negative_zero; positive_inf; negative_inf]) in
    let hx = Header.(of_flags [negative_zero; positive_inf]) in
    let hx = Header.(set_flag hx at_least_one_NaN) in
    let x = Header.(allocate_abstract_float_with_NaN
                      hx (One_NaN 0x7ff0000024560001L)) in
    let a = Header.allocate_abstract_float ha in
    let b = Header.allocate_abstract_float hb in
    test x a b

  (* fix range combine, fix zero setting *)
  let test_bug8 () =
    let x = inject_float 0.0 in
    let a = inject_float 2.2250738585072014e-308 in
    let b = inject_float 2.2250738585072014e-308 in
    test x a b

  let test_bug9 () =
    let hx = Header.(of_flag positive_normalish) in
    let x = Header.allocate_abstract_float hx in
    set_pos x 3.0707164255425355e+03 7.8646307536638469e+03;
    let ha = Header.(set_flag (of_flag positive_normalish) positive_zero) in
    let a = Header.allocate_abstract_float ha in
    let b = Header.allocate_abstract_float ha in
    set_pos a 9.9928593609454120e+305 4.1172212759061929e+306;
    set_pos b 9.9928593609454120e+305 4.1172212759061929e+306;
    test x a b

  let test_bug10 () =
    let x = inject_float 1.0 in
    let h = Header.(of_flag positive_normalish) in
    let a = Header.allocate_abstract_float h in
    let b = Header.allocate_abstract_float h in
    set_pos a 0.4 1.8;
    set_pos b 0.4 1.8;
    test x a b

  let ntest1 () =
    let x = top () in
    let a = inject_float 0.4 in
    let b = inject_float 1.8 in
    test x a b

  let ntest2 () =
    let x = Header.(allocate_abstract_float (of_flag positive_normalish)) in
    set_pos x 1.0 5.0;
    let a = inject_float 1.0 in
    let b = inject_float 11.0 in
    test x a b

  let test_norm_all () =
    test_bug1 ();
    test_bug2 ();
    test_bug3 ();
    test_bug4 ();
    test_bug5 ();
    test_bug6 ();
    test_bug7 ();
    test_bug8 ();
    test_bug9 ();
    test_bug10 ();
    ntest1 ();
    ntest2 ()

end

module TestReverseMult = struct

  let debug = false

  let test x a b =
    if debug then begin
      print_endline (String.make 15 '-');
      Format.printf "a: %a\nb: %a\n" pretty a pretty b;
      Format.printf "x:  %a\n" pretty x end;
    let nx = reverse_mult x a b in
    assert(Header.check nx);
    if debug then Format.printf "x': %a\n" pretty nx;
    if (not (is_included nx x)) then begin
      dump_af x; dump_af nx; assert false
    end;
    match RandomAF.diff_selector x nx with
    | None ->
      if not (is_included nx x) then
        (dump_internal x; dump_internal nx; assert false)
      else ()
    | Some f -> begin
        for i = 0 to 1000 do
          let fa, fb = RandomAF.(select a, select b) in
          let nxf = f () in
          if bits_eq (nxf *. fa) fb then begin
          Format.printf "%s\n" (String.make 10 '~');
          Format.printf "x : %a\nx': %a\na : %a\nb : %a\n\n"
            pretty x pretty nx pretty a pretty b;
          Format.printf "fx': %a\nfa : %a\nfb : %a\n\n"
            ppf nxf ppf fa ppf fb;
          assert false
          end
        done
      end

  let test_rand () =
    print_endline "ReverseMult: start random tests";
    for i = 0 to 100000 do
      let a, b = RandomAF.pair () in
      let x = RandomAF.abstract_float () in
      test x a b
    done;
    print_endline "ReverseMult: random tests successful"

end


let test_other () =
  let h = Header.(set_flag (of_flag negative_normalish) negative_zero) in
  let h = Header.(set_all_NaNs h) in
  let a = Header.allocate_abstract_float h in
  let b = Header.allocate_abstract_float h in
  set_neg a (-3.6330785545677325e+04) (-7.0799234305157606e+03);
  set_neg b (-3.6330785545677325e+04) (-3.6330785545677325e+04);
  assert(is_included b a)

let test_other1 () =
  let hx = Header.(of_flags [negative_zero; positive_inf; at_least_one_NaN]) in
  let nhx = Header.(of_flags [negative_zero; positive_inf]) in
  let x = Header.(allocate_abstract_float_with_NaN hx (One_NaN 0x7ff1234569876121L)) in
  let nx = Header.allocate_abstract_float nhx in
  assert(is_included nx x)

let test_join = true
let test_meet = true
let test_sqrt = true
let test_arith = true
let test_pretty = true
let test_reverse = true

(*
let () = TestArithmetic.regress_add1 ()
let () = if test_join then TestJoins.(test_others (); test_rand ())
let () = if test_meet then TestMeet.test_rand ()
let () = if test_sqrt then TestSqrt.test_rand ()
let () = if test_arith then TestArithmetic.test_rand ()
let () = if test_pretty then TestPretty.test_rand ()
let () = if test_reverse then TestReverseAdd.(test_norm_all (); test_rand ())
*)

let () = TestReverseMult.test_rand ()
