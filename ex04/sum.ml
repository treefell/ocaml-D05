let sum float1 float2 =
    try
        float1 +. float2
    with
    |Failure err -> print_endline err; nan
    |_ -> float1+. float2

let () =
    print_string "nan + 2 = ";print_float (sum nan 2.);print_endline"";
    print_string "1 + neg_infinity = "; print_float (sum 1. neg_infinity);print_endline"";
    print_string "infinity + 2 = "; print_float (sum infinity 2.);print_endline"";
    print_string "1 + 2.22 = "; print_float (sum 1. 2.22);print_endline "";
    print_string "153 + 2 = "; print_float (sum 153. 2.);print_endline"";
    print_string "1 + -4 = "; print_float (sum 1. (-4.));print_endline"";
    print_string "-1 + -4 = "; print_float (sum (-1.) (-4.));print_endline"";
    print_string "1 + 0 = "; print_float (sum 1. 0.);print_endline"";
    print_string "0.3 + 2.1 = "; print_float (sum 0.3 2.1);print_endline"";
    print_string "1. + 2.5 = "; print_float (sum 1. 2.5);print_endline"";
    print_string "1. + max float = "; print_float (sum 1. max_float);print_endline"";
    print_string "max float + max float = "; print_float (sum max_float max_float);print_endline"";
    print_string "-1. + -max_float = "; print_float (sum (-1.) (-.max_float));print_endline""
