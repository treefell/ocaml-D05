

let eu_dist a b =
    
    let rec rec_eu_dist n =
        if n >  ((Array.length a) - 1)
            then 0.
        else
            ((a.(n) -. b.(n))**2.) +. rec_eu_dist (n + 1)
    in
    sqrt (rec_eu_dist 0)

let () =
    let a = [|1.|] in
    let b = [|1.|] in
    print_float(eu_dist a b);
    print_endline "";
    let a = [|16.|] in
    let b = [|17.|] in
    print_float(eu_dist a b);
    print_endline "";
    let a = [|1.; 4.; 5.|] in
    let b = [|1.; 6.; 7.|] in
    print_float(eu_dist a b);
    print_endline "";
    let a = [|-1.; 4.; 20.|] in
    let b = [|-1.; 5.; -30.|] in
    print_float(eu_dist a b);
    print_endline ""

