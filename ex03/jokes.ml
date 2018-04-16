let randomPick nb =
    Random.self_init ();
    (Random.int nb)

let populate_joke a_joke joke i =
    ignore(a_joke.(i) <- joke)

        
let () =
    if Array.length Sys.argv = 2 then
    ( 
        let ic = open_in Sys.argv.(1) in
        let count = ref 0 in
        try
            while true do
                ignore(input_line ic);
                incr count
            done;
        with
            |Sys_error err -> print_endline err
            |end_of_file ->();
    
        close_in ic;
        let ic2 = open_in Sys.argv.(1) in
        let a_joke = (Array.make !count "") in
        try
            for i = 0 to !count do
                populate_joke a_joke (input_line ic2) i
            done;
        with
            |Sys_error err -> print_endline err
            |end_of_file ->();
        close_in ic2;
        print_endline (a_joke.(randomPick !count));
    )
    else ()
