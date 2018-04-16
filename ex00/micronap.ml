let my_sleep () = Unix.sleep 1

let () =
    if Array.length Sys.argv > 1 then
     begin try 
            let nb = int_of_string (Sys.argv.(1)) in
            if nb < 1 then
                ()(*print_endline "number must be over 0"*)
            else(
                for i = 0 to nb do
                    my_sleep()
                done;
            )
         with
            |Failure _ ->() (*print_endline "must be a number"*)
            |_ -> ()
     end
    else () 
