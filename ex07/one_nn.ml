
let eu_dist a b =
    
    let rec rec_eu_dist n =
        if n >  ((Array.length a) - 1)
            then 0.
        else
            ((a.(n) -. b.(n))**2.) +. rec_eu_dist (n + 1)
    in
    sqrt (rec_eu_dist 0)

let converter data =
    let l_data = String.split_on_char ',' data in
    let n = List.length l_data in
    let a_float = Array.create_float (n - 1) in
    let rec populate_a_float l_data count =  
    try
        if count > n - 2
            then (a_float, List.nth l_data (n - 1))
        else
            (
                Array.set a_float count (float_of_string (List.nth l_data count));populate_a_float l_data (count + 1)
            )
    with
        |Failure _ ->  print_endline "converting string to float fail"; (Array.make (1) nan , "")
       (* |_ -> populate_a_float l_data (count + 1)*)

    in
        populate_a_float l_data 0 


let get_1_2 (a,_) = a
let get_2_2 (_,a) = a

let example_of_file file =
    let ic = open_in file in
    let rec auto_read l_data =
        try
            auto_read (l_data @ [input_line ic])
        with
            |Sys_error err -> print_endline err;[]
            |end_of_file ->l_data
    in 
    let l_data = auto_read [] in
    close_in ic;
    let rec digest_data l_radar count =
        if count > ((List.length l_data) - 1)
            then l_radar
        else
        (     
            let v_radar = converter (List.nth l_data count) in
            if ((get_1_2 v_radar).(0) = nan)
                then [([||],"")]
            else digest_data (l_radar @ [v_radar]) (count + 1)
        ) 
    in
    digest_data [] 0

let form_float value =
        print_float value;
        print_string " | "


let rec print_exoff l_radar =
    match l_radar with
    |[] -> ()
    |head::tail -> (Array.iter form_float (get_1_2 head);
                    print_string " ~ ";
                    print_string (get_2_2 head);
                    print_char '\n';
                    print_exoff tail)
    
let one_nn l_radar radar =
    match l_radar with
    |[] -> ""
    |head::tail -> (
        let rec rec_one_nn l_radar closer =
            match l_radar with
                |[] -> closer
                |hydra::rest -> (if (eu_dist (get_1_2 radar)(get_1_2 hydra)) 
                                < (eu_dist(get_1_2 radar)(get_1_2 closer))
                                    then rec_one_nn rest hydra
                                else
                                    rec_one_nn rest closer
                                )
        in
        get_2_2(rec_one_nn tail head)
    )

let () =

    let l_radar = example_of_file "ionosphere.test.csv" in
    let l_radar2 = example_of_file"ionosphere.test2.csv" in
    match l_radar with
    |[] -> ()
    |head::tail -> print_string "prediction ";print_endline(one_nn l_radar head);
    print_string "reality ";print_endline (get_2_2 head);
    print_string "prediction ";print_endline(one_nn l_radar2 head);
    print_string "reality ";print_endline (get_2_2 head)



