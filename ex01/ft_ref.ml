type ('a) ft_ref = {mutable elem : 'a}

let return elem =
        {elem = elem}

let get elemRef =
        elemRef.elem

let set elemRef elem =
    ignore(elemRef.elem <- elem)

let bind elemRef funk: 'b ft_ref =
    (*let element = funk (elem_ref.elem)*)
    funk (elemRef.elem)

let () =
    let binding isaac =
        {elem = isaac ^ "alot"}
    in
    let elemRef = return "string" in
    print_endline (get elemRef);
    ignore (set elemRef "changed");
    print_endline (get elemRef);
    print_endline (get (bind elemRef binding));
    print_endline (get elemRef)
    
