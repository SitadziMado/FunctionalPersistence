module Stack
    type 't Stack = Version of 't * 't Stack
                  | Empty

    let push value stack =
        match stack with
        | Empty -> Version(value, stack)
        | x -> Version(value, x)

    let pop stack = 
        match stack with
        | Empty -> failwith "Стек пуст."
        | Version(_, prev) -> prev
