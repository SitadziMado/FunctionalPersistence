module Stack
    type 't Stack = Version of 't Stack * 't
                  | Empty

    let push value stack =
        match stack with
        | Empty -> Version(stack, value)
        | x -> Version(x, value)

    let pop stack = 
        match stack with
        | Empty -> failwith "Стек пуст."
        | Version(prev, _) -> prev