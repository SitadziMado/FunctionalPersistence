module Stack
    type 't Stack = Version of 't * 't Stack
                  | Empty

    let push value stack =
        match stack with
        | Empty -> Version(value, stack)
        | x -> Version(value, x)

    let pop = function
    | Empty -> failwith "Стек пуст."
    | Version(_, prev) -> prev

    let top = function
    | Empty -> failwith "Пустой стек не имеет значений"
    | Version(x, _) -> x

    let isEmpty = function
    | Empty -> true
    | _ -> false

    let size stack = 
        let rec size' acc = function
        | Empty -> acc
        | Version(_, rest) -> size' (acc + 1) rest

        size' 0 stack
    