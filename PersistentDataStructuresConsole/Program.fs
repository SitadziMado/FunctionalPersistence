// Learn more about F# at http://fsharp.org

open System
let list = Persistent.createList [1; 2; 3; 4; 5; 6]
[<EntryPoint>]
let main argv =
    let ins p = list |>  Persistent.insertAfter 10 p |> Persistent.toList
    printf "%A" (ins 2) // [1; 2; 3; 10; 4; 5; 6] 
    printf "%A" (ins 0) // [1; 10; 2; 3; 4; 5; 6]
    printf "%A" (ins 5) // [1; 2; 3; 4; 5; 6; 10;]
    printf "%A" (ins -2) // [1; 2; 3; 4; 5; 6; 10;]
    0