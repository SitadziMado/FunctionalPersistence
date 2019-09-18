// Learn more about F# at http://fsharp.org

open System

let tree = BinaryTree.construct [2; 1; 3; 4; 5; 3; 4]

[<EntryPoint>]
let main argv =
    printf "%A\n" tree
    printf "%A" (BinaryTree.remove 4 tree)
    0