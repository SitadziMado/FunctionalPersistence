// Learn more about F# at http://fsharp.org

module Persistent.App

let tree = BinaryTree.construct [2; 1; 3; 5; 7; 3; 5]

[<EntryPoint>]
let main argv =
    //printf "%A\n" (BinaryTree.remove 4 tree)
    //printf "%A\n" (tree |> trav)
    let trav = BinaryTree.toList BinaryTree.Prefix
    printf "%A\n" (tree |> trav)
    printf "%A\n" (BinaryTree.remove 3 tree |> trav)
    printf "%A\n" (BinaryTree.remove 5 tree |> trav)
    0