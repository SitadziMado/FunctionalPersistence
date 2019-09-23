// Learn more about F# at http://fsharp.org

open System

let tree = BinaryTree.construct [2; 1; 3; 4; 5; 3; 4]

[<EntryPoint>]
let main argv =
    printf "%A\n" tree
    printf "%A\n" (BinaryTree.remove 4 tree)
    let trav = BinaryTree.traverse (fun acc x -> x :: acc) BinaryTree.TraversalOrder.Prefix [] >> List.rev
    printf "%A\n" (tree |> trav)
    0