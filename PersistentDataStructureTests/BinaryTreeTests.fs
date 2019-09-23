namespace PersistentDataStructureTests

open Microsoft.VisualStudio.TestTools.UnitTesting
open Utility

[<TestClass>]
type BinaryTreeTestClass () =
    let cons = BinaryTree.construct
    let tree = cons [2; 1; 3; 5; 7; 3; 5]
    let trav = BinaryTree.toList BinaryTree.Prefix
    
    [<TestMethod>]
    member this.structureTest () =
        eq ([2; 1; 3; 3; 5; 5; 7]) (tree |> trav)

    [<TestMethod>]
    member this.insertTest () = 
        eq ([2; 1; 0; 3; 3; 5; 5; 7]) (BinaryTree.insert 0 tree |> trav |> Seq.toList)
        eq ([2; 1; 3; 3; 5; 5; 4; 7]) (BinaryTree.insert 4 tree |> trav |> Seq.toList)

    [<TestMethod>]
    member this.removeTest () =
        eq ([2; 1; 5; 3; 7; 5]) (BinaryTree.remove 3 tree |> trav)
        eq ([2; 1; 3; 3; 7; 5]) (BinaryTree.remove 5 tree |> trav)

    [<TestMethod>]
    member this.minMaxTest () =
        eq (1) (BinaryTree.min tree)
        eq (7) (BinaryTree.max tree)

    [<TestMethod>]
    member this.containsTest () =
        eq (true) (BinaryTree.contains 7 tree)
        eq (false) (BinaryTree.contains -1 tree)
        eq (false) (BinaryTree.contains 0 tree)
        eq (true) (BinaryTree.contains 199 (BinaryTree.insert 199 tree))