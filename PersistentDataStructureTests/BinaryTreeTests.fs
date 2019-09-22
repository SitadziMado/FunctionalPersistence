namespace PersistentDataStructureTests

open Microsoft.VisualStudio.TestTools.UnitTesting
open Utility

[<TestClass>]
type BinaryTreeTestClass () =
    let cons = BinaryTree.construct
    let tree = cons [2; 1; 3; 4; 5; 3; 4]
    let trav = BinaryTree.toSeq BinaryTree.Prefix
    
    [<TestMethod>]
    member this.insertTest () = 
        eq ([2; 1; 0; 3; 3; 4; 4; 5]) (BinaryTree.insert 0 tree |> trav |> Seq.toList)

    [<TestMethod>]
    member this.removeTest () =
        eq ([2; 1; 4; 3; 4; 5]) (BinaryTree.remove 3 tree |> trav |> Seq.toList)