namespace PersistentDataStructureTests

open Microsoft.VisualStudio.TestTools.UnitTesting
open Utility

[<TestClass>]
type BinaryTreeTestClass () =
    let cons = BinaryTree.construct
    let tree = cons [2; 1; 3; 4; 5; 3; 4]
    let trav = BinaryTree.toSeq BinaryTree.Postfix
    
    [<TestMethod>]
    member this.insertTest () = 
        eq (BinaryTree.insert 0 tree |> trav) ([0; 1; 3; 4; 5; 4; 3; 2])