namespace PersistentDataStructureTests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open Persistent
open Utility

[<TestClass>]
type TestClass () =
    let list = Persistent.createList [1; 2; 3; 4; 5; 6]
    
    [<TestMethod>]
    member this.TestMethodPassing() =
        Assert.IsTrue(true)

    [<TestMethod>]
    member this.reverseTest () =
        eq [6; 5; 4; 3; 2; 1] (list |> reverse |> toList)

    [<TestMethod>]
    member this.foldTest () =
        eq 720 (list |> fold (*) 1 )

    [<TestMethod>]
    member this.mapTest () =
        eq [3; 4; 5; 6; 7; 8] (list |> map ((+) 2) |> toList)
    
    [<TestMethod>]
    member this.take () =
        eq [1; 2; ] (list |> take 2 |> toList)    
    
    [<TestMethod>]
    member this.insertAfterTest () =
        let ins p = list |>  insertAfter 10 p |> toList
        eq [1; 2; 3; 10; 4; 5; 6] (ins 2)
        eq [1; 10; 2; 3; 4; 5; 6] (ins 0)
        eq [1; 2; 3; 4; 5; 6; 10;] (ins 5)
        fails (fun () -> ins 8 |> ignore)
        fails (fun () -> ins -2 |> ignore)
    
    [<TestMethod>]
    member this.insertBeforeTest () =
        let ins p = list |> insertBefore 20 p |> toList
        eq [1; 2; 20; 3; 4; 5; 6] (ins 2)
        eq [20; 1; 2; 3; 4; 5; 6] (ins 0)
        eq [1; 2; 3; 4; 5; 20; 6] (ins 5)     
        fails (fun () -> ins 8 |> ignore)
        fails (fun () -> ins -2 |> ignore)

    [<TestMethod>]
    member this.removeAtTest () =
        let rem p = list |> removeAt p |> toList
        eq [1; 2; 4; 5; 6] (rem 2)
        eq [1; 2; 3; 4; 5] (rem 5)
        eq [2; 3; 4; 5; 6] (rem 0)
        eq [1; 2; 3; 5; 6] (rem 3)
        fails (fun () -> rem -2 |> ignore)
        fails (fun () -> rem 89 |> ignore)