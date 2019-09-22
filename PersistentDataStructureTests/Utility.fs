module Utility

    open Microsoft.VisualStudio.TestTools.UnitTesting
    open System

    let eq (exp : 'a) (act : 'a) = Assert.AreEqual(exp, act)
    let fails f = Assert.ThrowsException<Exception>(Action f) 
                |> ignore