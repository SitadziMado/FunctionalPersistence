module Utility

    open Microsoft.VisualStudio.TestTools.UnitTesting
    open System

    let eq exp act = Assert.AreEqual(exp, act)
    let fails f = Assert.ThrowsException<Exception>(Action f) 
                |> ignore