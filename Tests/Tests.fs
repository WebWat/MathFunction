module Tests

open System
open Main
open Xunit

[<Fact>]
let ``Limit default tests`` () =
    Assert.Equal(limit (fun x -> x * x) 2 Position.Left, 4)