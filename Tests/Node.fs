module Node

open System
open Test
open Xunit


[<Fact>]
let ``Test`` () =
    Assert.Equal(8, (calculate (convert "2+2*2+2")))
    Assert.Equal(8, (calculate (convert "(2+2)*2")))
    Assert.Equal(8, (calculate (convert "2*(2+2)")))
