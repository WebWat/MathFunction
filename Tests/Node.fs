module Node

open System
open Test
open Xunit


[<Fact>]
let ``Test`` () =
    Assert.Equal(8, (calculate (convert2func "2+2*2+2")))
    Assert.Equal(8, (calculate (convert2func "(2+2)*2")))
    Assert.Equal(8, (calculate (convert2func "2*(2+2)")))
    Assert.Equal(16, (calculate (convert2func "(2+2)*(2+2)")))
    Assert.Equal(22, (calculate (convert2func "(2+2)*2+(2+2)*2+(2+2*2)")))
    Assert.Equal(3, (calculate (convert2func "(5+10)/5")))
