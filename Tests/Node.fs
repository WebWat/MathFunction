module Node

open System
open Test
open Xunit


[<Fact>]
let ``Main tests`` () =
    Assert.Equal(8, (calculate (convert2func "2+2*2+2")))
    Assert.Equal(8, (calculate (convert2func "(2+2)*2")))
    Assert.Equal(8, (calculate (convert2func "2*(2+2)")))
    Assert.Equal(16, (calculate (convert2func "(2+2)*(2+2)")))
    Assert.Equal(22, (calculate (convert2func "(2+2)*2+(2+2)*2+(2+2*2)")))
    Assert.Equal(3, (calculate (convert2func "(5+10)/5")))

[<Fact>]
let ``Clear brackets tests`` () =
    Assert.Equal("2+2", (clearBrackets "2+2"))
    Assert.Equal("2+2", (clearBrackets "(2+2)"))
    Assert.Equal("2+2", (clearBrackets "(((2+2)))"))
    Assert.Equal("(2+2)*(2+2)", (clearBrackets "(2+2)*(2+2)"))
