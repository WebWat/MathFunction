module Node

open System
open Test
open Xunit


[<Fact>]
let ``Main tests`` () =
    Assert.Equal(8., (calculateFunc (convert2func "2+2*2+2")))
    Assert.Equal(8., (calculateFunc (convert2func "(2+2)*2")))
    Assert.Equal(8., (calculateFunc (convert2func "2*(2+2)")))
    Assert.Equal(16., (calculateFunc (convert2func "(2+2)*(2+2)")))
    Assert.Equal(0., (calculateFunc (convert2func "(222+2)*(2-2)/(2+2)")))
    Assert.Equal(22., (calculateFunc (convert2func "(2+2)*2+(2+2)*2+(2+2*2)")))
    Assert.Equal(3., (calculateFunc (convert2func "(5+10)/5")))
    Assert.Equal(1., (calculateFunc (convert2func "((-5)+10)/5")))
    Assert.Equal(-10., (calculateFunc (convert2func "((-5)*(-10))/(-5)")))

[<Fact>]
let ``Clear brackets tests`` () =
    Assert.Equal("2+2", (clearBrackets "2+2"))
    Assert.Equal("2+2", (clearBrackets "(2+2)"))
    Assert.Equal("2+2", (clearBrackets "(((2+2)))"))
    Assert.Equal("(2+2)*(2+2)", (clearBrackets "(2+2)*(2+2)"))
