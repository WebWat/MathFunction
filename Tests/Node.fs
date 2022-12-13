module Node

open System
open Test
open Xunit


[<Fact>]
let ``Main tests`` () =
    Assert.Equal(8., (calculateFunc 0 (convert2func "2+2*2+2")))
    Assert.Equal(8., (calculateFunc 0 (convert2func "(2+2)*2")))
    Assert.Equal(8., (calculateFunc 0 (convert2func "2*(2+2)")))
    Assert.Equal(16., (calculateFunc 0 (convert2func "(2+2)*(2+2)")))
    Assert.Equal(0., (calculateFunc 0 (convert2func "(222+2)*(2-2)/(2+2)")))
    Assert.Equal(22., (calculateFunc 0 (convert2func "(2+2)*2+(2+2)*2+(2+2*2)")))
    Assert.Equal(3., (calculateFunc 0 (convert2func "(5+10)/5")))
    Assert.Equal(1., (calculateFunc 0 (convert2func "((-5)+10)/5")))
    Assert.Equal(-10., (calculateFunc 0 (convert2func "((-5)*(-10))/(-5)")))

//[<Fact>]
//let ``Clear brackets tests`` () =
//    Assert.Equal("2+2", (clearBrackets "2+2"))
//    Assert.Equal("2+2", (clearBrackets "(2+2)"))
//    Assert.Equal("2+2", (clearBrackets "(((2+2)))"))
//    Assert.Equal("(2+2)*(2+2)", (clearBrackets "(2+2)*(2+2)"))
