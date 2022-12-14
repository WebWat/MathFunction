module Node

open System
open Test
open Xunit


[<Fact>]
let ``Simple calculations`` () =
    Assert.Equal(8., (calculateFunc 0 (convert2func "2+2*2+2")))
    Assert.Equal(8., (calculateFunc 0 (convert2func "(2+2)*2")))
    Assert.Equal(8., (calculateFunc 0 (convert2func "2*(2+2)")))
    Assert.Equal(0., (calculateFunc 0 (convert2func "(222+2)*(2-2)/(2+2)")))
    Assert.Equal(22., (calculateFunc 0 (convert2func "(2+2)*2+(2+2)*2+(2+2*2)")))
    Assert.Equal(-30., (calculateFunc 0 (convert2func "(2+2*(2+3*(2+4*(-2))))")))

[<Fact>]
let ``Complex calculations`` () =
    let mutable x = 0.05
    Assert.Equal((-(sin(2.* x)-2.*sin(Math.PI))/(x * log(cos(5.* x)))), 
                 (calculateFunc x (convert2func "(-1)*(sin(2*x)-2*sin(pi))/(x*ln(cos(5*x)))")))
    
    x <- 0.5
    Assert.Equal(((1./tan x *(1.-cos(3.*x) * cos(3.*x)))/(x*x+5.*x)), 
                 (calculateFunc x (convert2func "ctg(x)*(1-cos(3*x)^2)/(x^2+5*x)")))
    
    x <- 2
    Assert.Equal(((1.-cos(x))/(sqrt(x+1.)-1.)), (calculateFunc x (convert2func "(1-cos(x))/(sqrt(x+1)-1)")))

[<Fact>]
let ``Degree and Root`` () =
    Assert.Equal(16., (calculateFunc 0 (convert2func "2^3+2^3")))
    Assert.Equal(2.,  (calculateFunc 0 (convert2func "2^(3-2)^3")))
    Assert.Equal(16., (calculateFunc 0 (convert2func "(2+2)^(4/2)")))
    Assert.Equal(10.,  (calculateFunc 0 (convert2func "2*sqrt(4^2)+2")))

[<Fact>]
let ``Logarithms`` () =
    Assert.Equal(2., (calculateFunc 0 (convert2func "lg(1+9)+ln(e)")))
    Assert.Equal(25.,  (calculateFunc 0 (convert2func "5^lg(10^2)")))
    Assert.Equal(2.,  (calculateFunc 0 (convert2func "lg(100)/lg(10)")))

[<Fact>]
let ``Trigonometry`` () =
    Assert.Equal(1., (calculateFunc 0 (convert2func "cos(1)^2+sin(1)^2")))
    Assert.Equal(0.,  (calculateFunc 0 (convert2func "(1-cos(2*pi))/2")))
    Assert.Equal(1., round (calculateFunc 0 (convert2func "tg(e)*ctg(e)")))

[<Fact>]
let ``Module`` () =
    Assert.Equal(6., (calculateFunc 0 (convert2func "2+(|-2|)*2")))
    Assert.Equal(60., (calculateFunc 0 (convert2func "(|(-10)+2*(|-20|)|)*2")))
    Assert.Equal(14., (calculateFunc 0 (convert2func "(|-4+2+2*(-2)*2|)+(|-2|)^2")))

//[<Fact>]
//let ``Clear brackets tests`` () =
//    Assert.Equal("2+2", (clearBrackets "2+2"))
//    Assert.Equal("2+2", (clearBrackets "(2+2)"))
//    Assert.Equal("2+2", (clearBrackets "(((2+2)))"))
//    Assert.Equal("(2+2)*(2+2)", (clearBrackets "(2+2)*(2+2)"))
