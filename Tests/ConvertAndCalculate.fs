module ConvertAndCalculate

open System
open Function
open Xunit


[<Fact>]
let ``Simple calculations`` () =
    Assert.Equal(8., (calculateFunc (convertToFunc "2+2*2+2") 0))
    Assert.Equal(8., (calculateFunc (convertToFunc "(2+2)*2") 0))
    Assert.Equal(8., (calculateFunc (convertToFunc "2*(2+2)") 0))
    Assert.Equal(0., (calculateFunc (convertToFunc "(222+2)*(2-2)/(2+2)") 0))
    Assert.Equal(22., (calculateFunc (convertToFunc "(2+2)*2+(2+2)*2+(2+2*2)") 0))
    Assert.Equal(-30., (calculateFunc (convertToFunc "(2+2*(2+3*(2+4*(-2))))") 0))
    Assert.Equal(2., (calculateFunc (convertToFunc "(10^2+11^2+12^2+13^2+14^2)/365") 0))

[<Fact>]
let ``Complex calculations`` () =
    let mutable x = 0.05
    Assert.Equal((-(sin(2.* x)-2.*sin(Math.PI))/(x * log(cos(5.* x)))), 
                 (calculateFunc (convertToFunc "(-1)*(sin(2*x)-2*sin(pi))/(x*ln(cos(5*x)))") x))
    
    x <- 0.5
    Assert.Equal(((1./tan x *(1.-cos(3.*x) * cos(3.*x)))/(x*x+5.*x)), 
                 (calculateFunc (convertToFunc "ctg(x)*(1-cos(3*x)^2)/(x^2+5*x)") x))
    
    x <- 2
    Assert.Equal(((1.-cos(x))/(sqrt(x+1.)-1.)), (calculateFunc (convertToFunc "(1-cos(x))/(sqrt(x+1)-1)") x))

[<Fact>]
let ``Degree and Root`` () =
    Assert.Equal(16., (calculateFunc (convertToFunc "2^3+2^3") 0))
    Assert.Equal(8.,  (calculateFunc (convertToFunc "2^(3-2)^3") 0))
    Assert.Equal(16., (calculateFunc (convertToFunc "(2+2)^(4/2)") 0))
    Assert.Equal(10.,  (calculateFunc (convertToFunc "2*sqrt(4^2)+2") 0))

[<Fact>]
let ``Logarithms`` () =
    Assert.Equal(2., (calculateFunc (convertToFunc "lg(1+9)+ln(e)") 0))
    Assert.Equal(25.,  (calculateFunc (convertToFunc "5^lg(10^2)") 0))
    Assert.Equal(2.,  (calculateFunc (convertToFunc "lg(100)/lg(10)") 0))

[<Fact>]
let ``Trigonometry`` () =
    Assert.Equal(1., (calculateFunc (convertToFunc "cos(1)^2+sin(1)^2") 0))
    Assert.Equal(0.,  (calculateFunc (convertToFunc "(1-cos(2*pi))/2") 0))
    Assert.Equal(1., round (calculateFunc (convertToFunc "tg(e)*ctg(e)") 0))

[<Fact>]
let ``Module`` () =
    Assert.Equal(6., (calculateFunc (convertToFunc "2+|-2|*2") 0))
    Assert.Equal(60., (calculateFunc (convertToFunc "|(-10)+2*|-20||*2") 0))
    Assert.Equal(0., (calculateFunc (convertToFunc "|||22-24|-1|-1|*2") 0))
    Assert.Equal(12., (calculateFunc (convertToFunc "|2^2+|2||*(2)") 0))
    Assert.Equal(14., (calculateFunc (convertToFunc "|-4+2+2*(-2)*2|+|-2|^2") 0))

