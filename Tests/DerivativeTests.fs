module DerivativeTests

open Parser
open Derivative
open Xunit

let derivative (line: string) : string =
    (functionDerivative (convertToFunction line) ('x')).ToString()

[<Fact>]
let ``Consts`` () =
    Assert.Equal("0", derivative "10")
    Assert.Equal("0", derivative "-10")
    Assert.Equal("0", derivative "2.3e+5")
    Assert.Equal("1", derivative "x")

[<Fact>]
let ``Plus & minus`` () =
    Assert.Equal("1+0", derivative "x+2")
    Assert.Equal("-1-0", derivative "-x-2")

[<Fact>]
let ``Multiply`` () =
    Assert.Equal("2", derivative "2*x")
    Assert.Equal("(-2)", derivative "x*(-2)")
    Assert.Equal("2*(1-0)", derivative "2*(x-1)")
    Assert.Equal("(1-0)*(-2)", derivative "(x-1)*(-2)")
    Assert.Equal("(1+0)*(x-2)+(x+1)*(1-0)", derivative "(x+1)*(x-2)")

[<Fact>]
let ``Divide`` () =
    Assert.Equal("-2/x^2", derivative "2/x")
    Assert.Equal("1/2", derivative "x/2")
    Assert.Equal("-2*(2-0)/(2*x-3)^2", derivative "2/(2*x-3)")
    Assert.Equal("(2-0)/(-2)", derivative "(2*x-3)/(-2)")
    Assert.Equal("((2-0)*3*x-(2*x-3)*3)/(3*x)^2", derivative "(2*x-3)/(3*x)")

[<Fact>]
let ``Power`` () =
    Assert.Equal("(-2)*x^(-3)", derivative "x^(-2)")
    Assert.Equal("2^x*ln(2)", derivative "2^x")
    Assert.Equal("2*(3*x)^1*3", derivative "(3*x)^2")
    Assert.Equal("2^(x+2)*ln(2)*(1+0)", derivative "2^(x+2)")

    let comp = derivative "exp((x+2)*ln(3*x))";
    Assert.Equal(comp, derivative "(3*x)^(x+2)")
    Assert.Equal("exp((x+2)*ln(3*x))*((1+0)*ln(3*x)+(x+2)*3/(3*x))", comp)

[<Fact>]
let ``Exponenta`` () =
    Assert.Equal("exp(x)", derivative "exp(x)")
    Assert.Equal("exp(x+2)*(1+0)", derivative "exp(x+2)")

[<Fact>]
let ``Natural logarithm`` () =
    Assert.Equal("1/x", derivative "ln(x)")
    Assert.Equal("2/(2*x)", derivative "ln(2*x)")

[<Fact>]
let ``Sine`` () =
    Assert.Equal("cos(x)", derivative "sin(x)")
    Assert.Equal("cos(2*x)*2", derivative "sin(2*x)")

[<Fact>]
let ``Cosine`` () =
    Assert.Equal("-sin(x)", derivative "cos(x)")
    Assert.Equal("(-sin(2*x))*2", derivative "cos(2*x)")


[<Fact>]
let ``Decimal logarithm`` () =
    Assert.Equal("2/(2*x)/ln(10)", derivative "lg(2*x)")

[<Fact>]
let ``Square`` () =
    Assert.Equal("1/(2*sqrt(x))", derivative "sqrt(x)")
    Assert.Equal("3/(2*sqrt(3*x))", derivative "sqrt(3*x)")

[<Fact>]
let ``Log2`` () =
    Assert.Equal("5/(5*x)/ln(2)", derivative "log2(5*x)")

[<Fact>]
let ``Tangent`` () =
    Assert.Equal("1/cos(x)^2", derivative "tg(x)")
    Assert.Equal("2/cos(2*x)^2", derivative "tg(2*x)")

[<Fact>]
let ``Cotangent`` () =
    Assert.Equal("-1/sin(x)^2", derivative "ctg(x)")
    Assert.Equal("-2/sin(2*x)^2", derivative "ctg(2*x)")

[<Fact>]
let ``Arcsin`` () =
    Assert.Equal("1/sqrt(1-x^2)", derivative "arcsin(x)")
    Assert.Equal("2/sqrt(1-(2*x)^2)", derivative "arcsin(2*x)")

[<Fact>]
let ``Arccos`` () =
    Assert.Equal("-1/sqrt(1-x^2)", derivative "arccos(x)")
    Assert.Equal("-2/sqrt(1-(2*x)^2)", derivative "arccos(2*x)")

[<Fact>]
let ``Arctg`` () =
    Assert.Equal("1/(1+x^2)", derivative "arctg(x)")
    Assert.Equal("2/(1+(2*x)^2)", derivative "arctg(2*x)")

[<Fact>]
let ``Arcctg`` () =
    Assert.Equal("-1/(1+x^2)", derivative "arcctg(x)")
    Assert.Equal("-2/(1+(2*x)^2)", derivative "arcctg(2*x)")

[<Fact>]
let ``Sgn`` () =
    Assert.Equal("0", derivative "sgn(x)")

[<Fact>]
let ``Module`` () =
    Assert.Equal("sgn(x)", derivative "|x|")
    Assert.Equal("(-1)*sgn(-x)", derivative "|-x|")
    Assert.Equal("2*3*x^2*sgn(2*x^3)", derivative "|2*x^3|")

[<Fact>]
let ``Complex`` () =
    Assert.Equal(
        "((-1)*(cos(2*x)*2*cos(1/ln(x))-sin(2*x)*(-sin(1/ln(x)))" +
        "*(-1*1/x/ln(x)^2))/cos(1/ln(x))^2+((1+0)*(x+3)+(x+2)*(1+0))*" +
        "(x^3-1)+(x+2)*(x+3)*(3*x^2-0))*exp(sin(x))+((-1)*sin(2*x)" +
        "/cos(1/ln(x))+(x+2)*(x+3)*(x^3-1))*exp(sin(x))*cos(x)", 
        derivative "((-1)*sin(2*x)/cos(1/ln(x))+(x+2)*(x+3)*(x^3-1))*exp(sin(x))")



