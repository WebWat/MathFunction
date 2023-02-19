module ConvertAndCalculate

open System
open Node
open Xunit

let calculator (line: string) : float =
    calculateFunc (convertToFunc line) (Map [])

[<Fact>]
let ``Float & int parse`` () =
    Assert.Equal(10., calculator "10")
    Assert.Equal(-10., calculator "-10")

    Assert.Equal(10.23, calculator "10.23")
    Assert.Equal(-10.23, calculator "-10.23")

    Assert.Equal(1e-5, calculator "1e-5")
    Assert.Equal(2.3e+5, calculator "2.3e+5")

[<Fact>]
let ``Addition`` () =
    Assert.Equal(4., calculator "2+2")
    Assert.Equal(10.5, calculator "4+6+0.5")

[<Fact>]
let ``Subtraction`` () =
    Assert.Equal(0., calculator "2-2")
    Assert.Equal(-0.5, calculator "0.5-0.5-0.5")
    Assert.Equal(-30., calculator "-10-20")
    Assert.Equal(-60., calculator "-10-20-30")

[<Fact>]
let ``Multiplication`` () =
    Assert.Equal(4., calculator "2*2")
    Assert.Equal(0., calculator "1.5*0")
    Assert.Equal(137.5, calculator "5*5*5.5")
    Assert.Equal(6.25, calculator "2.5*2.5")

[<Fact>]
let ``Division`` () =
    Assert.Equal(1., calculator "2/2")
    Assert.Equal(0., calculator "0/2")
    Assert.Equal(3., calculator "1.5/0.5")
    Assert.Equal(0.375, calculator "3/2/2/2")
    Assert.Equal(infinity, calculator "2/0")

[<Fact>]
let ``Power`` () =
    Assert.Equal(4., calculator "2^2")
    Assert.Equal(5.76, calculator "2.4^2")
    Assert.Equal(Math.Pow(0.5, 0.5), calculator "0.5^0.5")
    Assert.Equal(1., calculator "1.5^0")
    Assert.Equal(0., calculator "0^1.5")
    Assert.Equal(65536., calculator "2^2^2^2")

[<Fact>]
let ``Operation priority`` () =
    Assert.Equal(-6., calculator "-2+-2+-2")
    Assert.Equal(6., calculator "2+2*2")
    Assert.Equal(3., calculator "2*2-2/2")
    Assert.Equal(9., calculator "6/2*3")
    Assert.Equal(-8., calculator "-2^3")
    Assert.Equal(-5., calculator "2*2/2+3^2-4*4")

[<Fact>]
let ``Operation with modules and brackets`` () =
    Assert.Equal(4., calculator "|-2|*2")
    Assert.Equal(-4., calculator "|2|*(-2)")
    Assert.Equal(64., calculator "(-(2+2)*2)^2")
    Assert.Equal(9., calculator "(2+1)*|1+2|")
    Assert.Equal(-1., calculator "||(1-2)-2|-2|+(-2)")
    Assert.Equal(4., calculator "|-2+|2-(-2)||*|-2|")
    Assert.Equal(3., calculator "(|-1|+1+|-1|)*|-1|")
    Assert.Equal(256., calculator "((2^2)^2)^2")
    Assert.Equal(256., calculator "|-(2^2)^2|^2")
    Assert.Equal(8., calculator "32/(16/(8/(4/2)))")
    Assert.Equal(8., calculator "32/|16/(8/|-4/2|)|")
    Assert.Equal(16., calculator "((2*(3+1))/|(2-1)*1|)*(1+1)")

[<Fact>]
let ``Operation with functions`` () =
    Assert.Equal(2., calculator "sqrt(2^2)")
    Assert.Equal(0., calculator "2*sin(0)")
    Assert.Equal(0., calculator "sin(0)*2")
    Assert.Equal(1., calculator "2+-lg(10)")
    Assert.Equal(-2., calculator "-ln(e^2)")
    Assert.Equal(1., calculator "2^cos(pi/2)")
    Assert.Equal(1., calculator "ln(e)^lg(10)")
    Assert.Equal(1., calculator "lg(9-sin(0)*1+1)")

[<Fact>]
let ``Complex operation`` () =
    Assert.Equal((-(sin(2.* 0.05) - 2. * sin(Math.PI))/(0.05 * log(cos(5.* 0.05)))), 
                 (calculator "(-1)*(sin(2*0.05)-2*sin(pi))/(0.05*ln(cos(5*0.05)))"))
    
    Assert.Equal(((1./tan 0.5 *(1.-cos(3. * 0.5) * cos(3. * 0.5)))/(0.5 * 0.5 + 5. * 0.5)), 
                 (calculator "ctg(0.5)*(1-cos(3*0.5)^2)/(0.5^2+5*0.5)"))
  
    Assert.Equal(((1. - cos(2.))/(sqrt(2. + 1.) - 1.)), calculator "(1-cos(2))/(sqrt(2+1)-1)")

// TODO: Need more!
[<Fact>]
let ``Functions with parametrs`` () =
    Assert.Equal(14., calculateFunc (convertToFunc "x^3+2*x+2") (Map [('x', 2)]))
    Assert.Equal(-10., calculateFunc (convertToFunc "x^3+2*x+2") (Map [('x', -2)]))

    Assert.Equal(2., calculateFunc (convertToFunc "x*2/x") (Map [('x', Math.PI)]))

    Assert.Equal(-9., calculateFunc (convertToFunc "-a^b") (Map [('a', 3); ('b', 2)]))


[<Fact>]
let ``Exceptions`` () =
    Assert.Throws<UnknownOperation>(new Action(fun _ -> 
        convertToFunc "awdh9q7yd3+7ax&^&"; ()))
    Assert.Throws<ArgumentNotExist>(new Action(fun _ -> 
        calculateFunc (convertToFunc "x^2+2*x+2") (Map []); ()))
    Assert.Throws<InvalidBracket>(new Action(fun _ -> 
        convertToFunc "(2+3))"; ()))
    Assert.Throws<InvalidModule>(new Action(fun _ -> 
        convertToFunc "|2+3||"; ()))


