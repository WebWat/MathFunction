module ConvertAndCalculateTests

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
    Assert.Equal(2.5, calculator "1.5/0.6")
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
    Assert.Equal(6., calculator "2+2*2")
    Assert.Equal(9., calculator "6/2*(1+2)")


