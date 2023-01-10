﻿module ExpandNode

open Node
open Expand
open Xunit

[<Fact>]
let ``Simplify multiply`` () =
    // Simple
    Assert.Equal("x",    (simplifyMultiply (convertToFunc "x")).ToString())
    Assert.Equal("(-1)", (simplifyMultiply (convertToFunc "(-1)")).ToString())

    // Default
    Assert.Equal("2*x^2",    (simplifyMultiply (convertToFunc "x*2*x")).ToString())
    Assert.Equal("24*x",     (simplifyMultiply (convertToFunc "2*3*4*x")).ToString())
    Assert.Equal("x^5",      (simplifyMultiply (convertToFunc "x^2*x^3")).ToString())
    Assert.Equal("(-1)*x^4", (simplifyMultiply (convertToFunc "x*x*x*(-x)")).ToString())
    Assert.Equal("x^4",      (simplifyMultiply (convertToFunc "x*(-x)*x*(-x)")).ToString())

    // Complex
    Assert.Equal("16*x^4",    (simplifyMultiply (convertToFunc "(2*x)^2*(2*x)^2")).ToString())
    Assert.Equal("(-32)*x^5", (simplifyMultiply (convertToFunc "(-2*x)^2*(-2*x)^3")).ToString())
    Assert.Equal("sin(x)^2*cos(x)^2", (simplifyMultiply (convertToFunc "cos(x)*sin(x)*cos(x)*sin(x)")).ToString())

[<Fact>]
let ``Symplify sum`` () =
    // Default
    Assert.Equal("(-1)",   (simplifySum (convertToFunc "-1")).ToString())
    Assert.Equal("x",      (simplifySum (convertToFunc "x")).ToString())
    Assert.Equal("2*x",    (simplifySum (convertToFunc "x+x")).ToString())
    Assert.Equal("(-2)*x", (simplifySum (convertToFunc "-x-x")).ToString())
    Assert.Equal("2*x^5",  (simplifySum (convertToFunc "2*x^2+2*x^5-2*x^2")).ToString())
    Assert.Equal("2*x^5",  (simplifySum (convertToFunc "x^5+x^5")).ToString())

    // Complex
    Assert.Equal("2*cos(x)*sin(x)", (simplifySum (convertToFunc "sin(x)*cos(x)+cos(x)*sin(x)")).ToString())
    Assert.Equal("0", (simplifySum (convertToFunc "2*(x^2)-(-2)*(-x^2)")).ToString())

[<Fact>]
let ``Open brackets`` () =
    // Default
    Assert.Equal("x",       (openBrackets (convertToFunc "x")).ToString())
    Assert.Equal("x-x-1",   (openBrackets (convertToFunc "x-(x+1)")).ToString())
    Assert.Equal("x+x+1",   (openBrackets (convertToFunc "x-(-x-1)")).ToString())
    Assert.Equal("x-x+x+2", (openBrackets (convertToFunc "x-(x-(x+2))")).ToString())
    Assert.Equal("x-x-1-x-x-1", (openBrackets (convertToFunc "x-(x+1)-x-(x+1)")).ToString())

    // Complex
    Assert.Equal("x+sin(x+2)*x-1",   (openBrackets (convertToFunc "x-(-sin(x+2)*x+1)")).ToString())
    Assert.Equal("sin(x-cos(2+x)-2)",   (openBrackets (convertToFunc "sin(x-(cos(2+x)+2))")).ToString())









