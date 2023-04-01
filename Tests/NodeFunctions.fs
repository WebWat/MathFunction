module NodeFunctions

open Parser
open Xunit

[<Fact>]
let ``Get default brackets indexes (from left to right)`` () =
    // Not found
    Assert.Equal((-1, -1), (getBracketsIndexesLeftToRight "x+2"))

    // Default
    Assert.Equal((0, 2), (getBracketsIndexesLeftToRight "(x)"))
    Assert.Equal((0, 4), (getBracketsIndexesLeftToRight "(x+3)"))

    // Inner
    Assert.Equal((0, 8), (getBracketsIndexesLeftToRight "(x+(x-1))"))    
    Assert.Equal((0, 8), (getBracketsIndexesLeftToRight "((x-1)+x)"))
    Assert.Equal((0, 12), (getBracketsIndexesLeftToRight "(x+(x-(1-x)))"))
    Assert.Equal((0, 12), (getBracketsIndexesLeftToRight "(((1-x)+x)+2)"))

    // Outside
    Assert.Equal((7, 11), (getBracketsIndexesLeftToRight "(x+10)*(x+3)"))
    Assert.Equal((13, 17), (getBracketsIndexesLeftToRight "(x+10)*(x+3)*(x+2)"))

[<Fact>]
let ``Get default brackets indexes (from right to left)`` () =
    // Not found
    Assert.Equal((-1, -1), (getBracketsIndexesRightToLeft "x+2"))

    // Default
    Assert.Equal((0, 2), (getBracketsIndexesRightToLeft "(x)"))
    Assert.Equal((0, 4), (getBracketsIndexesRightToLeft "(x+3)"))

    // Inner
    Assert.Equal((0, 8), (getBracketsIndexesRightToLeft "(x+(x-1))"))    
    Assert.Equal((0, 8), (getBracketsIndexesRightToLeft "((x-1)+x)"))
    Assert.Equal((0, 12), (getBracketsIndexesRightToLeft "(x+(x-(1-x)))"))
    Assert.Equal((0, 12), (getBracketsIndexesRightToLeft "(((1-x)+x)+2)"))

    // Outside
    Assert.Equal((0, 5), (getBracketsIndexesRightToLeft "(x+10)*(x+3)"))
    Assert.Equal((0, 5), (getBracketsIndexesRightToLeft "(x+10)*(x+3)*(x+2)"))

[<Fact>]
let ``Get module brackets indexes (from left to right)`` () =
    // Not found
    Assert.Equal((-1, -1), (getBracketsIndexesLeftToRight "x+2"))

    // Default
    Assert.Equal((0, 2), (getBracketsIndexesLeftToRight "|x|"))
    Assert.Equal((0, 4), (getBracketsIndexesLeftToRight "|x+3|"))

    // Inner
    Assert.Equal((0, 8), (getBracketsIndexesLeftToRight "|x+|x-1||"))
    Assert.Equal((0, 8), (getBracketsIndexesLeftToRight "||x-1|+x|"))
    Assert.Equal((2, 14), (getBracketsIndexesLeftToRight "2+|x+|x-|1-x|||+2"))
    Assert.Equal((0, 19), (getBracketsIndexesLeftToRight "|x+|x-|1-x*|23+x||||"))
    Assert.Equal((2, 14), (getBracketsIndexesLeftToRight "2+|||1-x|+x|+2|"))

    // Outside
    Assert.Equal((7, 11), (getBracketsIndexesLeftToRight "|x+10|*|x+3|"))
    Assert.Equal((13, 17), (getBracketsIndexesLeftToRight "|x+10|*|x+3|*|x+2|"))

    // Not found
    Assert.Equal((-1, -1), (getBracketsIndexesRightToLeft "x+2"))

[<Fact>]
let ``Get module brackets indexes (from right to left)`` () =
    // Not found
    Assert.Equal((-1, -1), (getBracketsIndexesRightToLeft "x+2"))

    // Default
    Assert.Equal((0, 2), (getBracketsIndexesRightToLeft "|x|"))
    Assert.Equal((0, 4), (getBracketsIndexesRightToLeft "|x+3|"))

    // Inner
    Assert.Equal((0, 8), (getBracketsIndexesRightToLeft "|x+|x-1||"))
    Assert.Equal((0, 8), (getBracketsIndexesRightToLeft "||x-1|+x|"))
    Assert.Equal((0, 12), (getBracketsIndexesRightToLeft "|x+|x-|1-x|||"))
    Assert.Equal((2, 14), (getBracketsIndexesRightToLeft "2+|||1-x|+x|+2|+2"))
    Assert.Equal((0, 16), (getBracketsIndexesRightToLeft "||||1-x|+x|+2|+3|"))

    // Outside
    Assert.Equal((0, 5), (getBracketsIndexesRightToLeft "|x+10|*|x+3|"))
    Assert.Equal((0, 5), (getBracketsIndexesRightToLeft "|x+10|*|x+3|*|x+2|"))

[<Fact>]
let ``Get symbol index (from left to right)`` () =
    // Default
    let line1 = "x+2"
    let (l1, r1) = getBracketsIndexesLeftToRight line1
    Assert.Equal(1, (getSymbolIndexLeftToRight l1 r1 '+' line1))
    Assert.Equal(-1, (getSymbolIndexLeftToRight l1 r1  '-' line1))

    let line2 = "-10.0^2"
    let (l2, r2) = getBracketsIndexesLeftToRight line2
    Assert.Equal(0, (getSymbolIndexLeftToRight l2 r2 '-' line2))

    // With brackets
    let line3 = "2.2+(x+2)"
    let (l3, r3) = getBracketsIndexesLeftToRight line3
    Assert.Equal(3, (getSymbolIndexLeftToRight l3 r3 '+' line3))

    let line4 = "(x+2)+2"
    let (l4, r4) = getBracketsIndexesLeftToRight line4
    Assert.Equal(5, (getSymbolIndexLeftToRight l4 r4 '+' line4))

    // Complex
    let line5 = "2.2+(x-2)*(x-3)"
    let (l5, r5) = getBracketsIndexesLeftToRight line5
    Assert.Equal(-1, (getSymbolIndexLeftToRight l5 r5 '-' line5))

    let line6 = "(2.2-x)-(x-2)*(x-3)"
    let (l6, r6) = getBracketsIndexesLeftToRight line6
    Assert.Equal(7, (getSymbolIndexLeftToRight l6 r6 '-' line6))

[<Fact>]
let ``Remove extra brackets`` () =
    // Default
    Assert.Equal("x+2", fst (removeExtraBrackets "x+2"))
    Assert.Equal("x+2", fst (removeExtraBrackets "(x+2)"))
    Assert.Equal("x+2", fst (removeExtraBrackets "((x+2))"))
    Assert.Equal("x+2", fst (removeExtraBrackets "(((x+2)))"))

    // Complex
    Assert.Equal("(x+2)+(x+3)", fst (removeExtraBrackets "(x+2)+(x+3)"))
    Assert.Equal("(x+2*(x+3))+(x+3)", fst (removeExtraBrackets "(x+2*(x+3))+(x+3)"))
    Assert.Equal("(x+2*(x+3))+(x+3)", fst (removeExtraBrackets "((x+2*(x+3))+(x+3))"))








