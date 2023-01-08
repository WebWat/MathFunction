module NodeFunctions

open Function
open Xunit

[<Fact>]
let ``Get default brackets indexes (from left to right)`` () =
    // Not found
    Assert.Equal((-1, -1), (getBracketsIndexesLeft2Right "x+2"))

    // Default
    Assert.Equal((0, 2), (getBracketsIndexesLeft2Right "(x)"))
    Assert.Equal((0, 4), (getBracketsIndexesLeft2Right "(x+3)"))

    // Inner
    Assert.Equal((0, 8), (getBracketsIndexesLeft2Right "(x+(x-1))"))    
    Assert.Equal((0, 8), (getBracketsIndexesLeft2Right "((x-1)+x)"))
    Assert.Equal((0, 12), (getBracketsIndexesLeft2Right "(x+(x-(1-x)))"))
    Assert.Equal((0, 12), (getBracketsIndexesLeft2Right "(((1-x)+x)+2)"))

    // Outside
    Assert.Equal((7, 11), (getBracketsIndexesLeft2Right "(x+10)*(x+3)"))
    Assert.Equal((13, 17), (getBracketsIndexesLeft2Right "(x+10)*(x+3)*(x+2)"))

[<Fact>]
let ``Get default brackets indexes (from right to left)`` () =
    // Not found
    Assert.Equal((-1, -1), (getBracketsIndexesRight2Left "x+2"))

    // Default
    Assert.Equal((0, 2), (getBracketsIndexesRight2Left "(x)"))
    Assert.Equal((0, 4), (getBracketsIndexesRight2Left "(x+3)"))

    // Inner
    Assert.Equal((0, 8), (getBracketsIndexesRight2Left "(x+(x-1))"))    
    Assert.Equal((0, 8), (getBracketsIndexesRight2Left "((x-1)+x)"))
    Assert.Equal((0, 12), (getBracketsIndexesRight2Left "(x+(x-(1-x)))"))
    Assert.Equal((0, 12), (getBracketsIndexesRight2Left "(((1-x)+x)+2)"))

    // Outside
    Assert.Equal((0, 5), (getBracketsIndexesRight2Left "(x+10)*(x+3)"))
    Assert.Equal((0, 5), (getBracketsIndexesRight2Left "(x+10)*(x+3)*(x+2)"))

[<Fact>]
let ``Get module brackets indexes (from left to right)`` () =
    // Not found
    Assert.Equal((-1, -1), (getBracketsIndexesLeft2Right "x+2"))

    // Default
    Assert.Equal((0, 2), (getBracketsIndexesLeft2Right "|x|"))
    Assert.Equal((0, 4), (getBracketsIndexesLeft2Right "|x+3|"))

    // Inner
    Assert.Equal((0, 8), (getBracketsIndexesLeft2Right "|x+|x-1||"))
    Assert.Equal((0, 8), (getBracketsIndexesLeft2Right "||x-1|+x|"))
    Assert.Equal((2, 14), (getBracketsIndexesLeft2Right "2+|x+|x-|1-x|||+2"))
    Assert.Equal((0, 19), (getBracketsIndexesLeft2Right "|x+|x-|1-x*|23+x||||"))
    Assert.Equal((2, 14), (getBracketsIndexesLeft2Right "2+|||1-x|+x|+2|"))

    // Outside
    Assert.Equal((7, 11), (getBracketsIndexesLeft2Right "|x+10|*|x+3|"))
    Assert.Equal((13, 17), (getBracketsIndexesLeft2Right "|x+10|*|x+3|*|x+2|"))

    // Not found
    Assert.Equal((-1, -1), (getBracketsIndexesRight2Left "x+2"))

[<Fact>]
let ``Get module brackets indexes (from right to left)`` () =
    // Not found
    Assert.Equal((-1, -1), (getBracketsIndexesRight2Left "x+2"))

    // Default
    Assert.Equal((0, 2), (getBracketsIndexesRight2Left "|x|"))
    Assert.Equal((0, 4), (getBracketsIndexesRight2Left "|x+3|"))

    // Inner
    Assert.Equal((0, 8), (getBracketsIndexesRight2Left "|x+|x-1||"))
    Assert.Equal((0, 8), (getBracketsIndexesRight2Left "||x-1|+x|"))
    Assert.Equal((0, 12), (getBracketsIndexesRight2Left "|x+|x-|1-x|||"))
    Assert.Equal((2, 14), (getBracketsIndexesRight2Left "2+|||1-x|+x|+2|+2"))
    Assert.Equal((0, 16), (getBracketsIndexesRight2Left "||||1-x|+x|+2|+3|"))

    // Outside
    Assert.Equal((0, 5), (getBracketsIndexesRight2Left "|x+10|*|x+3|"))
    Assert.Equal((0, 5), (getBracketsIndexesRight2Left "|x+10|*|x+3|*|x+2|"))

[<Fact>]
let ``Get symbol index (from left to right)`` () =
    // Default
    let line1 = "x+2"
    let (l1, r1) = getBracketsIndexesLeft2Right line1
    Assert.Equal(1, (getSymbolIndexLeft2Right l1 r1 '+' line1))
    Assert.Equal(-1, (getSymbolIndexLeft2Right l1 r1  '-' line1))

    let line2 = "-10.0^2"
    let (l2, r2) = getBracketsIndexesLeft2Right line2
    Assert.Equal(0, (getSymbolIndexLeft2Right l2 r2 '-' line2))

    // With brackets
    let line3 = "2.2+(x+2)"
    let (l3, r3) = getBracketsIndexesLeft2Right line3
    Assert.Equal(3, (getSymbolIndexLeft2Right l3 r3 '+' line3))

    let line4 = "(x+2)+2"
    let (l4, r4) = getBracketsIndexesLeft2Right line4
    Assert.Equal(5, (getSymbolIndexLeft2Right l4 r4 '+' line4))

    // Complex
    let line5 = "2.2+(x-2)*(x-3)"
    let (l5, r5) = getBracketsIndexesLeft2Right line5
    Assert.Equal(-1, (getSymbolIndexLeft2Right l5 r5 '-' line5))

    let line6 = "(2.2-x)-(x-2)*(x-3)"
    let (l6, r6) = getBracketsIndexesLeft2Right line6
    Assert.Equal(7, (getSymbolIndexLeft2Right l6 r6 '-' line6))

[<Fact>]
let ``Get symbol index (from right to left)`` () =
    // Default
    let line1 = "x+2"
    let (l1, r1) = getBracketsIndexesRight2Left line1
    Assert.Equal(1, (getSymbolIndexRight2Left l1 r1 '+' line1))
    Assert.Equal(-1, (getSymbolIndexRight2Left l1 r1  '-' line1))

    let line2 = "-10.0^2"
    let (l2, r2) = getBracketsIndexesRight2Left line2
    Assert.Equal(0, (getSymbolIndexRight2Left l2 r2 '-' line2))

    // With brackets
    let line3 = "2.2+(x+2)"
    let (l3, r3) = getBracketsIndexesRight2Left line3
    Assert.Equal(3, (getSymbolIndexRight2Left l3 r3 '+' line3))

    let line4 = "(x+2)+2"
    let (l4, r4) = getBracketsIndexesRight2Left line4
    Assert.Equal(5, (getSymbolIndexRight2Left l4 r4 '+' line4))

    // Complex
    let line5 = "(2.2-x)*(x-2)+(x-3)"
    let (l5, r5) = getBracketsIndexesRight2Left line5
    Assert.Equal(-1, (getSymbolIndexRight2Left l5 r5 '-' line5))

    let line6 = "(2.2-x)*(x-2)-(x-3)"
    let (l6, r6) = getBracketsIndexesRight2Left line6
    Assert.Equal(13, (getSymbolIndexRight2Left l6 r6 '-' line6))

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

//[<Fact>]
//let ``Open brackets`` () =
    // Default
    //Assert.Equal("x+2", fst (removeExtraBrackets "x+2"))
    //Assert.Equal("x+2", fst (removeExtraBrackets "(x+2)"))
    //Assert.Equal("x+2", fst (removeExtraBrackets "((x+2))"))
    //Assert.Equal("x+2", fst (removeExtraBrackets "(((x+2)))"))

    //// Complex
    //Assert.Equal("(x+2)+(x+3)", fst (removeExtraBrackets "(x+2)+(x+3)"))
    //Assert.Equal("(x+2*(x+3))+(x+3)", fst (removeExtraBrackets "(x+2*(x+3))+(x+3)"))
    //Assert.Equal("(x+2*(x+3))+(x+3)", fst (removeExtraBrackets "((x+2*(x+3))+(x+3))"))







