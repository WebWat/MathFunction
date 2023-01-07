module NodeFunctions

open Function
open Xunit

[<Fact>]
let ``Get default brackets indexes`` () =
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
let ``Get module brackets indexes`` () =
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
let ``Get symbol index`` () =
    // Default
    Assert.Equal(1, (getSymbolIndexLeft2Right -1 -1 '+' "x+2"))
    Assert.Equal(-1, (getSymbolIndexLeft2Right -1 -1 '+' "x-2"))
    Assert.Equal(0, (getSymbolIndexLeft2Right -1 -1 '-' "-10.0^2"))

    // With brackets
    Assert.Equal(3, (getSymbolIndexLeft2Right 4 8 '+' "2.2+(x+2)"))
    Assert.Equal(5, (getSymbolIndexLeft2Right 0 4 '+' "(x+2)+2"))
    Assert.Equal(-1, (getSymbolIndexLeft2Right 0 4 '+' "(x+2)"))

    //// Complex
    Assert.Equal(3, (getSymbolIndexLeft2Right 10 14 '+' "2.2+(x+2)*(x+3)"))
    Assert.Equal(-1, (getSymbolIndexLeft2Right 14 18 '-' "(2.2-x)+(x-2)*(x-3)"))

    Assert.Equal(7, (getSymbolIndexLeft2Right 14 18 '+' "(x-2.2)+(x+2)*(x+3)"))
    Assert.Equal(-1, (getSymbolIndexRight2Left 0 6 '+' "(2.2+x)*(x+2)-(x+3)*(x+2)"))

    // With brackets
    Assert.Equal(3, (getSymbolIndexRight2Left 4 8 '+' "2.2+(x+2)"))
    Assert.Equal(5, (getSymbolIndexRight2Left 0 4 '+' "(x+2)+2"))
    Assert.Equal(-1, (getSymbolIndexRight2Left 0 4 '+' "(x+2)"))

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







