module NodeFunctions

open Function
open Xunit

[<Fact>]
let ``Get default brackets indexes`` () =
    // Not found
    Assert.Equal((-1, -1), (getBracketsIndexes "x+2"))

    // Default
    Assert.Equal((0, 2), (getBracketsIndexes "(x)"))
    Assert.Equal((0, 4), (getBracketsIndexes "(x+3)"))

    // Inner
    Assert.Equal((0, 8), (getBracketsIndexes "(x+(x-1))"))    
    Assert.Equal((0, 8), (getBracketsIndexes "((x-1)+x)"))
    Assert.Equal((0, 12), (getBracketsIndexes "(x+(x-(1-x)))"))
    Assert.Equal((0, 12), (getBracketsIndexes "(((1-x)+x)+2)"))

    // Outside
    Assert.Equal((7, 11), (getBracketsIndexes "(x+10)*(x+3)"))
    Assert.Equal((13, 17), (getBracketsIndexes "(x+10)*(x+3)*(x+2)"))

[<Fact>]
let ``Get module brackets indexes`` () =
    // Not found
    Assert.Equal((-1, -1), (getBracketsIndexes "x+2"))

    // Default
    Assert.Equal((0, 2), (getBracketsIndexes "|x|"))
    Assert.Equal((0, 4), (getBracketsIndexes "|x+3|"))

    // Inner
    Assert.Equal((0, 8), (getBracketsIndexes "|x+|x-1||"))
    Assert.Equal((0, 8), (getBracketsIndexes "||x-1|+x|"))
    Assert.Equal((0, 12), (getBracketsIndexes "|x+|x-|1-x|||"))
    Assert.Equal((0, 12), (getBracketsIndexes "|||1-x|+x|+2|"))

    // Outside
    Assert.Equal((7, 11), (getBracketsIndexes "|x+10|*|x+3|"))
    Assert.Equal((13, 17), (getBracketsIndexes "|x+10|*|x+3|*|x+2|"))

[<Fact>]
let ``Get symbol index`` () =
    // Default
    Assert.Equal(1, (getSymbolIndex -1 -1 '+' "x+2"))
    Assert.Equal(-1, (getSymbolIndex -1 -1 '+' "x-2"))
    Assert.Equal(0, (getSymbolIndex -1 -1 '-' "-10.0^2"))

    // With brackets
    Assert.Equal(3, (getSymbolIndex 4 8 '+' "2.2+(x+2)"))
    Assert.Equal(5, (getSymbolIndex 0 4 '+' "(x+2)+2"))
    Assert.Equal(-1, (getSymbolIndex 0 4 '+' "(x+2)"))

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







