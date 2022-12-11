module Tests

open System
open Main
open Xunit


let signifNumbers count (value: float) = 
    let str = String.Format("{0:f20}", value)
    Double.Parse str[..count]

[<Fact>]
let ``Simple limits`` () =
    Assert.Equal(4., (limit (fun x -> x * x) 2 10 Position.Left))
    Assert.Equal(1., (limit (fun x -> cos x) 0 10 Position.Left))

    Assert.Equal(1., (limit (fun x -> cos x) 0 10 Position.Right))
    Assert.Equal(0., (limit (fun x -> tan x) 0 10 Position.Left))

[<Fact>]
let ``Left & right limits`` () =
    Assert.Equal(-infinity, (limit (fun x -> 1.0 / x) 0 10 Position.Left))
    Assert.Equal(infinity, (limit (fun x -> 1.0 / x) 0 10 Position.Right))

    Assert.Equal(-1., (limit (fun x -> (x - 5.0) / abs(x - 5.0)) 5 10 Position.Left))
    Assert.Equal(1., (limit (fun x -> (x - 5.0) / abs(x - 5.0)) 5 10 Position.Right))

    Assert.Equal(-infinity, (limit (fun x -> x * x / (1.0 - x * x)) 1 10 Position.Right))
    Assert.Equal(infinity, (limit (fun x -> x * x / (1.0 - x * x)) 1 10 Position.Left))

[<Fact>]
let ``Not exist exception`` () =
    Assert.Throws<NotExist>(fun _ -> limit (fun x -> (sin(2.0 * x) - 2.0 * sin x)/(x * log(cos(5.0 * x)))) 3 6 Position.Left |> ignore)

[<Fact>]
let ``Complex limits`` () =
    let l1 = limit (fun x -> (sin(2.0 * x) - 2.0 * sin x)/(x * log(cos(5.0 * x)))) 0 6 Position.Left
    let r1 = limit (fun x -> (sin(2.0 * x) - 2.0 * sin x)/(x * log(cos(5.0 * x)))) 0 6 Position.Right
    Assert.Equal(0.079999, signifNumbers 7 l1)
    Assert.Equal(0.079999, signifNumbers 7 r1)

    let l1 = limit (fun x -> (sqrt(cos x) - Math.Pow(cos x, 1./3.))/pown (Math.Atan(x)) 2) 0 3 Position.Left
    let r1 = limit (fun x -> (sqrt(cos x) - Math.Pow(cos x, 1./3.))/pown (Math.Atan(x)) 2) 0 3 Position.Right
    Assert.Equal(-0.083, signifNumbers 5 l1)
    Assert.Equal(-0.083, signifNumbers 5 r1)
