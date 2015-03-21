namespace FoolsTests

open Fools
open AST
open NUnit.Framework
open Swensen.Unquote.Assertions

[<TestFixture>]
module PatternMatchTests =
  
  [<Test>]
  let t0() =
    let l = PatternMatchHelper.matches ("A", [PatVar "x"]) ("A", ["1"]) 
    test <@ l = Some (Map.ofList ["x", "1"]) @>

  [<Test>]
  let t1() =
    let l = PatternMatchHelper.matches ("A", [PatVar "x"]) ("B", ["1"]) 
    test <@ l = None @>


  [<Test>]
  let t() =
    let facts : Fact list =
      [("A",["1"])
       ("A",["2"])
       ("B",["10"])]
    let patterns : FactPattern list =
      [("A", [PatVar "x"])
       ("B", [PatVar "y"])]
    let s = Set.ofSeq <| PatternMatchHelper.findAllMatches facts patterns



    test <@ Set.ofList [Map.ofList["x","1"; "y","10"];Map.ofList["x","2"; "y","10"]] = s @>

