[<AutoOpen>]
module Persimmon.Helper

open Persimmon

let assertSeqEquals (expected: seq<'T>) (actual: seq<'T>) =
  let eLen = Seq.length expected
  let aLen = Seq.length actual
  let violated message = NotPassed(None, NotPassedCause.Violated message)
  if eLen <> aLen
    then violated <| sprintf "%A ≠ %A\nExpect: length is %d\nActual: length is %d" expected actual eLen aLen
    else
      let result =
        (expected, actual)
        ||> Seq.zip
        |> Seq.tryPick (fun (e, a) -> if e <> a then Some (e, a) else None)
      match result with
      | Some (e, a) ->
        violated <| sprintf "%A ≠ %A\nExpect: %A\nActual: %A" expected actual e a
      | None -> Passed ()