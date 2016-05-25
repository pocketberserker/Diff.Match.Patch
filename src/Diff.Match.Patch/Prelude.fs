[<AutoOpen>]
module internal DiffMatchPatch.Prelude

open System
open System.Collections.Generic

[<RequireQualifiedAccess>]
module  String =

  let javaSubstring begin' end' (s: string) = s.Substring(begin', end' - begin')

type List<'T> with

  member this.Splice(start, count, [<ParamArray>] objects: 'T []) =
    let deletedRange = this.GetRange(start, count)
    this.RemoveRange(start, count)
    this.InsertRange(start, objects)
    deletedRange
