[<AutoOpen>]
module internal DiffMatchPatch.Prelude

open System
open System.Text
open System.Collections.Generic

[<RequireQualifiedAccess>]
module String =

  let javaSubstring begin' end' (s: string) = s.Substring(begin', end' - begin')

type List<'T> with

  member this.Splice(start, count, [<ParamArray>] objects: 'T []) =
    let deletedRange = this.GetRange(start, count)
    this.RemoveRange(start, count)
    this.InsertRange(start, objects)
    deletedRange

let isHexDigit (c: char) =
  let i = int c
  (i >= int '0' && i <= int '9') || (i >= int 'a' && i <= int 'f') || (i >= int 'A' && i <= int 'F')

let isHexDigits xs = List.forall isHexDigit xs

type Uri with

  static member Encode(data: string) =
    let appendHex xs (x: byte) = sprintf "%s%%%02x" xs x
    let encoding = UTF8Encoding()
    data.ToCharArray()
    |> Array.fold (fun xs c ->
      match encoding.GetBytes([|c|]) with
      | [|x|] ->
        match c with
        | _ when x < 128uy && Char.IsLetterOrDigit(c) -> xs + string c
        | ' ' -> xs + "+"
        | '!' | '(' | ')' | '*' | '-' | '_' | '.' -> xs + string c
        | _ -> appendHex xs x
      | cs -> cs |> Array.fold appendHex xs
    ) ""

  static member Decode(data: string) =
    let encoding = UTF8Encoding(false, true)
    let getHeadEscapedChar cs =
      let rec inner xs = function
      | '%' :: a :: b :: cs when isHexDigits [a; b] ->
        inner (Convert.ToByte(string a + string b, 16) :: xs) cs
      | _ -> xs
      inner [] cs |> List.rev |> List.toArray
    let rec inner xs = function
    | [] -> xs
    | ('%'::css) as cs ->
      match getHeadEscapedChar cs with
      | [||] -> inner (xs + "%") css
      | c ->
        let l = Array.length c
        inner (xs + encoding.GetString(c, 0, l)) (cs |> Seq.skip (l * 3) |> Seq.toList)
    | c::cs ->
      let c =
        match c with
        | '+' -> " "
        | _ -> Uri.UnescapeDataString(string c)
      inner (xs + c) cs
    data.ToCharArray()
    |> Array.toList
    |> inner ""
