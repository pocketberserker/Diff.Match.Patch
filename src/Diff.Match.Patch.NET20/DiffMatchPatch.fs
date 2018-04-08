namespace DiffMatchPatch

open System
open System.Text
open System.Collections.Generic
open System.Text.RegularExpressions
open FSharp.Reflection

type Operation =
  | Delete
  | Insert
  | Equal
with
  override this.ToString() =
    match FSharpValue.GetUnionFields(this, typeof<Operation>) with
    | (case, _) -> case.Name

[<CustomEquality; NoComparison>]
type Diff = {
  mutable Text: string
  mutable Operation: Operation
}
with

  override this.ToString() =
    let prettyText = this.Text.Replace('\n', '\u00b6')
    sprintf "Diff(%O,\"%s\")" this.Operation this.Text

  override this.Equals(other) =
    match other with
    | null -> false
    | :? Diff as other -> this.Operation = other.Operation && this.Text = other.Text
    | _ -> false

  override this.GetHashCode() = hash this.Text ^^^ hash this.Operation

module internal Helper =

  let unescapeForEncodeUriCompatability (str: string) =
    str.Replace("%21", "!").Replace("%7e", "~")
      .Replace("%27", "'").Replace("%28", "(").Replace("%29", ")")
      .Replace("%3b", ";").Replace("%2f", "/").Replace("%3f", "?")
      .Replace("%3a", ":").Replace("%40", "@").Replace("%26", "&")
      .Replace("%3d", "=").Replace("%2b", "+").Replace("%24", "$")
      .Replace("%2c", ",").Replace("%23", "#")

type Patch = {
  Diffs: ResizeArray<Diff>
  mutable Start1: int
  mutable Start2: int
  mutable Length1: int
  mutable Length2: int
}
with

  override this.ToString() =

    let coord1 =
      if this.Length1 = 0 then string this.Start1 + ",0"
      elif this.Length1 = 1 then Convert.ToString(this.Start1 + 1)
      else string (this.Start1 + 1) + "," + string this.Length1

    let coord2 =
      if this.Length2 = 0 then string this.Start2 + ",0"
      elif this.Length2 = 1 then Convert.ToString(this.Start2 + 1)
      else string (this.Start2 + 1) + "," + string this.Length2

    let text = StringBuilder()
    Printf.bprintf text "@@ -%s +%s @@\n" coord1 coord2
    for diff in this.Diffs do
      match diff.Operation with
      | Insert -> text.Append("+")
      | Delete -> text.Append("-")
      | Equal -> text.Append(" ")
      |> ignore
      text.Append(Uri.Encode(diff.Text).Replace('+', ' ')).Append('\n')
      |> ignore

    text.ToString()
    |> Helper.unescapeForEncodeUriCompatability

type DiffMatchPatch = {
  DiffTimeout: float32
  DiffDualThreshold: int16
  DiffEditCost: int
  MatchThreshold: float32
  MatchDistance: int
  MatchMaxBits: int
  PatchDeleteThreshold: float32
  PatchMargin: int16
}
with

  member this.DiffMain(text1, text2) = this.DiffMain(text1, text2, true)

  member this.DiffLinesToChars(text1, text2) =
    let lineArray = ResizeArray<string>()
    let lineHash = Dictionary<string, int>()
    lineArray.Add("")
    let chars1 = this.DiffLinesToCharsMunge(text1, lineArray, lineHash)
    let chars2 = this.DiffLinesToCharsMunge(text2, lineArray, lineHash)
    (chars1, chars2, lineArray)

  member private __.DiffLinesToCharsMunge(text, lineArray, lineHash: Dictionary<string, int>) =
    let chars = StringBuilder()
    let rec inner lineStart lineEnd =
      if lineEnd >= String.length text - 1 then ()
      else
        let lineEnd = text.IndexOf('\n', lineStart)
        let lineEnd = if lineEnd = -1 then text.Length - 1 else lineEnd
        let line = String.javaSubstring lineStart (lineEnd + 1) text
        let lineStart = lineEnd + 1
        if lineHash.ContainsKey(line) then chars.Append(char lineHash.[line]) |> ignore
        else
          lineArray.Add(line)
          let c = lineArray.Count - 1
          lineHash.Add(line, c)
          chars.Append(char c) |> ignore
        inner lineStart lineEnd
    inner 0 -1
    chars.ToString()

  member __.DiffCharsToLines(diffs: ICollection<Diff>, lineArray: ResizeArray<string>) =
    for diff in diffs do
      let text = StringBuilder()
      for y in [0 .. diff.Text.Length - 1] do
        text.Append(lineArray.[int diff.Text.[y]]) |> ignore
      diff.Text <- text.ToString()

  member internal __.DiffFootprint(x: int, y: int) = ((int64 x) <<< 32) + int64 y

  member internal this.DiffPath1(v_map: ResizeArray<Set<int64>>, text1, text2) =
    let path = LinkedList<Diff>()
    let x = String.length text1 |> ref
    let y = String.length text2 |> ref
    let rec inner lastOp d =
      if v_map.[d] |> Set.contains (this.DiffFootprint(!x - 1, !y)) then
        decr x
        match lastOp with
        | Some Delete ->
          let diff = Seq.head path
          diff.Text <- string text1.[!x] + diff.Text
        | _ ->
          path.AddFirst({ Text = text1.Substring(!x, 1); Operation = Delete }) |> ignore
        Some Delete
      elif v_map.[d] |> Set.contains (this.DiffFootprint(!x, !y - 1)) then
        decr y
        match lastOp with
        | Some Insert ->
          let diff = Seq.head path
          diff.Text <- string text2.[!y] + diff.Text
        | _ ->
          path.AddFirst({ Text = text2.Substring(!y, 1); Operation = Insert }) |> ignore
        Some Insert
      else
        decr x
        decr y
        match lastOp with
        | Some Equal ->
          let diff = Seq.head path
          diff.Text <- string text1.[!x] + diff.Text
        | _ ->
          path.AddFirst({ Text = text1.Substring(!x, 1); Operation = Equal }) |> ignore
        inner (Some Equal) d
    [|v_map.Count - 2 .. -1 .. 0|]
    |> Array.fold inner None
    |> ignore
    ResizeArray<Diff>(path)

  member internal this.DiffPath2(v_map: ResizeArray<Set<int64>>, text1, text2) =
    let path = LinkedList<Diff>()
    let x = String.length text1 |> ref
    let y = String.length text2 |> ref
    let rec inner lastOp d =
      if v_map.[d] |> Set.contains (this.DiffFootprint(!x - 1, !y)) then
        decr x
        match lastOp with
        | Some Delete ->
          let diff = Seq.last path
          diff.Text <- diff.Text + string text1.[text1.Length - !x - 1]
        | _ ->
          path.AddLast({ Text = text1.Substring(text1.Length - !x - 1, 1); Operation = Delete }) |> ignore
        Some Delete
      elif v_map.[d] |> Set.contains (this.DiffFootprint(!x, !y - 1)) then
        decr y
        match lastOp with
        | Some Insert ->
          let diff = Seq.last path
          diff.Text <- diff.Text + string text2.[text2.Length - !y - 1]
        | _ ->
          path.AddLast({ Text = text2.Substring(text2.Length - !y - 1, 1); Operation = Insert }) |> ignore
        Some Insert
      else
        decr x
        decr y
        match lastOp with
        | Some Equal ->
          let diff = Seq.last path
          diff.Text <- diff.Text + string text1.[text1.Length - !x - 1]
        | _ ->
          path.AddLast({ Text = text1.Substring(text1.Length - !x - 1, 1); Operation = Equal }) |> ignore
        inner (Some Equal) d
    [|v_map.Count - 2 .. -1 .. 0|]
    |> Array.fold inner None
    |> ignore
    ResizeArray<Diff>(path)

  member internal this.DiffMap(text1, text2) =
    let ms_end = DateTime.Now + new TimeSpan(0, 0, int this.DiffTimeout)
    let text1_length = String.length text1
    let text2_length = String.length text2
    let max_d = text1_length + text2_length - 1
    let doubleEnd = int this.DiffDualThreshold * 2 < max_d
    let mutable v_map1 = ResizeArray<Set<int64>>()
    let mutable v_map2 = ResizeArray<Set<int64>>()
    let v1 = Dictionary<int, int>()
    let v2 = Dictionary<int, int>()
    v1.Add(1, 0)
    v2.Add(1, 0)
    let footstep = ref 0L
    let footsteps = Dictionary<int64, int>()
    let mutable done' = false
    let front = ((text1_length + text2_length) % 2 = 1)

    let rec inner d =
      if d >= max_d then None
      elif this.DiffTimeout > 0.0f && DateTime.Now > ms_end then None
      else
        v_map1.Add(Set.empty)
        [|-d .. 2 .. d|]
        |> Array.fold (fun result k ->
          match result with
          | Some _ -> result
          | None ->
            let x =
              if k = -d || k <> d && v1.[k - 1] < v1.[k + 1] then v1.[k + 1]
              else v1.[k - 1] + 1
              |> ref
            let y = !x - k |> ref
            if doubleEnd then
              footstep := this.DiffFootprint(!x, !y)
              if front && (footsteps.ContainsKey(!footstep)) then done' <- true
              if not front then footsteps.Add(!footstep, d)
            while not done' && !x < text1_length && !y < text2_length  && text1.[!x] = text2.[!y] do
              incr x
              incr y
              if doubleEnd then
                footstep := this.DiffFootprint(!x, !y)
                if front && (footsteps.ContainsKey(!footstep)) then done' <- true
                if not front then footsteps.Add(!footstep, d)
            if v1.ContainsKey(k) then v1.[k] <- !x
            else v1.Add(k, !x)
            v_map1.[d] <- v_map1.[d] |> Set.add (this.DiffFootprint(!x, !y))
            if !x = text1_length && !y = text2_length then Some(this.DiffPath1(v_map1, text1, text2))
            elif done' then
              v_map2 <- v_map2.GetRange(0, footsteps.[!footstep] + 1)
              let a = this.DiffPath1(v_map1, text1.Substring(0, !x), text2.Substring(0, !y))
              a.AddRange(this.DiffPath2(v_map2, text1.Substring(!x), text2.Substring(!y)))
              Some a
            else None
          ) None
          |> function
          | Some result -> Some result
          | None when doubleEnd ->
            v_map2.Add(Set.empty)
            [|-d .. 2 .. d|]
            |> Array.fold (fun result k ->
              match result with
              | Some _ -> result
              | None ->
                let x =
                  if k = -d || k <> d && v2.[k - 1] < v2.[k + 1] then v2.[k + 1]
                  else v2.[k - 1] + 1
                  |> ref
                let y = !x - k |> ref
                footstep := this.DiffFootprint(text1_length - !x, text2_length - !y)
                if not front && footsteps.ContainsKey(!footstep) then done' <- true
                if front then footsteps.Add(!footstep, d)
                while not done' && !x < text1_length && !y < text2_length && text1.[text1_length - !x - 1] = text2.[text2_length - !y - 1] do
                  incr x
                  incr y
                  footstep := this.DiffFootprint(text1_length - !x, text2_length - !y)
                  if not front && footsteps.ContainsKey(!footstep) then done' <- true
                  if front then footsteps.Add(!footstep, d)
                if v2.ContainsKey(k) then v2.[k] <- !x
                else v2.Add(k, !x)
                v_map2.[d] <- v_map2.[d] |> Set.add (this.DiffFootprint(!x, !y))
                if done' then
                  v_map1 <- v_map1.GetRange(0, footsteps.[!footstep] + 1)
                  let a = this.DiffPath1(v_map1, text1.Substring(0, text1_length - !x), text2.Substring(0, text2_length - !y))
                  a.AddRange(this.DiffPath2(v_map2, text1.Substring(text1_length - !x), text2.Substring(text2_length - !y)))
                  Some a
                else None
            ) None
          | None -> None
        |> function
        | Some v -> Some v
        | None -> inner (d + 1)
    inner 0

  member __.DiffCommonPrefix(text1, text2) =
    let n = min (String.length text1) (String.length text2)
    let rec inner i =
      if i >= n then n
      elif text1.[i] <> text2.[i] then i
      else inner (i + 1)
    inner 0

  member __.DiffCommonSuffix(text1, text2) =
    let text1_length = String.length text1
    let text2_length = String.length text2
    let n = min text1_length text2_length
    let rec inner i =
      if i > n then n
      elif text1.[text1_length - i] <> text2.[text2_length - i] then i - 1
      else inner (i + 1)
    inner 1

  member private this.DiffHalfMatchI(longText: string, shortText: string, i) =
    let seed = longText.Substring(i, longText.Length / 4)
    let j = ref -1
    let best_common = ref ""
    let best_longText_a = ref ""
    let best_longText_b = ref ""
    let best_shortText_a = ref ""
    let best_shortText_b = ref ""
    while !j < shortText.Length && shortText.IndexOf(seed, !j + 1) <> -1 do
      j := shortText.IndexOf(seed, !j + 1)
      let prefixLength = this.DiffCommonPrefix(longText.Substring(i), shortText.Substring(!j))
      let suffixLength = this.DiffCommonSuffix(longText.Substring(0, i), shortText.Substring(0, !j))
      if String.length !best_common < suffixLength + prefixLength then
        best_common := shortText.Substring(!j - suffixLength, suffixLength) + shortText.Substring(!j, prefixLength)
        best_longText_a := longText.Substring(0, i - suffixLength)
        best_longText_b := longText.Substring(i + prefixLength)
        best_shortText_a := shortText.Substring(0, !j - suffixLength)
        best_shortText_b := shortText.Substring(!j + prefixLength)
    if String.length !best_common >= longText.Length / 2 then
      Some [|!best_longText_a; !best_longText_b; !best_shortText_a; !best_shortText_b; !best_common|]
    else None

  member internal this.DiffHalfMatch(text1, text2) =
    let longText = if String.length text1 > String.length text2 then text1 else text2
    let shortText = if String.length text1 > String.length text2 then text2 else text1
    if longText.Length < 10 || shortText.Length < 1 then None
    else
      let hm1 = this.DiffHalfMatchI(longText, shortText, (longText.Length + 3) / 4)
      let hm2 = this.DiffHalfMatchI(longText, shortText, (longText.Length + 1) / 2)
      match (hm1, hm2) with
      | (None, None) -> Choice1Of2 ()
      | (_, None) -> Choice2Of2 hm1
      | (None, _) -> Choice2Of2 hm2
      | (Some hm1, Some hm2) -> (if hm1.[4].Length > hm2.[4].Length then Some hm1 else Some hm2) |> Choice2Of2
      |> function
      | Choice1Of2 () -> None
      | Choice2Of2 hm ->
        if text1.Length > text2.Length then hm
        else hm |> Option.map (fun hm -> [|hm.[2]; hm.[3]; hm.[0]; hm.[1]; hm.[4]|])

  member private __.BLANKLINEEND = Regex("\\n\\r?\\n\\Z")
  member private __.BLANKLINESTART = Regex("\\A\\r?\\n\\r?\\n")

  member private this.DiffCleanupSemanticScore(one, two) =
    if String.IsNullOrEmpty one || String.IsNullOrEmpty two then 5
    else
      let score = ref 0
      if not <| Char.IsLetterOrDigit(one.[one.Length - 1]) || not <| Char.IsLetterOrDigit(two.[0]) then
        incr score
        if Char.IsWhiteSpace(one.[one.Length - 1]) || Char.IsWhiteSpace(two.[0]) then
          incr score
          if Char.IsControl(one.[one.Length - 1]) || Char.IsControl(two.[0]) then
            incr score
            if (this.BLANKLINEEND.IsMatch(one) || this.BLANKLINESTART.IsMatch(two)) then
              incr score
      !score

  member this.DiffCleanupSemanticLossless(diffs: ResizeArray<Diff>) =

    let pointer = ref 1

    while !pointer < diffs.Count - 1 do

      if diffs.[!pointer - 1].Operation = Equal && diffs.[!pointer + 1].Operation = Equal then

        let equality1 = ref diffs.[!pointer - 1].Text
        let edit = ref diffs.[!pointer].Text
        let equality2 = ref diffs.[!pointer + 1].Text

        let commonOffset = this.DiffCommonSuffix(!equality1, !edit)
        if commonOffset > 0 then
          let commonString = (!edit).Substring((!edit).Length - commonOffset)
          equality1 := (!equality1).Substring(0, (!equality1).Length - commonOffset)
          edit := commonString + (!edit).Substring(0, (!edit).Length - commonOffset)
          equality2 := commonString + !equality2

        let bestEquality1 = ref !equality1
        let bestEdit = ref !edit
        let bestEquality2 = ref !equality2
        let bestScore = this.DiffCleanupSemanticScore(!equality1, !edit) + this.DiffCleanupSemanticScore(!edit, !equality2) |> ref
        while (!edit).Length <> 0 && (!equality2).Length <> 0 && (!edit).[0] = (!equality2).[0] do
          equality1 := !equality1 + string (!edit).[0]
          edit := (!edit).Substring(1) + string (!equality2).[0]
          equality2 := (!equality2).Substring(1);
          let score = this.DiffCleanupSemanticScore(!equality1, !edit) + this.DiffCleanupSemanticScore(!edit, !equality2)
          if score >= !bestScore then
            bestScore := score
            bestEquality1 := !equality1
            bestEdit := !edit
            bestEquality2 := !equality2

        if diffs.[!pointer - 1].Text <> !bestEquality1 then
          if (!bestEquality1).Length <> 0 then diffs.[!pointer - 1].Text <- !bestEquality1
          else
            diffs.RemoveAt(!pointer - 1)
            decr pointer
          diffs.[!pointer].Text <- !bestEdit
          if (!bestEquality2).Length <> 0 then diffs.[!pointer + 1].Text <- !bestEquality2
          else
            diffs.RemoveAt(!pointer + 1)
            decr pointer
      incr pointer

  member this.DiffCleanupMerge(diffs: ResizeArray<Diff>) =
    diffs.Add({ Text = ""; Operation = Equal })
    let pointer = ref 0
    let count_delete = ref 0
    let count_insert = ref 0
    let text_delete = ref ""
    let text_insert = ref ""
    while !pointer < diffs.Count do
      let diff = diffs.[!pointer]
      match diff.Operation with
      | Insert ->
        incr count_insert
        text_insert := !text_insert + diff.Text
        incr pointer
      | Delete ->
        incr count_delete
        text_delete := !text_delete + diff.Text
        incr pointer
      | Equal ->
        if !count_delete <> 0 || !count_insert <> 0 then
          if !count_delete <> 0 && !count_insert <> 0 then
            let commonLength = this.DiffCommonPrefix(!text_insert, !text_delete)
            if commonLength <> 0 then
              if (!pointer - !count_delete - !count_insert) > 0 && diffs.[!pointer - !count_delete - !count_insert - 1].Operation = Equal then
                let diff = diffs.[!pointer - !count_delete - !count_insert - 1]
                diff.Text <- diff.Text + (!text_insert).Substring(0, commonLength)
              else
                diffs.Insert(0, { Text = (!text_insert).Substring(0, commonLength); Operation = Equal })
                incr pointer
              text_insert := (!text_insert).Substring(commonLength)
              text_delete := (!text_delete).Substring(commonLength)
            let commonLength = this.DiffCommonSuffix(!text_insert, !text_delete)
            if commonLength <> 0 then
              let diff = diffs.[!pointer]
              diff.Text <- (!text_insert).Substring((!text_insert).Length - commonLength) + diff.Text
              text_insert := (!text_insert).Substring(0, (!text_insert).Length - commonLength)
              text_delete := (!text_delete).Substring(0, (!text_delete).Length - commonLength)
          if !count_delete = 0 then
            diffs.Splice(!pointer - !count_delete - !count_insert, !count_delete + !count_insert, { Text = !text_insert; Operation = Insert })
          elif !count_insert = 0 then
            diffs.Splice(!pointer - !count_delete - !count_insert, !count_delete + !count_insert, { Text = !text_delete; Operation = Delete })
          else diffs.Splice(!pointer - !count_delete - !count_insert, !count_delete + !count_insert, { Text = !text_delete; Operation = Delete }, { Text = !text_insert; Operation = Insert})
          |> ignore
          pointer := !pointer - !count_delete - !count_insert + (if !count_delete <> 0 then 1 else 0) + (if !count_insert <> 0 then 1 else 0) + 1
        elif !pointer <> 0 && diffs.[!pointer - 1].Operation = Equal then
          diffs.[!pointer - 1].Text <- diffs.[!pointer - 1].Text + diff.Text
          diffs.RemoveAt(!pointer)
        else incr pointer
        count_insert := 0
        count_delete := 0
        text_delete := ""
        text_insert := ""
    if String.IsNullOrEmpty diffs.[diffs.Count - 1].Text then
      diffs.RemoveAt(diffs.Count - 1)
    let changes = ref false
    pointer := 1
    while !pointer < diffs.Count - 1 do
      let diff1 = diffs.[!pointer - 1]
      let diff2 = diffs.[!pointer]
      let diff3 = diffs.[!pointer + 1]
      if diff1.Operation = Equal && diff3.Operation = Equal then
        if diff2.Text.EndsWith(diff1.Text) then
          diff2.Text <- diff1.Text + diff2.Text.Substring(0, diff2.Text.Length - diff1.Text.Length)
          diff3.Text <- diff1.Text + diff3.Text
          diffs.Splice(!pointer - 1, 1) |> ignore
          changes := true
        elif diff2.Text.StartsWith(diff3.Text) then
          diff1.Text <- diff1.Text + diff3.Text
          diff2.Text <- diff2.Text.Substring(diff3.Text.Length) + diff3.Text
          diffs.Splice(!pointer + 1, 1) |> ignore
          changes := true
      incr pointer
    if !changes then this.DiffCleanupMerge(diffs)

  member this.DiffCleanupEfficiency(diffs: ResizeArray<Diff>) =
    let changes = ref false
    let equalities = Stack<int>()
    let lastEquality = ref ""
    let pointer = ref 0
    let pre_ins = ref false
    let pre_del = ref false
    let post_ins = ref false
    let post_del = ref false
    while !pointer < diffs.Count do
      let diff = diffs.[!pointer]
      match diff.Operation with
      | Equal ->
        if diff.Text.Length < this.DiffEditCost && (!post_ins || !post_del) then
          equalities.Push(!pointer);
          pre_ins := !post_ins
          pre_del := !post_del
          lastEquality := diff.Text
        else
          equalities.Clear()
          lastEquality := ""
        post_ins := false
        post_del := false
      | _ ->
        if diff.Operation = Delete then post_del := true
        else post_ins := true
        if (!lastEquality).Length <> 0 && ((!pre_ins && !pre_del && !post_ins && !post_del) || (((!lastEquality).Length < this.DiffEditCost / 2) && ((if !pre_ins then 1 else 0) + (if !pre_del then 1 else 0) + (if !post_ins then 1 else 0) + (if !post_del then 1 else 0)) = 3)) then
          diffs.Insert(equalities.Peek(), { Operation = Delete; Text = !lastEquality })
          diffs.[equalities.Peek() + 1].Operation <- Insert;
          equalities.Pop() |> ignore
          lastEquality := ""
          if !pre_ins && !pre_del then
            post_ins := true
            post_del := true
            equalities.Clear()
          else
            if equalities.Count > 0 then equalities.Pop() |> ignore
            pointer := if equalities.Count > 0 then equalities.Peek() else -1
            post_ins := false
            post_del := false
          changes := true
      incr pointer
    if !changes then this.DiffCleanupMerge(diffs)

  member this.DiffCleanupSemantic(diffs: ResizeArray<Diff>) =
    let changes = ref false
    let equalities = Stack<int>()
    let lastEquality: string option ref = ref None
    let pointer = ref 0
    let length_changes1 = ref 0
    let length_changes2 = ref 0
    while !pointer < diffs.Count do
      let diff = diffs.[!pointer]
      match diff.Operation with
      | Equal ->
        equalities.Push(!pointer)
        length_changes1 := !length_changes2
        length_changes2 := 0
        lastEquality := Some diff.Text
      | _ ->
        length_changes2 := !length_changes2 + diff.Text.Length
        match !lastEquality with
        | Some ls when ls.Length <= !length_changes1 && ls.Length <= !length_changes2 ->
          diffs.Insert(equalities.Peek(), { Text = ls; Operation = Delete })
          diffs.[equalities.Peek() + 1].Operation <- Insert
          equalities.Pop() |> ignore
          if equalities.Count > 0 then equalities.Pop() |> ignore
          pointer := if equalities.Count > 0 then equalities.Peek() else -1
          length_changes1 := 0
          length_changes2 := 0
          lastEquality := None
          changes := true
        | _ -> ()
      incr pointer
    if !changes then this.DiffCleanupMerge(diffs)
    this.DiffCleanupSemanticLossless(diffs)

  member internal this.DiffCompute(text1, text2, checkLines) =

    let diffs = ResizeArray<Diff>()

    if String.IsNullOrEmpty text1 then
      diffs.Add({ Text = text2; Operation = Insert })
      diffs

    elif String.IsNullOrEmpty text2 then
      diffs.Add({ Text = text1; Operation = Delete })
      diffs

    else

      let longText = if text1.Length > text2.Length then text1 else text2
      let shortText = if text1.Length > text2.Length then text2 else text1
      let i = longText.IndexOf(shortText)

      if i <> -1 then
        let op = if text1.Length > text2.Length then Delete else Insert
        diffs.Add({ Text = longText.Substring(0, i); Operation = op })
        diffs.Add({ Text = shortText; Operation = Equal })
        diffs.Add({ Text = longText.Substring(i + shortText.Length); Operation = op })
        diffs

      else

        match this.DiffHalfMatch(text1, text2) with
        | Some hm ->

          let text1_a = hm.[0]
          let text1_b = hm.[1]
          let text2_a = hm.[2]
          let text2_b = hm.[3]
          let mid_common = hm.[4]

          let diffs = this.DiffMain(text1_a, text2_a, checkLines)
          let diffs_b = this.DiffMain(text1_b, text2_b, checkLines)

          diffs.Add({ Text = mid_common; Operation = Equal })
          diffs.AddRange(diffs_b)
          diffs

        | None ->

          let checkLines = if checkLines && (text1.Length < 100 || text2.Length < 100) then false else checkLines

          let (text1, text2, lineArray) =
            if checkLines then
              let (text1, text2, lineArray) = this.DiffLinesToChars(text1, text2)
              (text1, text2, Some lineArray)
            else (text1, text2, None)

          let diffs =
            match this.DiffMap(text1, text2) with
            | Some diffs -> diffs
            | None ->
              let diffs = ResizeArray<Diff>()
              diffs.Add({ Text = text1; Operation = Delete })
              diffs.Add({ Text = text2; Operation = Insert })
              diffs

          if checkLines then

            // TODO: null safe
            this.DiffCharsToLines(diffs, match lineArray with Some v -> v | None -> null)
            this.DiffCleanupSemantic(diffs)

            diffs.Add({ Text = ""; Operation = Equal })

            let rec inner pointer count_delete count_insert text_delete text_insert =
              if pointer >= diffs.Count then ()
              else
                let diff = diffs.[pointer]
                match diff.Operation with
                | Insert -> inner (pointer + 1) count_delete (count_insert + 1) text_delete (text_insert + diff.Text)
                | Delete -> inner (pointer + 1) (count_delete + 1) count_insert (text_delete + diff.Text) text_insert
                | Equal ->
                  let pointer =
                    if count_delete >= 1 && count_insert >= 1 then
                      let a = this.DiffMain(text_delete, text_insert, false)
                      diffs.RemoveRange(pointer - count_delete - count_insert, count_delete + count_insert)
                      let pointer = pointer - count_delete - count_insert
                      diffs.InsertRange(pointer, a)
                      pointer + a.Count
                    else pointer
                  inner (pointer + 1) 0 0 "" ""

            inner 0 0 0 "" ""

            diffs.RemoveAt(diffs.Count - 1)

          diffs

  member this.DiffMain(text1, text2, checkLines): ResizeArray<Diff> =

    if text1 = text2 then

      let diffs = ResizeArray<Diff>()
      diffs.Add({ Text = text1; Operation = Equal})
      diffs

    else

      let commonLength = this.DiffCommonPrefix(text1, text2)
      let commonPrefix = text1.Substring(0, commonLength)
      let text1 = text1.Substring(commonLength)
      let text2 = text2.Substring(commonLength)

      let commonLength = this.DiffCommonSuffix(text1, text2)
      let commonSuffix = text1.Substring(text1.Length - commonLength)
      let text1 = text1.Substring(0, text1.Length - commonLength)
      let text2 = text2.Substring(0, text2.Length - commonLength)

      let diffs = this.DiffCompute(text1, text2, checkLines)

      if not <| String.IsNullOrEmpty commonPrefix then
        diffs.Insert(0, { Text = commonPrefix; Operation = Equal })
      if not <| String.IsNullOrEmpty commonSuffix then
        diffs.Add({ Text = commonSuffix; Operation = Equal })

      this.DiffCleanupMerge(diffs)
      diffs

  member __.DiffXIndex(diffs: ResizeArray<Diff>, loc) =
    let rec inner chars1 chars2 last_chars1 last_chars2 = function
    | [] -> (None, last_chars1, last_chars2)
    | x::xs ->
      let chars1 = if x.Operation <> Insert then chars1 + x.Text.Length else chars1
      let chars2 = if x.Operation <> Delete then chars2 + x.Text.Length else chars2
      if chars1 > loc then (Some x, last_chars1, last_chars2)
      else inner chars1 chars2 chars1 chars2 xs
    match inner 0 0 0 0 (List.ofSeq diffs) with
    | (Some lastDiff, _, last_chars2) when lastDiff.Operation = Delete -> last_chars2
    | (_, last_chars1, last_chars2) -> last_chars2 + (loc - last_chars1)

  member __.DiffPrettyHtml(diffs: ResizeArray<Diff>) =
    let html = StringBuilder()
    let rec inner (i: int) = function
    | [] -> html.ToString()
    | x::xs ->
      let text = x.Text.Replace("&", "&amp;").Replace("<", "&lt;").Replace(">", "&gt;").Replace("\n", "&para;<BR>")
      match x.Operation with
      | Insert ->
        html.Append("<INS STYLE=\"background:#E6FFE6;\" TITLE=\"i=").Append(i).Append("\">").Append(text).Append("</INS>")
        |> ignore
      | Delete ->
        html.Append("<DEL STYLE=\"background:#FFE6E6;\" TITLE=\"i=").Append(i).Append("\">").Append(text).Append("</DEL>")
        |> ignore
      | Equal ->
        html.Append("<SPAN TITLE=\"i=").Append(i).Append("\">").Append(text).Append("</SPAN>")
        |> ignore
      if x.Operation <> Delete then inner (i + x.Text.Length) xs
      else inner i xs
    diffs |> Seq.toList |> inner 0

  member __.DiffText1(diffs: ResizeArray<Diff>) =
    diffs
    |> Seq.fold (fun (text: StringBuilder) diff ->
      match diff.Operation with
      | Delete | Equal -> text.Append(diff.Text)
      | Insert -> text
    ) (StringBuilder())
    |> fun text -> text.ToString()

  member __.DiffText2(diffs: ResizeArray<Diff>) =
    diffs
    |> Seq.fold (fun (text: StringBuilder) diff ->
      match diff.Operation with
      | Insert | Equal -> text.Append(diff.Text)
      | Delete -> text
    ) (StringBuilder())
    |> fun text -> text.ToString()

  member __.DiffLevenshtein(diffs: ResizeArray<Diff>) =
    diffs
    |> Seq.fold (fun (levenshtein, insertions, deletions) diff ->
      match diff.Operation with
      | Insert -> (levenshtein, insertions + diff.Text.Length, deletions)
      | Delete -> (levenshtein, insertions, deletions + diff.Text.Length)
      | Equal -> (levenshtein + max insertions deletions, 0, 0)
    ) (0, 0, 0)
    |> fun (levenshtein, insertions, deletions) -> levenshtein + max insertions deletions

  member __.DiffToDelta(diffs: ResizeArray<Diff>) =
    let text = StringBuilder()
    for diff in diffs do
      match diff.Operation with
      | Insert -> text.Append("+").Append(Uri.Encode(diff.Text).Replace('+', ' ')).Append("\t")
      | Delete -> text.Append("-").Append(diff.Text.Length).Append("\t")
      | Equal -> text.Append("=").Append(diff.Text.Length).Append("\t")
      |> ignore
    let delta = text.ToString()
    if delta.Length <> 0 then
      let delta = delta.Substring(0, delta.Length - 1)
      Helper.unescapeForEncodeUriCompatability delta
    else delta

  member __.DiffFromDelta(text1: string, delta: string) =
    let diffs = ResizeArray<Diff>()
    let pointer = ref 0
    delta.Split([| "\t" |], StringSplitOptions.None)
    |> Array.iter (fun token ->
      if String.IsNullOrEmpty token then ()
      else
        let param = token.Substring(1)
        match token.[0] with
        | '+' ->
          let param = param.Replace("+", "%2b")
          let param = Uri.Decode(param)
          diffs.Add({ Text = param; Operation = Insert })
        | ('-' | '=') as c ->
          let n =
            try
              Convert.ToInt32(param)
            with :? FormatException as e -> raise <| ArgumentException("Invalid number in diff_fromDelta: " + param, e)
          if n < 0 then raise <| ArgumentException("Negative number in diff_fromDelta: " + param)
          let text =
            try
              let text = text1.Substring(!pointer, n)
              pointer := !pointer + n
              text
            with :? ArgumentOutOfRangeException as e ->
              raise <| ArgumentException(sprintf "Delta length (%d) larger than source text length (%d)." !pointer text1.Length, e)
          if c = '=' then diffs.Add({ Text = text; Operation = Equal })
          else diffs.Add({ Text = text; Operation = Delete })
        | c ->
          raise <| ArgumentException("Invalid diff operation in diff_fromDelta: " + string c)
    )
    if !pointer <> text1.Length then
      raise <| ArgumentException(sprintf "Delta length (%d) smaller than source text length (%d)." !pointer text1.Length)
    else diffs

  member __.MatchAlphabet(pattern: string) =
    let s = Dictionary<char, int>()
    let char_pattern = pattern.ToCharArray()
    for c in char_pattern do
      if not <| s.ContainsKey(c) then s.Add(c, 0)
    let i = ref 0
    for c in char_pattern do
      let value = s.[c] ||| (1 <<< (pattern.Length - !i - 1))
      s.[c] <- value
      incr i
    s

  member this.MatchBitapScore(e: int, x: int, loc: int, pattern) =
    let accuracy = float32 e / float32 (String.length pattern)
    let proximity = abs (loc - x)
    if this.MatchDistance = 0 then if proximity = 0 then float accuracy else 1.0
    else float accuracy + (float proximity / float this.MatchDistance)

  member this.MatchBitap(text: string, pattern, loc: int) =

    let s = this.MatchAlphabet(pattern)

    let score_threshold = float this.MatchThreshold |> ref
    let best_loc = text.IndexOf(pattern, loc) |> ref
    if !best_loc <> -1 then
      score_threshold := min (this.MatchBitapScore(0, !best_loc, loc, pattern)) !score_threshold
      best_loc := text.LastIndexOf(pattern, min (loc + pattern.Length) text.Length)
      if !best_loc <> -1 then
        score_threshold := min (this.MatchBitapScore(0, !best_loc, loc, pattern)) !score_threshold

    let matchMask = 1 <<< (pattern.Length - 1)
    best_loc := -1

    let bin_min = ref 0
    let bin_mid = ref 0
    let bin_max = pattern.Length + text.Length |> ref
    let last_rd = Array.zeroCreate<int> 1 |> ref

    let rec inner d =

      if d >= pattern.Length then ()
      else
        bin_min := 0
        bin_mid := !bin_max
        while !bin_min < !bin_mid do
          if this.MatchBitapScore(d, loc + !bin_mid, loc, pattern) <= !score_threshold then bin_min := !bin_mid
          else bin_max := !bin_mid
          bin_mid := (!bin_max - !bin_min) / 2 + !bin_min
        bin_max := !bin_mid
        let start = max 1 (loc - !bin_mid + 1) |> ref
        let finish = (min (loc + !bin_mid) text.Length) + pattern.Length

        let rd = Array.zeroCreate<int> (finish + 2)
        rd.[finish + 1] <- (1 <<< d) - 1

        let rec innerj j =
          if j < !start then ()
          else
            let charMatch =
              if text.Length <= j - 1 || (not <| s.ContainsKey(text.[j - 1])) then 0
              else s.[text.[j - 1]]
            if d = 0 then rd.[j] <- ((rd.[j + 1] <<< 1) ||| 1) &&& charMatch
            else rd.[j] <- ((rd.[j + 1] <<< 1) ||| 1) &&& charMatch ||| ((((!last_rd).[j + 1] ||| (!last_rd).[j]) <<< 1) ||| 1) ||| (!last_rd).[j + 1]

            if rd.[j] &&& matchMask <> 0 then
              let score = this.MatchBitapScore(d, j - 1, loc, pattern)
              if score <= float !score_threshold then
                score_threshold := score
                best_loc := j - 1
                if !best_loc > loc then
                  start := max 1 (2 * loc - !best_loc)
                  innerj (j - 1)
                else ()
              else innerj (j - 1)
            else innerj (j - 1)

        innerj finish
        if this.MatchBitapScore(d + 1, loc, loc, pattern) > !score_threshold then ()
        else
          last_rd := rd
          inner (d + 1)
    inner 0

    !best_loc

  member this.MatchMain(text, pattern, loc) =
    let loc = max 0 (min loc (String.length text))
    if text = pattern then 0
    elif String.IsNullOrEmpty text then -1
    elif loc + pattern.Length <= text.Length && text.Substring(loc, pattern.Length) = pattern then loc
    else this.MatchBitap(text, pattern, loc)

  member internal this.PatchAddContext(patch, text) =
    if String.length text = 0 then ()
    else
      let patchMargin = int this.PatchMargin
      let pattern = text.Substring(patch.Start2, patch.Length1) |> ref
      let padding = ref 0
      while text.IndexOf(!pattern, StringComparison.Ordinal) <> text.LastIndexOf(!pattern, StringComparison.Ordinal) && String.length !pattern < this.MatchMaxBits - patchMargin - patchMargin do
        padding := !padding + patchMargin
        pattern := String.javaSubstring (max 0 (patch.Start2 - !padding)) (min text.Length (patch.Start2 + patch.Length1 + !padding)) text
      padding := !padding + patchMargin
      let prefix = String.javaSubstring (max 0 (patch.Start2 - !padding)) patch.Start2 text
      if not <| String.IsNullOrEmpty prefix then patch.Diffs.Insert(0, { Text = prefix; Operation = Equal })
      let suffix = String.javaSubstring (patch.Start2 + patch.Length1) (min text.Length (patch.Start2 + patch.Length1 + !padding)) text
      if not <| String.IsNullOrEmpty suffix then patch.Diffs.Add({ Text = suffix; Operation = Equal })
      patch.Start1 <- patch.Start1 - prefix.Length
      patch.Start2 <- patch.Start2 - prefix.Length
      patch.Length1 <- patch.Length1 + prefix.Length + suffix.Length
      patch.Length2 <- patch.Length2 + prefix.Length + suffix.Length

  member this.PatchMake(text1: string, diffs: ResizeArray<Diff>) =
    let patches = ResizeArray<Patch>()
    if Seq.isEmpty diffs then patches
    else
      let patch = ref { Diffs = ResizeArray<Diff>(); Start1 = 0; Start2 = 0; Length1 = 0; Length2 = 0 }
      let char_count1 = ref 0
      let char_count2 = ref 0
      let prepatch_text = ref text1
      let postpatch_text = ref text1
      for diff in diffs do
        if (!patch).Diffs.Count = 0 && diff.Operation <> Equal then
          (!patch).Start1 <- !char_count1
          (!patch).Start2 <- !char_count2
        match diff.Operation with
        | Insert ->
          (!patch).Diffs.Add(diff)
          (!patch).Length2 <- (!patch).Length2 + diff.Text.Length
          postpatch_text := (!postpatch_text).Substring(0, !char_count2) + diff.Text + (!postpatch_text).Substring(!char_count2)
        | Delete ->
          (!patch).Length1 <- (!patch).Length1 + diff.Text.Length
          (!patch).Diffs.Add(diff)
          postpatch_text := (!postpatch_text).Substring(0, !char_count2) + (!postpatch_text).Substring(!char_count2 + diff.Text.Length)
        | Equal ->
          if diff.Text.Length <= 2 * int this.PatchMargin && (!patch).Diffs.Count <> 0 && diff <> Seq.last diffs then
            (!patch).Diffs.Add(diff)
            (!patch).Length1 <- (!patch).Length1 + diff.Text.Length
            (!patch).Length2 <- (!patch).Length2 + diff.Text.Length
          if diff.Text.Length >= 2 * int this.PatchMargin then
            if (!patch).Diffs.Count <> 0 then
              this.PatchAddContext(!patch, !prepatch_text)
              patches.Add(!patch)
              patch := { Diffs = ResizeArray<Diff>(); Start1 = 0; Start2 = 0; Length1 = 0; Length2 = 0 }
              prepatch_text := !postpatch_text
              char_count1 := !char_count2

        if diff.Operation <> Insert then char_count1 := !char_count1 + diff.Text.Length
        if diff.Operation <> Delete then char_count2 := !char_count2 + diff.Text.Length
      if (!patch).Diffs.Count <> 0 then
        this.PatchAddContext(!patch, !prepatch_text)
        patches.Add(!patch)
      patches

  member this.PatchMake(text1: string, _: string, diffs: ResizeArray<Diff>) =
    this.PatchMake(text1, diffs)

  member this.Patchmake(diffs: ResizeArray<Diff>) =
    let text1 = this.DiffText1(diffs)
    this.PatchMake(text1, diffs)

  member this.PatchMake(text1, text2) =
    let diffs = this.DiffMain(text1, text2, true)
    if diffs.Count > 3 then
      this.DiffCleanupSemantic(diffs)
      this.DiffCleanupEfficiency(diffs)
    this.PatchMake(text1, diffs)

  member __.PatchDeepCopy(patches) =
    let patchesCopy = ResizeArray<Patch>()
    for patch in patches do
      let patchCopy = { Diffs = ResizeArray<Diff>(); Start1 = 0; Start2 = 0; Length1 = 0; Length2 = 0 }
      for diff in patch.Diffs do
        let diffCopy = { Text = diff.Text; Operation = diff.Operation }
        patchCopy.Diffs.Add(diffCopy)
      patchCopy.Start1 <- patch.Start1
      patchCopy.Start2 <- patch.Start2
      patchCopy.Length1 <- patch.Length1
      patchCopy.Length2 <- patch.Length2
      patchesCopy.Add(patchCopy)
    patchesCopy

  member this.PatchAddPadding(patches: ResizeArray<Patch>) =
    let paddingLength = int this.PatchMargin
    let nullPadding = String(Array.map char [|1 .. paddingLength|])
    for patch in patches do
      patch.Start1 <- patch.Start1 + paddingLength
      patch.Start2 <- patch.Start2 + paddingLength
    let patch = Seq.head patches
    let diffs = patch.Diffs
    if diffs.Count = 0 || (Seq.head diffs).Operation <> Equal then
      diffs.Insert(0, { Text = nullPadding; Operation = Equal })
      patch.Start1 <- patch.Start1 - paddingLength
      patch.Start2 <- patch.Start2 - paddingLength
      patch.Length1 <- patch.Length1 + paddingLength
      patch.Length2 <- patch.Length2 + paddingLength
    elif paddingLength > (Seq.head diffs).Text.Length then
      let diff = Seq.head diffs
      let extraLength = paddingLength - diff.Text.Length
      diff.Text <- nullPadding.Substring(diff.Text.Length) + diff.Text
      patch.Start1 <- patch.Start1 - extraLength
      patch.Start2 <- patch.Start2 - extraLength
      patch.Length1 <- patch.Length1 + extraLength
      patch.Length2 <- patch.Length2 + extraLength
    let patch = Seq.last patches
    let diffs = patch.Diffs
    if diffs.Count = 0 || (Seq.last diffs).Operation <> Equal then
      diffs.Add({ Text = nullPadding; Operation = Equal })
      patch.Length1 <- patch.Length1 + paddingLength
      patch.Length2 <- patch.Length2 + paddingLength
    elif paddingLength > (Seq.last diffs).Text.Length then
      let diff = Seq.last diffs
      let extraLength = paddingLength - diff.Text.Length
      diff.Text <- diff.Text + nullPadding.Substring(0, extraLength)
      patch.Length1 <- patch.Length1 + extraLength
      patch.Length2 <- patch.Length2 + extraLength
    nullPadding

  member this.PatchSplitMax(patches: ResizeArray<Patch>) =
    let x = ref 0
    while !x < patches.Count do
      if patches.[!x].Length1 > this.MatchMaxBits then
        let bigPatch = patches.[!x]
        patches.Splice(!x, 1) |> ignore
        decr x
        let patchSize = this.MatchMaxBits
        let start1 = ref bigPatch.Start1
        let start2 = ref bigPatch.Start2
        let preContext = ref ""
        while bigPatch.Diffs.Count <> 0 do
          let patch = {
            Diffs = ResizeArray<Diff>()
            Start1 = !start1 - String.length !preContext
            Start2 = !start2 - String.length !preContext
            Length1 = 0
            Length2 = 0
          }
          let empty = ref true
          if not <| String.IsNullOrEmpty !preContext then
            patch.Length1 <- String.length !preContext
            patch.Length2 <- String.length !preContext
            patch.Diffs.Add({ Text = !preContext; Operation = Equal })
          while bigPatch.Diffs.Count <> 0 && patch.Length1 < patchSize - int this.PatchMargin do
            let diffType = bigPatch.Diffs.[0].Operation
            let diffText = bigPatch.Diffs.[0].Text
            match diffType with
            | Insert ->
              patch.Length2 <- patch.Length2 + diffText.Length
              start2 := !start2 + diffText.Length
              patch.Diffs.Add(Seq.head bigPatch.Diffs)
              bigPatch.Diffs.RemoveAt(0)
              empty := false
            | Delete when patch.Diffs.Count = 1 && (Seq.head patch.Diffs).Operation = Equal && diffText.Length > 2 * patchSize ->
              patch.Length1 <- patch.Length1 + diffText.Length
              start1 := !start1 + diffText.Length
              empty := false
              patch.Diffs.Add({ Text = diffText; Operation = diffType })
              bigPatch.Diffs.RemoveAt(0)
            | _ ->
              let diffText = diffText.Substring(0, min diffText.Length (patchSize - patch.Length1 - int this.PatchMargin))
              patch.Length1 <- patch.Length1 + diffText.Length
              start1 := !start1 + diffText.Length
              match diffType with
              | Equal ->
                patch.Length2 <- patch.Length2 + diffText.Length
                start2 := !start2 + diffText.Length
              | _ -> empty := false
              patch.Diffs.Add({ Text = diffText; Operation = diffType })
              if diffText = bigPatch.Diffs.[0].Text then bigPatch.Diffs.RemoveAt(0)
              else bigPatch.Diffs.[0].Text <- bigPatch.Diffs.[0].Text.Substring(diffText.Length)
          preContext := this.DiffText2(patch.Diffs)
          preContext := (!preContext).Substring(max 0 (String.length !preContext - int this.PatchMargin))
          let postContext =
            let x = this.DiffText1(bigPatch.Diffs)
            if x.Length > int this.PatchMargin then x.Substring(0, int this.PatchMargin)
            else x
          if not <| String.IsNullOrEmpty postContext then
            patch.Length1 <- patch.Length1 + postContext.Length
            patch.Length2 <- patch.Length2 + postContext.Length
            if patch.Diffs.Count <> 0 && (Seq.last patch.Diffs).Operation = Equal then
              let diff = Seq.last patch.Diffs
              diff.Text <- diff.Text + postContext
            else patch.Diffs.Add({ Text = postContext; Operation = Equal })
          if not !empty then
            incr x
            patches.Splice(!x, 0, patch) |> ignore
      incr x

  member this.PatchApply(patches: ResizeArray<Patch>, text) =
    if patches.Count = 0 then [|box text; Array.zeroCreate<bool> 0 |> box|]
    else

      let patches = this.PatchDeepCopy(patches)
      let nullPadding = this.PatchAddPadding(patches)
      let text = nullPadding + text + nullPadding |> ref
      this.PatchSplitMax(patches)

      let x = ref 0

      let delta = ref 0
      let results = Array.zeroCreate<bool> patches.Count
      for patch in patches do
        let expected_loc = patch.Start2 + !delta
        let text1 = this.DiffText1(patch.Diffs)
        let end_loc = ref -1
        let start_loc =
          if text1.Length > this.MatchMaxBits then
            let start_loc = this.MatchMain(!text, text1.Substring(0, this.MatchMaxBits), expected_loc)
            if start_loc <> -1 then
              end_loc := this.MatchMain(!text, text1.Substring(text1.Length - this.MatchMaxBits), expected_loc + text1.Length - this.MatchMaxBits)
              if !end_loc = -1 || start_loc >= !end_loc then
                -1
              else start_loc
            else start_loc
          else
            this.MatchMain(!text, text1, expected_loc)
        if start_loc = -1 then
          results.[!x] <- false
          delta := !delta - (patch.Length2 - patch.Length1)
        else
          results.[!x] <- true
          delta := start_loc - expected_loc
          let text2 =
            if !end_loc = -1 then String.javaSubstring start_loc (min (start_loc + text1.Length) (!text).Length) !text
            else  String.javaSubstring start_loc (min (!end_loc + this.MatchMaxBits) (!text).Length) !text
          if text1 = text2 then
            text := (!text).Substring(0, start_loc) + this.DiffText2(patch.Diffs) + (!text).Substring(start_loc + text1.Length)
          else
            let diffs = this.DiffMain(text1, text2, false)
            if text1.Length > this.MatchMaxBits && float32 (this.DiffLevenshtein(diffs)) / float32 text1.Length > this.PatchDeleteThreshold then
              results.[!x] <- false
            else
              this.DiffCleanupSemanticLossless(diffs)
              let index1 = ref 0
              for diff in patch.Diffs do
                if diff.Operation <> Equal then
                  let index2 = this.DiffXIndex(diffs, !index1)
                  match diff.Operation with
                  | Insert -> text := (!text).Insert(start_loc + index2, diff.Text)
                  | Delete -> text := (!text).Substring(0, start_loc + index2) + (!text).Substring(start_loc + this.DiffXIndex(diffs, !index1 + diff.Text.Length))
                  | Equal -> ()
                if diff.Operation <> Delete then index1 := !index1 + diff.Text.Length
        incr x
      let text = String.javaSubstring nullPadding.Length ((!text).Length - nullPadding.Length) !text
      [|box text; results|]

  member __.PatchToText(patches: ResizeArray<Patch>) =
    patches
    |> Seq.map (fun x -> x.ToString())
    |> String.concat ""

  member this.PatchFromText(textLine) =
    let patches = ResizeArray<Patch>()
    if String.IsNullOrEmpty textLine then patches
    else
      let textPointer = ref 0
      let textList = ResizeArray<string>(textLine.Split([| '\n' |]))
      let text = LinkedList<string>(textList)
      let patchHeader = Regex("^@@ -(\\d+),?(\\d*) \\+(\\d+),?(\\d*) @@$")
      while !textPointer < text.Count do
        let m = patchHeader.Match(Seq.nth !textPointer text)
        if not m.Success then raise <| ArgumentException(sprintf "Invalid patch string: %s" (Seq.nth !textPointer text))
        let patch = {
          Diffs = ResizeArray<Diff>()
          Start1 = Convert.ToInt32(m.Groups.[1].Value)
          Start2 = Convert.ToInt32(m.Groups.[3].Value)
          Length1 = 0
          Length2 = 0
        }
        patches.Add(patch)

        patch.Start1 <- Convert.ToInt32(m.Groups.[1].Value)
        if m.Groups.[2].Length = 0 then
          patch.Start1 <- patch.Start1 - 1
          patch.Length1 <- 1
        elif m.Groups.[2].Value = "0" then patch.Length1 <- 0
        else
          patch.Start1 <- patch.Start1 - 1
          patch.Length1 <- Convert.ToInt32(m.Groups.[2].Value)

        patch.Start2 <- Convert.ToInt32(m.Groups.[3].Value)
        if m.Groups.[4].Length = 0 then
          patch.Start2 <- patch.Start2 - 1
          patch.Length2 <- 1
        elif m.Groups.[4].Value = "0" then patch.Length2 <- 0
        else
          patch.Start2 <- patch.Start2 - 1
          patch.Length2 <- Convert.ToInt32(m.Groups.[4].Value)
        incr textPointer
        
        let rec inner () =
          if !textPointer >= text.Count then ()
          else
            try
              let sign = (Seq.nth !textPointer text).[0]
              let line = (Seq.nth !textPointer text).Substring(1)
              let line = line.Replace("+", "%2b")
              let line = Uri.Decode(line)
              match sign with
              | '-' ->
                patch.Diffs.Add({ Text = line; Operation = Delete })
                true
              | '+' ->
                patch.Diffs.Add({ Text = line; Operation = Insert })
                true
              | ' ' ->
                patch.Diffs.Add({ Text = line; Operation = Equal })
                true
              | '@' -> false
              | _ -> raise <| ArgumentException(sprintf "Invalid patch mode '%c' in: %s" sign line)
              |> function
              | true ->
                incr textPointer
                inner ()
              | false -> ()
            with :? IndexOutOfRangeException ->
              incr textPointer
              inner ()
        inner ()
      patches

  member __.UnescapeForEncodeUriCompatability(str) = Helper.unescapeForEncodeUriCompatability str

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module DiffMatchPatch =

  let Default = {
    DiffTimeout = 1.0f
    DiffEditCost = 4
    DiffDualThreshold = 32s
    MatchThreshold = 0.5f
    MatchDistance = 1000
    PatchDeleteThreshold = 0.5f
    PatchMargin = 4s
    MatchMaxBits = 32
  }
