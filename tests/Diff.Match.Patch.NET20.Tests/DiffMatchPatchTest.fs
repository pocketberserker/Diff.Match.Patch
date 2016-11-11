module DiffMatchPatch.Tests.DiffMatchPatchTest

open System
open System.Collections.Generic
open Persimmon
open UseTestNameByReflection
open DiffMatchPatch

let ``diff commonPrefix`` = parameterize {
  source [
    ("abc", "xyz", 0)
    ("1234abcdef", "1234xyz", 4)
    ("1234", "1234xyz", 4)
  ]
  run (fun (text1, text2, expected) -> test {
    let dmp = DiffMatchPatch.Default
    do! dmp.DiffCommonPrefix(text1, text2) |> assertEquals expected
  })
}

let ``diff commonSuffix`` = parameterize {
  source [
    ("abc", "xyz", 0)
    ("abcdef1234", "xyz1234", 4)
    ("1234", "xyz1234", 4)
  ]
  run (fun (text1, text2, expected) -> test {
    let dmp = DiffMatchPatch.Default
    do! dmp.DiffCommonSuffix(text1, text2) |> assertEquals expected
  })
}

let ``diff halfmatch`` = parameterize {
  source [
    ("1234567890", "abcdef", None)
    ("1234567890", "a345678z", Some [|"12"; "90"; "a"; "z"; "345678"|])
    ("a345678z", "1234567890", Some [|"a"; "z"; "12"; "90"; "345678"|])
    ("121231234123451234123121", "a1234123451234z", Some [|"12123"; "123121"; "a"; "z"; "1234123451234"|])
    ("x-=-=-=-=-=-=-=-=-=-=-=-=", "xx-=-=-=-=-=-=-=", Some [|""; "-=-=-=-=-="; "x"; ""; "x-=-=-=-=-=-=-="|])
    ("-=-=-=-=-=-=-=-=-=-=-=-=y", "-=-=-=-=-=-=-=yy", Some [|"-=-=-=-=-="; ""; ""; "y"; "-=-=-=-=-=-=-=y"|])
  ]
  run (fun (text1, text2, expected) -> test {
    let dmp = DiffMatchPatch.Default
    do! dmp.DiffHalfMatch(text1, text2) |> assertEquals expected
  })
}

let ``diff linesToChars`` = test {
  let dmp = DiffMatchPatch.Default

  let tmp = ResizeArray<string>()
  tmp.Add("")
  tmp.Add("alpha\n")
  tmp.Add("beta\n")
  let (r0, r1, r2) = dmp.DiffLinesToChars("alpha\nbeta\nalpha\n", "beta\nalpha\nbeta\n")
  do! assertEquals "\u0001\u0002\u0001" r0
  do! assertEquals "\u0002\u0001\u0002" r1
  do! assertSeqEquals tmp r2

  tmp.Clear()
  tmp.Add("")
  tmp.Add("alpha\r\n")
  tmp.Add("beta\r\n")
  tmp.Add("\r\n")
  let (r0, r1, r2) = dmp.DiffLinesToChars("", "alpha\r\nbeta\r\n\r\n\r\n")
  do! assertEquals "" r0
  do! assertEquals "\u0001\u0002\u0003\u0003" r1
  do! assertSeqEquals tmp r2

  tmp.Clear()
  tmp.Add("")
  tmp.Add("a")
  tmp.Add("b")
  let (r0, r1, r2) = dmp.DiffLinesToChars("a", "b")
  do! assertEquals "\u0001" r0
  do! assertEquals "\u0002" r1
  do! assertSeqEquals tmp r2

  let n = 300
  tmp.Clear()
  tmp.Add("")
  [1 .. n] |> List.iter (fun n -> tmp.Add(string n + "\n"))
  let lines = String.init n (fun n -> string (n + 1) + "\n")
  let chars = String.init n (fun n -> Convert.ToChar(n + 1) |> string)
  let (r0, r1, r2) = dmp.DiffLinesToChars(lines, "")
  do! assertEquals chars r0
  do! assertEquals "" r1
  do! assertSeqEquals tmp r2
}

let ``diff charsToLines`` = test {
  let dmp = DiffMatchPatch.Default
  
  let diffs =
    ResizeArray<Diff>(
      [
        { Text = "\u0001\u0002\u0001"; Operation = Equal }
        { Text = "\u0002\u0001\u0002"; Operation = Insert }
      ]
    )
  let tmp = ResizeArray<string>()
  tmp.Add("")
  tmp.Add("alpha\n")
  tmp.Add("beta\n")
  dmp.DiffCharsToLines(diffs, tmp)
  do!
    diffs
    |> assertSeqEquals (seq [{ Text = "alpha\nbeta\nalpha\n"; Operation = Equal }; { Text = "beta\nalpha\nbeta\n"; Operation = Insert }])

  let n = 300
  tmp.Clear()
  tmp.Add("")
  [1 .. n] |> List.iter (fun n -> tmp.Add(string n + "\n"))
  let lines = String.init n (fun n -> string (n + 1) + "\n")
  let chars = String.init n (fun n -> Convert.ToChar(n + 1) |> string)
  let diffs =
    ResizeArray<Diff>(
      [
        { Text = chars; Operation = Delete }
      ]
    )
  dmp.DiffCharsToLines(diffs, tmp)
  do!
    diffs
    |> assertSeqEquals (seq [{ Text = lines; Operation = Delete }])
}

let ``diff cleanupMerge`` = parameterize {
  source [
    ([], [])
    (
      [
        { Text = "a"; Operation = Equal }
        { Text = "b"; Operation = Delete }
        { Text = "c"; Operation = Insert }
      ],
      [
        { Text = "a"; Operation = Equal }
        { Text = "b"; Operation = Delete }
        { Text = "c"; Operation = Insert }
      ]
    )
    (
      [
        { Text = "a"; Operation = Equal }
        { Text = "b"; Operation = Equal }
        { Text = "c"; Operation = Equal }
      ],
      [{ Text = "abc"; Operation = Equal }]
    )
    (
      [
        { Text = "a"; Operation = Delete }
        { Text = "b"; Operation = Delete }
        { Text = "c"; Operation = Delete }
      ],
      [{ Text = "abc"; Operation = Delete }]
    )
    (
      [
        { Text = "a"; Operation = Insert }
        { Text = "b"; Operation = Insert }
        { Text = "c"; Operation = Insert }
      ],
      [{ Text = "abc"; Operation = Insert }]
    )
    (
      [
        { Text = "a"; Operation = Delete }
        { Text = "b"; Operation = Insert }
        { Text = "c"; Operation = Delete }
        { Text = "d"; Operation = Insert }
        { Text = "e"; Operation = Equal }
        { Text = "f"; Operation = Equal }
      ],
      [
        { Text = "ac"; Operation = Delete }
        { Text = "bd"; Operation = Insert }
        { Text = "ef"; Operation = Equal }
      ]
    )
    (
      [
        { Text = "a"; Operation = Delete }
        { Text = "abc"; Operation = Insert }
        { Text = "dc"; Operation = Delete }
      ],
      [
        { Text = "a"; Operation = Equal }
        { Text = "d"; Operation = Delete }
        { Text = "b"; Operation = Insert }
        { Text = "c"; Operation = Equal }
      ]
    )
    (
      [
        { Text = "a"; Operation = Equal }
        { Text = "ba"; Operation = Insert }
        { Text = "c"; Operation = Equal }
      ],
      [
        { Text = "ab"; Operation = Insert }
        { Text = "ac"; Operation = Equal }
      ]
    )
    (
      [
        { Text = "c"; Operation = Equal }
        { Text = "ab"; Operation = Insert }
        { Text = "a"; Operation = Equal }
      ],
      [
        { Text = "ca"; Operation = Equal }
        { Text = "ba"; Operation = Insert }
      ]
    )
    (
      [
        { Text = "a"; Operation = Equal }
        { Text = "b"; Operation = Delete }
        { Text = "c"; Operation = Equal }
        { Text = "ac"; Operation = Delete }
        { Text = "x"; Operation = Equal }
      ],
      [
        { Text = "abc"; Operation = Delete }
        { Text = "acx"; Operation = Equal }
      ]
    )
    (
      [
        { Text = "x"; Operation = Equal }
        { Text = "ca"; Operation = Delete }
        { Text = "c"; Operation = Equal }
        { Text = "b"; Operation = Delete }
        { Text = "a"; Operation = Equal }
      ],
      [
        { Text = "xca"; Operation = Equal }
        { Text = "cba"; Operation = Delete }
      ]
    )
  ]
  run (fun (input, expected) -> test {
    let dmp = DiffMatchPatch.Default
    let diffs = ResizeArray<Diff>(input)
    dmp.DiffCleanupMerge(diffs)
    do! assertSeqEquals expected diffs
  })
}

let ``diff cleanupSemanticLossless`` = parameterize {
  source [
    ([], [])
    (
      [
        { Text = "AAA\r\n\r\nBBB"; Operation = Equal }
        { Text = "\r\nDDD\r\n\r\nBBB"; Operation = Insert }
        { Text = "\r\nEEE"; Operation = Equal }
      ],
      [
        { Text = "AAA\r\n\r\n"; Operation = Equal }
        { Text = "BBB\r\nDDD\r\n\r\n"; Operation = Insert }
        { Text = "BBB\r\nEEE"; Operation = Equal }
      ]
    )
    (
      [
        { Text = "AAA\r\nBBB"; Operation = Equal }
        { Text = " DDD\r\nBBB"; Operation = Insert }
        { Text = " EEE"; Operation = Equal }
      ],
      [
        { Text = "AAA\r\n"; Operation = Equal }
        { Text = "BBB DDD\r\n"; Operation = Insert }
        { Text = "BBB EEE"; Operation = Equal }
      ]
    )
    (
      [
        { Text = "The c"; Operation = Equal }
        { Text = "ow and the c"; Operation = Insert }
        { Text = "at."; Operation = Equal }
      ],
      [
        { Text = "The "; Operation = Equal }
        { Text = "cow and the "; Operation = Insert }
        { Text = "cat."; Operation = Equal }
      ]
    )
    (
      [
        { Text = "The-c"; Operation = Equal }
        { Text = "ow-and-the-c"; Operation = Insert }
        { Text = "at."; Operation = Equal }
      ],
      [
        { Text = "The-"; Operation = Equal }
        { Text = "cow-and-the-"; Operation = Insert }
        { Text = "cat."; Operation = Equal }
      ]
    )
    (
      [
        { Text = "a"; Operation = Equal }
        { Text = "a"; Operation = Delete }
        { Text = "ax"; Operation = Equal }
      ],
      [
        { Text = "a"; Operation = Delete }
        { Text = "aax"; Operation = Equal }
      ]
    )
    (
      [
        { Text = "xa"; Operation = Equal }
        { Text = "a"; Operation = Delete }
        { Text = "a"; Operation = Equal }
      ],
      [
        { Text = "xaa"; Operation = Equal }
        { Text = "a"; Operation = Delete }
      ]
    )
  ]
  run (fun (input, expected) -> test {
    let dmp = DiffMatchPatch.Default
    let diffs = ResizeArray<Diff>(input)
    dmp.DiffCleanupSemanticLossless(diffs)
    do! assertSeqEquals expected diffs
  })
}

let ``diff cleanupSemantic`` = parameterize {
  source [
    ([], [])
    (
      [
        { Text = "a"; Operation = Delete }
        { Text = "b"; Operation = Insert }
        { Text = "cd"; Operation = Equal }
        { Text = "e"; Operation = Delete }
      ],
      [
        { Text = "a"; Operation = Delete }
        { Text = "b"; Operation = Insert }
        { Text = "cd"; Operation = Equal }
        { Text = "e"; Operation = Delete }
      ]
    )
    (
      [
        { Text = "a"; Operation = Delete }
        { Text = "b"; Operation = Equal }
        { Text = "c"; Operation = Delete }
      ],
      [
        { Text = "abc"; Operation = Delete }
        { Text = "b"; Operation = Insert }
      ]
    )
    (
      [
        { Text = "ab"; Operation = Delete }
        { Text = "cd"; Operation = Equal }
        { Text = "e"; Operation = Delete }
        { Text = "f"; Operation = Equal }
        { Text = "g"; Operation = Insert }
      ],
      [
        { Text = "abcdef"; Operation = Delete }
        { Text = "cdfg"; Operation = Insert }
      ]
    )
    (
      [
        { Text = "1"; Operation = Insert }
        { Text = "A"; Operation = Equal }
        { Text = "B"; Operation = Delete }
        { Text = "2"; Operation = Insert }
        { Text = "_"; Operation = Equal }
        { Text = "1"; Operation = Insert }
        { Text = "A"; Operation = Equal }
        { Text = "B"; Operation = Delete }
        { Text = "2"; Operation = Insert }
      ],
      [
        { Text = "AB_AB"; Operation = Delete }
        { Text = "1A2_1A2"; Operation = Insert }
      ]
    )
    (
      [
        { Text = "The c"; Operation = Equal }
        { Text = "ow and the c"; Operation = Delete }
        { Text = "at."; Operation = Equal }
      ],
      [
        { Text = "The "; Operation = Equal }
        { Text = "cow and the "; Operation = Delete }
        { Text = "cat."; Operation = Equal }
      ]
    )
  ]
  run (fun (input, expected) -> test {
    let dmp = DiffMatchPatch.Default
    let diffs = ResizeArray<Diff>(input)
    dmp.DiffCleanupSemantic(diffs)
    do! assertSeqEquals expected diffs
  })
}

let ``diff cleanupEfficiency`` = parameterize {
  source [
    (4, [], [])
    (
      4,
      [
        { Text = "ab"; Operation = Delete }
        { Text = "12"; Operation = Insert }
        { Text = "wxyz"; Operation = Equal }
        { Text = "cd"; Operation = Delete }
        { Text = "34"; Operation = Insert }
      ],
      [
        { Text = "ab"; Operation = Delete }
        { Text = "12"; Operation = Insert }
        { Text = "wxyz"; Operation = Equal }
        { Text = "cd"; Operation = Delete }
        { Text = "34"; Operation = Insert }
      ]
    )
    (
      4,
      [
        { Text = "ab"; Operation = Delete }
        { Text = "12"; Operation = Insert }
        { Text = "xyz"; Operation = Equal }
        { Text = "cd"; Operation = Delete }
        { Text = "34"; Operation = Insert }
      ],
      [
        { Text = "abxyzcd"; Operation = Delete }
        { Text = "12xyz34"; Operation = Insert }
      ]
    )
    (
      4,
      [
        { Text = "ab"; Operation = Delete }
        { Text = "12"; Operation = Insert }
        { Text = "xy"; Operation = Equal }
        { Text = "34"; Operation = Insert }
        { Text = "z"; Operation = Equal }
        { Text = "cd"; Operation = Delete }
        { Text = "56"; Operation = Insert }
      ],
      [
        { Text = "abxyzcd"; Operation = Delete }
        { Text = "12xy34z56"; Operation = Insert }
      ]
    )
    (
      5,
      [
        { Text = "ab"; Operation = Delete }
        { Text = "12"; Operation = Insert }
        { Text = "wxyz"; Operation = Equal }
        { Text = "cd"; Operation = Delete }
        { Text = "34"; Operation = Insert }
      ],
      [
        { Text = "abwxyzcd"; Operation = Delete }
        { Text = "12wxyz34"; Operation = Insert }
      ]
    )
  ]
  run (fun (cost, input, expected) -> test {
    let dmp = { DiffMatchPatch.Default with DiffEditCost = cost }
    let diffs = ResizeArray<Diff>(input)
    dmp.DiffCleanupEfficiency(diffs)
    do! assertSeqEquals expected diffs
  })
}

let ``diff prettyHtml`` = test {
  let dmp = DiffMatchPatch.Default
  let diffs =
    ResizeArray<Diff>(
      [
        { Text = "a\n"; Operation = Equal }
        { Text = "<B>b</B>"; Operation = Delete }
        { Text = "c&d"; Operation = Insert }
      ]
    )
  do!
    dmp.DiffPrettyHtml(diffs)
    |> assertEquals "<SPAN TITLE=\"i=0\">a&para;<BR></SPAN><DEL STYLE=\"background:#FFE6E6;\" TITLE=\"i=2\">&lt;B&gt;b&lt;/B&gt;</DEL><INS STYLE=\"background:#E6FFE6;\" TITLE=\"i=2\">c&amp;d</INS>"
}

let ``diff text`` = test {
  let dmp = DiffMatchPatch.Default
  let diffs =
    ResizeArray<Diff>(
      [
        { Text = "jump"; Operation = Equal }
        { Text = "s"; Operation = Delete }
        { Text = "ed"; Operation = Insert }
        { Text = " over "; Operation = Equal }
        { Text = "the"; Operation = Delete }
        { Text = "a"; Operation = Insert }
        { Text = " lazy"; Operation = Equal }
      ]
    )
  let text1 = "jumps over the lazy"
  do! assertEquals text1 (dmp.DiffText1(diffs))
  do! assertEquals "jumped over a lazy" (dmp.DiffText2(diffs))
  return (text1, List.ofSeq diffs)
}

let ``diff delta`` = test {
  let dmp = DiffMatchPatch.Default
  let! (expected, diffs) = ``diff text``
  let diffs = ResizeArray<Diff>(diffs)
  diffs.Add({ Text = "old dog"; Operation = Insert })
  let text1 = dmp.DiffText1(diffs)
  do! assertEquals expected text1
  let delta = dmp.DiffToDelta(diffs)
  do! assertEquals "=4\t-1\t+ed\t=6\t-3\t+a\t=5\t+old dog" delta
  do!
    dmp.DiffFromDelta(text1, delta)
    |> assertSeqEquals diffs
    
  let! e = trap { it (dmp.DiffFromDelta(text1 + "x", delta)) }
  do! assertEquals typeof<ArgumentException> (e.GetType())
  let! e = trap { it (dmp.DiffFromDelta(text1.Substring(1), delta)) }
  do! assertEquals typeof<ArgumentException> (e.GetType())
  let! e = trap { it (dmp.DiffFromDelta("", "+%c3%xy")) }
  do! assertPred (typeof<ArgumentException>.IsAssignableFrom(e.GetType()))

  let zero = char 0 |> string
  let one = char 1 |> string
  let two = char 2 |> string
  let diffs =
    ResizeArray<Diff>(
      [
        { Text = "\u0680 " + zero + " \t %"; Operation = Equal }
        { Text = "\u0681 " + one + " \n ^"; Operation = Delete }
        { Text = "\u0682 " + two + " \\ |"; Operation = Insert }
      ]
    )
  let text1 = dmp.DiffText1(diffs)
  do! assertEquals ("\u0680 " + zero + " \t %\u0681 " + one + " \n ^") text1

  let delta = dmp.DiffToDelta(diffs)
  do! assertEquals "=7\t-7\t+%da%82 %02 %5c %7c" delta
  do! assertSeqEquals diffs (dmp.DiffFromDelta(text1, delta))

  let diffs = ResizeArray<Diff>([{ Text = "A-Z a-z 0-9 - _ . ! ~ * ' ( ) ; / ? : @ & = + $ , # "; Operation = Insert }])
  let text2 = dmp.DiffText2(diffs)
  do! assertEquals "A-Z a-z 0-9 - _ . ! ~ * \' ( ) ; / ? : @ & = + $ , # " text2

  let delta = dmp.DiffToDelta(diffs)
  do! assertEquals "+A-Z a-z 0-9 - _ . ! ~ * \' ( ) ; / ? : @ & = + $ , # " delta

  do! assertSeqEquals diffs (dmp.DiffFromDelta("", delta))
}

let ``diff xindex`` = parameterize {
  source [
    (
      [
        { Text = "a"; Operation = Delete }
        { Text = "1234"; Operation = Insert }
        { Text = "xyz"; Operation = Equal }
      ],
      2,
      5
    )
    (
      [
        { Text = "a"; Operation = Equal }
        { Text = "1234"; Operation = Delete }
        { Text = "xyz"; Operation = Equal }
      ],
      3,
      1
    )
  ]
  run (fun (diffs, loc, expected) -> test {
    let dmp = DiffMatchPatch.Default
    let diffs = ResizeArray<Diff>(diffs)
    do! assertEquals expected (dmp.DiffXIndex(diffs, loc))
  })
}

let ``diff levenshtein`` = parameterize {
  source [
    (
      [
        { Text = "abc"; Operation = Delete }
        { Text = "1234"; Operation = Insert }
        { Text = "xyz"; Operation = Equal }
      ],
      4
    )
    (
      [
        { Text = "xyz"; Operation = Equal }
        { Text = "abc"; Operation = Delete }
        { Text = "1234"; Operation = Insert }
      ],
      4
    )
    (
      [
        { Text = "abc"; Operation = Delete }
        { Text = "xyz"; Operation = Equal }
        { Text = "1234"; Operation = Insert }
      ],
      7
    )
  ]
  run (fun (diffs, expected) -> test {
    let dmp = DiffMatchPatch.Default
    let diffs = ResizeArray<Diff>(diffs)
    do! assertEquals expected (dmp.DiffLevenshtein(diffs))
  })
}

let ``diff path`` = test {
  let dmp = DiffMatchPatch.Default
  do! assertPred (dmp.DiffFootprint(1, 10) <> dmp.DiffFootprint(10, 1))

  let vmap =
    ResizeArray<Set<int64>>(
      [
        Set.ofList [dmp.DiffFootprint(0, 0)]
        Set.ofList [dmp.DiffFootprint(0, 1); dmp.DiffFootprint(1, 0)]
        Set.ofList [dmp.DiffFootprint(0, 2); dmp.DiffFootprint(2, 0); dmp.DiffFootprint(2, 2)]
        Set.ofList [dmp.DiffFootprint(0, 3); dmp.DiffFootprint(2, 3); dmp.DiffFootprint(3, 0); dmp.DiffFootprint(4, 3)]
        Set.ofList [dmp.DiffFootprint(0, 4); dmp.DiffFootprint(2, 4); dmp.DiffFootprint(4, 0); dmp.DiffFootprint(4, 4); dmp.DiffFootprint(5, 3)]
        Set.ofList [dmp.DiffFootprint(0, 5); dmp.DiffFootprint(2, 5); dmp.DiffFootprint(4, 5); dmp.DiffFootprint(5, 0); dmp.DiffFootprint(6, 3); dmp.DiffFootprint(6, 5)]
        Set.ofList [dmp.DiffFootprint(0, 6); dmp.DiffFootprint(2, 6); dmp.DiffFootprint(4, 6); dmp.DiffFootprint(6, 6); dmp.DiffFootprint(7, 5)]
      ]
    )
  let diffs =
    ResizeArray<Diff>(
      [
        { Text = "W"; Operation = Insert }
        { Text = "A"; Operation = Delete }
        { Text = "1"; Operation = Equal }
        { Text = "B"; Operation = Delete }
        { Text = "2"; Operation = Equal }
        { Text = "X"; Operation = Insert }
        { Text = "C"; Operation = Delete }
        { Text = "3"; Operation = Equal }
        { Text = "D"; Operation = Delete }
      ]
    )
  do! assertSeqEquals diffs (dmp.DiffPath1(vmap, "A1B2C3D", "W12X3"))

  vmap.RemoveAt(vmap.Count - 1)
  let diffs =
    ResizeArray<Diff>(
      [
        { Text = "4"; Operation = Equal }
        { Text = "E"; Operation = Delete }
        { Text = "Y"; Operation = Insert }
        { Text = "5"; Operation = Equal }
        { Text = "F"; Operation = Delete }
        { Text = "6"; Operation = Equal }
        { Text = "G"; Operation = Delete }
        { Text = "Z"; Operation = Insert }
      ]
    )
  do! assertSeqEquals diffs (dmp.DiffPath2(vmap, "4E5F6G", "4Y56Z"))

  let vmap =
    ResizeArray<Set<int64>>(
      [
        Set.ofList [dmp.DiffFootprint(0, 0)]
        Set.ofList [dmp.DiffFootprint(0, 1); dmp.DiffFootprint(1, 0)]
        Set.ofList [dmp.DiffFootprint(0, 2); dmp.DiffFootprint(2, 0); dmp.DiffFootprint(2, 2)]
        Set.ofList [dmp.DiffFootprint(0, 3); dmp.DiffFootprint(1, 2); dmp.DiffFootprint(2, 1); dmp.DiffFootprint(3, 0)]
        Set.ofList [dmp.DiffFootprint(0, 4); dmp.DiffFootprint(1, 3); dmp.DiffFootprint(3, 1); dmp.DiffFootprint(4, 0); dmp.DiffFootprint(4, 4)]
      ]
    )
  let diffs =
    ResizeArray<Diff>(
      [
        { Text = "WX"; Operation = Insert }
        { Text = "AB"; Operation = Delete }
        { Text = "12"; Operation = Equal }
      ]
    )
  do! assertSeqEquals diffs (dmp.DiffPath1(vmap, "AB12", "WX12"))

  let vmap =
    ResizeArray<Set<int64>>(
      [
        Set.ofList [dmp.DiffFootprint(0, 0)]
        Set.ofList [dmp.DiffFootprint(0, 1); dmp.DiffFootprint(1, 0)]
        Set.ofList [dmp.DiffFootprint(1, 1); dmp.DiffFootprint(2, 0); dmp.DiffFootprint(2, 4)]
        Set.ofList [dmp.DiffFootprint(2, 1); dmp.DiffFootprint(2, 5); dmp.DiffFootprint(3, 0); dmp.DiffFootprint(3, 4)]
        Set.ofList [dmp.DiffFootprint(2, 6); dmp.DiffFootprint(3, 5); dmp.DiffFootprint(4, 4)]
      ]
    )
  let diffs =
    ResizeArray<Diff>(
      [
        { Text = "CD"; Operation = Delete }
        { Text = "34"; Operation = Equal }
        { Text = "YZ"; Operation = Insert }
      ]
    )
  do! assertSeqEquals diffs (dmp.DiffPath2(vmap, "CD34", "34YZ"))
}

let ``diff main(Perform a trivial diff)`` = parameterize {
  source [
    ([{ Text = "abc"; Operation = Equal }], "abc", "abc")
    (
      [
        { Text = "ab"; Operation = Equal }
        { Text = "123"; Operation = Insert }
        { Text = "c"; Operation = Equal }
      ],
      "abc", "ab123c")
    (
      [
        { Text = "a"; Operation = Equal }
        { Text = "123"; Operation = Delete }
        { Text = "bc"; Operation = Equal }
      ],
      "a123bc", "abc")
    (
      [
        { Text = "a"; Operation = Equal }
        { Text = "123"; Operation = Insert }
        { Text = "b"; Operation = Equal }
        { Text = "456"; Operation = Insert }
        { Text = "c"; Operation = Equal }
      ],
      "abc", "a123b456c")
    (
      [
        { Text = "a"; Operation = Equal }
        { Text = "123"; Operation = Delete }
        { Text = "b"; Operation = Equal }
        { Text = "456"; Operation = Delete }
        { Text = "c"; Operation = Equal }
      ],
      "a123b456c", "abc")
  ]
  run(fun (diffs, text1, text2) -> test {
    let dmp = DiffMatchPatch.Default
    let diffs = ResizeArray<Diff>(diffs)
    do! assertSeqEquals diffs (dmp.DiffMain(text1, text2, false))
  })
}

let ``diff main(Perform a real diff)`` = parameterize {
  source [
    (
      [
        { Text = "a"; Operation = Delete }
        { Text = "b"; Operation = Insert }
      ],
      "a", "b")
    (
      [
        { Text = "Apple"; Operation = Delete }
        { Text = "Banana"; Operation = Insert }
        { Text = "s are a"; Operation = Equal }
        { Text = "lso"; Operation = Insert }
        { Text = " fruit."; Operation = Equal }
      ],
      "Apples are a fruit.", "Bananas are also fruit.")
    (
      [
        { Text = "a"; Operation = Delete }
        { Text = "\u0680"; Operation = Insert }
        { Text = "x"; Operation = Equal }
        { Text = "\t"; Operation = Delete }
        { Text = char 0 |> string; Operation = Insert }
      ],
      "ax\t", "\u0680x" + string (char 0))
    (
      [
        { Text = "1"; Operation = Delete }
        { Text = "a"; Operation = Equal }
        { Text = "y"; Operation = Delete }
        { Text = "b"; Operation = Equal }
        { Text = "2"; Operation = Delete }
        { Text = "xab"; Operation = Insert }
      ],
      "1ayb2", "abxab")
    (
      [
        { Text = "xaxcx"; Operation = Insert }
        { Text = "abc"; Operation = Equal }
        { Text = "y"; Operation = Delete }
      ],
      "abcy", "xaxcxabc")
  ]
  run(fun (diffs, text1, text2) -> test {
    let dmp = { DiffMatchPatch.Default with DiffTimeout = 0.0f }
    let diffs = ResizeArray<Diff>(diffs)
    do! assertSeqEquals diffs (dmp.DiffMain(text1, text2, false))
  })
}

let ``diff main(Sub-optimal double-ended diff)`` = test {
  let dmp = { DiffMatchPatch.Default with DiffTimeout = 0.0f; DiffDualThreshold = 2s; }
  let diffs =
    ResizeArray<Diff>(
      [
        { Text = "x"; Operation = Insert }
        { Text = "a"; Operation = Equal }
        { Text = "b"; Operation = Delete }
        { Text = "x"; Operation = Insert }
        { Text = "c"; Operation = Equal }
        { Text = "y"; Operation = Delete }
        { Text = "xabc"; Operation = Insert }
      ]
    )
  do! assertSeqEquals diffs (dmp.DiffMain("abcy", "xaxcxabc", false))
}

let ``diff map(timeout)`` = test {
  let dmp = { DiffMatchPatch.Default with DiffTimeout = 0.001f }
  let a = String.replicate 10 "`Twas brillig, and the slithy toves\nDid gyre and gimble in the wabe:\nAll mimsy were the borogoves,\nAnd the mome raths outgrabe.\n";
  let b = String.replicate 10 "I am the very model of a modern major general,\nI've information vegetable, animal, and mineral,\nI know the kings of England, and I quote the fights historical,\nFrom Marathon to Waterloo, in order categorical.\n";
  do! assertEquals None (dmp.DiffMap(a, b))
}

let ``diff main: simple`` = test {
  let dmp = { DiffMatchPatch.Default with DiffTimeout = 0.0f }
  let a = "1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n";
  let b = "abcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\nabcdefghij\n";
  do! assertSeqEquals (dmp.DiffMain(a, b, true)) (dmp.DiffMain(a, b, false))
}

let rebuildTexts (diffs: ResizeArray<Diff>) =
  let text = [|""; ""|]
  for diff in diffs do
    if diff.Operation <> Insert then text.[0] <- text.[0] + diff.Text
    if diff.Operation <> Delete then text.[1] <- text.[1] + diff.Text
  text

let ``diff main: overlap`` = test {
  let dmp = { DiffMatchPatch.Default with DiffTimeout = 0.0f }
  let a = "1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n1234567890\n";
  let b = "abcdefghij\n1234567890\n1234567890\n1234567890\nabcdefghij\n1234567890\n1234567890\n1234567890\nabcdefghij\n1234567890\n1234567890\n1234567890\nabcdefghij\n";
  let lineMode = dmp.DiffMain(a, b, true) |> rebuildTexts
  let textMode = dmp.DiffMain(a, b, false) |> rebuildTexts
  do! assertSeqEquals lineMode textMode
}

let ``match alphabet`` = parameterize {
  source [
    (dict [('a', 4); ('b', 2); ('c', 1)], "abc")
    (dict [('a', 37); ('b', 18); ('c', 8)], "abcaba")
  ]
  run (fun (bitmask, alphabet) -> test {
    let dmp = DiffMatchPatch.Default
    let bitmask = Dictionary<char, int>(bitmask)
    do!
      dmp.MatchAlphabet(alphabet)
      |> assertSeqEquals bitmask
  })
}

let ``match bitap`` = parameterize {
  source [
    ("abcdefghijk", "fgh", 5, 5, DiffMatchPatch.Default)
    ("abcdefghijk", "fgh", 0, 5, DiffMatchPatch.Default)
    ("abcdefghijk", "efxhi", 0, 4, DiffMatchPatch.Default)
    ("abcdefghijk", "cdefxyhijk", 5, 2, DiffMatchPatch.Default)
    ("abcdefghijk", "bxy", 1, -1, DiffMatchPatch.Default)
    ("123456789xx0", "3456789x0", 2, 2, DiffMatchPatch.Default)
    ("abcdef", "xxabc", 4, 0, DiffMatchPatch.Default)
    ("abcdef", "defyy", 4, 3, DiffMatchPatch.Default)
    ("abcdef", "xabcdefy", 0, 0, DiffMatchPatch.Default)
    ("abcdefghijk", "efxyhi", 1, 4, { DiffMatchPatch.Default with MatchThreshold = 0.4f })
    ("abcdefghijk", "efxyhi", 1, -1, { DiffMatchPatch.Default with MatchThreshold = 0.3f })
    ("abcdefghijk", "bcdef", 1, 1, { DiffMatchPatch.Default with MatchThreshold = 0.0f })
    ("abcdexyzabcde", "abccde", 3, 0, DiffMatchPatch.Default)
    ("abcdexyzabcde", "abccde", 5, 8, DiffMatchPatch.Default)
    ("abcdefghijklmnopqrstuvwxyz", "abcdefg", 24, -1, { DiffMatchPatch.Default with MatchDistance = 10 })
    ("abcdefghijklmnopqrstuvwxyz", "abcdxxefg", 1, 0, { DiffMatchPatch.Default with MatchDistance = 10 })
    ("abcdefghijklmnopqrstuvwxyz", "abcdefg", 24, 0, { DiffMatchPatch.Default with MatchDistance = 1000 })
  ]
  run (fun (text, pattern, loc, expected, dmp) -> test {
    do! assertEquals expected (dmp.MatchBitap(text, pattern, loc))
  })
}

let ``match main`` = parameterize {
  source [
    ("abcdef", "abcdef", 1000, 0, DiffMatchPatch.Default)
    ("", "abcdef", 1, -1, DiffMatchPatch.Default)
    ("abcdef", "", 3, 3, DiffMatchPatch.Default)
    ("abcdef", "de", 3, 3, DiffMatchPatch.Default)
    ("abcdef", "defy", 4, 3, DiffMatchPatch.Default)
    ("abcdef", "abcdefy", 0, 0, DiffMatchPatch.Default)
    ("I am the very model of a modern major general.", " that berry ", 5, 4, { DiffMatchPatch.Default with MatchThreshold = 0.7f })
  ]
  run (fun (text, pattern, loc, expected, dmp) -> test {
    do! assertEquals expected (dmp.MatchMain(text, pattern, loc))
  })
}

let ``patch object`` = test {
  let patch = {
    Diffs =
      ResizeArray<Diff>(
        [
          { Text = "jump"; Operation = Equal }
          { Text = "s"; Operation = Delete }
          { Text = "ed"; Operation = Insert }
          { Text = " over "; Operation = Equal }
          { Text = "the"; Operation = Delete }
          { Text = "a"; Operation = Insert }
          { Text = "\nlaz"; Operation = Equal }
        ]
      )
    Start1 = 20
    Start2 = 21
    Length1 = 18
    Length2 = 17
  }
  do!
    patch.ToString().Replace("\r", "").Split([|'\n'|])
    |> assertEquals [|"@@ -21,18 +22,17 @@"; " jump"; "-s"; "+ed"; "  over "; "-the"; "+a"; " %0alaz"; ""|]
}

let ``patch fromText`` = parameterize {
  source [
    [|"@@ -1 +1 @@"; "-a"; "+b"; ""|]
    [|"@@ -1,3 +0,0 @@"; "-abc"; ""|]
    [|"@@ -0,0 +1,3 @@"; "+abc"; ""|]
  ]
  run (fun input -> test {
    let dmp = DiffMatchPatch.Default
    do!
      dmp.PatchFromText(String.concat "\n" input).[0].ToString().Replace("\r", "").Split([|'\n'|])
      |> assertEquals input
  })
}

let ``patch toText`` = parameterize {
  source [
    [|"@@ -21,18 +22,17 @@"; " jump"; "-s"; "+ed"; "  over "; "-the"; "+a"; "  laz"; ""|]
    [|"@@ -1,9 +1,9 @@"; "-f"; "+F"; " oo+fooba"; "@@ -7,9 +7,9 @@"; " obar"; "-,"; "+."; "  tes"; ""|]
  ]
  run (fun input -> test {
    let dmp = DiffMatchPatch.Default
    let patches = dmp.PatchFromText(String.concat "\n" input)
    do! assertEquals input (dmp.PatchToText(patches).Replace("\r", "").Split([|'\n'|]))
  })
}

let ``patch addContext`` = parameterize {
  source [
    (
      "@@ -21,4 +21,10 @@\n-jump\n+somersault\n",
      "The quick brown fox jumps over the lazy dog.",
      [|"@@ -17,12 +17,18 @@"; " fox "; "-jump"; "+somersault"; " s ov"; ""|]
    )
    (
      "@@ -21,4 +21,10 @@\n-jump\n+somersault\n",
      "The quick brown fox jumps.",
      [|"@@ -17,10 +17,16 @@"; " fox "; "-jump"; "+somersault"; " s."; ""|]
    )
    (
      "@@ -3 +3,2 @@\n-e\n+at\n",
      "The quick brown fox jumps.",
      [|"@@ -1,7 +1,8 @@"; " Th"; "-e"; "+at"; "  qui"; ""|]
    )
    (
      "@@ -3 +3,2 @@\n-e\n+at\n",
      "The quick brown fox jumps.  The quick brown fox crashes.",
      [|"@@ -1,27 +1,28 @@"; " Th"; "-e"; "+at"; "  quick brown fox jumps. "; ""|]
    )
  ]
  run (fun (input, text, expected) -> test {
    let dmp = DiffMatchPatch.Default
    let p = dmp.PatchFromText(input).[0]
    dmp.PatchAddContext(p, text)
    do! assertEquals expected (p.ToString().Replace("\r", "").Split([|'\n'|]))
  })
}

let ``patch make`` = test {
  let dmp = DiffMatchPatch.Default
  let text1 = "The quick brown fox jumps over the lazy dog."
  let text2 = "That quick brown fox jumped over a lazy dog."
  let expected = [|"@@ -1,8 +1,7 @@"; " Th"; "-at"; "+e"; "  qui"; "@@ -21,17 +21,18 @@"; " jump"; "-ed"; "+s"; "  over "; "-a"; "+the"; "  laz"; ""|]
  let patches = dmp.PatchMake(text2, text1)
  do! assertEquals expected (dmp.PatchToText(patches).Replace("\r", "").Split([|'\n'|]))

  let expected = [|"@@ -1,11 +1,12 @@"; " Th"; "-e"; "+at"; "  quick b"; "@@ -22,18 +22,17 @@"; " jump"; "-s"; "+ed"; "  over "; "-the"; "+a"; "  laz"; ""|]
  let patches = dmp.PatchMake(text1, text2)
  do! assertEquals expected (dmp.PatchToText(patches).Replace("\r", "").Split([|'\n'|]))

  let diffs = dmp.DiffMain(text1, text2, false)
  let patches = dmp.PatchMake(text1, diffs)
  do! assertEquals expected (dmp.PatchToText(patches).Replace("\r", "").Split([|'\n'|]))

  let patches = dmp.PatchMake(text1, text2, diffs)
  do! assertEquals expected (dmp.PatchToText(patches).Replace("\r", "").Split([|'\n'|]))

  let patches = dmp.PatchMake("`1234567890-=[]\\;',./", "~!@#$%^&*()_+{}|:\"<>?")
  do!
    (dmp.PatchToText(patches).Replace("\r", "").Split([|'\n'|]))
    |> assertEquals [|"@@ -1,21 +1,21 @@"; "-%601234567890-=%5b%5d%5c;',./"; "+~!@#$%25%5e&*()_+%7b%7d%7c:%22%3c%3e?"; ""|]

  let text1 = String.replicate 100 "abcdef"
  let text2 = text1 + "123"
  let expected = [|"@@ -573,28 +573,31 @@"; " cdefabcdefabcdefabcdefabcdef"; "+123"; ""|]
  let patches = dmp.PatchMake(text1, text2)
  do! assertEquals expected (dmp.PatchToText(patches).Replace("\r", "").Split([|'\n'|]))
}

let ``patch splitMax`` = test {
  let dmp = DiffMatchPatch.Default
  let patches = dmp.PatchMake("abcdefghijklmnopqrstuvwxyz01234567890", "XabXcdXefXghXijXklXmnXopXqrXstXuvXwxXyzX01X23X45X67X89X0")
  dmp.PatchSplitMax(patches)
  do!
    (dmp.PatchToText(patches).Replace("\r", "").Split([|'\n'|]))
    |> assertEquals [|"@@ -1,32 +1,46 @@"; "+X"; " ab"; "+X"; " cd"; "+X"; " ef"; "+X"; " gh"; "+X"; " ij"; "+X"; " kl"; "+X"; " mn"; "+X"; " op"; "+X"; " qr"; "+X"; " st"; "+X"; " uv"; "+X"; " wx"; "+X"; " yz"; "+X"; " 012345"; "@@ -25,13 +39,18 @@"; " zX01"; "+X"; " 23"; "+X"; " 45"; "+X"; " 67"; "+X"; " 89"; "+X"; " 0"; ""|]
    
  let patches = dmp.PatchMake("abcdef1234567890123456789012345678901234567890123456789012345678901234567890uvwxyz", "abcdefuvwxyz")
  let oldToText = dmp.PatchToText(patches)
  dmp.PatchSplitMax(patches)
  do! assertEquals oldToText (dmp.PatchToText(patches))

  let patches = dmp.PatchMake("1234567890123456789012345678901234567890123456789012345678901234567890", "abc")
  dmp.PatchSplitMax(patches)
  do!
    (dmp.PatchToText(patches).Replace("\r", "").Split([|'\n'|]))
    |> assertEquals [|"@@ -1,32 +1,4 @@"; "-1234567890123456789012345678"; " 9012"; "@@ -29,32 +1,4 @@"; "-9012345678901234567890123456"; " 7890"; "@@ -57,14 +1,3 @@"; "-78901234567890"; "+abc"; ""|]

  let patches = dmp.PatchMake("abcdefghij , h : 0 , t : 1 abcdefghij , h : 0 , t : 1 abcdefghij , h : 0 , t : 1", "abcdefghij , h : 1 , t : 1 abcdefghij , h : 1 , t : 1 abcdefghij , h : 0 , t : 1")
  dmp.PatchSplitMax(patches)
  do!
    (dmp.PatchToText(patches).Replace("\r", "").Split([|'\n'|]))
    |> assertEquals [|"@@ -2,32 +2,32 @@"; " bcdefghij , h : "; "-0"; "+1"; "  , t : 1 abcdef"; "@@ -29,32 +29,32 @@"; " bcdefghij , h : "; "-0"; "+1"; "  , t : 1 abcdef"; ""|]
}

let ``patch addPadding`` = test {
  let dmp = DiffMatchPatch.Default
  let patches = dmp.PatchMake("", "test")
  dmp.PatchAddPadding(patches) |> ignore
  do!
    (dmp.PatchToText(patches).Replace("\r", "").Split([|'\n'|]))
    |> assertEquals [|"@@ -1,8 +1,12 @@"; " %01%02%03%04"; "+test"; " %01%02%03%04"; ""|]

  let patches = dmp.PatchMake("XY", "XtestY")
  dmp.PatchAddPadding(patches) |> ignore
  do!
    (dmp.PatchToText(patches).Replace("\r", "").Split([|'\n'|]))
    |> assertEquals [|"@@ -2,8 +2,12 @@"; " %02%03%04X"; "+test"; " Y%01%02%03"; ""|]

  let patches = dmp.PatchMake("XXXXYYYY", "XXXXtestYYYY")
  dmp.PatchAddPadding(patches) |> ignore
  do!
    (dmp.PatchToText(patches).Replace("\r", "").Split([|'\n'|]))
    |> assertEquals [|"@@ -5,8 +5,12 @@"; " XXXX"; "+test"; " YYYY"; ""|]
}

let ``patch apply`` = parameterize {
  source [
    (
      "The quick brown fox jumps over the lazy dog.",
      "That quick brown fox jumped over a lazy dog.",
      "The quick brown fox jumps over the lazy dog.",
      "That quick brown fox jumped over a lazy dog.\tTrue\tTrue",
      DiffMatchPatch.Default
    )
    (
      "The quick brown fox jumps over the lazy dog.",
      "That quick brown fox jumped over a lazy dog.",
      "The quick red rabbit jumps over the tired tiger.",
      "That quick red rabbit jumped over a tired tiger.\tTrue\tTrue",
      DiffMatchPatch.Default
    )
    (
      "The quick brown fox jumps over the lazy dog.",
      "That quick brown fox jumped over a lazy dog.",
      "I am the very model of a modern major general.",
      "I am the very model of a modern major general.\tFalse\tFalse",
      DiffMatchPatch.Default
    )
    (
      "x1234567890123456789012345678901234567890123456789012345678901234567890y",
      "xabcy",
      "x123456789012345678901234567890-----++++++++++-----123456789012345678901234567890y",
      "xabcy\tTrue\tTrue",
      DiffMatchPatch.Default
    )
    (
      "x1234567890123456789012345678901234567890123456789012345678901234567890y",
      "xabcy",
      "x12345678901234567890---------------++++++++++---------------12345678901234567890y",
      "xabc12345678901234567890---------------++++++++++---------------12345678901234567890y\tFalse\tTrue",
      DiffMatchPatch.Default
    )
    (
      "x1234567890123456789012345678901234567890123456789012345678901234567890y",
      "xabcy",
      "x12345678901234567890---------------++++++++++---------------12345678901234567890y",
      "xabcy\tTrue\tTrue",
      { DiffMatchPatch.Default with PatchDeleteThreshold = 0.6f }
    )
    (
      "abcdefghijklmnopqrstuvwxyz--------------------1234567890",
      "abcXXXXXXXXXXdefghijklmnopqrstuvwxyz--------------------1234567YYYYYYYYYY890",
      "ABCDEFGHIJKLMNOPQRSTUVWXYZ--------------------1234567890",
      "ABCDEFGHIJKLMNOPQRSTUVWXYZ--------------------1234567YYYYYYYYYY890\tFalse\tTrue",
      { DiffMatchPatch.Default with MatchThreshold = 0.0f; MatchDistance = 0 }
    )
  ]
  run(fun (text1, text2, text, expected, dmp) -> test {
    let patches = dmp.PatchMake(text1, text2)
    let results = dmp.PatchApply(patches, text)
    let bools = results.[1] |> unbox<bool []>
    let resultStr = sprintf "%O\t%O\t%O" results.[0] bools.[0] bools.[1]
    do! assertEquals expected resultStr
  })
}

let ``patch apply(no side effects)`` = test {
  let dmp = DiffMatchPatch.Default
  let patches = dmp.PatchMake("The quick brown fox jumps over the lazy dog.", "Woof")
  let patchStr = dmp.PatchToText(patches)
  dmp.PatchApply(patches, "The quick brown fox jumps over the lazy dog.") |> ignore
  do! assertEquals patchStr (dmp.PatchToText(patches))
}

let ``patch apply(edge)`` = parameterize {
  source [
    ("", "test", "", "test\tTrue", DiffMatchPatch.Default)
    ("XY", "XtestY", "XY", "XtestY\tTrue", DiffMatchPatch.Default)
    ("y", "y123", "x", "x123\tTrue", DiffMatchPatch.Default)
  ]
  run(fun (text1, text2, text, expected, dmp) -> test {
    let patches = dmp.PatchMake(text1, text2)
    let results = dmp.PatchApply(patches, text)
    let bools = results.[1] |> unbox<bool []>
    let resultStr = sprintf "%O\t%O" results.[0] bools.[0]
    do! assertEquals expected resultStr
  })
}
