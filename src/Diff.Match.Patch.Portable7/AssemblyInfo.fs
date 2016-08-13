namespace System
open System.Reflection
open System.Runtime.CompilerServices

[<assembly: AssemblyTitleAttribute("Diff.Match.Patch")>]
[<assembly: AssemblyProductAttribute("Diff.Match.Patch")>]
[<assembly: AssemblyDescriptionAttribute("")>]
[<assembly: InternalsVisibleToAttribute("Diff.Match.Patch.Tests")>]
[<assembly: AssemblyVersionAttribute("2.0.0")>]
[<assembly: AssemblyFileVersionAttribute("2.0.0")>]
[<assembly: AssemblyInformationalVersionAttribute("2.0.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "2.0.0"
    let [<Literal>] InformationalVersion = "2.0.0"
