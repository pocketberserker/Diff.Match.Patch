#r @"packages/build/FAKE/tools/FakeLib.dll"
#r @"packages/build/FAKE.Persimmon/lib/net451/FAKE.Persimmon.dll"
open Fake
open Fake.Git
open Fake.AssemblyInfoFile
open Fake.ReleaseNotesHelper
open System
open System.IO
#if MONO
#else
#load "packages/build/SourceLink.Fake/tools/Fake.fsx"
open SourceLink
#endif

let configuration = getBuildParamOrDefault "configuration" "Release"

let isDotnetInstalled = DotNetCli.isInstalled()

let outDir = "bin"

let project = "Diff.Match.Patch"

let solutionFile = "Diff.Match.Patch.sln"

// Pattern specifying assemblies to be tested using NUnit
let testAssemblies = "tests/**/bin" @@ configuration @@ "*Tests*.dll"

// Git configuration (used for publishing documentation in gh-pages branch)
// The profile where the project is posted
let gitOwner = "pocketberserer"
let gitHome = "https://github.com/" + gitOwner

// The name of the project on GitHub
let gitName = "Diff.Match.Patch"

// The url for the raw files hosted
let gitRaw = environVarOrDefault "gitRaw" "https://raw.github.com/pocketberserker"

// Read additional information from the release notes document
let release = LoadReleaseNotes "RELEASE_NOTES.md"

// Helper active pattern for project types
let (|Fsproj|Csproj|Vbproj|) (projFileName:string) =
    match projFileName with
    | f when f.EndsWith("fsproj") -> Fsproj
    | f when f.EndsWith("csproj") -> Csproj
    | f when f.EndsWith("vbproj") -> Vbproj
    | _                           -> failwith (sprintf "Project file %s not supported. Unknown project type." projFileName)

// Generate assembly info files with the right version & up-to-date information
Target "AssemblyInfo" (fun _ ->
    let replace (oldValue:string) newValue (str:string) = str.Replace(oldValue, newValue)
    let getAssemblyInfoAttributes projectName =
        [ Attribute.Title (projectName |> replace ".NET20" "" |> replace ".Portable259" "" |> replace ".Portable78" "" |> replace ".Portable47" "" |> replace ".Portable7" "")
          Attribute.Product project
          Attribute.InternalsVisibleTo("Diff.Match.Patch.Tests")
          Attribute.Version release.AssemblyVersion
          Attribute.FileVersion release.AssemblyVersion
          Attribute.InformationalVersion release.NugetVersion ]

    let getProjectDetails projectPath =
        let projectName = System.IO.Path.GetFileNameWithoutExtension(projectPath)
        ( projectPath,
          projectName,
          System.IO.Path.GetDirectoryName(projectPath),
          (getAssemblyInfoAttributes projectName)
        )

    !! "src/**/*.??proj"
    |> Seq.choose (fun p ->
      let name = Path.GetFileNameWithoutExtension(p)
      if name.EndsWith("NETCore") || name.EndsWith("NET40") || name.EndsWith("NET45") then None
      else getProjectDetails p |> Some
    )
    |> Seq.iter (fun (projFileName, projectName, folderName, attributes) ->
        match projFileName with
        | Fsproj -> CreateFSharpAssemblyInfo (("src" @@ folderName) @@ "AssemblyInfo.fs") attributes
        | Csproj -> CreateCSharpAssemblyInfo ((folderName @@ "Properties") @@ "AssemblyInfo.cs") attributes
        | Vbproj -> CreateVisualBasicAssemblyInfo ((folderName @@ "My Project") @@ "AssemblyInfo.vb") attributes
    )
)

// Copies binaries from default VS location to exepcted bin folder
// But keeps a subdirectory structure for each project in the
// src folder to support multiple project outputs
Target "CopyBinaries" (fun _ ->
  !! "src/**/*.??proj"
  |> Seq.filter (fun p ->
    let p = Path.GetFileNameWithoutExtension(p)
    not (p.EndsWith("NET40") || p.EndsWith("NET45") || p.EndsWith("NETCore"))
  )
  |>  Seq.map (fun f -> ((System.IO.Path.GetDirectoryName f) @@ "bin" @@ configuration, outDir @@ (System.IO.Path.GetFileNameWithoutExtension f)))
  |>  Seq.iter (fun (fromDir, toDir) -> CopyDir toDir fromDir (fun _ -> true))
)

// --------------------------------------------------------------------------------------
// Clean build results

Target "Clean" (fun _ ->
  CleanDirs [outDir; "temp"]
  !! ("./src/**/bin" @@ configuration)
  |> CleanDirs
)

// --------------------------------------------------------------------------------------
// Build library & test project

let isTravisCI = (environVarOrDefault "TRAVIS" "") = "true"

Target "Build" (fun _ ->
  !! solutionFile
  |> MSBuild "" "Rebuild" [ ("Configuration", configuration) ]
  |> ignore
)

Target "Build.NETCore" (fun _ ->

  let args = [ sprintf "/p:Version=%s" release.NugetVersion ]

  let net40Project = "src/Diff.Match.Patch.NET40/Diff.Match.Patch.NET40.fsproj"
  let net45Project = "src/Diff.Match.Patch.NET45/Diff.Match.Patch.NET45.fsproj"
  let netCoreProject = "src/Diff.Match.Patch.NETCore/Diff.Match.Patch.NETCore.fsproj"

  DotNetCli.Restore (fun p ->
    { p with
        Project = net40Project
    }
  )
  DotNetCli.Build (fun p ->
    { p with
        Project = net40Project
        Configuration = configuration
        AdditionalArgs = args
    }
  )

  DotNetCli.Restore (fun p ->
    { p with
        Project = net45Project
    }
  )
  DotNetCli.Build (fun p ->
    { p with
        Project = net45Project
        Configuration = configuration
        AdditionalArgs = args
    }
  )

  DotNetCli.Restore (fun p ->
    { p with
        Project = netCoreProject
    }
  )
  DotNetCli.Build (fun p ->
    { p with
        Project = netCoreProject
        Configuration = configuration
        AdditionalArgs = args
    }
  )
)

// --------------------------------------------------------------------------------------
// Run the unit tests using test runner
Target "RunTests" (fun _ ->
  !! testAssemblies
  |> Persimmon id
)

Target "RunTests.NETCore" (fun _ ->
  ()
)

#if MONO
#else
// --------------------------------------------------------------------------------------
// SourceLink allows Source Indexing on the PDB generated by the compiler, this allows
// the ability to step through the source code of external libraries https://github.com/ctaggart/SourceLink

Target "SourceLink" (fun _ ->
    let baseUrl = sprintf "%s/%s/{0}/%%var2%%" gitRaw project
    !! "src/**/*.??proj"
    -- "src/**/*.shproj"
    |> Seq.iter (fun projFile ->
        let proj = VsProj.LoadRelease projFile
        SourceLink.Index proj.CompilesNotLinked proj.OutputFilePdb __SOURCE_DIRECTORY__ baseUrl
    )
)

#endif

// --------------------------------------------------------------------------------------
// Build a NuGet package

Target "NuGet" (fun _ ->

  NuGet (fun p ->
    {
      p with
        OutputPath = outDir
        WorkingDir = outDir
        Version = release.NugetVersion
        ReleaseNotes = toLines release.Notes
    }
  ) "src/Diff.Match.Patch.nuspec"
)

Target "PublishNuget" (fun _ ->
  Paket.Push(fun p ->
    { p with
        WorkingDir = outDir })
)

#load "paket-files/build/fsharp/FAKE/modules/Octokit/Octokit.fsx"
open Octokit

Target "Release" (fun _ ->
    StageAll ""
    Git.Commit.Commit "" (sprintf "Bump version to %s" release.NugetVersion)
    Branches.push ""

    Branches.tag "" release.NugetVersion
    Branches.pushTag "" "origin" release.NugetVersion

    // release on github
    createClient (getBuildParamOrDefault "github-user" "") (getBuildParamOrDefault "github-pw" "")
    |> createDraft gitOwner gitName release.NugetVersion (release.SemVer.PreRelease <> None) release.Notes
    // TODO: |> uploadFile "PATH_TO_FILE"
    |> releaseDraft
    |> Async.RunSynchronously
)

Target "NETCore" DoNothing

Target "All" DoNothing

"Clean"
  ==> "AssemblyInfo"
  =?> ("Build", not isTravisCI)
  =?> ("CopyBinaries", not isTravisCI)
  =?> ("RunTests", not isTravisCI)
  =?> ("NETCore", isDotnetInstalled)
  ==> "All"

"Build.NETCore"
  ==> "RunTests.NETCore"
  ==> "NETCore"

"All"
#if MONO
#else
  =?> ("SourceLink", Pdbstr.tryFind().IsSome )
#endif
  ==> "NuGet"

"NuGet"
  ==> "PublishNuget"
  ==> "Release"

RunTargetOrDefault "All"
