#r @"packages/build/FAKE/tools/FakeLib.dll"
open Fake
open Fake.Git
open Fake.AssemblyInfoFile
open Fake.ReleaseNotesHelper
open System
open System.IO

let configuration = getBuildParamOrDefault "configuration" "Release"

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

  DotNetCli.Restore (fun p ->
    { p with
        Project = solutionFile
    }
  )

  !! solutionFile
  |> MSBuild "" "Rebuild" [ ("Configuration", configuration) ]
  |> ignore
)

Target "RunTests" (fun _ ->
  DotNetCli.Test (fun p -> { p with Project = "./tests/Diff.Match.Patch.Tests" })
)


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

Target "All" DoNothing

"Clean"
  ==> "Build"
  ==> "CopyBinaries"
  ==> "RunTests"
  ==> "All"

"All"
  ==> "NuGet"

"NuGet"
  ==> "PublishNuget"
  ==> "Release"

RunTargetOrDefault "All"
