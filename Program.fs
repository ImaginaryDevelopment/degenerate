// Learn more about F# at http://fsharp.org
open System

open Helpers
open Schema
open Helpers.FS


[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    let target = Environment.GetEnvironmentVariable("Degenerate_Target") |> Option.ofValueString
    let mutStr name = {Name=name;PropType="string";FinalMutability=Mutable}
    let mutInt name = {Name=name;PropType="int";FinalMutability=Mutable}
    let imStr name = {Name=name;PropType="string";FinalMutability=Immutable}
    let toGen = [
      { Name="Powerset"
        Properties=[
          imStr "TestImmute"
          mutStr "ATClass"
          mutStr "Description"
          mutStr "DisplayName"
          mutStr "FullName"
        ]
      }
    ]
    genAll toGen.[0] "HeroDesigner.Schema" (fun i x ->
      sprintf "%s%s" (String.replicate i "    ") x
    )
    |> delimit "\r\n"
    |> fun text ->
      match target |> Option.map adaptPath with
      | None -> printfn "%s" text
      // accept only a full path with file name for now,
      // consider generating file name and allowing just a dirpath
      | Some (ParentDir (DirExists _) as t) ->
        printfn "It exists %s" t
        IO.File.WriteAllText(t,text)
      | Some (ParentDir t) ->
        eprintfn "Could not find parent %s" t
      | Some x -> eprintfn "Could not find %s" x

    0 // return an integer exit code
