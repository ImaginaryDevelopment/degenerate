// Learn more about F# at http://fsharp.org
open System

open Helpers
open Schema



[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
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
    |> printfn "%s"

    0 // return an integer exit code
